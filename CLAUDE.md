# CLAUDE.md

Personal XMonad configuration as a Stack project. `src/xmonad` holds the WM
config (entry point `src/xmonad/xmonad.hs`), `src/xmobar` the bar. The `build`
script is xmonad's custom build hook: `xmonad --recompile` runs it to stack
build the `:xmonad` executable and hard-link it over the binary xmonad execs
on restart. Plain `stack build` is enough to type-check everything.

## Hidden window subsystem

The main layout, `autohideThreeLayout` in `Config/Layouts.hs`, is `hideNAt 2 3`
(from `XMonad.Layout.Hidden`) around a reflected `ThreeColMid`: at most 3
windows are visible; surplus windows are hidden into a per-workspace queue and
brought back automatically as vacancies appear.

- `XMonad.Util.Hidden` — store: `Hidden (Map WorkspaceId (Seq Window))`, a
  *persistent* extension (survives restarts via the state file).
- `XMonad.Actions.Hidden` — hide/unhide/swap actions, `Direction (L | R)`
  selects a queue end. `avoidHiddenFocus` runs in the log hook.
- `XMonad.Layout.Hidden` — `HiddenL` (outer modifier) filters hidden windows
  out of the stack at layout time; `HideNAt n at` (inner) counts visible
  non-floating tiles and hides the n-th / unhides one to keep `at` visible.
- `XMonad.Layout.BoringWindows` integration: hidden windows are marked boring
  via `BW.Replace` on `UpdateBoring` broadcasts, so M-j/M-k skip them.

## Invariants (violating these caused every bug so far)

1. **Hidden windows stay in the real StackSet.** `HideNAt` must fetch hidden
   windows and the true stack order from the window set by tag
   (`workspaceStackByTag`), never from the workspace it is handed — `HiddenL`
   already stripped that one.
2. **Unhide in the same layout pass.** The returning window must be tiled by
   the very pass that notices the vacancy (rebuild the tiled stack from real
   stack order restricted to visible + target); a separate `windows` call
   means a visible flash.
3. **Stack head == master area.** `isMasterTile` and the swap bindings
   (M-M1-j/k) take the head of the non-floating stack, hidden included. When
   the unhidden window takes the master slot (it is first in the tiled order),
   `promoteInStack` moves it to the top of the *tiled* portion of the real
   stack — leading floats stay put, because a dismissed float's focus falls to
   its stack neighbor, which should be the master. Promotion is a pure state
   edit (`runOnWorkspaces`) done *after* computing the pass's tiled order, so
   screen and stack cannot disagree. Never promote a window that fills a side
   slot.
4. **Focus never rests on hidden windows.** Focus inherited from a closed or
   dismissed window lands on stack neighbors, which can be hidden.
   `avoidHiddenFocus` (log hook) bounces such focus to the first visible tile.
   Intentional focus paths unhide first: `Fzfmenu.selectWindow`, and the
   `setEwmhActivateHook` hook in `xmonad.hs` for `_NET_ACTIVE_WINDOW`.
   Never reintroduce unhide-on-focus: it double-unhides against the layout.
5. **Queue ends.** Automatic hide and unhide both use the left end (LIFO): a
   closed master is replaced by the master it displaced. Manual `hideWindow`
   inserts right, so the refresh it triggers cannot immediately reconsume the
   window; `withLastHidden` (M-M1-l) also reads the right end.
6. **Hands off WM_STATE.** Core's `hide`/`reveal` set iconic/normal when they
   unmap/map (`Operations.hs`). Setting it independently, or rewriting
   `_NET_WM_STATE` with read-modify-write, desynchronizes clients (they act
   iconified while visible) and races `ewmhFullscreen`.

## Facts verified in xmonad 0.18 source

- `setFocusX` calls `setInputFocus` without checking viewability; on an
  unmapped window the X server answers BadMatch, which xmonad ignores, so the
  server keeps the old focus while xmonad's model disagrees.
- `Operations.focus` does nothing when the model already deems the window
  focused — pointer events cannot repair the mismatch above; only cycling
  focus away and back re-issues `setFocusX`. This was the "unhidden window
  accepts no input until cycled" symptom.
- `XS`/ExtensibleState keys are `show . typeOf` — the *unqualified* type name.
  Both hidden stores key as `"Hidden"`. On a failed parse of a persisted
  string, `XS.get` **puts `initialValue` back, destroying the stored string**
  on first access.
- `restart "xmonad" True` (also the `--restart` handler) writes the state file
  and execs in one action — an `XS.put` directly before it is atomic with
  respect to hooks.

## State of the work (July 2026)

- `master` carries the fixes on the old global-`Seq` store (commit
  `37ced5d`, "fix: resolve issues with hidden windows").
- This branch (`claude/fix/hidden-layout-issues`) re-implements them on top of
  the old `fix/hidden-layout-issues` branch, whose per-workspace store and
  `Direction` API are better, but which manipulated WM_STATE (removed), unhid
  from the wrong end, tiled via one-pass `prepend`, and relied on
  unhide-on-focus. Six fix commits, then a merge porting all unrelated master
  changes. `HiddenQueueLayout` is deleted.

## Migrating live hidden-window state to this branch

The two stores share the state key `"Hidden"` but not the format, and the new
config destroys the old string on first read (see above), so migrate **from
the running old config, atomically with the restart into the new binary**:

1. On the running config's branch, add a module defining a *bridge* type with
   the same name and shape as the new store, plus a temporary binding:

   ```haskell
   newtype Hidden = Hidden (M.Map WorkspaceId (S.Seq Window))
     deriving (Read, Show)
   instance ExtensionClass Hidden where
     initialValue = Hidden M.empty
     extensionType = PersistentExtension

   migrateHiddenState :: X ()
   migrateHiddenState = do
     old <- XS.gets hiddenWindows        -- the old store's global Seq
     wset <- gets windowset
     let assign w m = maybe m (\t -> M.insertWith (S.><) t (S.singleton w) m)
                        (W.findTag w wset)
     XS.put . Hidden $ foldr assign M.empty (toList old)
     restart "xmonad" True
   ```

   `foldr` + `insertWith (S.><)` preserves left-to-right (newest-first) order
   per workspace, matching invariant 5. Because the bridge type is *named*
   `Hidden`, its `XS.put` lands on the shared key in exactly the format the
   new store's derived `Read` parses.
2. `xmonad --recompile && xmonad --restart` to get the binding live (state is
   still old-format; the old code keeps reading it fine).
3. `git checkout claude/fix/hidden-layout-issues && xmonad --recompile` — this
   installs the new binary but does not restart into it yet.
4. Press the migration binding: state is rewritten and the process execs the
   new binary in one action; the new config reads the migrated map and every
   hidden window stays hidden, queues intact.

Without migration nothing breaks permanently: the hidden set resets, formerly
hidden windows reappear, and the layout re-hides surplus ones on the following
refreshes — but *which* windows are hidden is forgotten.
