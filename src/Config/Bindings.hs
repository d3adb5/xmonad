module Config.Bindings (keyBindings, mouseBindings, removedBindings) where

import XMonad hiding (mouseResizeWindow, mouseBindings)
import XMonad.Actions.CopyWindow (copy, kill1)
import XMonad.Actions.CycleWS
import XMonad.Actions.DmenuWorkspaces
import XMonad.Actions.DynamicWorkspaceOrder (withNthWorkspace')
import XMonad.Actions.FlexibleResize (mouseResizeWindow)
import XMonad.Actions.FloatKeys (keysMoveWindowTo)
import XMonad.Actions.Hidden
import XMonad.Actions.PhysicalScreens
import XMonad.Layout.BoringWindows (focusDown, focusUp)
import XMonad.Util.DmenuPrompts (windowsMenu, windowTagsMenuArgs)
import XMonad.Util.NamedScratchpad
import XMonad.Util.WindowTags

import Control.Monad (when)

import qualified Config.ManageHook as MH
import qualified XMonad.StackSet as W

keyBindings :: [(String, X ())]
keyBindings =
  [ ("M-]", ask >>= spawn . terminal . config)
  , ("M-j", focusDown)
  , ("M-k", focusUp)
  , ("M-<R>", moveTo Next (WSIs . return $ (/= "NSP") . W.tag))
  , ("M-<L>", moveTo Prev (WSIs . return $ (/= "NSP") . W.tag))
  , ("M-C-t", withFocused addWindowTagMenu)
  , ("M-S-t", withFocused removeWindowTagMenu)
  , ("M-a", windowsMenu "fzfmenu" >>= windows . W.focusWindow)
  , ("M-y", selectWorkspace' "fzfmenu" fzfmenuArgsSelect)
  , ("M-u", renameWorkspace' "fzfmenu" fzfmenuArgsRename)
  , ("M-i", removeWorkspaceIfEmpty)
  , ("M-M1-h", withFocused hideWindow)
  , ("M-M1-j", withFocused swapWithNextHidden)
  , ("M-M1-k", withFocused swapWithLastHidden)
  , ("M-M1-l", withLastHidden unhideWindow)
  , ("M-[", namedScratchpadAction MH.scratchpads "term")
  , ("M-C-y", chooseWorkspace >>= windows . copy)
  , ("M-<Backspace>", kill1)
  , ("M-S-y", withFocused $ moveToWorkspace' "fzfmenu" fzfmenuArgsSelect)
  , ("M-M1-c", withFocused $ keysMoveWindowTo (681,392) (1/2,1/2))
  ] ++ [ ("M-" ++ show n, withNthWorkspace' notNSP W.view (n - 1))
         | n <- [1..9] ]
    ++ [ ("M-C-" ++ show n, withNthWorkspace' notNSP copy (n - 1))
         | n <- [1..9] ]
    ++ [ ("M-S-" ++ show n, withNthWorkspace' notNSP W.shift (n - 1))
         | n <- [1..9] ]
    ++ [ ("M-" ++ key, viewScreen def sc)
         | (key, sc) <- zip ["w", "e", "r"] [0..] ]
    ++ [ ("M-S-" ++ key, sendToScreen def sc)
         | (key, sc) <- zip ["w", "e", "r"] [0..] ]

removedBindings :: [String]
removedBindings = [ "M-p", "M-S-r" ]

mouseBindings :: [((ButtonMask, Button), Window -> X ())]
mouseBindings = [((mod4Mask, button3), \w -> focus w >> mouseResizeWindow w)]

notNSP :: [WorkspaceId] -> [WorkspaceId]
notNSP = filter (/= "NSP")

fzfmenuArgs :: [String]
fzfmenuArgs = [ "--print-query", "--reverse", "+m" ]

fzfmenuArgsSelect :: [String]
fzfmenuArgsSelect = fzfmenuArgs ++ [ "--prompt", "'Workspace: '" ]

fzfmenuArgsRename :: [String]
fzfmenuArgsRename = fzfmenuArgs ++ [ "--prompt", "'New workspace name: '" ]

fzfmenuArgsAddTag :: [String]
fzfmenuArgsAddTag = fzfmenuArgs ++ [ "--prompt", "' New window tag: '" ]

fzfmenuArgsRemoveTag :: [String]
fzfmenuArgsRemoveTag = fzfmenuArgs ++ [ "--prompt", "' Remove window tag: '" ]

addWindowTagMenu :: Window -> X ()
addWindowTagMenu w = do
  selection <- windowTagsMenuArgs "fzfmenu" fzfmenuArgsAddTag w
  when (not $ null selection) $
    addWindowTag w selection

removeWindowTagMenu :: Window -> X ()
removeWindowTagMenu w = do
  selection <- windowTagsMenuArgs "fzfmenu" fzfmenuArgsRemoveTag w
  when (not $ null selection) $
    removeWindowTag w selection
