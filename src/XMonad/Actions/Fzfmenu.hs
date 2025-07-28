module XMonad.Actions.Fzfmenu
  ( selectWorkspace
  , moveToWorkspace
  , copyToWorkspace
  , renameWorkspace
  , removeWorkspace
  , removeWorkspaceIfEmpty
  , removeWorkspaceWhen
  , addWindowTag
  , removeWindowTag
  , selectWindow
  ) where

import XMonad hiding (workspaces)
import XMonad.StackSet hiding (filter)
import XMonad.Util.MenuPrompts
import XMonad.Actions.CopyWindow (copyWindow)
import XMonad.Actions.DynamicWorkspaceOrder (updateName, removeName)

import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Util.WindowTags as WT

import Control.Monad (when, join)
import Data.List (find)

-- | Switches to a workspace given its tag. Will create it if not present.
goToWorkspace :: String -> X ()
goToWorkspace "" = return ()
goToWorkspace ws = do
  workspaceExists <- tagMember ws <$> gets windowset
  if workspaceExists
    then windows $ greedyView ws
    else DW.addWorkspace ws

-- | Moves a window to a workspace given its tag. Will create it if not present.
sendToWorkspace :: Window -> String -> X ()
sendToWorkspace _ "" = return ()
sendToWorkspace w ws = do
  whenX (not . tagMember ws <$> gets windowset) $
    DW.addWorkspace ws
  windows $ greedyView ws . shiftWin ws w

selectWindow :: X ()
selectWindow = chooseWindow fzfMenu fzfWindowArgs >>= windows . focusWindow

addWindowTag :: Window -> X ()
addWindowTag w = do
  selection <- chooseWindowTag fzfMenu fzfAddTagArgs w
  when (not $ null selection) $
    WT.addWindowTag w selection

removeWindowTag :: Window -> X ()
removeWindowTag w = do
  selection <- chooseWindowTag fzfMenu fzfDelTagArgs w
  when (not $ null selection) $
    WT.removeWindowTag w selection

selectWorkspace :: X ()
selectWorkspace = chooseWorkspace fzfMenu fzfSelectArgs >>= goToWorkspace

-- | Move a window to a user-selected workspace.
moveToWorkspace :: Window -> X ()
moveToWorkspace w = chooseWorkspace fzfMenu fzfSelectArgs >>= sendToWorkspace w

-- | Copy a window to a user-selected workspace.
copyToWorkspace :: Window -> X ()
copyToWorkspace w = chooseWorkspace fzfMenu fzfSelectArgs >>= windows . copyWindow w

-- | Renames the current workspace by prompting the user for a new workspace
-- tag. No entries are given, as renaming should not be picked from a list of
-- already used tags.
renameWorkspace :: X ()
renameWorkspace = runMenu fzfMenu fzfRenameArgs [] >>= renameCurrentWorkspace

-- | Update the name of the current workspace in the window set.
setCurrentTag :: WorkspaceId -> WindowSet -> WindowSet
setCurrentTag t ws = ws
  { current = (current ws) {workspace = (workspace $ current ws) {tag = t}} }

-- | Sets the current workspace's tag to the given string. If another workspace
-- already exists with the desired name, that workspace will be removed and its
-- windows will be brought to the current workspace's stack.
renameCurrentWorkspace :: WorkspaceId -> X ()
renameCurrentWorkspace "" = return ()
renameCurrentWorkspace ws = do
  gets (currentTag . windowset) >>= flip updateName ws
  windows $ setCurrentTag ws . removeWorkspaceWithoutRefresh ws

removeWorkspace :: X ()
removeWorkspace = removeWorkspaceWhen (const True)

removeWorkspaceIfEmpty :: X ()
removeWorkspaceIfEmpty = removeWorkspaceWhen $ null . integrate' . stack

removeWorkspaceWhen :: (WindowSpace -> Bool) -> X ()
removeWorkspaceWhen predicate = do
  ws <- gets $ workspace . current . windowset
  when (predicate ws) $ do
    DW.removeWorkspace
    removeName $ tag ws

removeWorkspaceWithoutRefresh :: WorkspaceId -> WindowSet -> WindowSet
removeWorkspaceWithoutRefresh name wset = newWindowSet
  where targetWS = find ((== name) . tag) $ hidden wset
        currentWS = workspace currentSC
        currentSC = current wset
        newStack = meldStacks (join $ stack <$> targetWS) (stack currentWS)
        newWindowSet = wset
          { hidden = filter ((/= name) . tag) $ hidden wset
          , current = currentSC {workspace = currentWS {stack = newStack}} }

-- | Merges two possibly empty stacks.
meldStacks :: Maybe (Stack a) -> Maybe (Stack a) -> Maybe (Stack a)
meldStacks a b = differentiate $ integrate' a ++ integrate' b

fzfMenu :: String
fzfMenu = "fzfmenu"

fzfCommonArgs :: [String]
fzfCommonArgs = ["--print-query", "--reverse", "+m"]

fzfWindowArgs :: [String]
fzfWindowArgs = withPrompt "Focus window" ["+m"]

withPrompt :: String -> [String] -> [String]
withPrompt prompt args = args ++ ["--prompt", "' " ++ prompt ++ ": '"]

fzfSelectArgs, fzfRenameArgs, fzfAddTagArgs, fzfDelTagArgs :: [String]
fzfSelectArgs = withPrompt "Workspace" fzfCommonArgs
fzfRenameArgs = withPrompt "New workspace name" fzfCommonArgs
fzfAddTagArgs = withPrompt "New window tag" fzfCommonArgs
fzfDelTagArgs = withPrompt "Remove window tag" fzfCommonArgs
