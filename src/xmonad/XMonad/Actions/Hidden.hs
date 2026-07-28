module XMonad.Actions.Hidden
  ( hideWindow
  , hideWindowThen
  , hideWindowInThen
  , unhideWindow
  , unhideWindowThen
  , unhideWindowThenRefresh
  , unhideWindowInThen
  , avoidHiddenFocus
  , swapWithHidden
  , withLastHidden
  , Direction(..)
  , peekOn
  , peekOn'
  , insertOn
  ) where

import XMonad
import XMonad.Util.Hidden

import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as W

import Data.Map (notMember)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import Safe (headMay)

-- | Simple semantic mapping for 'S.Seq' operations.
data Direction = L | R

-- | Hide a visible window and refresh. Inserts on the right, away from the
-- left end where the autohide layout hides and unhides, so the refresh this
-- action triggers does not pull the window straight back out; the right end
-- is also what 'withLastHidden' retrieves.
hideWindow :: Window -> X ()
hideWindow w = hideWindowThen R w $ do
  BW.focusDown
  windows (W.sink w)

-- | Hide a visible window and perform an 'X' action afterwards.
hideWindowThen :: Direction -> Window -> X () -> X ()
hideWindowThen d w a = currentWorkspace >>= flip (hideWindowInThen d w) a

-- | Hide a visible window and perform an 'X' action afterwards.
hideWindowInThen :: Workspace w => Direction -> Window -> w -> X () -> X ()
hideWindowInThen d w ws action = whenX (not <$> isHidden w) $ do
  modifyHiddenIn ws $ insertOn d w
  action

-- | Unhide a window and swap it with the current master window.
unhideWindow :: Window -> X ()
unhideWindow w = unhideWindowThenRefresh w (W.swapMaster . W.focusWindow w)

-- | Unhide a window, modify the 'WindowSet' and refresh the display.
unhideWindowThenRefresh :: Window -> (WindowSet -> WindowSet) -> X ()
unhideWindowThenRefresh w mws = unhideWindowThen w $ windows mws

-- | Unhide a hidden window and perform an 'X' action afterwards.
unhideWindowThen :: Window -> X () -> X ()
unhideWindowThen w a = currentWorkspace >>= flip (unhideWindowInThen w) a

-- | Unhide a hidden window and perform an 'X' action afterwards.
unhideWindowInThen :: Workspace w => Window -> w -> X () -> X ()
unhideWindowInThen w ws action = whenX (isHidden w) $ do
  modifyHiddenIn ws $ delete w
  broadcastMessage BW.UpdateBoring
  action

-- | Hidden windows should never hold focus, but focus inherited from a closed
-- or dismissed window can land on one, since hidden windows remain in the
-- stack. Pass such accidental focus on to the first visible tile (the master)
-- instead of unhiding: were the hidden window given focus intentionally, it
-- should have gone through 'unhideWindow' and friends. Focus resting on an
-- unmapped window is lost entirely, as the X server rejects the focus request
-- and pointer events cannot reach a window that is not viewable.
avoidHiddenFocus :: X ()
avoidHiddenFocus = withFocused $ \fw ->
  whenX (isHiddenHere fw) $ do
    hiddenW <- getHiddenHere
    visible <- withWindowSet $ \ws -> return $
      filter (\w -> notElem w hiddenW && notMember w (W.floating ws)) (W.index ws)
    whenJust (headMay visible) (windows . W.focusWindow)

-- | Swap the master window with a hidden one.
swapWithHidden :: Direction -> Window -> X ()
swapWithHidden d w = whenX (hiddenHereNotNull <&&> runQuery isMasterTile w) $ do
  target <- peekOn d <$> getHiddenHere
  hideWindowThen (invert d) w $ unhideWindowThenRefresh target $
    W.swapMaster . W.sink w . W.focusWindow target

-- | Perform an action with the last hidden window, if any.
withLastHidden :: (Window -> X ()) -> X ()
withLastHidden a = whenX hiddenHereNotNull $ peekOn R <$> getHiddenHere >>= a

-- | Invert a 'Direction' to its opposite.
invert :: Direction -> Direction
invert L = R
invert R = L

-- | Insert a window into a 'S.Seq' at an end based on the direction.
insertOn :: Direction -> Window -> S.Seq Window -> S.Seq Window
insertOn R = flip (S.|>)
insertOn L = (S.<|)

-- | Peek at the first or last element of a 'S.Seq' unsafely.
peekOn :: Direction -> S.Seq a -> a
peekOn d s = fromMaybe (error "no hidden wins") $ peekOn' d s

-- | Peek at the first or last element of a 'S.Seq' without removing it.
peekOn' :: Direction -> S.Seq a -> Maybe a
peekOn' L s | (l S.:< _) <- S.viewl s = Just l
            | otherwise = Nothing
peekOn' R s | (_ S.:> r) <- S.viewr s = Just r
            | otherwise = Nothing

-- | Delete an element from a 'S.Seq'.
delete :: (Eq a) => a -> S.Seq a -> S.Seq a
delete x = S.filter (/= x)

-- | Whether there are hidden windows in the current workspace.
hiddenHereNotNull :: X Bool
hiddenHereNotNull = not . S.null <$> getHiddenHere

-- A query to determine whether a window is tile AND the master window
isMasterTile :: Query Bool
isMasterTile = ask >>= \win -> liftX . withWindowSet $ \ws -> do
  let tiled = filter (`notMember` W.floating ws) $ W.index ws
  return . fromMaybe False $ (win ==) <$> headMay tiled
