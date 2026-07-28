-- TODO: fix hideWindow(AndAct) behavior, maybe add hideWindowWith

module XMonad.Actions.Hidden
  ( hideWindow
  , unhideWindow
  , hideWindowAndAct
  , unhideWindowAndAct
  , unhideWindowAndWindows
  , avoidHiddenFocus
  , swapWithLastHidden
  , swapWithNextHidden
  , withLastHidden
  , withNextHidden
  , rightmost
  , leftmost
  ) where

import XMonad
import XMonad.Util.Hidden

import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.StackSet as W

import Control.Monad (when)
import Data.Map (notMember)
import Data.Maybe (listToMaybe)
import qualified Data.Sequence as S

hideWindowAndAct :: InsertFunc -> Window -> X () -> X ()
hideWindowAndAct ins win act =
  whenX (modified $ ins win) $ do
    setHidden win
    act

unhideWindowAndAct :: Window -> X () -> X ()
unhideWindowAndAct win act =
  whenX (modified $ delete win) $ do
    setNotHidden win
    broadcastMessage BW.UpdateBoring
    act

unhideWindowAndWindows :: Window -> (WindowSet -> WindowSet) -> X ()
unhideWindowAndWindows win mws = unhideWindowAndAct win (windows mws)

-- | Append to the right end of the queue, away from the left end where the
-- autohide layout both hides and unhides (most recently hidden first, so a
-- closed master is replaced by its predecessor). Inserting left instead would
-- get the window pulled straight back out by the refresh this action
-- triggers; the right end is also what 'withLastHidden' retrieves.
hideWindow :: Window -> X ()
hideWindow win = hideWindowAndAct (flip (S.|>)) win $ do
  BW.focusDown
  windows $ W.sink win

unhideWindow :: Window -> X ()
unhideWindow win = unhideWindowAndWindows win (W.swapMaster . W.focusWindow win)

swapWithHidden :: Window -> Window -> InsertFunc -> X ()
swapWithHidden uwin hwin insf =
  whenX (not . null <$> getHiddenHere <&&> runQuery isMasterTile uwin) $
    hideWindowAndAct insf uwin $ unhideWindowAndWindows hwin $
      W.swapMaster . W.sink uwin . W.focusWindow hwin

swapWithLastHidden :: Window -> X ()
swapWithLastHidden w = withLastHidden $ flip (swapWithHidden w) (S.<|)

swapWithNextHidden :: Window -> X ()
swapWithNextHidden w = withNextHidden $ flip (swapWithHidden w) (flip (S.|>))

withLastHidden :: (Window -> X a) -> X a
withLastHidden = withHidden . (. rightmost)

withNextHidden :: (Window -> X a) -> X a
withNextHidden = withHidden . (. leftmost)

-- | Hidden windows should never hold focus, but focus inherited from a closed
-- or dismissed window can land on one, since hidden windows remain in the
-- stack. Pass such accidental focus on to the first visible tile (the master)
-- rather than unhiding; intentionally focusing a hidden window should instead
-- go through 'unhideWindow' and friends.
avoidHiddenFocus :: X ()
avoidHiddenFocus = withFocused $ \fwin -> do
  hiddenW <- getHidden
  when (fwin `elem` hiddenW) $ do
    visible <- withWindowSet $ \ws -> return $
      filter (\w -> w `notElem` hiddenW && w `notMember` W.floating ws) (W.index ws)
    whenJust (listToMaybe visible) (windows . W.focusWindow)

-- these shall fail if the sequence is empty, but we expect that never to happen
leftmost  :: (Eq a) => S.Seq a -> a
rightmost :: (Eq a) => S.Seq a -> a
leftmost s  | (l S.:< _) <- S.viewl s = l
            | otherwise = error "empty sequence"
rightmost s | (_ S.:> r) <- S.viewr s = r
            | otherwise = error "empty sequence"

-- this won't fail at all, but yeah, I just needed a delete
delete :: (Eq a) => a -> S.Seq a -> S.Seq a
delete x = S.filter (/= x)

-- a query to determine whether a window is tile AND the master window
isMasterTile :: Query Bool
isMasterTile = ask >>= \win -> liftX . withWindowSet $ \ws ->
  let floats = W.floating ws
      tiled = filter (`notMember` floats) $ W.index ws
      master = head tiled
  in return (win == master)
