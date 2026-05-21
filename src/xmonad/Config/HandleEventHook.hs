module Config.HandleEventHook (handleEventHook) where

import XMonad hiding (handleEventHook)

import XMonad.Util.WindowTags (cleanWindowTagsEventHook)
import qualified XMonad.Util.Hacks as Hacks

import Control.Monad (when)
import Data.Semigroup (All(..))

handleEventHook :: Event -> X All
handleEventHook
    = Hacks.windowedFullscreenFixEventHook
  <+> Hacks.fixSteamFlicker
  <+> cleanWindowTagsEventHook
  <+> raiseFullscreenEventHook
  <+> defaultHEHook

-- | When a window requests _NET_WM_STATE_FULLSCREEN, raise it above all
-- siblings via XRaiseWindow. XMonad's restackWindows only orders managed
-- windows relative to each other, so unmanaged windows (e.g. stalonetray)
-- can still sit above the fullscreen window without this.
raiseFullscreenEventHook :: Event -> X All
raiseFullscreenEventHook ClientMessageEvent
  { ev_window      = w
  , ev_message_type = mt
  , ev_data        = action : prop1 : _
  } = do
  netWmState <- getAtom "_NET_WM_STATE"
  fullscreen  <- getAtom "_NET_WM_STATE_FULLSCREEN"
  when (mt == netWmState
     && fromIntegral prop1 == fullscreen
     && action `elem` [1, 2]) $   -- 1 = add, 2 = toggle
    withDisplay $ \dpy -> io $ raiseWindow dpy w
  return (All True)
raiseFullscreenEventHook _ = return (All True)

defaultHEHook :: Event -> X All
defaultHEHook = const $ return (All True)
