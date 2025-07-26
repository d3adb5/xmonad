module Config.HandleEventHook (handleEventHook) where

import XMonad hiding (handleEventHook)

import XMonad.Util.WindowTags (cleanWindowTagsEventHook)
import qualified XMonad.Util.Hacks as Hacks

import Data.Semigroup (All(..))

handleEventHook :: Event -> X All
handleEventHook
    = Hacks.windowedFullscreenFixEventHook
  <+> Hacks.fixSteamFlicker
  <+> cleanWindowTagsEventHook
  <+> defaultHEHook

defaultHEHook :: Event -> X All
defaultHEHook = const $ return (All True)
