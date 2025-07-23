module Config.HandleEventHook (handleEventHook) where

import XMonad hiding (handleEventHook)

import qualified XMonad.Util.Hacks as Hacks

import Data.Semigroup (All(..))

handleEventHook :: Event -> X All
handleEventHook
    = Hacks.windowedFullscreenFixEventHook
  <+> Hacks.fixSteamFlicker
  <+> defaultHEHook

defaultHEHook :: Event -> X All
defaultHEHook = const $ return (All True)
