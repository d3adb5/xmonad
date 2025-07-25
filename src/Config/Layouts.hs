{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Config.Layouts (layoutHook) where

import XMonad (Full(..), (|||))
import XMonad.Layout.BoringWindows
import XMonad.Layout.Grid
import XMonad.Layout.Hidden
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.SimpleFloat (SimpleFloat(..))
import XMonad.Layout.Spacing hiding (windowBorder)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.VoidBorders
import XMonad.Layout.WindowArranger (windowArrangeAll)
import XMonad.Hooks.ManageDocks (avoidStruts)

import qualified Config.Dimensions as D

layoutHook
  = lessBorders OnlyScreenFloat
  . boringWindows
  . avoidStruts
  . onWorkspaces ["chat"] (gridLayout ||| fullLayout ||| threeLayout)
  . onWorkspaces ["gimp", "media"] (fullLayout ||| floatLayout)
  . onWorkspaces ["games"] (fullLayout ||| gridLayout ||| floatLayout)
  . onWorkspaces ["float"] (floatLayout ||| gridLayout)
  $ threeLayout ||| fullLayout

floatLayout
  = normalBorders
  . windowArrangeAll
  $ SF 0

gridLayout
  = normalBorders Grid

threeLayout
  = reflectHoriz
  . hideNAt 2 3
  . normalBorders
  $ ThreeColMid 1 (2/100) (1/2)

fullLayout
  = voidBorders
  . spacingRaw False (borderAll 0) True windowBorder False
  $ Full

windowBorder :: Border
windowBorder = borderAll D.windowGap

borderAll :: Integer -> Border
borderAll b = Border b b b b
