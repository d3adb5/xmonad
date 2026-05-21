{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ScreenFloatBorders
-- Description :  Hide borders of screen-filling floats without restoring on
--                detile.
-- License     :  BSD-style
--
-- A drop-in replacement for @lessBorders OnlyScreenFloat@ from
-- "XMonad.Layout.NoBorders" that avoids a border flash when a window leaves
-- the screen-filling-float state by being re-tiled (e.g. exiting EWMH
-- fullscreen via the default @doSink@ unfullscreen hook).
--
-- The upstream 'lessBorders' modifier sits outside the rest of the layout
-- stack and restores the configured border width on any tracked window that
-- is no longer hidden. Because the outer modifier's @redoLayout@ runs /after/
-- inner border modifiers like 'XMonad.Layout.VoidBorders.voidBorders', that
-- restoration overrides them and the user briefly sees the default border on
-- borderless layouts.
--
-- This modifier behaves identically for windows that remain floating, but
-- delegates to the inner layout for windows that are now tiled: the inner
-- modifier (typically 'voidBorders' or 'normalBorders') is the authority on
-- their border width.
--
-----------------------------------------------------------------------------

module XMonad.Layout.ScreenFloatBorders ( screenFloatBorders ) where

import XMonad
import XMonad.Layout.LayoutModifier

import qualified Data.Map               as M
import qualified XMonad.StackSet        as W
import qualified XMonad.Util.Rectangle  as R

newtype ScreenFloatBorders a = ScreenFloatBorders [Window] deriving (Read, Show)

instance LayoutModifier ScreenFloatBorders Window where
  modifierDescription _ = "ScreenFloatBorders"

  redoLayout (ScreenFloatBorders prev) lr _ wrs = do
    wset <- gets windowset
    let flt   = W.floating wset
        scrs  = [ scr | scr <- W.screens wset
                      , screenRect (W.screenDetail scr) `R.supersetOf` lr ]
        now   = [ w
                | scr <- scrs
                , let sr = screenRect (W.screenDetail scr)
                , w  <- W.integrate' . W.stack . W.workspace $ scr
                , Just wr <- [M.lookup w flt]
                , scaleRationalRect sr wr `R.supersetOf` sr
                ]
    mapM_ (setBorder 0) now
    bw <- asks (borderWidth . config)
    -- Only restore borders for windows still floating. If a tracked window has
    -- become tiled, the inner layout's border modifier owns it.
    mapM_ (setBorder bw)
      [ w | w <- prev, w `notElem` now, w `M.member` flt ]
    return (wrs, Just $ ScreenFloatBorders now)

  unhook (ScreenFloatBorders prev) = do
    bw <- asks (borderWidth . config)
    mapM_ (setBorder bw) prev

setBorder :: Dimension -> Window -> X ()
setBorder bw w = withDisplay $ \d -> io $ setWindowBorderWidth d w bw

screenFloatBorders :: l Window -> ModifiedLayout ScreenFloatBorders l Window
screenFloatBorders = ModifiedLayout (ScreenFloatBorders [])
