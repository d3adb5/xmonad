{-# LANGUAGE DisambiguateRecordFields #-}

module Util.Scratch (Scratch(..), toggleScratch, scratchManageHook) where

import XMonad
import XMonad.Util.CenterRationalRect

import qualified XMonad.Util.NamedScratchpad as NS
import qualified XMonad.StackSet as W

import Config.Dimensions
import Control.Monad (when, filterM)
import Data.List (find, (\\))

-- | A named scratchpad, allowing for fixed window geometry.
data Scratch = Scratch
  { name      :: String
  , command   :: String
  , query     :: Query Bool
  , width     :: Int
  , height    :: Int
  , ephemeral :: Bool }

-- | Convert a `Scratch` into a `NS.NamedScratchpad`.
toNamedScratchpad :: Scratch -> NS.NamedScratchpad
toNamedScratchpad ns = NS.NS
  { name  = name ns
  , cmd   = command ns
  , query = query ns
  , hook  = scratchWindowHook ns }

-- | Get a list of `Window` that match a `Scratch`'s `query`, on and off screen.
screenMatches :: Scratch -> X ([Window], [Window])
screenMatches scratch = withWindowSet $ \wset -> do
  off <- filterM (runQuery (query scratch)) (W.allWindows wset \\ W.index wset)
  on <- filterM (runQuery (query scratch)) (W.index wset)
  return (on, off)

-- | Calculate a `W.RationalRect` desired by a `Scratch`.
scratchRect :: Scratch -> X W.RationalRect
scratchRect scratch = let (w, h) = (width scratch, height scratch) in
  uncurry (centerIRectOffsetY panelHeight w h) <$> screenDimsM

-- | Derive the `ManageHook` for a `Scratch`.
scratchWindowHook :: Scratch -> ManageHook
scratchWindowHook scratch = liftX (scratchRect scratch) >>= NS.customFloating

-- | `ManageHook` for these scratchpads.
scratchManageHook :: [Scratch] -> ManageHook
scratchManageHook = NS.namedScratchpadManageHook . fmap toNamedScratchpad

-- | Toggle a `Scratch` from a `[Scratch]` on the current screen.
toggleScratch :: [Scratch] -> String -> X ()
toggleScratch ss tgt = whenJust (find ((== tgt) . name) ss) $ toggleScratch' nss
  where nss = toNamedScratchpad <$> ss

toggleScratch' :: [NS.NamedScratchpad] -> Scratch -> X ()
toggleScratch' nss scratch = do
  (rect, (onsc, offsc)) <- (,) <$> scratchRect scratch <*> screenMatches scratch
  when (not $ null offsc) $ do
    windows $ \wset -> foldr (flip W.float rect) wset offsc
  NS.namedScratchpadAction nss (name scratch)
  when (null offsc && ephemeral scratch) $ do
    sequence_ $ map killWindow onsc
