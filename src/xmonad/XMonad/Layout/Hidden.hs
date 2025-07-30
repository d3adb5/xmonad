{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module XMonad.Layout.Hidden
  ( hidden
  , hideNAt
  ) where

import XMonad
import XMonad.Actions.Hidden
import XMonad.Util.Hidden
import XMonad.Layout.LayoutModifier

import qualified XMonad.StackSet as W
import qualified XMonad.Layout.BoringWindows as BW

import Control.Monad (when)
import Data.List (intersect)
import Data.Map.Strict (member, notMember)
import Safe (headMay)

data HiddenL a = HiddenL         deriving (Read, Show)
data HideNAt a = HideNAt Int Int deriving (Read, Show)

type PairedLayout l = ModifiedLayout HiddenL (ModifiedLayout HideNAt l) Window

hidden :: l Window -> ModifiedLayout HiddenL l Window
hidden = ModifiedLayout HiddenL

hideNAt :: Int -> Int -> l Window -> PairedLayout l
hideNAt n at = hidden . ModifiedLayout (HideNAt n at)

instance LayoutModifier HiddenL Window where
  modifierDescription = const "Hidden"

  modifyLayout HiddenL ws r = do
    stackFilter <- pure W.filter <*> flip notElem <$> getHiddenIn (W.tag ws)
    runLayout (ws { W.stack = W.stack ws >>= stackFilter }) r

  handleMess HiddenL m
    | Just BW.UpdateBoring <- fromMessage m = do
        currentWS <- gets $ W.workspace . W.current . windowset
        boringMessage <- BW.Replace "Hidden" <$> getAllHidden
        sendMessageWithNoRefresh boringMessage currentWS
        return Nothing
    | otherwise = return Nothing

instance LayoutModifier HideNAt Window where
  modifierDescription (HideNAt n at) = "Hide " ++ show n ++ " at " ++ show at

  modifyLayout (HideNAt n at) ws rect = do
    floatingW <- withWindowSet $ return . W.floating
    hiddenW <- getHiddenIn $ W.tag ws
    realStack <- workspaceStackByTag $ W.tag ws
    let visibleW = W.integrate' $ W.filter visibleFilter =<< W.stack ws
        visibleFilter w = notElem w hiddenW && notMember w floatingW
        willHideW = length visibleW > at
        willUnhideW = not (null hiddenW) && length visibleW < at
        unhideTarget = peekOn R hiddenW
        unhiddenTiles = W.integrate' realStack `intersect` (unhideTarget : visibleW)
        stackToBeTiled | willHideW = deleteNthElem (n - 1) (W.stack ws)
                       | willUnhideW = W.differentiate unhiddenTiles
                       | otherwise = W.stack ws
    when willUnhideW $ do
      unhideWindowThen unhideTarget (pure ())
      when (headMay unhiddenTiles == Just unhideTarget) $
        promoteInStack (W.tag ws) unhideTarget
    when willHideW $ hideWindowInThen L (visibleW !! (n - 1)) (W.tag ws) (pure ())
    runLayout (ws { W.stack = stackToBeTiled }) rect

deleteNthElem :: Int -> Maybe (W.Stack a) -> Maybe (W.Stack a)
deleteNthElem n s = let (start, end) = splitAt n $ W.integrate' s
  in W.differentiate $ take (n - 1) start ++ end

-- | Move a window to the top of its workspace's tiled stack, without a refresh
-- and preserving the focused window. Floating windows stay in place.
promoteInStack :: WorkspaceId -> Window -> X ()
promoteInStack t win = do
  floats <- withWindowSet $ return . W.floating
  let reorder rest = let (fs, ts) = span (`member` floats) rest in fs ++ win : ts
      promote ms = ms >>= \s ->
        if win `elem` W.integrate s
          then refocus (W.focus s) (reorder (filter (/= win) (W.integrate s)))
          else Just s
      refocus f xs = case break (== f) xs of
        (ups, x:downs) -> Just (W.Stack x (reverse ups) downs)
        _              -> W.differentiate xs
  runOnWorkspaces $ \w -> return $
    if W.tag w == t then w { W.stack = promote (W.stack w) } else w
