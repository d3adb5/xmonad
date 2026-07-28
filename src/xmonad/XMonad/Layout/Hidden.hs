{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module XMonad.Layout.Hidden
  ( hidden
  , hideNAt
  ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.Hidden
import XMonad.Util.Hidden
import XMonad.Util.ExtensibleState as XS
import XMonad.Layout.LayoutModifier
import qualified XMonad.Layout.BoringWindows as BW

import Control.Monad (msum, when)
import qualified Data.Map as M
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Sequence ((<|))
import qualified Data.Sequence as S

data HiddenL a = HiddenL         deriving (Read, Show)
data HideNAt a = HideNAt Int Int deriving (Read, Show)

hidden :: l Window -> ModifiedLayout HiddenL l Window
hidden = ModifiedLayout HiddenL

hideNAt :: Int -> Int -> l Window -> ModifiedLayout HiddenL (ModifiedLayout HideNAt l) Window
hideNAt n at = hidden . ModifiedLayout (HideNAt n at)

instance LayoutModifier HiddenL Window where
  modifierDescription = const "Hidden"

  modifyLayout HiddenL wksp rect = do
    allHidden <- XS.gets hiddenWindows
    let filtered = W.stack wksp >>= W.filter (`notElem` allHidden)
    runLayout (wksp {W.stack = filtered}) rect

  handleMess HiddenL m
    | Just BW.UpdateBoring <- fromMessage m = do
        hiddenW <- XS.gets hiddenWindows
        ws <- XMonad.gets (W.workspace . W.current . windowset)
        flip sendMessageWithNoRefresh ws $ BW.Replace "Hidden" (toList hiddenW)
        return Nothing
    | otherwise = return Nothing

instance LayoutModifier HideNAt Window where
  modifierDescription (HideNAt n at) = "Hide " ++ show n ++ " at " ++ show at

  modifyLayout (HideNAt n at) wksp rect = do
    floating <- withWindowSet $ return . M.keys . W.floating
    realStack <- workspaceStackByTag (W.tag wksp)
    allHidden <- XS.gets hiddenWindows
    let realWindows = W.integrate' realStack
        hiddenW = S.filter (`elem` realWindows) allHidden
        visibleWindows = W.integrate' $ W.filter (`notElem` floating) =<< W.stack wksp
        willHideWindow = length visibleWindows > at
        willUnhideWindow = not (null hiddenW) && length visibleWindows < at
        unhideTarget = leftmost hiddenW
        stackToBeTiled | willHideWindow = deleteNthElem n (W.stack wksp)
                       | willUnhideWindow = W.differentiate $
                         filter (\w -> w `elem` visibleWindows || w == unhideTarget) realWindows
                       | otherwise = W.stack wksp

    when willUnhideWindow $ do
      unhideWindowAndAct unhideTarget (pure ())
      when (listToMaybe (W.integrate' stackToBeTiled) == Just unhideTarget) $
        promoteInStack (W.tag wksp) unhideTarget
    when willHideWindow $ hideWindowAndAct (<|) (visibleWindows !! (n - 1)) (pure ())
    runLayout (wksp { W.stack = stackToBeTiled }) rect

-- | The workspace's stack as it exists in the window set. The stack given to
-- 'modifyLayout' has already been stripped of hidden windows by 'HiddenL', so
-- hidden windows (and the true tiling order) are only found in the real stack.
workspaceStackByTag :: WorkspaceId -> X (Maybe (W.Stack Window))
workspaceStackByTag tag = withWindowSet $ return
  . msum . map W.stack . filter ((== tag) . W.tag) . W.workspaces

-- | Move a window to the top of its workspace's tiled stack, without a refresh
-- and preserving the focused window. Floating windows stay in place.
promoteInStack :: WorkspaceId -> Window -> X ()
promoteInStack tag win = do
  floats <- withWindowSet $ return . W.floating
  let reorder rest = let (fs, ts) = span (`M.member` floats) rest in fs ++ win : ts
      promote ms = ms >>= \s ->
        if win `elem` W.integrate s
          then refocus (W.focus s) (reorder (filter (/= win) (W.integrate s)))
          else Just s
      refocus f xs = case break (== f) xs of
        (ups, x:downs) -> Just (W.Stack x (reverse ups) downs)
        _              -> W.differentiate xs
  runOnWorkspaces $ \w -> return $
    if W.tag w == tag then w { W.stack = promote (W.stack w) } else w

deleteNthElem :: Int -> Maybe (W.Stack a) -> Maybe (W.Stack a)
deleteNthElem n s =
  let listFromStack = W.integrate' s
      (start, end) = splitAt n listFromStack
      nRemoved = take (n - 1) start ++ end
  in W.differentiate nRemoved
