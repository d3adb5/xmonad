{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
--------------------------------------------------------------------------------
-- |
-- Creates the record that will be stored through X.U.ExtensibleState, as well
-- as some functions to modify it. It aims to simplify and replace X.U.Minimize
-- for my particular setup.
--------------------------------------------------------------------------------

module XMonad.Util.Hidden
  ( Hidden(..)
  , Workspace(..)
  , modifyHiddenHere
  , modifyHiddenIn
  , modifyHiddenHere'
  , modifyHiddenIn'
  , getHiddenHere
  , getHiddenIn
  , getHiddenIn'
  , getAllHidden
  , withHidden
  , currentWorkspace

  , isHidden
  , isHiddenHere
  , isHiddenIn
  , isVisible
  , isVisibleHere
  , isVisibleIn

  , visibleIn
  , visibleTilesIn
  , visibleTilesIn'

  -- miscellaneous helper functions
  , workspaceStackByTag
  , workspaceOf
  , notFloating
  ) where

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad (msum)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S

-- These are really just for brevity in type annotations.
type WindowSeq   = S.Seq Window
type HSWSWinMap  = Map WorkspaceId WindowSeq
type WindowStack = W.Stack Window

-- Generic version of `WindowSpace` for instancing.
type WindowSpace' l = W.Workspace WorkspaceId (l Window) Window

newtype Hidden = Hidden (Map WorkspaceId (S.Seq Window))
  deriving (Eq, Read, Show)

instance ExtensionClass Hidden where
  initialValue = Hidden M.empty
  extensionType = PersistentExtension

-- | A class to encompass types that can yield a 'WorkspaceId'.
class Workspace a where
  tag :: a -> WorkspaceId

instance (LayoutClass l Window) => Workspace (WindowSpace' l) where
  tag = W.tag
instance Workspace WorkspaceId where
  tag = id

-- | Modify the hidden windows in the current workspace with a pure function.
modifyHiddenHere :: (WindowSeq -> WindowSeq) -> X ()
modifyHiddenHere f = modifyHiddenHere' (return . f)

-- | Modify the hidden windows in a given workspace with a pure function.
modifyHiddenIn :: Workspace a => a -> (WindowSeq -> WindowSeq) -> X ()
modifyHiddenIn ws f = modifyHiddenIn' ws (return . f)

-- | Modify the hidden windows in the current workspace through an 'X' action.
modifyHiddenHere' :: (WindowSeq -> X WindowSeq) -> X ()
modifyHiddenHere' act = currentWorkspace >>= flip modifyHiddenIn' act

-- | Modify the hidden windows in the given workspace through an 'X' action.
modifyHiddenIn' :: Workspace a => a -> (WindowSeq -> X WindowSeq) -> X ()
modifyHiddenIn' w a = getHiddenIn w >>= a >>= XS.modify . inH . M.insert (tag w)

-- | Fetch hidden windows in the current workspace, but always return a 'S.Seq'.
getHiddenHere :: X WindowSeq
getHiddenHere = currentWorkspace >>= getHiddenIn

-- | Fetch hidden windows in the current workspace, but always return a 'S.Seq'.
getHiddenIn :: Workspace a => a -> X WindowSeq
getHiddenIn ws = fromMaybe S.empty <$> getHiddenIn' ws

-- | Fetch hidden windows in the given workspace.
getHiddenIn' :: Workspace a => a -> X (Maybe WindowSeq)
getHiddenIn' ws = withHidden $ return . M.lookup (tag ws)

-- | Get all hidden windows across all workspaces as a list.
getAllHidden :: X [Window]
getAllHidden = concat . map toList . M.elems <$> XS.gets fromHidden

-- | Run an action on the map of all hidden windows in a readonly manner.
withHidden :: (HSWSWinMap -> X a) -> X a
withHidden action = XS.gets fromHidden >>= action

-- | Return the current screen's 'WindowSpace'.
currentWorkspace :: X WindowSpace
currentWorkspace = withWindowSet $ return . W.workspace . W.current

-- | Check if a window is hidden in the current workspace.
isHiddenHere :: Window -> X Bool
isHiddenHere w = currentWorkspace >>= flip isHiddenIn w

-- | Check if a window is hidden in the given workspace.
isHiddenIn :: Workspace a => a -> Window -> X Bool
isHiddenIn ws w = withHidden $ return . maybe False (elem w) . M.lookup (tag ws)

-- | Check if a window is hidden in any workspace.
isHidden :: Window -> X Bool
isHidden w = withHidden $ return . M.foldr hasBeenSeen False
  where hasBeenSeen s acc = acc || elem w s

-- | Check if a window is visible in the current workspace.
isVisibleHere :: Window -> X Bool
isVisibleHere w = currentWorkspace >>= flip isVisibleIn w

-- | Check if a window is visible in the given workspace.
isVisibleIn :: Workspace a => a -> Window -> X Bool
isVisibleIn ws w = not <$> isHiddenIn ws w

-- | Check if a window is visible in any workspace.
isVisible :: Window -> X Bool
isVisible w = not <$> isHidden w

-- | Convenient window map extraction from the 'Hidden' type.
fromHidden :: Hidden -> Map WorkspaceId (S.Seq Window)
fromHidden (Hidden m) = m

-- | Run a transformation inside the 'Hidden' type.
inH :: (HSWSWinMap -> HSWSWinMap) -> Hidden -> Hidden
inH f (Hidden m) = Hidden (f m)

-- | The workspace's stack as it exists in the window set.
workspaceStackByTag :: Workspace a => a -> X (Maybe (W.Stack Window))
workspaceStackByTag ws = withWindowSet $ return
  . msum . map W.stack . filter ((== tag ws) . W.tag) . W.workspaces

-- | Find the workspace a window is in, maybe.
workspaceOf :: Window -> X (Maybe WorkspaceId)
workspaceOf w = withWindowSet $ return . W.findTag w

-- | Filter floating windows out of a `Maybe WindowStack`.
notFloating :: Maybe WindowStack -> X (Maybe WindowStack)
notFloating s = do
  floatingWindowMap <- withWindowSet $ return . W.floating
  return $ W.filter (flip M.notMember floatingWindowMap) =<< s

-- | Filter out hidden windows from a given 'WindowStack'.
visibleIn :: Workspace a => a -> X (Maybe WindowStack)
visibleIn ws = do
  workspaceStack <- workspaceStackByTag $ tag ws
  hiddenWindows <- getHiddenIn $ tag ws
  return $ W.filter (flip notElem hiddenWindows) =<< workspaceStack

-- | Get a '[Window]' without floating or hidden windows in a given workspace.
visibleTilesIn :: Workspace a => a -> X [Window]
visibleTilesIn ws = W.integrate' <$> visibleTilesIn' ws

-- | Get a 'WindowStack' w/o floating or hidden windows in a given workspace.
visibleTilesIn' :: Workspace a => a -> X (Maybe WindowStack)
visibleTilesIn' ws = visibleIn ws >>= notFloating
