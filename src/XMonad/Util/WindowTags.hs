--------------------------------------------------------------------------------
-- |
-- Add miscellaneous tags to windows and keep track of them while XMonad is
-- running. Built on top of ExtensibleState.
--------------------------------------------------------------------------------

module XMonad.Util.WindowTags
  ( addWindowTag
  , removeWindowTag
  , getWindowTags
  , hasWindowTag
  , cleanWindowTagsEventHook
  , windowTags
  ) where

import Prelude hiding (lookup)
import Data.Monoid (All(..))
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Set (Set)

import XMonad

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified XMonad.Util.ExtensibleState as XS

type TagIndex = Map Window (Set String)

newtype WindowTagsStorage = WTStorage (Map Window (Set String))
  deriving (Read, Show)

instance ExtensionClass WindowTagsStorage where
  initialValue = WTStorage M.empty
  extensionType = PersistentExtension

fromWTS :: WindowTagsStorage -> Map Window (Set String)
fromWTS (WTStorage tags) = tags

toWTS :: Map Window (Set String) -> WindowTagsStorage
toWTS = WTStorage

liftWTS :: (TagIndex -> TagIndex) -> WindowTagsStorage -> WindowTagsStorage
liftWTS f = toWTS . f . fromWTS

addWindowTag :: Window -> String -> X ()
addWindowTag w t = XS.modify . liftWTS $ M.insertWith S.union w (S.singleton t)

getWindowTags :: Window -> X (Set String)
getWindowTags w = fromMaybe S.empty . M.lookup w . fromWTS <$> XS.get

removeWindowTag :: Window -> String -> X ()
removeWindowTag w t = XS.modify . liftWTS $ M.adjust (S.delete t) w

hasWindowTag :: Window -> String -> X Bool
hasWindowTag w t = S.member t <$> getWindowTags w

cleanWindowTagsEventHook :: Event -> X All
cleanWindowTagsEventHook DestroyWindowEvent{ev_window = wid} = do
  XS.modify . liftWTS $ M.delete wid
  return (All True)
cleanWindowTagsEventHook _ = return (All True)

windowTags :: Query (Set String)
windowTags = ask >>= liftX . getWindowTags
