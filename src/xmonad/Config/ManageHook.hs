{-# OPTIONS_GHC -Wno-type-defaults #-}
module Config.ManageHook (manageHook) where

import XMonad hiding (manageHook, borderWidth)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsManageHook)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Util.CenterRationalRect
import XMonad.StackSet (floating)

import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.StackSet as W

import Data.List.Extra (lower)
import Data.Map (member)
import Data.Ratio

import Config.Dimensions
import Config.Scratchpads (scratchMHook)

manageHook :: ManageHook
manageHook = composeAll
  [ scratchMHook
  , ewmhDesktopsManageHook
  , classNameIn' chatClassNames --> moveToWorkspace "chat"
  , classNameIn' mediaClassNames --> moveToWorkspace "media"
  , className' =? "Gimp" --> moveToWorkspace "gimp"
  , composeOne $ hookClassNames' centerFloat floatClassNames
              ++ hookPropValues centerFloat floatPropValues
              ++ hookClassNames' (moveToWorkspace "games") gameClassNames
              ++ [ isTile -?> insertPosition Master Newer
                 , isDialog -?> centerFloat ]
  ]
  where
    mediaClassNames = ["Spotify", "plexmediaplayer"]
    gameClassNames = ["steam"]
    chatClassNames = ["discord", "mumble"]
    floatPropValues =
      [ ("WM_WINDOW_ROLE", "GtkFileChooserDialog")
      , ("WM_WINDOW_ROLE", "gimp-message-dialog")
      , ("WM_WINDOW_ROLE", "gimp-toolbox-color-dialog")
      , ("WM_WINDOW_ROLE", "gimp-query-box")
      , ("WM_WINDOW_ROLE", "file-png") ]
    floatClassNames =
      [ "Pqiv"
      , "sun-awt-X11-XFramePeer"
      , "Udiskie"
      , "fzfmenu"
      , "mpv" ]

-- | Properly center a floating window in the available screen real estate.
centerFloat :: ManageHook
centerFloat = do
  (scrWidth, scrHeight) <- liftX screenDimsM
  doFloatDep $ \(W.RationalRect _ _ widthRatio heightRatio) ->
    let addedWidthRatio = widthRatio + 2 * (borderWidth % scrWidth)
        addedHeightRatio = heightRatio + 2 * (borderWidth % scrHeight)
        panelHeightRatio = panelHeight % scrHeight
        offsetY | addedHeightRatio <= (1 - panelHeightRatio) = panelHeightRatio
                | otherwise = 0
    in centerRRectOffsetY offsetY addedWidthRatio addedHeightRatio

-- | Shift a window to a given workspace (create it if it doesn't exist) and
-- make it the current workspace.
moveToWorkspace :: WorkspaceId -> ManageHook
moveToWorkspace wkspc = do
  liftX (DW.addHiddenWorkspace wkspc)
  doF (W.greedyView wkspc . W.shift wkspc)

-- | Creates a list of MaybeManageHooks by maybe-hooking class names from a
-- given list to @hook@. Case insensitive.
hookClassNames' :: ManageHook -> [String] -> [MaybeManageHook]
hookClassNames' hook = map ((-?> hook) . (className' =?) . lower)

-- | Creates a list of MaybeManageHooks by querying for property values from a
-- list of expected property-value pairs and maybe hooking them to @hook@.
hookPropValues :: ManageHook -> [(String, String)] -> [MaybeManageHook]
hookPropValues hook = map (\(prop,val) -> stringProperty prop =? val -?> hook)

-- | Determines whether the current window's class name is a member of a given
-- list of possible ones, but insensitive of case.
classNameIn' :: [String] -> Query Bool
classNameIn' = foldr1 (<||>) . map ((className' =?) . lower)

-- | Determines whether the current window is floating.
isFloat :: Query Bool
isFloat = ask >>= \w -> liftX . withWindowSet $ return . member w . floating

-- | Determines whether the current window is being tiled.
isTile :: Query Bool
isTile = fmap not isFloat

-- | Class name, but lowercase, to be used in case insensitive queries.
className' :: Query String
className' = fmap lower className
