{-# OPTIONS_GHC -Wno-type-defaults -Wno-unused-top-binds #-}
module Config.ManageHook (manageHook, scratchpads) where

import XMonad hiding (manageHook, borderWidth)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsManageHook)
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Util.CenterRationalRect
import XMonad.Util.NamedScratchpad hiding (name, query, hook)
import XMonad.Util.WindowProperties (getProp32s)
import XMonad.StackSet (floating)

import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.StackSet as W

import Config.Dimensions
import Data.List (singleton)
import Data.Map (member)
import Data.Maybe (isJust)
import Data.Ratio

manageHook :: ManageHook
manageHook = composeAll
  [ namedScratchpadManageHook scratchpads
  , ewmhDesktopsManageHook
  , classNameIn dcClassNames --> moveToWorkspace "chat"
  , classNameIn mediaClassNames --> moveToWorkspace "media"
  , className =? "Gimp" --> moveToWorkspace "gimp"
  , composeOne $ hookClassNames centerFloat floatClassNames
              ++ hookPropValues centerFloat floatPropValues
              ++ hookClassNames (moveToWorkspace "games") gameClassNames
              ++ [ isTile -?> insertPosition Master Newer
                 , isDialog -?> centerFloat ]
  ]
  where
    floatClassNames = ["Pqiv", "sun-awt-X11-XFramePeer", "Udiskie", "fzfmenu"]
    mediaClassNames = ["Spotify", "plexmediaplayer"]
    gameClassNames = ["steam"]
    dcClassNames = ["discord"]
    floatPropValues =
      [ ("WM_WINDOW_ROLE", "GtkFileChooserDialog")
      , ("WM_WINDOW_ROLE", "gimp-message-dialog")
      , ("WM_WINDOW_ROLE", "gimp-toolbox-color-dialog")
      , ("WM_WINDOW_ROLE", "gimp-query-box")
      , ("WM_WINDOW_ROLE", "file-png") ]

scratchpads :: [NamedScratchpad]
scratchpads = singleton $ NS name command query hook
  where command = "st -n scratch -t scratch -e tmux new -A -s scratch"
        hook = customFloating $ centerIRectOffsetY panelHeight tw th sw sh
        (tw, th) = (columnsToWindowWidth 150, linesToWindowHeight 40)
        (sw, sh) = (screenWidth, screenHeight)
        query = appName =? "scratch"
        name = "term"

-- | Properly center a floating window in the available screen real estate.
centerFloat :: ManageHook
centerFloat = doFloatDep $ \(W.RationalRect _ _ widthRatio heightRatio) ->
  let addedWidthRatio = widthRatio + 2 * (borderWidth % screenWidth)
      addedHeightRatio = heightRatio + 2 * (borderWidth % screenHeight)
      panelHeightRatio = panelHeight % screenHeight
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
-- given list to @hook@.
hookClassNames :: ManageHook -> [String] -> [MaybeManageHook]
hookClassNames hook = map ((-?> hook) . (className =?))

-- | Creates a list of MaybeManageHooks by querying for the presence of
-- properties from a given list and maybe-hooking them to @hook@.
hookPropExists :: ManageHook -> [String] -> [MaybeManageHook]
hookPropExists hook = map ((-?> hook) . prop32Exists)
  where prop32Exists prop = fmap isJust $ ask >>= liftX . getProp32s prop

-- | Creates a list of MaybeManageHooks by querying for property values from a
-- list of expected property-value pairs and maybe hooking them to @hook@.
hookPropValues :: ManageHook -> [(String, String)] -> [MaybeManageHook]
hookPropValues hook = map (\(prop,val) -> stringProperty prop =? val -?> hook)

-- | Determines whether the current window's class name is a member of a given
-- list of possible ones.
classNameIn :: [String] -> Query Bool
classNameIn = foldr1 (<||>) . map (className =?)

-- | Determines whether the current window is floating.
isFloat :: Query Bool
isFloat = ask >>= \w -> liftX . withWindowSet $ return . member w . floating

-- | Determines whether the current window is being tiled.
isTile :: Query Bool
isTile = fmap not isFloat
