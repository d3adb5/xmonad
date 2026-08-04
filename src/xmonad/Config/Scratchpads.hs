module Config.Scratchpads (scratchpads, scratchMHook, toggleScratchpad) where

import XMonad (ManageHook, X)
import XMonad.ManageHook

import Config.Dimensions
import Util.Scratch

scratchpads :: [Scratch]
scratchpads =
  [ Scratch
    { name    = "terminal"
    , command = "st -n scratch -t scratch -e tmux new -A -s scratch"
    , query   = appName =? "scratch"
    , width   = columnsToWindowWidth 150
    , height  = linesToWindowHeight 40
    }
  , Scratch
    { name      = "volume-mixer"
    , command   = "pwvucontrol"
    , query     = appName =? "pwvucontrol"
    , width     = 800
    , height    = 600
    }
  ]

scratchMHook :: ManageHook
scratchMHook = scratchManageHook scratchpads

toggleScratchpad :: String -> X ()
toggleScratchpad = toggleScratch scratchpads
