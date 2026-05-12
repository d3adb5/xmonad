--------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.MenuPrompts
-- Description : Prompt presets for any external menu program.
--
-- Maintainer  : d3adb5
-- Stability   : unstable
-- Portability : unportable
--
-- Actions to allow making selections through an external menu, since I don't
-- like XMonad.Prompt and refuse to use it instead of things like dmenu or
-- st+fzf.
--
--------------------------------------------------------------------------------

module XMonad.Util.MenuPrompts
  ( runMenu
  , chooseWindow
  , chooseWorkspace
  , chooseWindowTag
  ) where

import XMonad hiding (workspaces)
import XMonad.StackSet (workspaces, allWindows, tag)
import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)
import XMonad.Util.WindowTags (windowTags)

import Control.Monad (join)
import Data.Char (toUpper)
import Data.List.Split (splitOneOf)
import Data.Set (toList)
import Data.Tuple.Extra (both)
import Safe (lastDef)
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.Process (runInteractiveProcess)

-- | Runs an external command with the given arguments and input, returning the
-- output as a list of lines. The input is fed to the command's stdin.
runProcess :: MonadIO m => FilePath -> [String] -> String -> m [String]
runProcess cmd args input = io $ do
  (pin, pout, perr, _) <- runInteractiveProcess cmd args Nothing Nothing
  hPutStr pin input
  hClose pin
  output <- hGetContents pout
  hClose pout
  hClose perr
  return $ lines output

-- | Runs an external menu program with the given command, arguments, and list
-- of options that are fed through stdin as separate lines. The last line of the
-- output is returned as a single string, with trailing newlines removed.
runMenu :: String -> [String] -> [String] -> X String
runMenu mcmd args opts = filter (/= '\n') . lastDef "" <$>
  runProcess mcmd args (unlines opts)

-- | Prompts the user to choose a workspace from all workspaces in the
-- windowset, with the exception of the "NSP" workspace. Each workspace is fed
-- to the menu program as a separate line.
chooseWorkspace :: String -> [String] -> X String
chooseWorkspace m a = do
  sortedWorkspaces <- getSortByOrder <*> gets (workspaces . windowset)
  runMenu m a . filter (/= "NSP") $ map tag sortedWorkspaces

-- | Prompts the user to choose a window from all the windows in the windowset.
-- Each window is fed to the menu program as a separate line, containing details
-- such as window ID, class, application name, title, and tags. See `menuLine`.
chooseWindow :: String -> [String] -> X Window
chooseWindow m a = do
  options <- join $ mapM (runQuery menuLine) <$> gets (allWindows . windowset)
  read . takeWhile (/= ' ') <$> runMenu m a options

-- | Prompts the user to choose a window tag from the ones associated with the
-- given window. Each window tag is fed to the menu program as a separate line.
chooseWindowTag :: String -> [String] -> Window -> X String
chooseWindowTag m a w = runQuery windowTags w >>= runMenu m a . toList

-- | Queries information about a Window and formats it into a single line for
-- consumption by an external menu program. Messy, but it works.
menuLine :: Query String
menuLine = concatMenuLine <$> sequence [wid, class', app', title, tags]
  where concatMenuLine = foldl1 (concatWithDelimiter " • ")
        (class', app') = both (titlefy <$>) (className, appName)
        tags = foldl (concatWithDelimiter ", ") "" <$> windowTags
        titlefy = unwords . map capitalize . splitOneOf " -_"
        capitalize (h:t) = toUpper h : t
        capitalize [] = []
        wid = show <$> ask

-- | Concatenates two strings with a delimiter, avoiding empty strings.
concatWithDelimiter :: String -> String -> String -> String
concatWithDelimiter _ "" s2 = s2
concatWithDelimiter _ s1 "" = s1
concatWithDelimiter d s1 s2 = s1 ++ d ++ s2
