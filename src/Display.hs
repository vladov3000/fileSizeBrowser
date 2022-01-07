module Display where

import FileTree (FileTree, getFilePath, getChildTrees)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import State (State, getSelected, getCurPath)
import System.Console.ANSI (setCursorPosition, getTerminalSize)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

displayState :: State -> IO ()
displayState state = do
  terminalSizeMaybe <- getTerminalSize
  if isNothing terminalSizeMaybe then getTerminalSizeError
  else do
    let terminalSize = fromJust terminalSizeMaybe
    setCursorPosition (fst terminalSize) 0
    putStrLn $ getCurPath state
  where getTerminalSizeError = hPutStrLn stderr "Failed to get terminal size." >> exitFailure


getSpacings :: FileTree -> Maybe [String]
getSpacings fileTree = (map $ (flip replicate '.') . ((-) maxFilePathLen) . length . getFilePath) <$> (getChildTrees fileTree)
  where maxFilePathLen = getMaxFilePathLen fileTree

getMaxFilePathLen :: FileTree -> Int
getMaxFilePathLen fileTree =
  foldr f pathLen $ fromMaybe [] $ getChildTrees fileTree
  where f x acc = max acc $ getMaxFilePathLen x
        pathLen = length $ getFilePath fileTree

--displayCurFileTree :: FileTree -> IO ()
--displayCurFileTree fileTree = case fileTree of
--  (File _ size (Just childTrees)) -> putStrLn ("." ++ spacing ++ show size) >> go childTrees
--  (File _ _ Nothing)              -> displayFileTree fileTree
--  where go [] = return ()
--        go (x : xs) = displayFileTree x >> go xs
--        spacingLen = fromInteger $ getMaxFilePathLen fileTree
--        spacing = replicate spacingLen ' '
--        displayFileTree (File path size _) = putStrLn $
--          (takeFileName path) ++ spacing ++ show size