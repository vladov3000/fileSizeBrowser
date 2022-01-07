module FileTree (
  FileTree,
  getFilePath,
  getFileSize,
  getChildTrees,
) where

import System.Directory (getDirectoryContents, makeAbsolute)
import System.PosixCompat (isDirectory, isRegularFile, fileSize, getFileStatus)
import Data.Maybe (catMaybes)
import System.FilePath.Posix (combine)


type FileSize = Integer
data FileTree = FileTree FilePath FileSize (Maybe [FileTree]) deriving Show

getFilePath :: FileTree -> FilePath
getFilePath (FileTree path _ _) = path

getFileSize :: FileTree -> Integer
getFileSize (FileTree _ size _) = size

getChildTrees :: FileTree -> Maybe [FileTree]
getChildTrees (FileTree _ _ childTrees) = childTrees

makeFileTree :: FilePath -> IO (Maybe FileTree)
makeFileTree file = do
  curFile <- makeAbsolute file
  curFileStatus <- getFileStatus curFile
  curFileSize <- return $ toInteger $ fileSize curFileStatus
  if isRegularFile curFileStatus then
    return $ Just $ FileTree curFile curFileSize Nothing
  else if isDirectory curFileStatus then do
    childFiles <- getChildFiles curFile
    childFileTrees <- catMaybes <$> (sequence $ map makeFileTree childFiles)
    let size = foldr (+) curFileSize $ map getFileSize childFileTrees
    return $ Just $ FileTree curFile size $ Just childFileTrees
  else
    return Nothing

getChildFiles :: FilePath -> IO [FilePath]
getChildFiles curFile =
  (map (combine curFile)) <$> (filter isChildFile) <$> getDirectoryContents curFile
  where isChildFile childFile = not $ elem childFile nonChildFiles
        nonChildFiles = [".", ".."]