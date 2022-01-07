module State (
  State,
  getCurPath,
  getCurTree,
  getSelected,
) where

import FileTree (FileTree, getFilePath)
  
data State = State FileTree FileTree

getCurPath :: State -> FilePath
getCurPath (State tree _) = getFilePath tree

getCurTree :: State -> FileTree
getCurTree (State tree _) = tree

getSelected :: State -> FileTree
getSelected (State _ tree) = tree
