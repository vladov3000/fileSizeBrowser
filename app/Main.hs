module Main where

  

main :: IO ()
main = do
  let initFilePath = "."
  fileTreeMaybe <- makeFileTree initFilePath
  if isNothing fileTreeMaybe then
    hPutStrLn stderr "Failed to build file tree for " ++ initFilePath ++ "."
    exitFailure
  else
    displayCurFileTree fileTree
