module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  showFile args
  where showFile [] = putStrLn "Need a file to parse"
        showFile (filePath:_) = readFile filePath >>= putStr
        
