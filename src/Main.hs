module Main where

import System.Environment (getArgs)
import Text.CSS.Parse
import Data.Text hiding (head)

main :: IO ()
main = do
  args <- getArgs
  parseFile args
  where showFile [] = printError
        showFile (filePath:_) = readFile filePath >>= putStr
        
        parseFile [] = printError
        parseFile (filePath:_) = do contents <- readFile filePath
                                    let Right rules = parseBlocks (pack contents)
                                    print $ head rules
          
        printError = putStrLn "Need a file to parse"
        
