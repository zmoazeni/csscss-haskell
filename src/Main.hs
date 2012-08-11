{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Text.CSS.Parse
import Data.Text (Text, pack)
import Rulesets

main :: IO ()
main = do
  args <- getArgs
  parseFile args
  where showFile [] = printError
        showFile (filePath:_) = readFile filePath >>= putStr
        
        parseFile [] = printError
        parseFile (filePath:_) = do contents <- readFile filePath
                                    either printParseError displayRulesets $ parseBlocks (pack contents)
                                    
        displayRulesets rawRulesets = do let rulesets = map buildRuleset rawRulesets
                                         print $ take 3 rulesets
        printError = putStrLn "Need a file to parse"
        printParseError error = putStrLn $ "Error parsing css: " ++ error

