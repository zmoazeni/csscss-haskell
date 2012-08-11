{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Text.CSS.Parse
import Data.Text (Text, pack)

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
        buildRulesets = map buildRuleset
        buildRuleset (selector, rawRules) = Ruleset selector (buildRules rawRules)
        buildRules = map (uncurry Rule)
        
        printError = putStrLn "Need a file to parse"
        printParseError error = putStrLn $ "Error parsing css: " ++ error

data Rule = Rule {property :: Text, value :: Text}
            deriving (Show)
                     
data Ruleset = Ruleset {selector :: Text, rules :: [Rule]}
             deriving (Show)