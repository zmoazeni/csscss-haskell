{-# LANGUAGE OverloadedStrings #-}
module PrintCSS where

import System.Environment (getArgs)
import Text.CSS.Parse
import Data.Text (Text, pack, unpack)
import Text.PrettyPrint
import Text.Printf

import Rulesets
import RedundancyCalc

main :: IO ()
main = do
  args <- getArgs
  if (null args) then printError
    else do
      errorOrRules <- parseFile args
      case errorOrRules of
        Left e   -> printParseError e
        Right rs -> putStrLn $ render (displayRulesets rs)

  where showFile [] = printError
        showFile (filePath:_) = readFile filePath >>= putStr

        parseFile :: [String] -> IO (Either String [(Text, [(Text, Text)])])
        parseFile (filePath:_) = do contents <- readFile filePath
                                    return $ parseBlocks (pack contents)

        printError = putStrLn "Need a file to parse"
        printParseError error = putStrLn $ "Error parsing css: " ++ error


displayRulesets :: [RawRuleset] -> Doc
displayRulesets rawRulesets = do let rulesets = map buildRuleset rawRulesets
                                     redundantRulesets = take 3 $ matches rulesets
                                 vcat $ concat (map format redundantRulesets)
  where
    format (ruleset, rulesetMatches) = map eachMatch rulesetMatches
      where
        eachMatch rulesetMatch = do let r1 = unpack $ getSelector (snd ruleset)
                                        r2 = unpack $ getMSelector rulesetMatch
                                        num = length $ getMRules rulesetMatch
                                        singOrPlural = if num > 1 then "rules" else "rule" :: String
                                        s = printf "{%s} and {%s} share %d %s" r1 r2 num singOrPlural
                                    text s
