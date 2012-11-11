{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit
import Text.CSS.Parse
import Data.Text (pack, unpack)
import Text.PrettyPrint
import Text.Printf
import System.Console.GetOpt
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad
import Text.JSON

import Development.CSSCSS.Rulesets
import Development.CSSCSS.RedundancyCalc

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optShowHelp    :: Bool
 , optNum         :: Maybe Int
 , optJSON        :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions    = Options
 { optVerbose     = False
 , optShowVersion = False
 , optShowHelp    = False
 , optNum         = Just 3
 , optJSON        = False
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option "v" ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True }))
     "Print each shared rule."
 , Option "V" ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "Show version number."
 , Option "n" ["num"]
     (OptArg ((\num opts -> opts { optNum = Just num }) . read . fromMaybe "3") "NUM")
     "Print selectors that match at least NUM times. Defaults 3."
 , Option "j" ["json"]
     (NoArg (\ opts -> opts { optJSON = True }))
     "Format output in JSON"
 , Option "hH" ["help"]
     (NoArg (\ opts -> opts { optShowHelp = True }))
     "Display this help message."
 ]

parseOpts :: String -> [String] -> IO (Options, [String])
parseOpts progName argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> do
        putStr $ concat errs
        printHelp progName

printHelp :: String -> IO a
printHelp progName = do
  putStr (usageInfo header options)
  exitSuccess
  where
    header = "Usage: " ++ progName ++ " [OPTION...] cssfiles..."

printVersion :: String -> IO a
printVersion progName = do
  putStrLn $ progName ++ " version 0.0.1"
  exitSuccess

main :: IO ()
main = do
  progName <- getProgName
  (opts, files) <- getArgs >>= parseOpts progName

  when (optShowVersion opts) $ printVersion progName
  when (optShowHelp opts || null files) $ printHelp progName

  errorOrRules <- parseFiles files
  case errorOrRules of
    Left e   -> printParseError e
    Right rs -> do
      let redundancies = findRedundancies opts rs
          output = if optJSON opts then jsonRulesets opts redundancies else render (printRulesets opts redundancies)
      putStrLn output

  where
    parseFiles filePaths = do contents <- fmap concat $ mapM readFile filePaths
                              return $ parseBlocks (pack contents)

    printParseError e = putStrLn $ "Error parsing css: " ++ e


printRulesets :: Options -> [(IndexedRuleset, Match)] -> Doc
printRulesets opts redundantRulesets = vcat (map format redundantRulesets)
  where
    format (ruleset, match) = do let r1 = unpack $ getSelector (snd ruleset)
                                     r2 = unpack $ getMSelector match
                                     rules = getMRules match
                                     num = length rules
                                     verbose = optVerbose opts
                                     singOrPlural = if num > 1 then "rules" else "rule" :: String
                                     s = printf "{%s} and {%s} share %d %s" r1 r2 num singOrPlural
                                     sharedRules = liftM (nest 2 . (<+>) (text "-") . text) (map formatSharedRule rules)
                                 if verbose
                                   then text s $+$ vcat sharedRules
                                   else text s

formatSharedRule :: Rule -> String
formatSharedRule rule = do
  let prop = unpack $ getProperty rule
      val = unpack $ getValue rule
  printf "%s:%s" prop val

jsonRulesets :: Options -> [(IndexedRuleset, Match)] -> String
jsonRulesets opts redundantRulesets = encode (map toJSON redundantRulesets)
  where toJSON (ruleset, match) = do let r1 = unpack $ getSelector (snd ruleset)
                                         r2 = unpack $ getMSelector match
                                         rules = getMRules match
                                         num = length rules
                                         verbose = optVerbose opts
                                         formattedRules = [("rules", showJSON (map formatSharedRule rules)) | verbose]
                                     makeObj $ [("selectors", showJSON [r1, r2]), ("count", showJSON num)] ++ formattedRules

findRedundancies :: Options -> [RawRuleset] -> [(IndexedRuleset, Match)]
findRedundancies opts rawRulesets = do
  let rulesets = map buildRuleset rawRulesets
      min' = fromJust (optNum opts)
  compactMatches min' (findMatches rulesets)
