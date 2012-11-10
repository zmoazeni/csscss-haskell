{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment (getArgs, getProgName)
import System.Exit
import Text.CSS.Parse
import Data.Text (Text, pack, unpack)
import Text.PrettyPrint
import Text.Printf
import System.Console.GetOpt
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad

import Rulesets
import RedundancyCalc

data Options = Options
 { optVerbose     :: Bool
 , optShowVersion :: Bool
 , optShowHelp    :: Bool
 , optNum         :: Maybe Int
 } deriving Show

defaultOptions    = Options
 { optVerbose     = False
 , optShowVersion = False
 , optShowHelp    = False
 , optNum         = Just 3
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['v'] ["verbose"]
     (NoArg (\ opts -> opts { optVerbose = True }))
     "Print each shared rule."
 , Option ['V'] ["version"]
     (NoArg (\ opts -> opts { optShowVersion = True }))
     "Show version number."
 , Option ['n'] ["num"]
     (OptArg ((\num opts -> opts { optNum = Just num }) . read . fromMaybe "3") "NUM")
     "Print selectors that match at least NUM times. Defaults 3."
 , Option ['h', 'H'] ["help"]
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

printHelp progName = do
  putStr (usageInfo header options)
  exitWith ExitSuccess
  where
    header = "Usage: " ++ progName ++ " [OPTION...] cssfiles..."

printVersion progName = do
  putStrLn $ progName ++ " version 0.0.1"
  exitWith ExitSuccess

main :: IO ()
main = do
  progName <- getProgName
  (opts, files) <- getArgs >>= parseOpts progName

  when (optShowVersion opts) $ printVersion progName
  when ((optShowHelp opts) || null files) $ printHelp progName

  errorOrRules <- parseFiles files
  case errorOrRules of
    Left e   -> printParseError e
    Right rs -> do
      let output = render (displayRulesets opts rs)
      putStrLn output

  where
    parseFiles filePaths = do contents <- fmap concat $ mapM readFile filePaths
                              return $ parseBlocks (pack contents)

    printParseError error = putStrLn $ "Error parsing css: " ++ error


displayRulesets :: Options -> [RawRuleset] -> Doc
displayRulesets opts rawRulesets = do
  let rulesets = map buildRuleset rawRulesets
      min = fromJust (optNum opts)
      redundantRulesets = compactMatches min (findMatches rulesets)
  vcat (map format redundantRulesets)
  where
    format (ruleset, match) = do let r1 = unpack $ getSelector (snd ruleset)
                                     r2 = unpack $ getMSelector match
                                     rules = getMRules match
                                     num = length rules
                                     verbose = optVerbose opts
                                     singOrPlural = if num > 1 then "rules" else "rule" :: String
                                     s = printf "{%s} and {%s} share %d %s" r1 r2 num singOrPlural
                                 if verbose
                                   then text s $+$ vcat (map formatSharedRule rules)
                                   else text s

    formatSharedRule rule = do let prop = unpack $ getProperty rule
                                   val = unpack $ getValue rule
                                   r = printf "- %s:%s;" prop val
                               nest 2 (text r)

