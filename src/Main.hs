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
    header = "Usage: " ++ progName ++ " [OPTION...] cssfile"

printVersion progName = do
  putStrLn $ progName ++ " version 0.0.1"
  exitWith ExitSuccess

main :: IO ()
main = do
  progName <- getProgName
  (args, files) <- getArgs >>= parseOpts progName

  when (optShowVersion args) $ printVersion progName
  when ((optShowHelp args) || null files) $ printHelp progName

  errorOrRules <- parseFile files
  case errorOrRules of
    Left e   -> printParseError e
    Right rs -> do
      let min = fromJust (optNum args)
          output = render (displayRulesets min rs)
      putStrLn output

  where
    parseFile :: [String] -> IO (Either String [(Text, [(Text, Text)])])
    parseFile (filePath:_) = do contents <- readFile filePath
                                return $ parseBlocks (pack contents)

    printParseError error = putStrLn $ "Error parsing css: " ++ error


displayRulesets :: Int -> [RawRuleset] -> Doc
displayRulesets num rawRulesets = do
  let rulesets = map buildRuleset rawRulesets
      redundantRulesets = compactMatches num (findMatches rulesets)
  vcat (map format redundantRulesets)
  where
    format (ruleset, match) = do let r1 = unpack $ getSelector (snd ruleset)
                                     r2 = unpack $ getMSelector match
                                     num = length $ getMRules match
                                     singOrPlural = if num > 1 then "rules" else "rule" :: String
                                     s = printf "{%s} and {%s} share %d %s" r1 r2 num singOrPlural
                                 text s

