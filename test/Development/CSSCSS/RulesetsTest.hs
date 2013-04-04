{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RulesetsTest (tests) where

import TestHelper
import Test.HUnit
import Development.CSSCSS.Rulesets

main  = $(defaultMainGenerator)
tests = $(testGroupGenerator)

case_buildsCleanly = do
  buildRulesets [("body", [("display", "block")])] @?= [
    Ruleset "body" [Rule "display" "block"]
    ]

case_sortsRulesAlpha = do
  buildRulesets [("body", [("display", "block"), ("background-color", "black")])] @?= [
    Ruleset "body" [
       Rule "background-color" "black",
       Rule "display" "block"
       ]
    ]

  buildRulesets [("p", [("visibility", "hidden"), ("background", "1px solid black")]),
                 ("body", [("display", "block"), ("background-color", "black")])] @?= [
    Ruleset "body" [
       Rule "background-color" "black",
       Rule "display" "block"
       ],
    Ruleset "p" [
       Rule "background" "1px solid black",
       Rule "visibility" "hidden"
       ]
    ]

