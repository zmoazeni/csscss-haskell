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
    Ruleset "body" [Declaration "display" "block"]
    ]
