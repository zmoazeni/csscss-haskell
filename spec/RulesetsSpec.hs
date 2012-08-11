{-# LANGUAGE OverloadedStrings #-}

module RulesetsSpec where

import Test.Hspec
--import Data.Text (Text)
import Rulesets

main = hspec $ do
  describe "building rulesets" $ do
    it "builds cleanly" $ do
       buildRulesets [("body", [("display", "block")])] == [Ruleset "body" [Rule "display" "block"]]
      