{-# LANGUAGE OverloadedStrings #-}

module RedundancyCalc where

import Data.Text (Text)
import Data.List

import Rulesets

data Match = Match {getMId       :: Integer,
                    getMSelector :: Text,
                    getMRules    :: [Rule]}
           deriving (Show, Eq, Ord)

data MatchResult = MatchResult {getMRSelectors :: [Text],
                                getMRRules     :: [Rule]}
                 deriving (Show, Eq, Ord)

type IndexedRuleset = (Integer, Ruleset)

matches :: [Ruleset] -> [(Ruleset, [Match])]
matches rulesets = reduce $ map match indexedRulesets
  where
    reduce = nubBy indexes . foldr atLeastTwo []
    indexes (_, ms1) (_, ms2) = (sort $ map getMId ms1) == (sort $ map getMId ms2)
    atLeastTwo item@(r, rs) ms = if (length rs) > 1 then item:ms else ms

    match (index, ruleset) = foldr matcher (ruleset, []) (otherIndexedRulesets index)


    otherIndexedRulesets skipIndex = [(index, ruleset) | (index, ruleset) <- indexedRulesets, index /= skipIndex]
    indexedRulesets = zip [0..] (map sortRuleset rulesets)
    sortRuleset ruleset = Ruleset (getSelector ruleset) (sort $ getRules ruleset)

matcher :: IndexedRuleset -> (Ruleset, [Match]) -> (Ruleset, [Match])
matcher iRuleset@(index, ruleset) iRulesetCheck@(checkRuleset, matches) = if (null sameRules) then
                                                                            iRulesetCheck
                                                                          else
                                                                            (checkRuleset, match:matches)

  where
    sameRules = filter (\r -> r `elem` rules) checkRules
    rules = getRules ruleset
    checkRules = getRules checkRuleset
    match = Match index (getSelector ruleset) sameRules
