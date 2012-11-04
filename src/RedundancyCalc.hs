{-# LANGUAGE OverloadedStrings #-}

module RedundancyCalc where

import Data.Text hiding (map, zip, foldr, length, head)
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

matches :: [Ruleset] -> [MatchResult]
matches rulesets = map wrapMatchResult reducedResults
  where
    wrapMatchResult (_, ms) = MatchResult (map (getSelector . snd) ms) (head $ map (getRules .snd) ms)
    reducedResults = reduce $ map match indexedRulesets

    reduce = nubBy indexes . foldr atLeastTwo []
    indexes (_, ms1) (_, ms2) = (sort $ map fst ms1) == (sort $ map fst ms2)
    atLeastTwo item@(r, rs) ms = if (length rs) > 1 then item:ms else ms

    match iRulesetToCheck@(index, ruleset) = foldr matcher (ruleset, [iRulesetToCheck]) (otherIndexedRulesets index)

    matcher iRuleset@(_, ruleset) iRulesetCheck@(checkRuleset, indexedRulesets) = if getRules ruleset == getRules checkRuleset then
                                                                                    (checkRuleset, iRuleset:indexedRulesets)
                                                                                  else
                                                                                    iRulesetCheck

    otherIndexedRulesets skipIndex = [(index, ruleset) | (index, ruleset) <- indexedRulesets, index /= skipIndex]
    indexedRulesets = zip [0..] (map sortRuleset rulesets)
    sortRuleset ruleset = Ruleset (getSelector ruleset) (sort $ getRules ruleset)
