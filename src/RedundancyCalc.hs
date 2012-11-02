{-# LANGUAGE OverloadedStrings #-}

module RedundancyCalc where

import Data.Text hiding (map, zip, foldr, length)
import Data.List

import Rulesets


type Match = [Text]

data RedundancyResult = RedundancyResult {get100 :: [Match]}
                      deriving (Show, Eq, Ord)


similarities :: [Ruleset] -> RedundancyResult
similarities rulesets = RedundancyResult $ map getSelectors duplicates
  where
    duplicates = unique $ map (snd . check100) indexedRulesets

    getSelectors = sort . map (getSelector . snd)
    indexedRulesets = zip [0..] (map sortRuleset rulesets)

    check100 rulesetToCheck@(index, ruleset) = foldr checkDuplicateRulesets (ruleset, [rulesetToCheck]) (rulesetsWithoutIndex index)

    rulesetsWithoutIndex index = [(i, ruleset) | (i, ruleset) <- indexedRulesets, i /= index]

    checkDuplicateRulesets indexedRuleset@(_, ruleset) checkTuple@(checkRuleset, rs) = if getRules ruleset == getRules checkRuleset then
                                                                                         (checkRuleset, indexedRuleset:rs)
                                                                                       else
                                                                                         checkTuple
    sortRuleset ruleset = Ruleset (getSelector ruleset) (sort $ getRules ruleset)

    unique indexedRs = nubBy (\rs1 rs2 -> sort(map fst rs1) == sort(map fst rs2)) [rs | rs <- indexedRs, length rs > 1]

