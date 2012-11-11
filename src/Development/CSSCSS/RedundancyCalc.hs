{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RedundancyCalc where

import Data.Text (Text)
import Data.List

import Development.CSSCSS.Rulesets
import Text.CSS.Shorthand

data Match = Match {getMId       :: Integer,
                    getMSelector :: Text,
                    getMRules    :: [Rule]}
           deriving (Show, Eq, Ord)

data MatchResult = MatchResult {getMRSelectors :: [Text],
                                getMRRules     :: [Rule]}
                   deriving (Show, Eq, Ord)


type IndexedRuleset = (Integer, Ruleset)

findMatches :: [Ruleset] -> [(IndexedRuleset, [Match])]
findMatches rulesets = reduce $ map match indexedRulesets
  where
    reduce = nubBy indexes . foldr atLeastTwo []
    indexes (_, ms1) (_, ms2) = sort (map getMId ms1) == sort (map getMId ms2)
    atLeastTwo item@(_, rs) ms = if length rs > 1 then item:ms else ms

    match ir@(index, _) = foldr matcher (ir, []) (otherIndexedRulesets index)


    otherIndexedRulesets skipIndex = [ir | ir@(index, _) <- indexedRulesets, index /= skipIndex]
    indexedRulesets = zip [0..] (map sortRuleset rulesets)
    sortRuleset ruleset = Ruleset (getSelector ruleset) (sort $ getRules ruleset)

matcher :: IndexedRuleset -> (IndexedRuleset, [Match]) -> (IndexedRuleset, [Match])
matcher (index, ruleset) iRulesetCheck@(checkRuleset, matches) = if null sameRules
                                                                   then iRulesetCheck
                                                                   else (checkRuleset, match:matches)

  where
    sameRules = filter (`elem` rules) checkRules
    rules = getRules ruleset
    checkRules = getRules (snd checkRuleset)
    match = Match index (getSelector ruleset) sameRules

groupMatches :: [(IndexedRuleset, [Match])] -> [(IndexedRuleset, Match)]
groupMatches = concatMap pairEach
  where
    pairEach (indexedRuleset, matches') = map (\match -> (indexedRuleset, match)) matches'


nubMatches :: [(IndexedRuleset, Match)] -> [(IndexedRuleset, Match)]
nubMatches = nubBy nubber
  where
    nubber ((i1, _), Match m1 _ _) ((i2, _), Match m2 _ _) = i1 == m2 && m1 == i2

reduceMatches :: Int -> [(IndexedRuleset, Match)] -> [(IndexedRuleset, Match)]
reduceMatches num matches = sortBy matchTotal $ filter matchLength matches
  where
    matchLength (_, match) = length (getMRules match) >= num
    matchTotal (_, m1) (_, m2) = length (getMRules m2) `compare` length (getMRules m1)

compactMatches :: Int -> [(IndexedRuleset, [Match])] -> [(IndexedRuleset, Match)]
compactMatches num pairs = reduceMatches num . nubMatches . groupMatches $ pairs
