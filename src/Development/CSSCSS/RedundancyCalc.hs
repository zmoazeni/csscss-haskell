{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RedundancyCalc where

import Data.Text (Text)
import Data.List
import Data.Map (fromListWith, toList)

import Development.CSSCSS.Rulesets

findMatches :: [Ruleset] -> [([Text], [Declaration])]
findMatches rs = let declarationMap   = fromListWith appendNub (concatMap decTuple rs)
                     pairedMap        = fromListWith appendNub $ concatMap pairSelectors (toList declarationMap)
                     combinedSeletors = weirdFold doDecsMatch combineSamePairs [] (toList pairedMap)
                 in sortBy (\(_, decs1) (_, decs2) -> length decs2 `compare` length decs1) combinedSeletors
  where
    decTuple (Ruleset selector declarations) = map (\dec -> (dec, [selector])) declarations
    pairSelectors (declaration, selectors)   = map (\pair -> (pair, [declaration])) $ pairs selectors

    appendNub a b = nub (a ++ b)
    doDecsMatch (_, decs1) (_, decs2) = decs1 == decs2
    combineSamePairs (selectors, decs) passed list = (sort . nub $ (fromTuple selectors) ++ (concatMap (fromTuple . fst) passed), decs):list
    fromTuple (a, b) = [a, b]

pairs :: [a] -> [(a, a)]
pairs = concat . pairs'
  where
    pairs' []     = []
    pairs' (x:[]) = []
    pairs' (x:xs) = map (\y -> (x, y)) xs : pairs' xs

weirdFold :: (a -> a -> Bool) -> (a -> [a] -> b -> b) -> b -> [a] -> b
weirdFold predicate f initial list = go initial list
  where
    go z []     = z
    go z (x:xs) = let (passList, failList) = partition (predicate x) xs
                  in go (f x passList z) failList
