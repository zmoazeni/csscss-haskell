{-# LANGUAGE OverloadedStrings #-}

module Development.CSSCSS.RedundancyCalc (
  findMatches
) where

import Data.Text (Text)
import Data.List
import Data.Map (fromListWith, toList)

import Development.CSSCSS.Rulesets

type Selector = Text
type PairedSelectors = ((Selector, Selector), [Declaration])
type MatchedSelector = ([Selector], [Declaration])

findMatches :: [Ruleset] -> [MatchedSelector]
findMatches rs = let declarationMap   = fromListWith appendNub (concatMap decTuple rs)
                     pairedMap        = fromListWith appendNub $ concatMap pairSelectors (toList declarationMap)
                     combinedSeletors = weirdFold doDeclarationsMatch combineSamePairs [] (toList pairedMap)
                 in sortBy compareDeclarationLengths combinedSeletors
  where
    decTuple (Ruleset selector declarations) = map (\dec -> (dec, [selector])) declarations
    pairSelectors (declaration, selectors)   = map (\pair -> (pair, [declaration])) $ pairs selectors
    appendNub a b = nub (a ++ b)

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

combineSamePairs :: PairedSelectors -> [PairedSelectors] -> [MatchedSelector] -> [MatchedSelector]
combineSamePairs (selectors, decs) passed xs = (sort . nub $ (fromTuple selectors) ++ (concatMap (fromTuple . fst) passed), decs):xs
  where fromTuple (a, b) = [a, b]

compareDeclarationLengths :: MatchedSelector -> MatchedSelector -> Ordering
compareDeclarationLengths (_, decs1) (_, decs2) = length decs2 `compare` length decs1

doDeclarationsMatch :: PairedSelectors -> PairedSelectors -> Bool
doDeclarationsMatch (_, decs1) (_, decs2) = decs1 == decs2
