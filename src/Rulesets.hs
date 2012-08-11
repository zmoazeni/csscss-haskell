module Rulesets where

import Data.Text (Text)

type RawRule = (Text, Text)
type RawRuleset = (Text, [RawRule])
data Rule = Rule {property :: Text, value :: Text}
            deriving (Show)
                     
data Ruleset = Ruleset {selector :: Text, rules :: [Rule]}
             deriving (Show)   

buildRulesets :: [RawRuleset] -> [Ruleset]
buildRulesets = map buildRuleset

buildRuleset :: RawRuleset -> Ruleset
buildRuleset (selector, rawRules) = Ruleset selector (buildRules rawRules)

buildRules :: [RawRule] -> [Rule]
buildRules = map (uncurry Rule)
        
