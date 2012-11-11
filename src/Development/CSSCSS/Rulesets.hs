module Development.CSSCSS.Rulesets where

import Data.Text (Text)
import Data.List

type RawRule = (Text, Text)
type RawRuleset = (Text, [RawRule])
data Rule = Rule {getProperty :: Text, getValue :: Text}
            deriving (Show, Eq, Ord)

data Ruleset = Ruleset {getSelector :: Text, getRules :: [Rule]}
             deriving (Show, Eq, Ord)

buildRulesets :: [RawRuleset] -> [Ruleset]
buildRulesets rs = sort $ map buildRuleset rs

buildRuleset :: RawRuleset -> Ruleset
buildRuleset (selector, rawRules) = Ruleset selector (buildRules rawRules)

buildRules :: [RawRule] -> [Rule]
buildRules rs = sort $ map (uncurry Rule) rs
