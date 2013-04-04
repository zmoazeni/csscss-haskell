module Development.CSSCSS.Rulesets where

import Data.Text (Text)
import Data.List

type RawDeclaration = (Text, Text)
type RawRuleset     = (Text, [RawDeclaration])

data Declaration = Declaration {getProperty :: Text, getValue :: Text}
                     deriving (Show, Eq, Ord)

data Ruleset = Ruleset {getSelector :: Text, getDeclarations :: [Declaration]}
                 deriving (Show, Eq, Ord)

buildRulesets :: [RawRuleset] -> [Ruleset]
buildRulesets = map buildRuleset

buildRuleset :: RawRuleset -> Ruleset
buildRuleset (selector, rawDecs) = Ruleset selector (buildDeclarations rawDecs)

buildDeclarations :: [RawDeclaration] -> [Declaration]
buildDeclarations = map (uncurry Declaration)
