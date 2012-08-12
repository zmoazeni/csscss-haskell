{-# LANGUAGE OverloadedStrings #-}

module ValueParser where

import Data.Attoparsec.Text -- hiding (space, take)
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Text.Lazy (Text)
import Control.Applicative


parseBackground :: Text -> Maybe Text
parseBackground s = AL.maybeResult $ AL.parse backgroundColor s

inherit :: Parser Text
inherit = do string "inherit"
             return "inherit"

backgroundColorKeyword :: Parser Text
backgroundColorKeyword = do string "black"
                            return "black"

backgroundColor :: Parser Text
backgroundColor = backgroundColorKeyword <|> inherit
                     
