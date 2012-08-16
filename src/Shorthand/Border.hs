{-# LANGUAGE OverloadedStrings #-}

module Shorthand.Border (
    Border (..)
  , BorderWidth (..)
  , Width (..)

  , parseBorder
  , borderParser
) where

import Shorthand.Utility
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Text.Lazy as L (Text)
import Control.Applicative
import Data.Foldable

-- import Prelude hiding (takeWhile)
-- import Data.Char
-- import Control.Monad (liftM)
-- import Data.Text (unpack)

-- import Data.Text as T (Text, length)



data Border = Border {getWidth      :: Maybe BorderWidth
                     } | InheritBorder
            deriving (Eq, Show, Ord)


data Width = Thin | Medium | Thick | WLength Length
           deriving (Eq, Show, Ord)

data BorderWidth = BorderWidth {getTopWidth    :: Maybe Width,
                                getRightWidth  :: Maybe Width,
                                getBottomWidth :: Maybe Width,
                                getLeftWidth   :: Maybe Width}
                 deriving (Eq, Show, Ord)

instance Value Width
instance Value BorderWidth

parseBorder :: L.Text -> Maybe Border
parseBorder s = AL.maybeResult $ AL.parse borderParser s

borderParser :: Parser Border
borderParser = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritBorder
    longhand = do width <- maybeTry borderWidth
                  -- skipSpace
                  return $ Border width


borderWidth :: Parser BorderWidth
borderWidth = do w <- widthParser
                 return $ BorderWidth (Just w) (Just w) (Just w) (Just w)
  where
    widthParser = asum $ (literalMap <$> keywords)
    keywords = [ ("thin",   Thin)
               , ("medium", Medium)
               , ("thick",  Thick)]

