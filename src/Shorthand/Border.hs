{-# LANGUAGE OverloadedStrings #-}

module Shorthand.Border (
    Border (..)
  , BorderWidth (..)
  , Width (..)

  , parseBorder
  , borderParser

  , parseBorderWidth
  , borderWidthParser
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


--
-- Parse Commands
--
parseBorder :: L.Text -> Maybe Border
parseBorder s = AL.maybeResult $ AL.parse borderParser s

parseBorderWidth :: L.Text -> Maybe BorderWidth
parseBorderWidth s = AL.maybeResult $ AL.parse borderWidthParser s

--
-- Parsers
--

borderParser :: Parser Border
borderParser = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritBorder
    longhand = do width <- maybeTry borderWidth
                  -- skipSpace
                  return $ Border width


widthParser :: Parser Width
widthParser = asum $ (literalMap <$> keywords)
  where
    keywords = [ ("thin",   Thin)
               , ("medium", Medium)
               , ("thick",  Thick)]

borderWidth :: Parser BorderWidth
borderWidth = do w <- widthParser
                 return $ BorderWidth (Just w) (Just w) (Just w) (Just w)


borderWidthParser :: Parser BorderWidth
borderWidthParser = do ws <- many1 widthParser
                       let (w1:w2:w3:w4:_) = pad (map Just ws)
                       return $ BorderWidth w1 w2 w3 w4
  where
    pad ws = Prelude.take 4 $ ws ++ (repeat Nothing)

