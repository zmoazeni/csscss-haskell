{-# LANGUAGE OverloadedStrings #-}

module Shorthand.Border (
    Border (..)
  , BorderWidth (..)
  , Width (..)
  , BorderStyle (..)
  , Style (..)

  , parseBorder
  , borderParser

  , parseBorderWidth
  , borderWidthParser

  , parseBorderStyle
  , borderStyleParser
) where

import Shorthand.Utility
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text.Lazy as AL hiding (take)
import Data.Text.Lazy as L (Text)
import Control.Applicative
import Data.Foldable
import Control.Monad

data Border = Border {  getWidth :: Maybe BorderWidth
                      , getStyle :: Maybe BorderStyle
                     } | InheritBorder
            deriving (Eq, Show, Ord)


data Width = Thin | Medium | Thick | WLength Length
           deriving (Eq, Show, Ord)

data BorderWidth = BorderWidth {getTopWidth    :: Maybe Width,
                                getRightWidth  :: Maybe Width,
                                getBottomWidth :: Maybe Width,
                                getLeftWidth   :: Maybe Width}
                 deriving (Eq, Show, Ord)

data Style = None | Hidden | Dotted | Dashed | Solid | Double | Groove |
             Ridge | Inset | Outset
           deriving (Eq, Show, Ord)

data BorderStyle = BorderStyle {getTopStyle    :: Maybe Style,
                                getRightStyle  :: Maybe Style,
                                getBottomStyle :: Maybe Style,
                                getLeftStyle   :: Maybe Style}
                 deriving (Eq, Show, Ord)

instance Value Width
instance Value BorderWidth
instance Value Style
instance Value BorderStyle


--
-- Parse Commands
--
parseBorder :: L.Text -> Maybe Border
parseBorder s = AL.maybeResult $ AL.parse borderParser s

parseBorderWidth :: L.Text -> Maybe BorderWidth
parseBorderWidth s = AL.maybeResult $ AL.parse borderWidthParser s

parseBorderStyle :: L.Text -> Maybe BorderStyle
parseBorderStyle s = AL.maybeResult $ AL.parse borderStyleParser s

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
                  skipSpace
                  style <- maybeTry borderStyle
                  return $ Border width style


widthParser :: Parser Width
widthParser = symbols [
    ("thin",   Thin)
  , ("medium", Medium)
  , ("thick",  Thick)] `mplus` (WLength <$> lengthParser)

borderWidth :: Parser BorderWidth
borderWidth = do w <- widthParser
                 return $ BorderWidth (Just w) (Just w) (Just w) (Just w)


borderWidthParser :: Parser BorderWidth
borderWidthParser = do ws <- many1 widthParser
                       let (w1:w2:w3:w4:_) = pad (map Just ws)
                       return $ BorderWidth w1 w2 w3 w4
  where
    pad ws = take 4 $ ws ++ (repeat Nothing)

styleParser :: Parser Style
styleParser = symbols [
    ("none",   None)
  , ("hidden", Hidden)
  , ("dotted", Dotted)
  , ("dashed", Dashed)
  , ("solid",  Solid)
  , ("double", Double)
  , ("groove", Groove)
  , ("ridge",  Ridge)
  , ("inset",  Inset)
  , ("outset", Outset)]

borderStyle :: Parser BorderStyle
borderStyle = do s <- styleParser
                 return $ BorderStyle (Just s) (Just s) (Just s) (Just s)

borderStyleParser :: Parser BorderStyle
borderStyleParser = do ws <- many1 styleParser
                       let (w1:w2:w3:w4:_) = pad (map Just ws)
                       return $ BorderStyle w1 w2 w3 w4
  where
    pad ws = take 4 $ ws ++ (repeat Nothing)

