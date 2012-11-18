{-# LANGUAGE OverloadedStrings #-}

module Text.CSS.Shorthand.Background (
    Background (..)
  , Repeat (..)
  , Attachment (..)

  , parseBackground
  , backgroundParser
  )
where

import Text.CSS.Shorthand.Utility
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as AL
import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Char
import Data.Text (unpack)
import Data.Text.Lazy as L (Text)
import Data.Text as T (Text, length)
import Data.Foldable

data Background = Background {getColor      :: Maybe Color,
                              getImage      :: Maybe Image,
                              getRepeat     :: Maybe Repeat,
                              getAttachment :: Maybe Attachment,
                              getPosition   :: Maybe Position} | InheritBackground
                deriving (Eq, Show, Ord)

data Repeat = Repeat | RepeatX | RepeatY | NoRepeat | InheritRepeat
            deriving (Eq, Show, Ord)

data Attachment = Scroll | Fixed | InheritAttachment
            deriving (Eq, Show, Ord)


instance Value Repeat
instance Value Attachment

parseBackground :: L.Text -> Maybe Background
parseBackground s = AL.maybeResult $ AL.parse backgroundParser s

backgroundParser :: Parser Background
backgroundParser = inherit <|> longhand
  where
    inherit = do symbol "inherit"
                 endOfInput
                 return InheritBackground
    longhand = do color' <- maybeTry color
                  skipSpace
                  image <- maybeTry bgImage
                  skipSpace
                  repeat' <- maybeTry bgRepeat
                  skipSpace
                  attachment <- maybeTry bgAttachment
                  skipSpace
                  position <- maybeTry bgPosition
                  return $ Background color' image repeat' attachment position


--
-- Images
--
bgImage :: Parser Image
bgImage = imageUrl <|> symbols [("none", NoneImage), ("inherit", InheritImage)]

--
-- Repeat
--
bgRepeat :: Parser Repeat
bgRepeat = symbols [
    ("repeat-x",  RepeatX)
  , ("repeat-y",  RepeatY)
  , ("no-repeat", NoRepeat)
  , ("repeat",    Repeat)
  , ("inherit",   InheritRepeat)]

--
-- Attachment
--
bgAttachment :: Parser Attachment
bgAttachment = symbols [
    ("scroll",  Scroll)
  , ("fixed",   Fixed)
  , ("inherit", InheritAttachment)]

--
-- Position
--
bgPosition :: Parser Position
bgPosition = points <|> inherit
  where
    points = do
      h <- asum $ (literalMap <$> hKeywords) ++ [percentParser' HPercent, lengthParser' HLength]
      v <- maybeTry . asum $ (literalMap <$> vKeywords) ++ [percentParser' VPercent, lengthParser' VLength]
      return $ Position (h, v)

    inherit = literalMap ("inherit", InheritPosition)

    percentParser' t = t <$> percentParser
    lengthParser' t = t <$> lengthParser

    hKeywords = [ ("left",   LeftPoint)
                , ("right",  RightPoint)
                , ("center", HCenterPoint)]

    vKeywords = [ ("top",    TopPoint)
                , ("bottom", BottomPoint)
                , ("center", VCenterPoint)]
