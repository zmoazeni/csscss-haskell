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
    longhand = do color <- maybeTry bgColor
                  skipSpace
                  image <- maybeTry bgImage
                  skipSpace
                  repeat' <- maybeTry bgRepeat
                  skipSpace
                  attachment <- maybeTry bgAttachment
                  skipSpace
                  position <- maybeTry bgPosition
                  return $ Background color image repeat' attachment position


--
-- Colors
--

bgColor :: Parser Color
bgColor = hexColor <|> rgbpColor <|> rgbColor <|> bgColorKeyword <|> inherit
  where inherit = literal "inherit" InheritColor

hexColor :: Parser Color
hexColor = do symbol "#"
              rawRGB <- takeWhile $ inClass "a-fA-F0-9"
              let rgb = expandRGB rawRGB (T.length rawRGB)
              return (Hex rgb)
  where expandRGB xs 3 = Prelude.concatMap (\x -> [x, x]) (unpack xs)
        expandRGB xs _ = unpack xs

rgbColor :: Parser Color
rgbColor = do
  symbol "rgb"
  (r, g, b) <- rgbParams (takeWhile isNumber)
  return $ RGB r g b

rgbpColor :: Parser Color
rgbpColor = do
  symbol "rgb"
  (r, g, b) <- rgbParams percent
  return $ RGBP r g b
  where percent = takeWhile isNumber <* symbol "%"

rgbParams :: Parser T.Text -> Parser (String, String, String)
rgbParams p = parens $ do
  r <- p
  symbol ","
  g <- p
  symbol ","
  b <- p
  return (unpack r, unpack g, unpack b)

bgColorKeyword :: Parser Color
bgColorKeyword = asum $ fmap parseNamedColor namedColors
  where
    parseNamedColor (name, hexColor') = stringCI name *> pure (Hex hexColor')
    namedColors = [ ("black",   "000000")
                  , ("silver",  "c0c0c0")
                  , ("gray",    "808080")
                  , ("white",   "ffffff")
                  , ("maroon",  "800000")
                  , ("red",     "ff0000")
                  , ("purple",  "800080")
                  , ("fuchsia", "ff00ff")
                  , ("green",   "008000")
                  , ("lime",    "00ff00")
                  , ("olive",   "808000")
                  , ("yellow",  "ffff00")
                  , ("navy",    "000080")
                  , ("blue",    "0000ff")
                  , ("teal",    "008080")
                  , ("aqua",    "00ffff")
                  ]

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