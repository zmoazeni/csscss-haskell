{-# LANGUAGE OverloadedStrings #-}

-- Parser is built from http://www.w3.org/TR/CSS/ as a reference

module ValueParser where

import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Text.Lazy (Text)
import Data.Text (unpack)
import qualified Data.Text as T (length)
import Control.Applicative
import Prelude hiding (takeWhile)
import Data.Char
import Data.Foldable

data Background = Background {getColor      :: Maybe Color,
                              getImage      :: Maybe Image,
                              getRepeat     :: Maybe Repeat,
                              getAttachment :: Maybe Attachment}

data Color = Hex {getRGB :: String} |
             RGB {getR :: String, getG :: String, getB ::String} |
             RGBP {getRP :: String, getGP :: String, getBP :: String} |
             InheritColor
           deriving (Eq, Show, Ord)

data Image = Url {getUrl :: String } |
             NoneImage |
             InheritImage
           deriving (Eq, Show, Ord)

data Repeat = Repeat | RepeatX | RepeatY | NoRepeat | InheritRepeat
            deriving (Eq, Show, Ord)

data Attachment = Scroll | Fixed | InheritAttachment
            deriving (Eq, Show, Ord)

class Value a
instance Value Color
instance Value Image
instance Value Repeat
instance Value Attachment

parseBackground :: Text -> Maybe Background
parseBackground s = AL.maybeResult $ AL.parse bg s

bg :: Parser Background
bg = do color <- Just <$> try bgColor <|> return Nothing
        skipSpace
        image <- Just <$> try bgImage <|> return Nothing
        skipSpace
        repeat <- Just <$> try bgRepeat <|> return Nothing
        skipSpace
        attachment <- Just <$> try bgAttachment <|> return Nothing
        return $ Background color image repeat attachment

--
-- Parsing Background Colors
--
literal s result = stringCI s *> pure result

bgColor :: Parser Color
bgColor = hexColor <|> rgbColor <|> bgColorKeyword <|> inherit
  where inherit = literal "inherit" InheritColor

hexColor :: Parser Color
hexColor = do string "#"
              rawRGB <- takeWhile $ inClass "a-fA-F0-9"
              let rgb = expandRGB rawRGB (T.length rawRGB)
              return (Hex rgb)
  where expandRGB xs 3 = Prelude.concat $ map (\x -> [x, x]) (unpack xs)
        expandRGB xs _ = unpack xs

rgbColor :: Parser Color
rgbColor = do stringCI "rgb"
              skipSpace
              string "("
              skipSpace
              r <- takeWhile $ inClass "0-9"
              percent <- optionalPercent
              skipSpace
              string ","
              skipSpace
              g <- takeWhile $ inClass "0-9"
              optionalPercent
              skipSpace
              string ","
              skipSpace
              b <- takeWhile $ inClass "0-9"
              optionalPercent
              skipSpace
              string ")"
              case percent of
                Just x -> return $ RGBP (unpack r) (unpack g) (unpack b)
                Nothing -> return $ RGB (unpack r) (unpack g) (unpack b)
  where optionalPercent = Just <$> try (string "%") <|> return Nothing

bgColorKeyword :: Parser Color
bgColorKeyword = asum $ fmap parseNamedColor namedColors
  where
    parseNamedColor (name, hexColor) = stringCI name *> pure (Hex hexColor)
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
-- Parsing Images
--
bgImage :: Parser Image
bgImage = bgImageUrl <|> none <|> inherit
  where none    = literal "none" NoneImage
        inherit = literal "inherit" InheritImage

bgImageUrl :: Parser Image
bgImageUrl = do stringCI "url"
                skipSpace
                string "("
                skipSpace
                skipOptionalQuote
                url <- takeTill (inClass "\"' ")
                skipOptionalQuote
                skipSpace
                string ")"
                return $ Url (unpack url)
  where skipOptionalQuote = Just <$> try (skip (inClass "\"'")) <|> return Nothing

--
-- Parsing Repeat
--
bgRepeat :: Parser Repeat
bgRepeat = repeatX <|> repeatY <|> repeat <|> norepeat <|> inherit
  where repeat   = literal "repeat"    Repeat
        repeatX  = literal "repeat-x"  RepeatX
        repeatY  = literal "repeat-y"  RepeatY
        norepeat = literal "no-repeat" NoRepeat
        inherit  = literal "inherit"   InheritRepeat

--
-- Parsing Attachment
--
bgAttachment :: Parser Attachment
bgAttachment = scroll <|> fixed <|> inherit
  where scroll  = literal "scroll"  Scroll
        fixed   = literal "fixed"   Fixed
        inherit = literal "inherit" InheritAttachment
