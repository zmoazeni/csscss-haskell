{-# LANGUAGE OverloadedStrings #-}

module ValueParser where

import Data.Attoparsec.Text -- hiding (space, take)
import qualified Data.Attoparsec.Text.Lazy as AL
import Data.Text.Lazy (Text)
import Data.Text (unpack)
import qualified Data.Text as T (length)
import Control.Applicative
import Prelude hiding (takeWhile)

parseBackground :: Text -> Maybe Color
parseBackground s = AL.maybeResult $ AL.parse bgColor s

data Color = Hex {getRGB :: String} |
             RGB {getR :: String, getG :: String, getB ::String} |
             RGBP {getRP :: String, getGP :: String, getBP :: String} |
             InheritColor
           deriving (Eq, Show, Ord)

bgColor :: Parser Color
bgColor = hexColor <|> rgbpColor <|> rgbColor <|> bgColorKeyword <|> inherit

hexColor :: Parser Color
hexColor = do string "#"
              rawRGB <- takeWhile $ inClass "a-fA-F0-9"
              let rgb = expandRGB rawRGB (T.length rawRGB)
              return (Hex rgb)
  where expandRGB xs 3 = concat $ map (\x -> [x, x]) (unpack xs)
        expandRGB xs _ = unpack xs
        
rgbColor :: Parser Color
rgbColor = do stringCI "rgb"
              skipSpace
              string "("
              skipSpace
              r <- takeWhile $ inClass "0-9"
              skipSpace
              string ","
              skipSpace
              g <- takeWhile $ inClass "0-9"
              skipSpace
              string ","
              skipSpace
              b <- takeWhile $ inClass "0-9"
              skipSpace
              string ")"
              return $ RGB (unpack r) (unpack g) (unpack b)
              
rgbpColor :: Parser Color
rgbpColor = do stringCI "rgb"
               skipSpace
               string "("
               skipSpace
               r <- takeWhile $ inClass "0-9"
               string "%"
               skipSpace
               string ","
               skipSpace
               g <- takeWhile $ inClass "0-9"
               string "%"
               skipSpace
               string ","
               skipSpace
               b <- takeWhile $ inClass "0-9"
               string "%"
               skipSpace
               string ")"
               return $ RGBP (unpack r) (unpack g) (unpack b)

bgColorKeyword :: Parser Color
bgColorKeyword = black <|> silver <|> gray <|> white <|> maroon <|> red <|> 
                 purple <|> fuchsia <|> green <|> lime <|> olive <|> yellow <|>
                 navy <|> blue <|> teal <|> aqua
  where black   = stringCI "black"   *> pure (Hex "000000")
        silver  = stringCI "silver"  *> pure (Hex "c0c0c0")
        gray    = stringCI "gray"    *> pure (Hex "808080")
        white   = stringCI "white"   *> pure (Hex "ffffff")
        maroon  = stringCI "maroon"  *> pure (Hex "800000")
        red     = stringCI "red"     *> pure (Hex "ff0000")
        purple  = stringCI "purple"  *> pure (Hex "800080")
        fuchsia = stringCI "fuchsia" *> pure (Hex "ff00ff")
        green   = stringCI "green"   *> pure (Hex "008000")
        lime    = stringCI "lime"    *> pure (Hex "00ff00")
        olive   = stringCI "olive"   *> pure (Hex "808000")
        yellow  = stringCI "yellow"  *> pure (Hex "ffff00")
        navy    = stringCI "navy"    *> pure (Hex "000080")
        blue    = stringCI "blue"    *> pure (Hex "0000ff")
        teal    = stringCI "teal"    *> pure (Hex "008080")
        aqua    = stringCI "aqua"    *> pure (Hex "00ffff")
                     
inherit :: Parser Color
inherit = stringCI "inherit" *> pure InheritColor

