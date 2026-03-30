-- | Hylograph.Scale.Interpolation — Pure PureScript color interpolation
-- |
-- | Provides RGB and HSL interpolation without any D3 dependency.
-- | Colors are represented as CSS hex strings (#rrggbb).
module Hylograph.Scale.Interpolation
  ( interpolateRgb
  , interpolateHsl
  , interpolateNumber
  ) where

import Prelude

import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Number as Num
import Data.String as String
import Data.String.CodeUnits as SCU

-- ============================================================================
-- PUBLIC API
-- ============================================================================

-- | Linear number interpolation
interpolateNumber :: Number -> Number -> Number -> Number
interpolateNumber a b t = a + (b - a) * t

-- | RGB color interpolation
-- |
-- | Interpolates between two CSS hex color strings in RGB space.
-- |
-- | ```purescript
-- | interpolateRgb "#ff0000" "#0000ff" 0.5  -- Returns "#800080"
-- | ```
interpolateRgb :: String -> String -> (Number -> String)
interpolateRgb a b =
  let
    ca = parseHex a
    cb = parseHex b
  in
    \t ->
      let
        r = Int.round (lerp (Int.toNumber ca.r) (Int.toNumber cb.r) t)
        g = Int.round (lerp (Int.toNumber ca.g) (Int.toNumber cb.g) t)
        b_ = Int.round (lerp (Int.toNumber ca.b) (Int.toNumber cb.b) t)
      in
        toHex (clampByte r) (clampByte g) (clampByte b_)

-- | HSL color interpolation
-- |
-- | Interpolates between two CSS hex color strings in HSL space,
-- | taking the shorter hue arc.
-- |
-- | ```purescript
-- | interpolateHsl "#ff0000" "#00ff00" 0.5  -- Yellow-ish
-- | ```
interpolateHsl :: String -> String -> (Number -> String)
interpolateHsl a b =
  let
    ca = rgbToHsl (parseHex a)
    cb = rgbToHsl (parseHex b)
    -- Take shorter hue arc
    dh = let diff = cb.h - ca.h
         in if diff > 180.0 then diff - 360.0
            else if diff < -180.0 then diff + 360.0
            else diff
  in
    \t ->
      let
        h = ca.h + dh * t
        s = lerp ca.s cb.s t
        l = lerp ca.l cb.l t
        rgb = hslToRgb { h: if h < 0.0 then h + 360.0 else if h >= 360.0 then h - 360.0 else h, s, l }
      in
        toHex rgb.r rgb.g rgb.b

-- ============================================================================
-- INTERNAL: Color parsing and conversion
-- ============================================================================

type RGB = { r :: Int, g :: Int, b :: Int }
type HSL = { h :: Number, s :: Number, l :: Number }

-- | Parse a hex color string (#rgb, #rrggbb, or rgb(r,g,b))
parseHex :: String -> RGB
parseHex str =
  let s = String.drop 1 str -- drop '#'
      len = SCU.length s
  in
    if len == 3 then
      { r: parseHexChar (SCU.slice 0 1 s) * 17
      , g: parseHexChar (SCU.slice 1 2 s) * 17
      , b: parseHexChar (SCU.slice 2 3 s) * 17
      }
    else if len >= 6 then
      { r: parseHex2 (SCU.slice 0 2 s)
      , g: parseHex2 (SCU.slice 2 4 s)
      , b: parseHex2 (SCU.slice 4 6 s)
      }
    else
      { r: 0, g: 0, b: 0 }

parseHexChar :: String -> Int
parseHexChar s = fromMaybe 0 (Int.fromStringAs Int.hexadecimal s)

parseHex2 :: String -> Int
parseHex2 s = fromMaybe 0 (Int.fromStringAs Int.hexadecimal s)

-- | Convert RGB to hex string
toHex :: Int -> Int -> Int -> String
toHex r g b = "#" <> hexByte r <> hexByte g <> hexByte b

hexByte :: Int -> String
hexByte n =
  let hex = Int.toStringAs Int.hexadecimal (clampByte n)
  in if SCU.length hex < 2 then "0" <> hex else hex

clampByte :: Int -> Int
clampByte n = max 0 (min 255 n)

-- | Linear interpolation
lerp :: Number -> Number -> Number -> Number
lerp a b t = a + (b - a) * t

-- | RGB to HSL conversion
rgbToHsl :: RGB -> HSL
rgbToHsl { r: ri, g: gi, b: bi } =
  let
    r = Int.toNumber ri / 255.0
    g = Int.toNumber gi / 255.0
    b = Int.toNumber bi / 255.0
    cMax = Num.max r (Num.max g b)
    cMin = Num.min r (Num.min g b)
    delta = cMax - cMin
    l = (cMax + cMin) / 2.0
    s = if delta == 0.0 then 0.0
        else delta / (1.0 - Num.abs (2.0 * l - 1.0))
    h = if delta == 0.0 then 0.0
        else if cMax == r then 60.0 * ((g - b) / delta `pmod` 6.0)
        else if cMax == g then 60.0 * ((b - r) / delta + 2.0)
        else 60.0 * ((r - g) / delta + 4.0)
  in
    { h: if h < 0.0 then h + 360.0 else h, s, l }

-- | HSL to RGB conversion
hslToRgb :: HSL -> RGB
hslToRgb { h, s, l } =
  let
    c = (1.0 - Num.abs (2.0 * l - 1.0)) * s
    x = c * (1.0 - Num.abs ((h / 60.0 `pmod` 2.0) - 1.0))
    m = l - c / 2.0
    rgb' = if h < 60.0 then { r: c, g: x, b: 0.0 }
           else if h < 120.0 then { r: x, g: c, b: 0.0 }
           else if h < 180.0 then { r: 0.0, g: c, b: x }
           else if h < 240.0 then { r: 0.0, g: x, b: c }
           else if h < 300.0 then { r: x, g: 0.0, b: c }
           else { r: c, g: 0.0, b: x }
  in
    { r: Int.round ((rgb'.r + m) * 255.0)
    , g: Int.round ((rgb'.g + m) * 255.0)
    , b: Int.round ((rgb'.b + m) * 255.0)
    }

-- | Positive modulo (always returns non-negative)
pmod :: Number -> Number -> Number
pmod a b =
  let r = a - Num.floor (a / b) * b
  in if r < 0.0 then r + b else r
