-- | HATS Axis Rendering
-- |
-- | Axis rendering using HATS (replaces PSD3.Axis.Axis for HATS-based visualizations)
module AxisHATS
  ( Axis
  , Orientation(..)
  , AxisConfig
  , Scale
  , defaultAxisConfig
  , axisBottom
  , axisLeft
  , axisTop
  , axisRight
  , renderAxisHATS
  ) where

import Prelude

import Data.Array (range)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (fixed, toStringWith)
import Hylograph.HATS (Tree, elem, forEach, staticStr, staticNum, thunkedStr)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- | Axis orientation
data Orientation
  = Top
  | Right
  | Bottom
  | Left

derive instance eqOrientation :: Eq Orientation

-- | Scale type (simplified - just linear with domain/range)
type Scale =
  { domain :: { min :: Number, max :: Number }
  , range :: { min :: Number, max :: Number }
  }

-- | Apply scale to a value
applyScale :: Scale -> Number -> Number
applyScale scale value =
  let
    domainSpan = scale.domain.max - scale.domain.min
    rangeSpan = scale.range.max - scale.range.min
    normalized = (value - scale.domain.min) / domainSpan
  in
    scale.range.min + (normalized * rangeSpan)

-- | Axis configuration
type AxisConfig =
  { tickSize :: Number
  , tickPadding :: Number
  , tickCount :: Int
  , tickFormat :: Maybe (Number -> String)
  }

defaultAxisConfig :: AxisConfig
defaultAxisConfig =
  { tickSize: 6.0
  , tickPadding: 3.0
  , tickCount: 10
  , tickFormat: Nothing
  }

-- | Opaque axis type
type Axis =
  { orientation :: Orientation
  , scale :: Scale
  , config :: AxisConfig
  }

-- | Create axis constructors
axisBottom :: Scale -> Axis
axisBottom scale = { orientation: Bottom, scale, config: defaultAxisConfig }

axisLeft :: Scale -> Axis
axisLeft scale = { orientation: Left, scale, config: defaultAxisConfig }

axisTop :: Scale -> Axis
axisTop scale = { orientation: Top, scale, config: defaultAxisConfig }

axisRight :: Scale -> Axis
axisRight scale = { orientation: Right, scale, config: defaultAxisConfig }

-- | Generate tick values
generateTicks :: Scale -> Int -> Array Number
generateTicks scale count =
  let
    step = (scale.domain.max - scale.domain.min) / Int.toNumber count
  in
    map (\i -> scale.domain.min + (Int.toNumber i * step)) (range 0 count)

-- | Default tick formatter
defaultTickFormat :: Number -> String
defaultTickFormat n =
  if n == 0.0
    then "0"
    else if n >= 1000000.0
      then toStringWith (fixed 1) (n / 1000000.0) <> "M"
      else if n >= 1000.0
        then toStringWith (fixed 0) (n / 1000.0) <> "k"
        else toStringWith (fixed 1) n

-- | Render axis as a HATS Tree
renderAxisHATS :: Axis -> Tree
renderAxisHATS ax =
  let
    ticks = generateTicks ax.scale ax.config.tickCount
    formatter = fromMaybe defaultTickFormat ax.config.tickFormat

    -- Calculate positions based on orientation
    { domainPath, tickTransform, tickLine, tickText } = case ax.orientation of
      Bottom ->
        { domainPath: "M" <> show ax.scale.range.min <> ",0H" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(" <> show pos <> ",0)"
        , tickLine: { x1: 0.0, y1: 0.0, x2: 0.0, y2: ax.config.tickSize }
        , tickText: { x: 0.0, y: ax.config.tickSize + ax.config.tickPadding, anchor: "middle", dyVal: 7.0 }
        }
      Left ->
        { domainPath: "M0," <> show ax.scale.range.min <> "V" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(0," <> show pos <> ")"
        , tickLine: { x1: -ax.config.tickSize, y1: 0.0, x2: 0.0, y2: 0.0 }
        , tickText: { x: -(ax.config.tickSize + ax.config.tickPadding), y: 0.0, anchor: "end", dyVal: 3.0 }
        }
      Top ->
        { domainPath: "M" <> show ax.scale.range.min <> ",0H" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(" <> show pos <> ",0)"
        , tickLine: { x1: 0.0, y1: -ax.config.tickSize, x2: 0.0, y2: 0.0 }
        , tickText: { x: 0.0, y: -(ax.config.tickSize + ax.config.tickPadding), anchor: "middle", dyVal: 0.0 }
        }
      Right ->
        { domainPath: "M0," <> show ax.scale.range.min <> "V" <> show ax.scale.range.max
        , tickTransform: \pos -> "translate(0," <> show pos <> ")"
        , tickLine: { x1: 0.0, y1: 0.0, x2: ax.config.tickSize, y2: 0.0 }
        , tickText: { x: ax.config.tickSize + ax.config.tickPadding, y: 0.0, anchor: "start", dyVal: 3.0 }
        }

    -- Pre-compute tick data with positions
    tickData :: Array { value :: Number, pos :: Number, label :: String }
    tickData = map (\tickValue ->
      { value: tickValue
      , pos: applyScale ax.scale tickValue
      , label: formatter tickValue
      }) ticks
  in
    elem Group
      [ staticStr "class" "axis"
      , staticStr "fill" "none"
      , staticNum "font-size" 10.0
      , staticStr "stroke" "currentColor"
      ]
      [ -- Domain path
        elem Path
          [ staticStr "class" "domain"
          , staticStr "stroke" "currentColor"
          , staticStr "d" domainPath
          ] []
      -- Ticks as a Fold (will appear as deck-of-cards in MetaHATS)
      , forEach "ticks" Group tickData (_.label) \tick ->
          elem Group
            [ staticStr "class" "tick"
            , thunkedStr "transform" (tickTransform tick.pos)
            ]
            [ elem Line
                [ staticNum "x1" tickLine.x1
                , staticNum "y1" tickLine.y1
                , staticNum "x2" tickLine.x2
                , staticNum "y2" tickLine.y2
                , staticStr "stroke" "currentColor"
                ] []
            , elem Text
                [ staticNum "x" tickText.x
                , staticNum "y" tickText.y
                , staticStr "text-anchor" tickText.anchor
                , staticNum "dy" tickText.dyVal
                , staticStr "fill" "currentColor"
                , staticNum "font-size" 10.0
                , thunkedStr "textContent" tick.label
                ] []
            ]
      ]
