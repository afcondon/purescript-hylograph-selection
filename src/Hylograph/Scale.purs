-- | Hylograph.Scale - Pure PureScript scales for data visualization
-- |
-- | This module re-exports the pure implementation from Scale.Pure,
-- | categorical color palettes from Scale.ColorSchemes,
-- | color interpolation from Scale.Interpolation, and
-- | sequential/diverging color interpolators from Scale.Sequential.
-- |
-- | No D3 or FFI dependency.
-- |
-- | ## Basic Usage
-- |
-- | ```purescript
-- | import Hylograph.Scale (linear, domain, range, applyScale, ticks)
-- |
-- | myScale = linear # domain [0.0, 100.0] # range [0.0, 800.0]
-- | pixelX = applyScale myScale 50.0  -- Returns 400.0
-- | tickValues = ticks 10 myScale     -- Returns nice tick values
-- | ```
module Hylograph.Scale
  ( -- * Scale Types
    module Scale

  -- * Color Schemes (Categorical)
  , module ColorSchemes

  -- * Color Interpolation
  , module Interpolation

  -- * Sequential/Diverging/Cyclical Interpolators
  , module Sequential
  ) where

-- Re-export everything from Scale.Pure
import Hylograph.Scale.Pure
  ( Scale(..)
  , ContinuousScale
  , Continuous
  , linear
  , pow
  , sqrt
  , log
  , domain
  , range
  , clamp
  , nice
  , niceCount
  , exponent
  , base
  , round
  , applyScale
  , invert
  , ticks
  , tickFormat
  , copy
  , andThen
  , contramap
  , map
  , dimap
  , Interpolator
  ) as Scale

-- Re-export categorical color schemes
import Hylograph.Scale.ColorSchemes
  ( schemeCategory10
  , schemeTableau10
  , schemePaired
  , schemeSet1
  , schemeSet2
  , schemeSet3
  , schemeAccent
  , schemeDark2
  , schemePastel1
  , schemePastel2
  , schemeCategory10At
  , schemeTableau10At
  , schemePairedAt
  ) as ColorSchemes

-- Re-export color interpolation
import Hylograph.Scale.Interpolation
  ( interpolateRgb
  , interpolateHsl
  , interpolateNumber
  ) as Interpolation

-- Re-export sequential, diverging, and cyclical color interpolators
import Hylograph.Scale.Sequential
  ( interpolateBlues
  , interpolateGreens
  , interpolateGreys
  , interpolateOranges
  , interpolatePurples
  , interpolateReds
  , interpolateViridis
  , interpolatePlasma
  , interpolateInferno
  , interpolateMagma
  , interpolateTurbo
  , interpolateWarm
  , interpolateCool
  , interpolateRainbow
  , interpolateCividis
  , interpolateCubehelixDefault
  , interpolateBuGn
  , interpolateBuPu
  , interpolateGnBu
  , interpolateOrRd
  , interpolatePuBuGn
  , interpolatePuBu
  , interpolatePuRd
  , interpolateRdPu
  , interpolateYlGnBu
  , interpolateYlGn
  , interpolateYlOrBr
  , interpolateYlOrRd
  , interpolateRdYlGn
  , interpolateRdBu
  , interpolatePiYG
  , interpolateBrBG
  , interpolatePRGn
  , interpolateSpectral
  , interpolateRdGy
  , interpolateRdYlBu
  , interpolateSinebow
  ) as Sequential
