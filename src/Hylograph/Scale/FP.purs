-- | Hylograph.Scale.FP - Functional Programming Abstractions for Scales
-- |
-- | Higher-level functional programming idioms built on top of Scale.Pure.
-- | No FFI or D3 dependency.
-- |
-- | ## Scale Modifiers
-- |
-- | Scale modifiers compose:
-- |
-- | ```purescript
-- | niceAndClamped = combineModifiers [niceModifier, clampModifier]
-- | myScale = linear # niceAndClamped # domain [...] # range [...]
-- | ```
-- |
-- | ## Sampling
-- |
-- | ```purescript
-- | gradient = sample 100 viridisColorScale
-- | ```
module Hylograph.Scale.FP
  ( -- * Scale Modifiers (Endo-like)
    ScaleModifier
  , niceModifier
  , clampModifier
  , roundModifier
  , combineModifiers

    -- * Sampling
  , sample
  , sampleRange
  , sampleWithDomain

    -- * Tick Operations
  , tickPositions
  , tickLabels
  , ticksWithLabels

    -- * Interpolation Combinators
  , blendInterpolators
  , reverseInterpolator
  , clampInterpolator
  , cycleInterpolator

    -- * Scale Transformations
  , normalize
  , quantize
  , threshold

    -- * Useful Combinators
  , scaleExtent
  , scaleMidpoint
  , scaleInRange
  ) where

import Prelude hiding (clamp)

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Hylograph.Scale.Pure (ContinuousScale, Interpolator)
import Hylograph.Scale.Pure (applyScale, clamp, domain, nice, range, round, tickFormat, ticks) as Scale
import Hylograph.Scale.Pure as Pure

-- ============================================================================
-- SCALE MODIFIERS (Endo-like composition)
-- ============================================================================

-- | A scale modifier transforms a scale while preserving its type
type ScaleModifier = ContinuousScale -> ContinuousScale

-- | Modifier that makes the domain nice (rounds to clean values)
niceModifier :: ScaleModifier
niceModifier = Scale.nice

-- | Modifier that enables clamping
clampModifier :: ScaleModifier
clampModifier = Scale.clamp true

-- | Modifier that enables rounding
roundModifier :: ScaleModifier
roundModifier = Scale.round true

-- | Combine multiple modifiers (apply left to right)
combineModifiers :: Array ScaleModifier -> ScaleModifier
combineModifiers mods = \scale -> Array.foldl (\s m -> m s) scale mods

-- ============================================================================
-- SAMPLING
-- ============================================================================

-- | Sample a scale at n evenly-spaced points in [0, 1]
sample :: Int -> ContinuousScale -> Array Number
sample n scale =
  let
    step = if n <= 1 then 0.0 else 1.0 / Int.toNumber (n - 1)
    ts = Array.range 0 (n - 1) <#> \i -> Int.toNumber i * step
  in
    ts <#> Scale.applyScale scale

-- | Sample within a specific range
sampleRange :: Int -> Number -> Number -> ContinuousScale -> Array Number
sampleRange n start end scale =
  let
    step = if n <= 1 then 0.0 else (end - start) / Int.toNumber (n - 1)
    ts = Array.range 0 (n - 1) <#> \i -> start + Int.toNumber i * step
  in
    ts <#> Scale.applyScale scale

-- | Sample and return both domain and range values
sampleWithDomain :: Int -> ContinuousScale -> Array (Tuple Number Number)
sampleWithDomain n scale =
  let
    step = if n <= 1 then 0.0 else 1.0 / Int.toNumber (n - 1)
    ts = Array.range 0 (n - 1) <#> \i -> Int.toNumber i * step
  in
    ts <#> \t -> Tuple t (Scale.applyScale scale t)

-- ============================================================================
-- TICK OPERATIONS
-- ============================================================================

-- | Get tick positions as pixel coordinates
tickPositions :: Int -> ContinuousScale -> Array Number
tickPositions count scale =
  Scale.ticks count scale <#> Scale.applyScale scale

-- | Get formatted tick labels
tickLabels :: Int -> String -> ContinuousScale -> Array String
tickLabels count specifier scale =
  let
    formatter = Scale.tickFormat count specifier scale
    tickVals = Scale.ticks count scale
  in
    tickVals <#> formatter

-- | Get ticks with both position and label
ticksWithLabels :: Int -> String -> ContinuousScale -> Array { position :: Number, label :: String }
ticksWithLabels count specifier scale =
  let
    formatter = Scale.tickFormat count specifier scale
    tickVals = Scale.ticks count scale
  in
    tickVals <#> \t -> { position: Scale.applyScale scale t, label: formatter t }

-- ============================================================================
-- INTERPOLATION COMBINATORS
-- ============================================================================

-- | Blend two interpolators together
blendInterpolators :: Number -> Interpolator String -> Interpolator String -> Interpolator String
blendInterpolators mix i1 i2 = \t ->
  if t < mix then i1 (t / mix) else i2 ((t - mix) / (1.0 - mix))

-- | Reverse an interpolator (1-t instead of t)
reverseInterpolator :: forall a. Interpolator a -> Interpolator a
reverseInterpolator interp = \t -> interp (1.0 - t)

-- | Clamp interpolator input to [0, 1]
clampInterpolator :: forall a. Interpolator a -> Interpolator a
clampInterpolator interp = \t -> interp (max 0.0 (min 1.0 t))

-- | Make an interpolator cycle (values outside [0,1] wrap)
cycleInterpolator :: forall a. Interpolator a -> Interpolator a
cycleInterpolator interp = \t ->
  let t' = t - Int.toNumber (Int.floor t)
  in interp (if t' < 0.0 then t' + 1.0 else t')

-- ============================================================================
-- SCALE TRANSFORMATIONS
-- ============================================================================

-- | Create a normalizing scale (maps domain to [0, 1])
normalize :: Number -> Number -> ContinuousScale
normalize minVal maxVal =
  Pure.linear # Scale.domain [minVal, maxVal] # Scale.range [0.0, 1.0]

-- | Create a quantizing scale (continuous -> discrete buckets)
quantize :: forall a. NonEmptyArray a -> Number -> Number -> Number -> a
quantize buckets minVal maxVal value =
  let
    arr = NEA.toArray buckets
    n = Array.length arr
    normalized = (value - minVal) / (maxVal - minVal)
    idx = min (n - 1) (max 0 (Int.floor (normalized * Int.toNumber n)))
  in
    fromMaybe (NEA.head buckets) (Array.index arr idx)

-- | Create a threshold scale with custom breakpoints
threshold :: forall a. Array Number -> NonEmptyArray a -> Number -> a
threshold thresholds values value =
  let
    arr = NEA.toArray values
    idx = Array.length (Array.filter (_ <= value) thresholds)
  in
    fromMaybe (NEA.head values) (Array.index arr (min idx (Array.length arr - 1)))

-- ============================================================================
-- USEFUL COMBINATORS
-- ============================================================================

-- | Get the extent (min, max) of a scale's output for inputs in [0, 1]
scaleExtent :: ContinuousScale -> { min :: Number, max :: Number }
scaleExtent scale =
  let
    v0 = Scale.applyScale scale 0.0
    v1 = Scale.applyScale scale 1.0
  in
    { min: min v0 v1, max: max v0 v1 }

-- | Get the midpoint of a scale
scaleMidpoint :: ContinuousScale -> Number
scaleMidpoint scale = Scale.applyScale scale 0.5

-- | Check if a value is within a scale's output range
scaleInRange :: ContinuousScale -> Number -> Boolean
scaleInRange scale value =
  let
    ext = scaleExtent scale
  in
    value >= ext.min && value <= ext.max
