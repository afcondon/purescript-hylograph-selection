-- | Hylograph.Scale.Pure - Pure PureScript implementation of continuous scales
-- |
-- | This module provides the same API as `Hylograph.Scale` for continuous scales
-- | but without any FFI or D3 dependency. Scales are represented as records
-- | containing forward/inverse functions plus metadata.
-- |
-- | The key insight: a scale is just a function with metadata. The domain and
-- | range define a linear (or transformed) mapping; configuration functions
-- | rebuild the internal functions accordingly.
-- |
-- | ## Basic Usage
-- |
-- | ```purescript
-- | import Hylograph.Scale.Pure
-- |
-- | myScale = linear # domain [0.0, 100.0] # range [0.0, 800.0]
-- | pixelX = applyScale myScale 50.0  -- Returns 400.0
-- | tickValues = ticks 10 myScale     -- Returns nice tick values
-- | ```
-- |
-- | ## Pipe-style Configuration
-- |
-- | All configuration functions are designed for `#` pipe syntax:
-- |
-- | ```purescript
-- | scale = linear
-- |   # domain [0.0, 100.0]
-- |   # range [0.0, 500.0]
-- |   # clamp true
-- |   # nice
-- | ```
module Hylograph.Scale.Pure
  ( -- * Scale Type
    Scale(..)
  , ContinuousScale
  , Continuous

  -- * Continuous Scale Constructors
  , linear
  , pow
  , sqrt
  , log

  -- * Scale Configuration
  , domain
  , range
  , clamp
  , nice
  , niceCount
  , exponent
  , base

  -- * Scale Operations
  , applyScale
  , invert
  , ticks
  , copy

  -- * Functional Combinators
  , andThen
  , contramap
  , map
  ) where

import Prelude hiding (map)

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Num

-- ============================================================================
-- SCALE TYPES
-- ============================================================================

-- | Phantom type for continuous scales
data Continuous

-- | A pure scale is a record holding the forward mapping function,
-- | an inverse function, the current domain and range arrays,
-- | clamping state, a tick generator, and transform-specific parameters.
-- |
-- | The phantom types mirror those in `Hylograph.Scale`:
-- |   - `domain` - the input type (always Number for continuous scales)
-- |   - `range`  - the output type (always Number for continuous scales)
-- |   - `kind`   - the scale kind (always Continuous here)
newtype Scale domain range kind = Scale
  { forward  :: domain -> range
  , inverse  :: range -> Maybe domain
  , domain_  :: Array domain
  , range_   :: Array range
  , clamped  :: Boolean
  , ticks_   :: Int -> Array domain
  -- Internal bookkeeping for rebuilding after config changes
  , transform :: Number -> Number
  , untransform :: Number -> Number
  , exponent_ :: Number
  , base_ :: Number
  }

-- | Convenient alias matching `Hylograph.Scale.ContinuousScale`
type ContinuousScale = Scale Number Number Continuous

-- ============================================================================
-- INTERNAL HELPERS
-- ============================================================================

-- | Retrieve the first element of an array, defaulting to 0.0
head0 :: Array Number -> Number
head0 arr = fromMaybe 0.0 (Array.head arr)

-- | Retrieve the last element of an array, defaulting to 1.0
last1 :: Array Number -> Number
last1 arr = fromMaybe 1.0 (Array.last arr)

-- | Clamp a number to [lo, hi]
clampNum :: Number -> Number -> Number -> Number
clampNum lo hi x =
  if x < lo then lo
  else if x > hi then hi
  else x

-- | Round to nearest integer (half-up), matching JavaScript's Math.round
roundNum :: Number -> Number
roundNum x = Num.floor (x + 0.5)

-- | Build the forward and inverse functions from transform, domain, range, and clamp settings.
-- | The mapping is: normalize in domain via transform, then interpolate into range.
buildForward
  :: (Number -> Number)
  -> (Number -> Number)
  -> Array Number
  -> Array Number
  -> Boolean
  -> (Number -> Number)
buildForward transform _untransform dom rng isClamped =
  let
    d0 = head0 dom
    d1 = last1 dom
    r0 = head0 rng
    r1 = last1 rng
    t0 = transform d0
    t1 = transform d1
    tSpan = t1 - t0
  in
    \x ->
      let
        tx = transform x
        -- Normalize to [0, 1] in transformed space
        t = if tSpan == 0.0 then 0.5 else (tx - t0) / tSpan
        -- Interpolate into range
        raw = r0 + t * (r1 - r0)
      in
        if isClamped then
          clampNum (Num.min r0 r1) (Num.max r0 r1) raw
        else
          raw

buildInverse
  :: (Number -> Number)
  -> (Number -> Number)
  -> Array Number
  -> Array Number
  -> Boolean
  -> (Number -> Maybe Number)
buildInverse transform untransform dom rng isClamped =
  let
    d0 = head0 dom
    d1 = last1 dom
    r0 = head0 rng
    r1 = last1 rng
    t0 = transform d0
    t1 = transform d1
    tSpan = t1 - t0
    rSpan = r1 - r0
  in
    \y ->
      if rSpan == 0.0 then Nothing
      else
        let
          -- Normalize y in range to [0, 1]
          t = (y - r0) / rSpan
          -- Map back to transformed domain space
          tx = t0 + t * tSpan
          -- Untransform to get the domain value
          result = untransform tx
        in
          if Num.isFinite result then
            Just (if isClamped then clampNum (Num.min d0 d1) (Num.max d0 d1) result else result)
          else Nothing

-- | Rebuild a scale's forward/inverse/ticks after a configuration change
rebuild :: ContinuousScale -> ContinuousScale
rebuild (Scale s) =
  let
    fwd = buildForward s.transform s.untransform s.domain_ s.range_ s.clamped
    inv = buildInverse s.transform s.untransform s.domain_ s.range_ s.clamped
    tks = \count -> ticksImpl count (head0 s.domain_) (last1 s.domain_)
  in
    Scale s { forward = fwd, inverse = inv, ticks_ = tks }

-- ============================================================================
-- D3-COMPATIBLE TICK ALGORITHM
-- ============================================================================
-- Ported faithfully from https://github.com/d3/d3-array/blob/main/src/ticks.js

-- | D3-compatible tick constants
e10 :: Number
e10 = Num.sqrt 50.0  -- 7.071...

e5 :: Number
e5 = Num.sqrt 10.0   -- 3.162...

e2 :: Number
e2 = Num.sqrt 2.0    -- 1.414...

-- | D3's tickSpec — the core tick computation.
-- | Returns { i1, i2, inc } where i1/i2 are integer-valued boundary indices
-- | and inc encodes the step: positive means multiply, negative means divide by -inc.
type TickSpec = { i1 :: Number, i2 :: Number, inc :: Number }

tickSpec :: Number -> Number -> Number -> TickSpec
tickSpec start stop count =
  let
    step = (stop - start) / max 0.0 count
    power = Num.floor (Num.log step / Num.ln10)
    err = step / Num.pow 10.0 power
    factor = if err >= e10 then 10.0
             else if err >= e5 then 5.0
             else if err >= e2 then 2.0
             else 1.0
  in
    if power < 0.0 then
      let
        inc0 = Num.pow 10.0 (negate power) / factor
        i1_0 = roundNum (start * inc0)
        i2_0 = roundNum (stop * inc0)
        i1 = if i1_0 / inc0 < start then i1_0 + 1.0 else i1_0
        i2 = if i2_0 / inc0 > stop then i2_0 - 1.0 else i2_0
        inc = negate inc0
      in
        if i2 < i1 && 0.5 <= count && count < 2.0
          then tickSpec start stop (count * 2.0)
          else { i1, i2, inc }
    else
      let
        inc0 = Num.pow 10.0 power * factor
        i1_0 = roundNum (start / inc0)
        i2_0 = roundNum (stop / inc0)
        i1 = if i1_0 * inc0 < start then i1_0 + 1.0 else i1_0
        i2 = if i2_0 * inc0 > stop then i2_0 - 1.0 else i2_0
      in
        if i2 < i1 && 0.5 <= count && count < 2.0
          then tickSpec start stop (count * 2.0)
          else { i1, i2, inc: inc0 }

-- | Compute tick increment following D3's tickIncrement.
-- | Returns negative values for step sizes < 1 (encoding trick: -n means 1/n).
-- | This encoding is used by the nice algorithm for correct floor/ceil.
tickIncrement :: Number -> Number -> Int -> Number
tickIncrement start stop count =
  (tickSpec start stop (Int.toNumber count)).inc

-- | Compute tick step size (always positive).
-- | Converts tickIncrement's negative encoding to actual step value.
tickStep :: Number -> Number -> Int -> Number
tickStep start stop count =
  let
    reverse = stop < start
    inc = if reverse
          then tickIncrement stop start count
          else tickIncrement start stop count
    sign = if reverse then -1.0 else 1.0
  in
    sign * (if inc < 0.0 then 1.0 / negate inc else inc)

-- | Generate ticks for a given domain range and count, matching D3 exactly.
ticksImpl :: Int -> Number -> Number -> Array Number
ticksImpl count start stop =
  if count <= 0 then []
  else if start == stop then [start]
  else
    let
      reverse = stop < start
      spec = if reverse
             then tickSpec stop start (Int.toNumber count)
             else tickSpec start stop (Int.toNumber count)
      n = Int.floor (spec.i2 - spec.i1) + 1
    in
      if n <= 0 then []
      else if reverse then
        if spec.inc < 0.0
          then Array.range 0 (n - 1) <#> \i -> (spec.i2 - Int.toNumber i) / negate spec.inc
          else Array.range 0 (n - 1) <#> \i -> (spec.i2 - Int.toNumber i) * spec.inc
      else
        if spec.inc < 0.0
          then Array.range 0 (n - 1) <#> \i -> (spec.i1 + Int.toNumber i) / negate spec.inc
          else Array.range 0 (n - 1) <#> \i -> (spec.i1 + Int.toNumber i) * spec.inc

-- ============================================================================
-- D3-COMPATIBLE NICE ALGORITHM
-- ============================================================================

-- | Expand a domain so that its endpoints land on tick boundaries.
-- | This is D3's "nice" algorithm: find the tick step, then floor the min
-- | and ceil the max to multiples of that step.
niceImpl :: Int -> Number -> Number -> { min :: Number, max :: Number }
niceImpl count lo hi =
  if lo == hi then { min: lo, max: hi }
  else if lo > hi then
    -- Reversed domain: nice the swapped version, then swap back
    let result = niceImpl count hi lo
    in { min: result.max, max: result.min }
  else
    -- D3's nice is iterative: apply floor/ceil, recompute step, repeat until stable
    -- NaN sentinel: never equals any step, matching D3's initial `undefined`
    niceLoop 10 (0.0 / 0.0) lo hi count

-- | Iterative nice loop — matches D3's behavior exactly
-- | Uses tickIncrement (with negative encoding for sub-1 steps)
-- | Recomputes after each floor/ceil pass until the increment converges
niceLoop :: Int -> Number -> Number -> Number -> Int -> { min :: Number, max :: Number }
niceLoop maxIter prestep lo hi count =
  if maxIter <= 0 then { min: lo, max: hi }
  else
    let step = tickIncrement lo hi count
    in
      if step == prestep then { min: lo, max: hi }
      else if step > 0.0 then
        let
          newLo = Num.floor (lo / step) * step
          newHi = Num.ceil (hi / step) * step
        in niceLoop (maxIter - 1) step newLo newHi count
      else if step < 0.0 then
        let
          negStep = negate step
          newLo = Num.floor (lo * negStep) / negStep
          newHi = Num.ceil (hi * negStep) / negStep
        in niceLoop (maxIter - 1) step newLo newHi count
      else { min: lo, max: hi }

-- ============================================================================
-- CONTINUOUS SCALE CONSTRUCTORS
-- ============================================================================

-- | Create a linear scale with default domain [0, 1] and range [0, 1]
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | applyScale scale 50.0  -- Returns 250.0
-- | ```
linear :: ContinuousScale
linear = rebuild $ Scale
  { forward: identity
  , inverse: Just
  , domain_: [0.0, 1.0]
  , range_: [0.0, 1.0]
  , clamped: false
  , ticks_: \count -> ticksImpl count 0.0 1.0
  , transform: identity
  , untransform: identity
  , exponent_: 1.0
  , base_: 10.0
  }

-- | Create a power scale with configurable exponent (default exponent 1.0)
-- |
-- | ```purescript
-- | scale = pow # exponent 2.0 # domain [0.0, 10.0] # range [0.0, 100.0]
-- | ```
pow :: ContinuousScale
pow = rebuild $ Scale
  { forward: identity
  , inverse: Just
  , domain_: [0.0, 1.0]
  , range_: [0.0, 1.0]
  , clamped: false
  , ticks_: \count -> ticksImpl count 0.0 1.0
  , transform: \x -> Num.pow x 1.0
  , untransform: \x -> Num.pow x 1.0
  , exponent_: 1.0
  , base_: 10.0
  }

-- | Create a square root scale (power scale with exponent 0.5)
-- |
-- | Useful for sizing circles by area:
-- |
-- | ```purescript
-- | radiusScale = sqrt # domain [0.0, maxValue] # range [0.0, 50.0]
-- | ```
sqrt :: ContinuousScale
sqrt =
  let
    exp_ = 0.5
    invExp = 1.0 / exp_
    xform = \x -> signedPow x exp_
    inv = \x -> signedPow x invExp
  in
    rebuild $ Scale
      { forward: identity
      , inverse: Just
      , domain_: [0.0, 1.0]
      , range_: [0.0, 1.0]
      , clamped: false
      , ticks_: \count -> ticksImpl count 0.0 1.0
      , transform: xform
      , untransform: inv
      , exponent_: exp_
      , base_: 10.0
      }

-- | Create a logarithmic scale (default base 10)
-- |
-- | Domain must not include zero.
-- |
-- | ```purescript
-- | scale = log # domain [1.0, 1000.0] # range [0.0, 300.0]
-- | ```
log :: ContinuousScale
log =
  let
    b = 10.0
    logB = Num.log b
    xform = \x -> Num.log x / logB
    inv = \x -> Num.pow b x
  in
    rebuild $ Scale
      { forward: identity
      , inverse: Just
      , domain_: [1.0, 10.0]
      , range_: [0.0, 1.0]
      , clamped: false
      , ticks_: \count -> ticksImpl count 1.0 10.0
      , transform: xform
      , untransform: inv
      , exponent_: 1.0
      , base_: b
      }

-- | Raise x to an exponent, preserving sign for negative values.
-- | This matches D3's behavior for power scales with negative domain values.
signedPow :: Number -> Number -> Number
signedPow x exp_ =
  if x < 0.0 then negate (Num.pow (negate x) exp_)
  else Num.pow x exp_

-- ============================================================================
-- SCALE CONFIGURATION
-- ============================================================================

-- | Set the domain (input extent) of a scale
-- |
-- | ```purescript
-- | myScale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | ```
domain :: Array Number -> ContinuousScale -> ContinuousScale
domain dom (Scale s) = rebuild $ Scale s { domain_ = dom }

-- | Set the range (output extent) of a scale
-- |
-- | ```purescript
-- | myScale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | ```
range :: Array Number -> ContinuousScale -> ContinuousScale
range rng (Scale s) = rebuild $ Scale s { range_ = rng }

-- | Enable or disable clamping
-- |
-- | When enabled, output values are constrained to the range
-- | even for out-of-domain inputs.
-- |
-- | ```purescript
-- | clamped = linear # domain [0.0, 100.0] # range [0.0, 500.0] # clamp true
-- | applyScale clamped 200.0  -- Returns 500.0, not 1000.0
-- | ```
clamp :: Boolean -> ContinuousScale -> ContinuousScale
clamp c (Scale s) = rebuild $ Scale s { clamped = c }

-- | Extend the domain to nice round values
-- |
-- | Uses 10 as the default tick count hint for computing step size.
-- |
-- | ```purescript
-- | niceScale = linear # domain [3.0, 97.0] # nice
-- | -- Domain becomes approximately [0.0, 100.0]
-- | ```
nice :: ContinuousScale -> ContinuousScale
nice = niceCount 10

-- | Extend the domain to nice round values with a specified tick count hint
-- |
-- | ```purescript
-- | niceScale = linear # domain [3.0, 97.0] # niceCount 5
-- | ```
niceCount :: Int -> ContinuousScale -> ContinuousScale
niceCount count (Scale s) =
  let
    lo = head0 s.domain_
    hi = last1 s.domain_
    niced = niceImpl count lo hi
    newDom = [niced.min, niced.max]
  in
    rebuild $ Scale s { domain_ = newDom }

-- | Set the exponent for a power scale
-- |
-- | ```purescript
-- | squareScale = pow # exponent 2.0 # domain [0.0, 10.0] # range [0.0, 100.0]
-- | ```
exponent :: Number -> ContinuousScale -> ContinuousScale
exponent exp_ (Scale s) =
  let
    invExp = 1.0 / exp_
    xform = \x -> signedPow x exp_
    inv = \x -> signedPow x invExp
  in
    rebuild $ Scale s { transform = xform, untransform = inv, exponent_ = exp_ }

-- | Set the base for a logarithmic scale (default 10)
-- |
-- | ```purescript
-- | scale = log # base 2.0 # domain [1.0, 1024.0] # range [0.0, 10.0]
-- | ```
base :: Number -> ContinuousScale -> ContinuousScale
base b (Scale s) =
  let
    logB = Num.log b
    xform = \x -> Num.log x / logB
    inv = \x -> Num.pow b x
  in
    rebuild $ Scale s { transform = xform, untransform = inv, base_ = b }

-- ============================================================================
-- SCALE OPERATIONS
-- ============================================================================

-- | Apply a scale to a domain value, producing a range value
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | applyScale scale 50.0  -- Returns 250.0
-- | ```
applyScale :: ContinuousScale -> Number -> Number
applyScale (Scale s) = s.forward

-- | Invert a continuous scale (range value back to domain value)
-- |
-- | Returns `Nothing` if the inversion is not defined (e.g. division by zero).
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | invert scale 250.0  -- Returns Just 50.0
-- | ```
invert :: ContinuousScale -> Number -> Maybe Number
invert (Scale s) = s.inverse

-- | Generate nice tick positions for a scale
-- |
-- | Uses D3's tick algorithm to find "nice" numbers (multiples of 1, 2, or 5
-- | times a power of 10).
-- |
-- | ```purescript
-- | scale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | ticks 5 scale  -- Returns [0.0, 20.0, 40.0, 60.0, 80.0, 100.0]
-- | ```
ticks :: Int -> ContinuousScale -> Array Number
ticks count (Scale s) = s.ticks_ count

-- | Create a copy of a scale (identity operation for pure data, included
-- | for API compatibility with the D3 FFI module)
copy :: ContinuousScale -> ContinuousScale
copy (Scale s) = Scale s

-- ============================================================================
-- FUNCTIONAL COMBINATORS
-- ============================================================================

-- | Compose two scales: apply the first, then the second
-- |
-- | ```purescript
-- | normalize = linear # domain [0.0, 100.0] # range [0.0, 1.0]
-- | toPixels  = linear # domain [0.0, 1.0]   # range [0.0, 500.0]
-- | combined  = normalize `andThen` toPixels
-- | combined 50.0  -- Returns 250.0
-- | ```
andThen :: ContinuousScale -> ContinuousScale -> (Number -> Number)
andThen s1 s2 = applyScale s2 <<< applyScale s1

-- | Transform the input before applying the scale (contravariant mapping)
-- |
-- | ```purescript
-- | fahrenheitScale = linear # domain [32.0, 212.0] # range [0.0, 100.0]
-- | celsiusScale = contramap (\c -> c * 9.0 / 5.0 + 32.0) fahrenheitScale
-- | celsiusScale 100.0  -- Returns 100.0
-- | ```
contramap :: forall a. (a -> Number) -> ContinuousScale -> (a -> Number)
contramap f scale = applyScale scale <<< f

-- | Transform the output after applying the scale (covariant/functor-like)
-- |
-- | ```purescript
-- | baseScale = linear # domain [0.0, 100.0] # range [0.0, 500.0]
-- | offsetScale = map (_ + 50.0) baseScale
-- | offsetScale 0.0  -- Returns 50.0
-- | ```
map :: forall b. (Number -> b) -> ContinuousScale -> (Number -> b)
map f scale = f <<< applyScale scale
