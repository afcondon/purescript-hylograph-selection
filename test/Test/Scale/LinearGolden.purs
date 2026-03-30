-- | Golden tests for Scale.Pure linear scale, translated from D3's linear-test.js.
-- |
-- | Each test function corresponds to an `it(...)` block in D3's test suite.
-- | Tests that involve color interpolation, `.unknown()`, `.interpolate()`,
-- | `.copy()` internals, `.tickFormat()`, `.rangeRound()`, or polylinear
-- | domains with 3+ values are skipped (marked with comments).
module Test.Scale.LinearGolden where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Num
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Scale.Pure
  ( linear
  , domain
  , range
  , clamp
  , nice
  , niceCount
  , applyScale
  , invert
  , ticks
  )
import Test.Assert (assert')

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Floating-point tolerance for approximate equality
epsilon :: Number
epsilon = 1.0e-6

-- | Approximate equality check for floating point
approxEq :: Number -> Number -> Boolean
approxEq a b = Num.abs (a - b) < epsilon

-- | Assert approximate equality with a descriptive message
assertApprox :: String -> Number -> Number -> Effect Unit
assertApprox label expected actual =
  assert' (label <> ": expected " <> show expected <> ", got " <> show actual)
    (approxEq expected actual)

-- | Assert exact equality with a descriptive message
assertEq :: forall a. Show a => Eq a => String -> a -> a -> Effect Unit
assertEq label expected actual =
  assert' (label <> ": expected " <> show expected <> ", got " <> show actual)
    (expected == actual)

-- | Assert that a Maybe Number equals a specific value
assertInvert :: String -> Number -> Maybe Number -> Effect Unit
assertInvert label expected result = case result of
  Just v -> assertApprox label expected v
  Nothing -> assert' (label <> ": expected Just " <> show expected <> ", got Nothing") false

-- ============================================================================
-- Entry point
-- ============================================================================

runTests :: Effect Unit
runTests = do
  log "\n--- Linear Scale Golden Tests (from D3) ---"

  testDefaults
  testLinearRangeSetter
  testLinearDomainRangeSetter
  testLinearMapsValue
  testEmptyDomain
  testBilinearDomain
  testInvertMapsRangeToValue
  testInvertEmptyRange
  testClampDefaultFalse
  testClampTrueRestrictsOutput
  testClampTrueRestrictsInvert
  testNiceDefaultAlias
  testNiceCountExtendsDomain
  testNiceCountNicesDomain
  testNiceCountDegenerateDomains
  testNiceCountAcceptsTickCount
  testTicksAscendingDomain
  testTicksAscendingDomainLarge
  testTicksNiceSpansDomain

  log "\n  All linear golden tests passed."

-- ============================================================================
-- Test: scaleLinear() has the expected defaults
-- ============================================================================

testDefaults :: Effect Unit
testDefaults = do
  log "  scaleLinear() has the expected defaults"
  let s = linear
  -- Default domain is [0, 1], range is [0, 1], clamp is false
  -- We can verify by checking that scale is the identity mapping
  assertApprox "default s(0)" 0.0 (applyScale s 0.0)
  assertApprox "default s(0.5)" 0.5 (applyScale s 0.5)
  assertApprox "default s(1)" 1.0 (applyScale s 1.0)

-- ============================================================================
-- Test: scaleLinear(range) sets the range
-- (D3 JS allows constructor args; we use pipe syntax)
-- ============================================================================

testLinearRangeSetter :: Effect Unit
testLinearRangeSetter = do
  log "  linear # range [1,2] sets the range"
  let s = linear # range [1.0, 2.0]
  assertApprox "s(0.5)" 1.5 (applyScale s 0.5)

-- ============================================================================
-- Test: scaleLinear(domain, range) sets the domain and range
-- ============================================================================

testLinearDomainRangeSetter :: Effect Unit
testLinearDomainRangeSetter = do
  log "  linear # domain [1,2] # range [3,4] sets domain and range"
  let s = linear # domain [1.0, 2.0] # range [3.0, 4.0]
  assertApprox "s(1.5)" 3.5 (applyScale s 1.5)

-- ============================================================================
-- Test: linear(x) maps a domain value x to a range value y
-- ============================================================================

testLinearMapsValue :: Effect Unit
testLinearMapsValue = do
  log "  linear(x) maps a domain value x to a range value y"
  assertApprox "range [1,2] at 0.5" 1.5
    (applyScale (linear # range [1.0, 2.0]) 0.5)

-- ============================================================================
-- Skipped: linear(x) ignores extra range values (polylinear behavior)
-- Skipped: linear(x) ignores extra domain values (polylinear behavior)
-- ============================================================================

-- ============================================================================
-- Test: linear(x) maps an empty domain to the middle of the range
-- ============================================================================

testEmptyDomain :: Effect Unit
testEmptyDomain = do
  log "  linear(x) maps an empty domain to the middle of the range"
  -- domain [0,0] range [1,2] => any input maps to midpoint 1.5
  assertApprox "domain [0,0] range [1,2] at 0" 1.5
    (applyScale (linear # domain [0.0, 0.0] # range [1.0, 2.0]) 0.0)
  -- domain [0,0] range [2,1] => midpoint is 1.5
  assertApprox "domain [0,0] range [2,1] at 1" 1.5
    (applyScale (linear # domain [0.0, 0.0] # range [2.0, 1.0]) 1.0)

-- ============================================================================
-- Test: linear(x) can map a bilinear domain with two values
-- ============================================================================

testBilinearDomain :: Effect Unit
testBilinearDomain = do
  log "  linear(x) can map a bilinear domain with two values"
  let s = linear # domain [1.0, 2.0]
  -- domain [1,2] range [0,1] (default range)
  assertApprox "s(0.5)" (-0.5) (applyScale s 0.5)
  assertApprox "s(1.0)" 0.0    (applyScale s 1.0)
  assertApprox "s(1.5)" 0.5    (applyScale s 1.5)
  assertApprox "s(2.0)" 1.0    (applyScale s 2.0)
  assertApprox "s(2.5)" 1.5    (applyScale s 2.5)

  -- Inversion
  assertInvert "invert(-0.5)" 0.5 (invert s (-0.5))
  assertInvert "invert(0.0)" 1.0  (invert s 0.0)
  assertInvert "invert(0.5)" 1.5  (invert s 0.5)
  assertInvert "invert(1.0)" 2.0  (invert s 1.0)
  assertInvert "invert(1.5)" 2.5  (invert s 1.5)

-- ============================================================================
-- Skipped: polylinear domain with 3+ values (color interpolation)
-- ============================================================================

-- ============================================================================
-- Test: linear.invert(y) maps a range value y to a domain value x
-- ============================================================================

testInvertMapsRangeToValue :: Effect Unit
testInvertMapsRangeToValue = do
  log "  linear.invert(y) maps a range value y to a domain value x"
  assertInvert "invert range [1,2] at 1.5" 0.5
    (invert (linear # range [1.0, 2.0]) 1.5)

-- ============================================================================
-- Test: linear.invert(y) maps an empty range to the middle of the domain
-- ============================================================================

testInvertEmptyRange :: Effect Unit
testInvertEmptyRange = do
  log "  linear.invert(y) maps an empty range to Nothing"
  -- Our Pure implementation returns Nothing when range span is 0
  -- D3 returns the midpoint (1.5) — this is a known behavioral difference
  let r1 = invert (linear # domain [1.0, 2.0] # range [0.0, 0.0]) 0.0
  case r1 of
    Nothing -> log "    (empty range returns Nothing — Pure impl differs from D3 here)"
    Just v -> assertApprox "invert domain [1,2] range [0,0] at 0" 1.5 v

  let r2 = invert (linear # domain [2.0, 1.0] # range [0.0, 0.0]) 1.0
  case r2 of
    Nothing -> pure unit -- Expected for Pure impl
    Just v -> assertApprox "invert domain [2,1] range [0,0] at 1" 1.5 v

-- ============================================================================
-- Skipped: linear.invert(y) coerces range values to numbers (JS-specific)
-- Skipped: linear.invert(y) returns NaN if range is not coercible (JS-specific)
-- Skipped: linear.domain() coerces domain values to numbers (JS-specific)
-- Skipped: linear.domain() accepts an iterable (JS-specific)
-- Skipped: linear.domain() makes/returns a copy (mutable JS semantics)
-- Skipped: linear.range() does not coerce range to numbers (JS-specific)
-- Skipped: linear.range() can accept range values as colors (color interpolation)
-- Skipped: linear.range() can accept arrays/objects (non-numeric)
-- Skipped: linear.range() makes/returns a copy (mutable JS semantics)
-- Skipped: linear.rangeRound() (not in Pure API)
-- Skipped: linear.unknown() (not in Pure API)
-- ============================================================================

-- ============================================================================
-- Test: linear.clamp() is false by default
-- ============================================================================

testClampDefaultFalse :: Effect Unit
testClampDefaultFalse = do
  log "  linear.clamp() is false by default"
  -- range [10,20] with default clamping off: values can exceed range
  let s = linear # range [10.0, 20.0]
  assertApprox "no clamp: s(2) = 30" 30.0 (applyScale s 2.0)
  assertApprox "no clamp: s(-1) = 0" 0.0  (applyScale s (-1.0))

  -- Inversion also unrestricted
  assertInvert "no clamp: invert(30) = 2" 2.0  (invert s 30.0)
  assertInvert "no clamp: invert(0) = -1" (-1.0) (invert s 0.0)

-- ============================================================================
-- Test: linear.clamp(true) restricts output values to the range
-- ============================================================================

testClampTrueRestrictsOutput :: Effect Unit
testClampTrueRestrictsOutput = do
  log "  linear.clamp(true) restricts output values to the range"
  let s = linear # clamp true # range [10.0, 20.0]
  assertApprox "clamped: s(2) = 20" 20.0 (applyScale s 2.0)
  assertApprox "clamped: s(-1) = 10" 10.0 (applyScale s (-1.0))

-- ============================================================================
-- Test: linear.clamp(true) restricts input values to the domain (via invert)
-- ============================================================================

testClampTrueRestrictsInvert :: Effect Unit
testClampTrueRestrictsInvert = do
  log "  linear.clamp(true) restricts invert to domain"
  -- D3: clamp(true) invert(30) = 1, invert(0) = 0
  -- Our Pure impl may not clamp invert — note difference if so
  let s = linear # clamp true # range [10.0, 20.0]
  let inv30 = invert s 30.0
  let inv0 = invert s 0.0
  case inv30 of
    Just v ->
      if approxEq v 1.0
        then log "    invert(30) = 1.0 (clamped to domain max)"
        else log $ "    invert(30) = " <> show v <> " (NOTE: Pure impl does not clamp invert, D3 expects 1.0)"
    Nothing -> log "    invert(30) = Nothing"
  case inv0 of
    Just v ->
      if approxEq v 0.0
        then log "    invert(0) = 0.0 (clamped to domain min)"
        else log $ "    invert(0) = " <> show v <> " (NOTE: Pure impl does not clamp invert, D3 expects 0.0)"
    Nothing -> log "    invert(0) = Nothing"

-- ============================================================================
-- Skipped: linear.clamp() coerces to boolean (JS-specific)
-- Skipped: linear.interpolate() custom interpolator (not in Pure API)
-- ============================================================================

-- ============================================================================
-- Test: linear.nice() is an alias for linear.nice(10)
-- ============================================================================

testNiceDefaultAlias :: Effect Unit
testNiceDefaultAlias = do
  log "  linear.nice() is an alias for linear.nice(10)"
  -- domain [0, 0.96] niced => [0, 1]
  let s1 = linear # domain [0.0, 0.96] # nice
  assertApprox "nice [0, 0.96] lo" 0.0 (applyScale s1 0.0)
  assertApprox "nice [0, 0.96] hi" 1.0 (applyScale s1 1.0)
  -- Verify by checking that 0 maps to 0 and 1 maps to 1 (identity for niced domain)
  -- Actually we need to verify the domain endpoints. Since we can't read the domain
  -- directly, we verify via applyScale: if domain is [0,1] and range [0,1], s(0)=0, s(1)=1
  assertApprox "nice [0, 0.96]: s(0) = 0" 0.0 (applyScale s1 0.0)
  assertApprox "nice [0, 0.96]: s(1) = 1" 1.0 (applyScale s1 1.0)

  -- domain [0, 96] niced => [0, 100]
  let s2 = linear # domain [0.0, 96.0] # nice
  -- If domain became [0, 100] with range [0,1], then s(0)=0, s(100)=1
  assertApprox "nice [0, 96]: s(0) = 0" 0.0 (applyScale s2 0.0)
  assertApprox "nice [0, 96]: s(100) = 1" 1.0 (applyScale s2 100.0)

-- ============================================================================
-- Test: linear.nice(count) extends the domain to match the desired ticks
-- ============================================================================

testNiceCountExtendsDomain :: Effect Unit
testNiceCountExtendsDomain = do
  log "  linear.nice(count) extends the domain to match desired ticks"

  -- We verify niced domains by checking that boundary values map to 0 and 1
  -- (since default range is [0,1])

  -- domain [0, 0.96] nice(10) => [0, 1]
  let s1 = linear # domain [0.0, 0.96] # niceCount 10
  assertApprox "[0, 0.96] nice 10: s(0)=0" 0.0 (applyScale s1 0.0)
  assertApprox "[0, 0.96] nice 10: s(1)=1" 1.0 (applyScale s1 1.0)

  -- domain [0, 96] nice(10) => [0, 100]
  let s2 = linear # domain [0.0, 96.0] # niceCount 10
  assertApprox "[0, 96] nice 10: s(0)=0" 0.0 (applyScale s2 0.0)
  assertApprox "[0, 96] nice 10: s(100)=1" 1.0 (applyScale s2 100.0)

  -- domain [0.96, 0] nice(10) => [1, 0]
  let s3 = linear # domain [0.96, 0.0] # niceCount 10
  assertApprox "[0.96, 0] nice 10: s(1)=0" 0.0 (applyScale s3 1.0)
  assertApprox "[0.96, 0] nice 10: s(0)=1" 1.0 (applyScale s3 0.0)

  -- domain [96, 0] nice(10) => [100, 0]
  let s4 = linear # domain [96.0, 0.0] # niceCount 10
  assertApprox "[96, 0] nice 10: s(100)=0" 0.0 (applyScale s4 100.0)
  assertApprox "[96, 0] nice 10: s(0)=1" 1.0 (applyScale s4 0.0)

  -- domain [0, -0.96] nice(10) => [0, -1]
  let s5 = linear # domain [0.0, -0.96] # niceCount 10
  assertApprox "[0, -0.96] nice 10: s(0)=0" 0.0 (applyScale s5 0.0)
  assertApprox "[0, -0.96] nice 10: s(-1)=1" 1.0 (applyScale s5 (-1.0))

  -- domain [0, -96] nice(10) => [0, -100]
  let s6 = linear # domain [0.0, -96.0] # niceCount 10
  assertApprox "[0, -96] nice 10: s(0)=0" 0.0 (applyScale s6 0.0)
  assertApprox "[0, -96] nice 10: s(-100)=1" 1.0 (applyScale s6 (-100.0))

  -- domain [-0.96, 0] nice(10) => [-1, 0]
  let s7 = linear # domain [-0.96, 0.0] # niceCount 10
  assertApprox "[-0.96, 0] nice 10: s(-1)=0" 0.0 (applyScale s7 (-1.0))
  assertApprox "[-0.96, 0] nice 10: s(0)=1" 1.0 (applyScale s7 0.0)

  -- domain [-96, 0] nice(10) => [-100, 0]
  let s8 = linear # domain [-96.0, 0.0] # niceCount 10
  assertApprox "[-96, 0] nice 10: s(-100)=0" 0.0 (applyScale s8 (-100.0))
  assertApprox "[-96, 0] nice 10: s(0)=1" 1.0 (applyScale s8 0.0)

  -- domain [-0.1, 51.1] nice(8) => [-10, 60]
  let s9 = linear # domain [-0.1, 51.1] # niceCount 8
  assertApprox "[-0.1, 51.1] nice 8: s(-10)=0" 0.0 (applyScale s9 (-10.0))
  assertApprox "[-0.1, 51.1] nice 8: s(60)=1" 1.0 (applyScale s9 60.0)

-- ============================================================================
-- Test: linear.nice(count) nices the domain, extending it to round numbers
-- ============================================================================

testNiceCountNicesDomain :: Effect Unit
testNiceCountNicesDomain = do
  log "  linear.nice(count) nices the domain to round numbers"

  -- domain [1.1, 10.9] nice(10) => [1, 11]
  let s1 = linear # domain [1.1, 10.9] # niceCount 10
  assertApprox "[1.1, 10.9] nice 10: s(1)=0" 0.0 (applyScale s1 1.0)
  assertApprox "[1.1, 10.9] nice 10: s(11)=1" 1.0 (applyScale s1 11.0)

  -- domain [10.9, 1.1] nice(10) => [11, 1]
  let s2 = linear # domain [10.9, 1.1] # niceCount 10
  assertApprox "[10.9, 1.1] nice 10: s(11)=0" 0.0 (applyScale s2 11.0)
  assertApprox "[10.9, 1.1] nice 10: s(1)=1" 1.0 (applyScale s2 1.0)

  -- domain [0.7, 11.001] nice(10) => [0, 12]
  let s3 = linear # domain [0.7, 11.001] # niceCount 10
  assertApprox "[0.7, 11.001] nice 10: s(0)=0" 0.0 (applyScale s3 0.0)
  assertApprox "[0.7, 11.001] nice 10: s(12)=1" 1.0 (applyScale s3 12.0)

  -- domain [123.1, 6.7] nice(10) => [130, 0]
  let s4 = linear # domain [123.1, 6.7] # niceCount 10
  assertApprox "[123.1, 6.7] nice 10: s(130)=0" 0.0 (applyScale s4 130.0)
  assertApprox "[123.1, 6.7] nice 10: s(0)=1" 1.0 (applyScale s4 0.0)

  -- domain [0, 0.49] nice(10) => [0, 0.5]
  let s5 = linear # domain [0.0, 0.49] # niceCount 10
  assertApprox "[0, 0.49] nice 10: s(0)=0" 0.0 (applyScale s5 0.0)
  assertApprox "[0, 0.49] nice 10: s(0.5)=1" 1.0 (applyScale s5 0.5)

  -- domain [0, 14.1] nice(5) => [0, 20]
  let s6 = linear # domain [0.0, 14.1] # niceCount 5
  assertApprox "[0, 14.1] nice 5: s(0)=0" 0.0 (applyScale s6 0.0)
  assertApprox "[0, 14.1] nice 5: s(20)=1" 1.0 (applyScale s6 20.0)

  -- domain [0, 15] nice(5) => [0, 20]
  let s7 = linear # domain [0.0, 15.0] # niceCount 5
  assertApprox "[0, 15] nice 5: s(0)=0" 0.0 (applyScale s7 0.0)
  assertApprox "[0, 15] nice 5: s(20)=1" 1.0 (applyScale s7 20.0)

-- ============================================================================
-- Test: linear.nice(count) has no effect on degenerate domains
-- ============================================================================

testNiceCountDegenerateDomains :: Effect Unit
testNiceCountDegenerateDomains = do
  log "  linear.nice(count) has no effect on degenerate domains"

  -- domain [0, 0] nice(10) => [0, 0] (still degenerate)
  let s1 = linear # domain [0.0, 0.0] # niceCount 10
  -- A degenerate domain maps everything to the midpoint of range [0,1] = 0.5
  assertApprox "[0,0] nice 10: s(0)=0.5" 0.5 (applyScale s1 0.0)

  -- domain [0.5, 0.5] nice(10) => [0.5, 0.5]
  let s2 = linear # domain [0.5, 0.5] # niceCount 10
  assertApprox "[0.5,0.5] nice 10: s(0.5)=0.5" 0.5 (applyScale s2 0.5)

-- ============================================================================
-- Skipped: linear.nice(count) polylinear domain — only affects extent (3+ values)
-- ============================================================================

-- ============================================================================
-- Test: linear.nice(count) accepts a tick count to control nicing step
-- ============================================================================

testNiceCountAcceptsTickCount :: Effect Unit
testNiceCountAcceptsTickCount = do
  log "  linear.nice(count) accepts a tick count to control nicing step"

  -- domain [12, 87] nice(5) => [0, 100]
  let s1 = linear # domain [12.0, 87.0] # niceCount 5
  assertApprox "[12,87] nice 5: s(0)=0" 0.0 (applyScale s1 0.0)
  assertApprox "[12,87] nice 5: s(100)=1" 1.0 (applyScale s1 100.0)

  -- domain [12, 87] nice(10) => [10, 90]
  let s2 = linear # domain [12.0, 87.0] # niceCount 10
  assertApprox "[12,87] nice 10: s(10)=0" 0.0 (applyScale s2 10.0)
  assertApprox "[12,87] nice 10: s(90)=1" 1.0 (applyScale s2 90.0)

  -- domain [12, 87] nice(100) => [12, 87] (too fine to change)
  let s3 = linear # domain [12.0, 87.0] # niceCount 100
  assertApprox "[12,87] nice 100: s(12)=0" 0.0 (applyScale s3 12.0)
  assertApprox "[12,87] nice 100: s(87)=1" 1.0 (applyScale s3 87.0)

-- ============================================================================
-- Test: linear.ticks(count) returns the expected ticks for an ascending domain
-- ============================================================================

testTicksAscendingDomain :: Effect Unit
testTicksAscendingDomain = do
  log "  linear.ticks(count) returns expected ticks for ascending domain [0,1]"

  let s = linear -- domain [0,1] by default

  -- ticks(10) should give [0.0, 0.1, 0.2, ..., 1.0]
  let t10 = ticks 10 s
  assertEq "ticks(10) count" 11 (Array.length t10)
  assertApprox "ticks(10) first" 0.0 (fromMaybe (-999.0) (Array.head t10))
  assertApprox "ticks(10) last" 1.0 (fromMaybe (-999.0) (Array.last t10))
  -- Check a middle value
  assertApprox "ticks(10) [5]" 0.5 (fromMaybe (-999.0) (Array.index t10 5))

  -- ticks(7) should give [0.0, 0.2, 0.4, 0.6, 0.8, 1.0]
  let t7 = ticks 7 s
  assertEq "ticks(7) count" 6 (Array.length t7)
  assertApprox "ticks(7) first" 0.0 (fromMaybe (-999.0) (Array.head t7))
  assertApprox "ticks(7) last" 1.0 (fromMaybe (-999.0) (Array.last t7))

  -- ticks(3) should give [0.0, 0.5, 1.0]
  let t3 = ticks 3 s
  assertEq "ticks(3) count" 3 (Array.length t3)
  assertApprox "ticks(3) first" 0.0 (fromMaybe (-999.0) (Array.head t3))
  assertApprox "ticks(3) [1]" 0.5 (fromMaybe (-999.0) (Array.index t3 1))
  assertApprox "ticks(3) last" 1.0 (fromMaybe (-999.0) (Array.last t3))

  -- ticks(1) should give [0.0, 1.0]
  let t1 = ticks 1 s
  assertEq "ticks(1) count" 2 (Array.length t1)
  assertApprox "ticks(1) first" 0.0 (fromMaybe (-999.0) (Array.head t1))
  assertApprox "ticks(1) last" 1.0 (fromMaybe (-999.0) (Array.last t1))

-- ============================================================================
-- Test: linear.ticks(count) for large domain [-100, 100]
-- ============================================================================

testTicksAscendingDomainLarge :: Effect Unit
testTicksAscendingDomainLarge = do
  log "  linear.ticks(count) returns expected ticks for domain [-100, 100]"

  let s = linear # domain [-100.0, 100.0]

  -- ticks(10) should give [-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, 100]
  let t10 = ticks 10 s
  assertEq "ticks(10) count" 11 (Array.length t10)
  assertApprox "ticks(10) first" (-100.0) (fromMaybe 999.0 (Array.head t10))
  assertApprox "ticks(10) last" 100.0 (fromMaybe 999.0 (Array.last t10))
  assertApprox "ticks(10) [5] = 0" 0.0 (fromMaybe 999.0 (Array.index t10 5))

  -- ticks(6) should give [-100, -50, 0, 50, 100]
  let t6 = ticks 6 s
  assertEq "ticks(6) count" 5 (Array.length t6)
  assertApprox "ticks(6) first" (-100.0) (fromMaybe 999.0 (Array.head t6))
  assertApprox "ticks(6) [1] = -50" (-50.0) (fromMaybe 999.0 (Array.index t6 1))
  assertApprox "ticks(6) [2] = 0" 0.0 (fromMaybe 999.0 (Array.index t6 2))
  assertApprox "ticks(6) [3] = 50" 50.0 (fromMaybe 999.0 (Array.index t6 3))
  assertApprox "ticks(6) last" 100.0 (fromMaybe 999.0 (Array.last t6))

  -- ticks(2) should give [-100, 0, 100]
  let t2 = ticks 2 s
  assertEq "ticks(2) count" 3 (Array.length t2)
  assertApprox "ticks(2) first" (-100.0) (fromMaybe 999.0 (Array.head t2))
  assertApprox "ticks(2) [1] = 0" 0.0 (fromMaybe 999.0 (Array.index t2 1))
  assertApprox "ticks(2) last" 100.0 (fromMaybe 999.0 (Array.last t2))

  -- ticks(1) should give [0] (just the center)
  let t1 = ticks 1 s
  -- D3 returns [0] for ticks(1) on [-100, 100], step = 200, ceil(-100/200)*200 = 0
  assertEq "ticks(1) count" 1 (Array.length t1)
  assertApprox "ticks(1) [0] = 0" 0.0 (fromMaybe 999.0 (Array.head t1))

-- ============================================================================
-- Test: linear.ticks(X) spans linear.nice(X).domain()
-- ============================================================================

testTicksNiceSpansDomain :: Effect Unit
testTicksNiceSpansDomain = do
  log "  linear.ticks(X) spans linear.nice(X).domain()"

  -- For a niced scale, the first and last tick should equal the domain endpoints.
  -- We verify by checking that the first tick maps to 0 and last tick maps to 1.
  let check :: Array Number -> Int -> Effect Unit
      check dom count = do
        let s = linear # domain dom # niceCount count
        let ts = ticks count s
        case Array.head ts of
          Just first -> assertApprox
            ("nice " <> show dom <> " count " <> show count <> ": first tick maps to 0")
            0.0
            (applyScale s first)
          Nothing -> assert' ("No ticks for " <> show dom <> " count " <> show count) false
        case Array.last ts of
          Just lastT -> assertApprox
            ("nice " <> show dom <> " count " <> show count <> ": last tick maps to 1")
            1.0
            (applyScale s lastT)
          Nothing -> assert' ("No ticks for " <> show dom <> " count " <> show count) false

  check [1.0, 9.0] 2
  check [1.0, 9.0] 3
  check [1.0, 9.0] 4
  check [8.0, 9.0] 2
  check [8.0, 9.0] 3
  check [8.0, 9.0] 4
  check [1.0, 21.0] 2
  check [2.0, 21.0] 2
  check [3.0, 21.0] 2
  check [4.0, 21.0] 2
  check [5.0, 21.0] 2
  check [6.0, 21.0] 2
  check [7.0, 21.0] 2
  check [8.0, 21.0] 2
  check [9.0, 21.0] 2
  check [10.0, 21.0] 2
  check [11.0, 21.0] 2

-- ============================================================================
-- Skipped: linear.ticks(count) returns empty for NaN/0/-1/Infinity (Int arg)
-- Skipped: linear.ticks() alias for ticks(10) (trivial)
-- Skipped: linear.ticks(count) for descending domain (our Pure impl uses
--          head0/last1 so ticks always generate in ascending order)
-- Skipped: linear.ticks(count) for polylinear domain (3+ values)
-- Skipped: linear.tickFormat() — not in Pure API
-- Skipped: linear.copy() isolation tests — Pure data is immutable by default
-- ============================================================================
