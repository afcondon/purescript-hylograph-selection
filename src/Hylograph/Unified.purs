-- | PSD3.Unified - Unified Data DSL for Visualization and Computation
-- |
-- | This module re-exports the unified DSL components, providing a single
-- | import for the new unified system.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import Hylograph.Unified
-- |
-- | -- Define a computation that works in both viz and spreadsheet
-- | growthAnalysis :: forall repr. DataDSL repr => repr (Array Number) -> repr Number
-- | growthAnalysis data_ = avgA (mapA (\x -> x * 1.1) data_)
-- |
-- | -- Use Display for formatting
-- | myDisplay :: Display Number String
-- | myDisplay = roundD 2 >>> showNumD >>> suffixD "%"
-- |
-- | -- Create attributes with Display
-- | myAttr :: Attribute DataPoint
-- | myAttr = attr "cx" _.x (scaleD 10.0 >>> showNumD)
-- | ```
-- |
-- | ## Module Organization
-- |
-- | - `Hylograph.Unified.DataDSL` - Core type class for data operations
-- | - `Hylograph.Unified.Display` - Profunctor-based display formatting
-- | - `Hylograph.Unified.Attribute` - Bridge to attribute system
-- | - `Hylograph.Unified.Sugar` - Syntactic sugar operators
module Hylograph.Unified
  ( -- * DataDSL
    module DataDSL
    -- * TrigDSL
  , module TrigDSL
    -- * Display
  , module Display
    -- * Attributes
  , module Attribute
    -- * Sugar
  , module Sugar
  ) where

import Hylograph.Unified.DataDSL
  ( class DataDSL
  , num, str, bool, arr
  , source
  , mapA, foldA, filterA, flatMapA, zipWithA, headA
  , add, sub, mul, div, negate
  , lt, lte, gt, gte, eqNum
  , strEq, strNeq, concat
  , and, or, not
  , ifThenElse
  , sumA, avgA, countA, maxA, minA, productA, absA, negateA
  , DataSource(..)
  , CellAddr
  , TypedCell(..)
  ) as DataDSL

import Hylograph.Unified.DataDSL
  ( class TrigDSL
  , sin, cos, tan, asin, acos, atan, atan2, pi
  ) as TrigDSL

import Hylograph.Unified.Display
  ( Display(..)
  , runDisplay
  , idD, composeD, (>>>)
  , lmapD, rmapD, dimapD
  , roundD, scaleD, clampD, absD, negateD, floorD, ceilD
  , showNumD, fixedD, sciD, intD
  , prefixD, suffixD, padLeftD, padRightD, upperD, lowerD, trimD
  , percentageD, currencyD, currencyWithSymbol, thousandsD, signedD, compactD
  , boolD, yesNoD
  ) as Display

import Hylograph.Unified.Attribute
  ( attr, attrStatic, attrIndexed
  , cxD, cyD, xD, yD, rD, widthD, heightD
  , fillD, strokeD, opacityD, textContentD, transformD
  , toAttribute, fromDisplay
  ) as Attribute

import Hylograph.Unified.Sugar
  ( (+.), addOp
  , (-.), subOp
  , (*.), mulOp
  , (/.), divOp
  , neg
  , (+:), addLit
  , (-:), subLit
  , (*:), mulLit
  , (/:), divLit
  , (<+>), concatOp
  , (<.), ltOp
  , (<=.), lteOp
  , (>.), gtOp
  , (>=.), gteOp
  , (==.), eqOp
  , (===), strEqOp
  , (/==), strNeqOp
  , n, s, b, ite
  ) as Sugar
