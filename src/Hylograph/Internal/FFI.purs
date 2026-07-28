-- | Internal: Low-level FFI bindings for selection utilities.
-- |
-- | This module contains minimal JavaScript FFI declarations for:
-- | - Index/key extraction from simulation data
-- |
-- | **Internal module** - prefer the high-level APIs in `Hylograph.HATS`.
-- |
-- | NOTE: All force simulation FFI has been moved to hylograph-simulation.
-- | NOTE: All D3 selection operations have been replaced with PureScript web-dom.
-- | NOTE: Arc generator has been replaced with pure PureScript in Hylograph.Shape.Arc.
module Hylograph.Internal.FFI
  ( getIndexFromDatum_
  , ComputeKeyFunction_
  , keyIsID_
  , keyIsSourceTarget_
  , swizzledLinkKey_
  ) where

import Hylograph.Foreign.Types (Datum_, Index_)

foreign import getIndexFromDatum_    :: Datum_ -> Int

type ComputeKeyFunction_ d key = d -> key
foreign import keyIsID_           :: forall d. ComputeKeyFunction_ d Index_
foreign import keyIsSourceTarget_ :: forall d. ComputeKeyFunction_ d Index_
foreign import swizzledLinkKey_   :: forall d. ComputeKeyFunction_ d String
