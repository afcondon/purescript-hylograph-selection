-- | DEPRECATED — moved to `Hylograph.Attribute`.
-- |
-- | This module now re-exports the attribute vocabulary from its new public home.
-- | It never should have lived under `Internal`: it is not an
-- | implementation detail, it is vocabulary that every consumer needs.
-- | It landed here by inheritance during an earlier refactor, and by the
-- | time that was noticed several hundred imports across the ecosystem
-- | already pointed at this path.
-- |
-- | Nothing here is going away without warning — this shim exists so that
-- | those imports keep working. New code should import `Hylograph.Attribute`,
-- | and for `ElementType` in particular `Hylograph.HATS` now re-exports it,
-- | so the common case needs no extra import at all.
module Hylograph.Internal.Attribute
  ( module ReExport
  ) where

import Hylograph.Attribute as ReExport
