-- | Type-level guard for `Hylograph.Behavior`, the public imperative
-- | behaviour API.
-- |
-- | That module exists because consumers were importing
-- | `Hylograph.Internal.Behavior.FFI` directly — the internal headers had
-- | been redirecting readers to a `Hylograph.Behavior` that was never
-- | written. The wrappers are thin, and thin wrappers are exactly where an
-- | argument order quietly goes wrong: the underlying zoom binding takes the
-- | scale bounds as two adjacent bare `Number`s.
-- |
-- | So every export is named here at its full type. Nothing runs; compiling
-- | *is* the assertion, and it fails if a wrapper's shape drifts from what
-- | callers were promised. It cannot catch a swapped `lo`/`hi` — both are
-- | `Number` — which is why `attachZoom` takes a `ZoomConfig` instead, and
-- | why the check below insists on that signature.
module Test.PublicBehavior where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertTrue')
import Hylograph.Behavior
  ( ScrollBehavior(..)
  , ScrollBlock(..)
  , ZoomTransform
  , attachZoom
  , attachZoomWithCallback
  , attachZoomWithTransform
  , identityTransform
  , readZoomTransform
  , registerSimulation
  , scrollToElement
  , scrollToElementWith
  , unregisterSimulation
  )
import Hylograph.Behavior.Types (ZoomConfig)
import Hylograph.Behavior.Types as BT
import Web.DOM.Element (Element)

-- =============================================================================
-- Every export, at its full type
--
-- The point of the `ZoomConfig` argument is that it cannot be confused with
-- anything: were these `Number -> Number -> String`, a caller could transpose
-- the bounds and the compiler would agree.
-- =============================================================================

attachZoomHasConfigShape :: Element -> ZoomConfig -> Effect Element
attachZoomHasConfigShape = attachZoom

attachZoomWithTransformHasConfigShape
  :: Element -> ZoomConfig -> ZoomTransform -> Effect Element
attachZoomWithTransformHasConfigShape = attachZoomWithTransform

attachZoomWithCallbackHasConfigShape
  :: Element
  -> ZoomConfig
  -> ZoomTransform
  -> (ZoomTransform -> Effect Unit)
  -> Effect Element
attachZoomWithCallbackHasConfigShape = attachZoomWithCallback

readZoomTransformIsAnEffect :: Element -> Effect ZoomTransform
readZoomTransformIsAnEffect = readZoomTransform

-- | The registry pairs with `simulationDrag`, which names a simulation by
-- | `String`. Both sides must agree on that, so the declarative constructor
-- | is mentioned here too — if one grew a newtype without the other, this
-- | stops compiling.
registryAgreesWithSimulationDrag ::
  { register :: String -> Effect Unit -> Effect Unit
  , unregister :: String -> Effect Unit
  , declare :: String -> BT.DragConfig
  }
registryAgreesWithSimulationDrag =
  { register: registerSimulation
  , unregister: unregisterSimulation
  , declare: BT.simulationDrag
  }

scrollToElementIsById :: String -> Effect Unit
scrollToElementIsById = scrollToElement

scrollToElementWithTakesADTs :: ScrollBehavior -> ScrollBlock -> String -> Effect Unit
scrollToElementWithTakesADTs = scrollToElementWith

-- =============================================================================
-- The re-export, and the identity
-- =============================================================================

-- | `ZoomTransform` is defined in `Hylograph.Behavior.Types` and re-exported
-- | by `Hylograph.Behavior`, so that using the zoom functions needs one
-- | import rather than two. Reaching it both ways must give the same
-- | synonym.
zoomTransformReExportAgrees :: ZoomTransform -> BT.ZoomTransform
zoomTransformReExportAgrees = identity

-- | The identity transform is the one D3 reports for an unzoomed element.
-- | Written out rather than compared, because getting it wrong would make
-- | "restore nothing" silently translate the view.
identityTransformIsIdentity :: Boolean
identityTransformIsIdentity =
  identityTransform.k == 1.0
    && identityTransform.x == 0.0
    && identityTransform.y == 0.0

-- | Every constructor of both scroll ADTs is named, so that adding a case
-- | to the DOM's vocabulary without handling it here is visible.
allScrollOptions :: { behaviors :: Array ScrollBehavior, blocks :: Array ScrollBlock }
allScrollOptions =
  { behaviors: [ Smooth, Instant, Auto ]
  , blocks: [ Start, Center, End, Nearest ]
  }

-- | The one claim above that is about values rather than types, so the one
-- | that has to actually run.
runTests :: Effect Unit
runTests = do
  assertTrue' "identityTransform is k=1, x=0, y=0" identityTransformIsIdentity
  log "  ✓ public behaviour surface"
