-- | Imperative behaviours — the public counterpart to the declarative
-- | `Behavior` ADT in `Hylograph.Behavior.Types`.
-- |
-- | Most interaction should be declared, not attached: `on (Zoom …)` and
-- | `on (Drag …)` inside a HATS tree are the ordinary route, and they carry
-- | the behaviour along with the elements it belongs to. But three things
-- | genuinely live outside a tree, and this module is where they belong:
-- |
-- | - **Zoom on a container you already hold**, typically an `<svg>` fetched
-- |   by `getElementById` in a Halogen component, where there is no HATS
-- |   tree to hang a `Zoom` behaviour from.
-- | - **The simulation registry**, which pairs with `simulationDrag` in the
-- |   declarative API. `simulationDrag "lesmis"` names a simulation; some
-- |   effectful code has to put one under that name.
-- | - **Scrolling an element into view**, which is not a behaviour of a
-- |   visualisation at all, just a thing components need to do.
-- |
-- | Zoom takes the same `ZoomConfig` the declarative path takes, so
-- | `defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group"` builds a value usable
-- | either way. That also removes a real hazard: the underlying binding
-- | takes the scale bounds as two adjacent bare `Number`s, which nothing but
-- | care keeps in the right order.
-- |
-- | Everything here wraps `Hylograph.Internal.Behavior.FFI`, which consumers
-- | had been importing directly for want of this module. Import this one.
module Hylograph.Behavior
  ( -- * Zoom
    module ReExportBehaviorTypes
  , identityTransform
  , attachZoom
  , attachZoomWithTransform
  , attachZoomWithCallback
  , readZoomTransform
    -- * Simulation registry
  , registerSimulation
  , unregisterSimulation
    -- * Scrolling
  , ScrollBehavior(..)
  , ScrollBlock(..)
  , scrollToElement
  , scrollToElementWith
  ) where

import Prelude

import Effect (Effect)
import Hylograph.Behavior.Types (ScaleExtent(..), ZoomConfig(..), ZoomTransform)
-- Re-exported so that using the zoom functions needs no second import.
import Hylograph.Behavior.Types (ZoomTransform) as ReExportBehaviorTypes
import Hylograph.Internal.Behavior.FFI as FFI
import Web.DOM.Element (Element)

-- =============================================================================
-- Zoom
-- =============================================================================

-- | The zoom transform with no scaling and no translation.
-- |
-- | `attachZoomWithTransform` and `attachZoomWithCallback` both take a
-- | transform to restore. Pass this one when there is nothing to restore —
-- | on a first render, say — rather than writing the record out.
identityTransform :: ZoomTransform
identityTransform = { k: 1.0, x: 0.0, y: 0.0 }

-- | Attach zoom and pan to an element, transforming whatever the config's
-- | selector matches.
-- |
-- | The element is typically the `<svg>`; the selector names the group
-- | inside it that actually moves.
-- |
-- | ```purescript
-- | _ <- attachZoom svgElem (defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group")
-- | ```
-- |
-- | Returns the element, for chaining.
attachZoom :: Element -> ZoomConfig -> Effect Element
attachZoom element (ZoomConfig { scaleExtent: ScaleExtent lo hi, targetSelector }) =
  FFI.attachZoom_ element lo hi targetSelector

-- | Attach zoom and immediately apply a transform.
-- |
-- | Use this to preserve the user's zoom across a re-render: read the
-- | transform with `readZoomTransform` before tearing down, hand it back
-- | here afterwards.
attachZoomWithTransform :: Element -> ZoomConfig -> ZoomTransform -> Effect Element
attachZoomWithTransform element (ZoomConfig { scaleExtent: ScaleExtent lo hi, targetSelector }) transform =
  FFI.attachZoomWithTransform_ element lo hi targetSelector transform

-- | Attach zoom, apply a transform, and run a callback on every zoom event.
-- |
-- | The callback is for things that must track the zoom but are not inside
-- | the transformed group — annotations positioned in screen space, arrows
-- | whose stroke width should not scale, a readout of the current level.
attachZoomWithCallback
  :: Element
  -> ZoomConfig
  -> ZoomTransform
  -> (ZoomTransform -> Effect Unit)
  -> Effect Element
attachZoomWithCallback element (ZoomConfig { scaleExtent: ScaleExtent lo hi, targetSelector }) transform onZoom =
  FFI.attachZoomWithCallback_ element lo hi targetSelector transform onZoom

-- | The element's current zoom transform, or `identityTransform` if it has
-- | never been zoomed.
readZoomTransform :: Element -> Effect ZoomTransform
readZoomTransform = FFI.getZoomTransform_

-- =============================================================================
-- Simulation registry
-- =============================================================================

-- | Make a simulation reachable by name, so that `simulationDrag` can find
-- | it.
-- |
-- | `simulationDrag "lesmis"` in a HATS tree says *which* simulation a drag
-- | should reheat; this says *what* is under that name. The second argument
-- | is the reheat action, run whenever a drag on a registered node begins.
-- |
-- | ```purescript
-- | registerSimulation "lesmis" (Sim.reheat sim)
-- | ```
-- |
-- | Names are global. Pair every call with `unregisterSimulation` when the
-- | visualisation is torn down, or a finalised simulation stays reachable.
registerSimulation :: String -> Effect Unit -> Effect Unit
registerSimulation = FFI.registerSimulation_

-- | Remove a simulation from the registry. Call this on teardown.
unregisterSimulation :: String -> Effect Unit
unregisterSimulation = FFI.unregisterSimulation_

-- =============================================================================
-- Scrolling
-- =============================================================================

-- | How a scroll should be animated.
data ScrollBehavior
  = Smooth
  | Instant
  -- | `Auto` defers to the element's computed `scroll-behavior`.
  | Auto

derive instance Eq ScrollBehavior
derive instance Ord ScrollBehavior

instance Show ScrollBehavior where
  show = case _ of
    Smooth -> "Smooth"
    Instant -> "Instant"
    Auto -> "Auto"

-- | Where the element should come to rest in the scrollport.
data ScrollBlock
  = Start
  | Center
  | End
  -- | `Nearest` scrolls the least amount that brings it into view.
  | Nearest

derive instance Eq ScrollBlock
derive instance Ord ScrollBlock

instance Show ScrollBlock where
  show = case _ of
    Start -> "Start"
    Center -> "Center"
    End -> "End"
    Nearest -> "Nearest"

-- | Scroll the element with the given id into view: smoothly, centred.
-- |
-- | These are the defaults you want almost always. Reach for
-- | `scrollToElementWith` when they are not.
scrollToElement :: String -> Effect Unit
scrollToElement elementId = scrollToElementWith Smooth Center elementId

-- | Scroll the element with the given id into view, choosing the animation
-- | and the resting position.
scrollToElementWith :: ScrollBehavior -> ScrollBlock -> String -> Effect Unit
scrollToElementWith behavior block elementId =
  FFI.scrollToElementById_ elementId (behaviorToken behavior) (blockToken block)

-- | The DOM's `scrollIntoView` spelling of these options. Kept private:
-- | the tokens are an implementation detail of the binding, not something
-- | callers should be handed.
behaviorToken :: ScrollBehavior -> String
behaviorToken = case _ of
  Smooth -> "smooth"
  Instant -> "instant"
  Auto -> "auto"

blockToken :: ScrollBlock -> String
blockToken = case _ of
  Start -> "start"
  Center -> "center"
  End -> "end"
  Nearest -> "nearest"
