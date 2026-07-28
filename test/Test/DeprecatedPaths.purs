-- | Compatibility guard for the deprecated `Hylograph.Internal.*` paths.
-- |
-- | Five vocabulary modules moved out of `Internal` into public homes. The
-- | old paths remain as re-export shims, because roughly 470 import
-- | statements across the ecosystem point at them and none of those
-- | consumers should have to change anything.
-- |
-- | This module imports every one of those paths *by its old name* and
-- | names the types that consumers actually import through them (counts
-- | from an ecosystem-wide survey). If a shim is ever narrowed or removed
-- | by accident, this stops compiling — which is the point. It asserts
-- | nothing at runtime; compiling *is* the assertion.
-- |
-- | When these paths are eventually retired for real, delete this module
-- | in the same commit, and only after the ecosystem has migrated.
module Test.DeprecatedPaths where

import Prelude

import Hylograph.Internal.Attribute (Attribute, AttributeName, AttributeValue, AttrSource, AnimatedValue, EasingType, defaultAnimationConfig)
import Hylograph.Internal.Behavior.Types (Behavior, DragConfig, ZoomConfig, ScaleExtent, HighlightClass, TooltipTrigger, defaultDrag, defaultZoom)
import Hylograph.Internal.Element.Types (ElementType(..), RenderContext, elementContext)
import Hylograph.Internal.Transition.Types (TransitionConfig, Easing, transitionWith, defaultTransition)
import Hylograph.Internal.Types (Datum_, Index_, D3Selection_, D3Simulation_, PointXY, Selector)

-- | The same names reached through their new public homes. These must be
-- | the *same* declarations, not lookalikes — the equality checks below
-- | would not typecheck otherwise.
import Hylograph.Attribute as A
import Hylograph.Behavior.Types as B
import Hylograph.Element.Types as E
import Hylograph.Foreign.Types as F
import Hylograph.Transition.Types as T

-- | `Hylograph.HATS` now re-exports the element vocabulary, so the common
-- | case — writing an `elem` call — needs no second import at all. This is
-- | the ergonomic half of the fix and deserves its own guard.
import Hylograph.HATS as HATS

-- =============================================================================
-- Old path and new path denote the same declarations
--
-- Each signature below mentions a type via the deprecated path and via the
-- public one. If the shim ever stopped re-exporting the original — say by
-- redefining a lookalike — these would fail to unify.
-- =============================================================================

elementTypesAgree :: ElementType -> E.ElementType
elementTypesAgree = identity

renderContextsAgree :: RenderContext -> E.RenderContext
renderContextsAgree = identity

attributesAgree :: Attribute Int -> A.Attribute Int
attributesAgree = identity

attributeNamesAgree :: AttributeName -> A.AttributeName
attributeNamesAgree = identity

attributeValuesAgree :: AttributeValue -> A.AttributeValue
attributeValuesAgree = identity

attrSourcesAgree :: AttrSource -> A.AttrSource
attrSourcesAgree = identity

-- `AnimatedValue` is parameterised over the datum type.
animatedValuesAgree :: AnimatedValue Datum_ -> A.AnimatedValue F.Datum_
animatedValuesAgree = identity

easingTypesAgree :: EasingType -> A.EasingType
easingTypesAgree = identity

-- `Behavior` is parameterised over the datum type.
behavioursAgree :: Behavior Datum_ -> B.Behavior F.Datum_
behavioursAgree = identity

dragConfigsAgree :: DragConfig -> B.DragConfig
dragConfigsAgree = identity

zoomConfigsAgree :: ZoomConfig -> B.ZoomConfig
zoomConfigsAgree = identity

scaleExtentsAgree :: ScaleExtent -> B.ScaleExtent
scaleExtentsAgree = identity

highlightClassesAgree :: HighlightClass -> B.HighlightClass
highlightClassesAgree = identity

tooltipTriggersAgree :: TooltipTrigger -> B.TooltipTrigger
tooltipTriggersAgree = identity

transitionConfigsAgree :: TransitionConfig -> T.TransitionConfig
transitionConfigsAgree = identity

easingsAgree :: Easing -> T.Easing
easingsAgree = identity

datumsAgree :: Datum_ -> F.Datum_
datumsAgree = identity

indicesAgree :: Index_ -> F.Index_
indicesAgree = identity

-- `D3Selection_` is parameterised over the bound datum type, so it must be
-- applied here.
selectionsAgree :: D3Selection_ Datum_ -> F.D3Selection_ F.Datum_
selectionsAgree = identity

simulationsAgree :: D3Simulation_ -> F.D3Simulation_
simulationsAgree = identity

pointsAgree :: PointXY -> F.PointXY
pointsAgree = identity

-- `Selector` is phantom-typed over the selection kind, so it must be
-- applied. Reaching it through the old path and the new one must still
-- give the same synonym.
selectorsAgree :: Selector (D3Selection_ Datum_) -> F.Selector (F.D3Selection_ F.Datum_)
selectorsAgree = identity

-- | And the same type reached through `Hylograph.HATS`, which is the path
-- | new code should take.
hatsReExportsElementType :: ElementType -> HATS.ElementType
hatsReExportsElementType = identity

hatsReExportsRenderContext :: RenderContext -> HATS.RenderContext
hatsReExportsRenderContext = identity

-- =============================================================================
-- Values, not just types, still resolve through the old paths
-- =============================================================================

-- `defaultZoom` takes arguments; naming it at its full type is a stronger
-- check than applying it would be, since the argument types come through
-- the shim too.
deprecatedValuesResolve ::
  { drag :: B.DragConfig
  , zoom :: B.ScaleExtent -> String -> B.ZoomConfig
  , transition :: T.TransitionConfig
  , animation :: A.AnimationConfig
  , context :: E.RenderContext
  }
deprecatedValuesResolve =
  { drag: defaultDrag
  , zoom: defaultZoom
  , transition: defaultTransition
  , animation: defaultAnimationConfig
  , context: elementContext Circle
  }

-- | `transitionWith` is reached by name through the deprecated path, and
-- | its record argument mentions `Easing` — which arrives through the shim
-- | as well, so this exercises the whole chain.
deprecatedTransitionWith :: TransitionConfig -> TransitionConfig
deprecatedTransitionWith = transitionWith
