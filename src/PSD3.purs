-- | Hylograph: Type-safe, composable data visualization
-- |
-- | Hylograph is a PureScript library for creating D3.js type visualizations using phantom types
-- | for compile-time safety and a declarative AST called HATS for clean, maintainable code.
-- |
-- | ## Quick Start
-- |
-- | ```purescript
-- | import Hylograph.AST as A
-- | import Hylograph.Expr.Friendly (computed, computedStr, num, text)
-- | import Hylograph.Render (runD3, select, renderTree)
-- |
-- | main :: Effect Unit
-- | main = void $ runD3 do
-- |   container <- select "body"
-- |   let ast =
-- |         A.elem SVG
-- |           [ computed "width" (num 800.0)
-- |           , computed "height" (num 600.0)
-- |           ]
-- |           `A.withChild`
-- |             A.elem Circle
-- |               [ computed "cx" (num 100.0)
-- |               , computed "cy" (num 100.0)
-- |               , computed "r" (num 50.0)
-- |               , computedStr "fill" (text "steelblue")
-- |               ]
-- |   renderTree container ast
-- | ```
-- |
-- | ## AST Example
-- |
-- | Declarative DOM structure with data binding:
-- |
-- | ```purescript
-- | import Hylograph.AST as A
-- | import Hylograph.Expr.Friendly (computed, computedStr, from, num, text)
-- |
-- | barChart :: Array Number -> A.AST Number
-- | barChart data =
-- |   A.elem SVG
-- |     [ computed "width" (num 500.0)
-- |     , computed "height" (num 300.0)
-- |     ]
-- |     `A.withChild`
-- |       A.joinData "bars" "rect" data \d ->
-- |         A.elem Rect
-- |           [ from "x" (\_ i -> toNumber i * 25.0)
-- |           , from "y" (\val _ -> 300.0 - val)
-- |           , computed "width" (num 20.0)
-- |           , from "height" (\val _ -> val)
-- |           , computedStr "fill" (text "steelblue")
-- |           ]
-- | ```
-- |
-- | ## Force Simulation
-- |
-- | For force-directed graphs, use the `Hylograph-simulation` package which provides:
-- | - `Hylograph.ForceEngine` - Force simulation engine
-- | - `Hylograph.ForceEngine.Setup` - Declarative force configuration
-- | - `Hylograph.ForceEngine.Halogen` - Halogen integration
-- |
-- | ## Module Organization
-- |
-- | **Core Modules**:
-- | - `Hylograph.AST` - Declarative visualization AST (element types, data joins)
-- | - `Hylograph.Expr.Friendly` - Finally-tagless attribute DSL (computed, from, num, text)
-- | - `Hylograph.Render` - D3 DOM rendering (runD3, select, renderTree)
-- | - `Hylograph.Internal.Behavior.Types` - Behaviors (drag, zoom)
-- |
-- | **Interpreters** (for debugging):
-- | - `Hylograph.Interpreter.Mermaid` - Mermaid diagram of AST structure
-- | - `Hylograph.Interpreter.English` - English description of AST
-- |
-- | **Shared Modules**:
-- | - `Hylograph.Data.Node` - SimulationNode, D3Link types
-- | - `Hylograph.Data.Tree` - Tree data structures (for layouts)
-- | - `Hylograph.Layout.Hierarchy.*` - Pure PureScript layouts (Tree, Cluster, Pack, etc.)
-- | - `Hylograph.Layout.Sankey` - Pure PureScript Sankey layout
-- | - `Hylograph.Scale` - D3 scale wrappers
-- |
-- | ## Import Patterns
-- |
-- | **Basic visualization**:
-- | ```purescript
-- | import Hylograph.AST as A
-- | import Hylograph.Expr.Friendly (computed, computedStr, from, num, text)
-- | import Hylograph.Render (runD3, select, renderTree)
-- | ```
-- |
-- | **With behaviors**:
-- | ```purescript
-- | import Hylograph.Render (on)
-- | import Hylograph.Internal.Behavior.Types (Behavior(..), defaultDrag, defaultZoom)
-- | ```
-- |
-- | ## Exports
-- |
-- | This module re-exports the core Hylograph modules for convenient imports.
-- |
module Hylograph (module X) where

-- Clean Public API
import Hylograph.AST (AST, ASTNode, ElementType(..), elem, joinData, withChild, withChildren, named, nestedJoin, updateJoin, updateNestedJoin, withBehaviors, beside, siblings, (>:), (+:)) as X
import Hylograph.Internal.Attribute (Attribute(..)) as X
import Hylograph.Expr.Friendly (computed, computedStr, computedWithIndex, computedStrWithIndex, from, fromStr, fromWithIndex, fromStrWithIndex, static, staticStr, num, text, bool, field, index, plus, minus, times, dividedBy, plusN, minusN, timesN, dividedByN, negated, lessThan, lessOrEqual, greaterThan, greaterOrEqual, equals, textEquals, textNotEquals, and_, or_, not_, ifThen, sin, cos, tan, asin, acos, atan, atan2, pi, append) as X
import Hylograph.Render (runD3, D3M, D3Selection) as X

-- Selection & Transition Capabilities
import Hylograph.Internal.Capabilities.Selection (class SelectionM, select, selectElement, appendChildInheriting, appendData, renderTree, renderData, setAttrs, on, clear) as X
import Hylograph.Internal.Capabilities.Transition (class TransitionM, withTransition, withTransitionExit, withTransitionStaggered, staggerByIndex) as X

-- Legacy type names (keep for backwards compatibility, prefer D3M/D3Selection)
import Hylograph.Interpreter.D3 (D3v2M, D3v2Selection_, runD3v2M, reselectD3v2) as X

-- Internal Types (for advanced use)
import Hylograph.Internal.Selection.Types (SEmpty, SBoundOwns, SBoundInherits, SPending, SExiting, Selection(..)) as X
import Hylograph.Internal.Behavior.Types (Behavior(..), defaultDrag, ScaleExtent(..), HighlightClass(..), TooltipTrigger(..), TooltipConfig, CoordinatedHighlightConfig, onCoordinatedHighlight) as X

-- Native Pointer Events (D3-free drag/interactions)
import Hylograph.Interaction.Pointer (attachPointerDrag, attachSimulationDrag, attachSimulationDragNested, pointerPosition, PointerDragConfig) as X

-- Native Zoom (D3-free zoom/pan)
import Hylograph.Interaction.Zoom (ZoomTransform, ZoomConfig, ZoomHandle, attachNativeZoom, attachZoomNative, attachZoomWithTransform, attachZoomWithCallback, identity) as X

-- Unified Coordinated Interactions (hover, brush, selection)
-- Note: InteractionState constructors are qualified to avoid conflict with HighlightClass
import Hylograph.Interaction.Coordinated (InteractionTrigger(..), InteractionState, BoundingBox, CoordinatedConfig, CoordinatedHandle, registerCoordinated, emitTrigger, clearInteractions, simpleHover, withBrush, withSelection, mkHoverTrigger, mkBrushTrigger, mkClearTrigger, pointInBox, boxesOverlap, isHighlighted, isSelected) as X

-- Shared data types
import Hylograph.Data.Node (SimulationNode) as X
import Hylograph.Internal.Types (D3Simulation_, Datum_, Index_, Selector) as X
import Data.Graph.Layout (TreeLayout(..)) as X
-- TODO: DAGTree module not yet implemented
-- import Hylograph.Data.DAGTree (DAGTree, DAGLink, PositionedDAGTree, PositionedNode, dagTree, addLink, addLinks, layoutDAGTree, getNodePosition, getExtraLinkPositions) as X

-- Scales
import Hylograph.Scale (Scale, ContinuousScale, BandScale, OrdinalScale, Continuous, Ordinal, Band, linear, log, pow, sqrt, symlog, ordinal, band, point, domain, range, clamp, nice, padding, applyScale, ticks, invert, schemeCategory10, schemeCategory10At, schemePaired, schemePairedAt, interpolateViridis, interpolatePlasma, interpolateInferno, interpolateRdYlGn, interpolateTurbo) as X

-- Shape Generators (D3-free pie/donut charts)
import Hylograph.Shape.Arc (ArcConfig, ArcDatum, arcPath, arcPathWithCenter, degreesToRadians, tau) as X
import Hylograph.Shape.Pie (PieSlice, PieConfig, pie, pieWithConfig, defaultPieConfig) as X
