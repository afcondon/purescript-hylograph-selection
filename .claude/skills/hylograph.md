# Hylograph Visualization Skill v2

You are an expert in building interactive data visualizations using the **Hylograph** PureScript library ecosystem. When a user describes data they have and what they want to see, you can design and implement a complete Halogen/Hylograph application.

## What Hylograph Is

Hylograph is a declarative, type-safe visualization ecosystem for PureScript. It takes D3's proven algorithms (scales, layouts, force simulation) and wraps them in functional programming abstractions:

- **HATS (Hylomorphic Abstract Trees)**: Describe visualizations as composable tree values. A `Tree` is an algebraic data type (`Elem | MkFold | Empty`) with a `Semigroup` instance, so trees compose freely via `<>` even when they bind different data types internally.
- **Finally-tagless expressions**: Attributes are defined via type classes (`NumExpr`, `AttrExpr`, `DatumExpr`), enabling the same visualization spec to be interpreted as D3/SVG rendering, English descriptions, Mermaid diagrams, or generated PureScript code.
- **Phantom-typed selections**: The `Selection state parent datum` type uses phantom types to track binding state (`SEmpty`, `SBoundOwns`, `SPending`, `SExiting`), making invalid D3 operations a compile-time error.
- **Pure layout algorithms**: Tree, treemap, circle packing, Sankey, chord, partition, and edge bundling are all implemented in pure PureScript (no FFI).
- **Dual-engine simulation**: Force-directed layouts work through a kernel abstraction -- same API for D3.js or Rust/WASM physics (3-4x faster).

**Philosophy**: Declarative over imperative. D3's algorithms, PureScript's type system and state management. Pass structure in, get visualization out.

**Companion skill**: See *Grammar of Graphics for Hylograph* (`.claude/skills/hylograph-grammar-of-graphics.md`) for a design-phase thinking tool that decomposes visualization problems into independent layers (data, aesthetics, geometry, statistics, scales, coordinates, facets, guides, theme) before translating each to Hylograph constructs.

---

## Package Map

| Package | What It Does | When You Need It |
|---------|-------------|-----------------|
| `hylograph-selection` | Core: HATS trees, attributes, scales, interactions, D3 interpreter | **Always** -- this is the foundation |
| `hylograph-layout` | Pure layout algorithms (tree, treemap, pack, partition, sankey, chord, adjacency, edge bundling) | Hierarchical data, flow diagrams, matrix views |
| `hylograph-graph` | Graph data structures, pathfinding (A*, Dijkstra, BFS/DFS), DAG operations, topological sort | Graph algorithms, network analysis |
| `hylograph-simulation` | Force-directed simulation (unified D3/WASM API) | Force-directed layouts, physics-based positioning |
| `hylograph-simulation-core` | Scene orchestration, engine adapter interface | Multi-scene transitions, scene lifecycle |
| `hylograph-d3-kernel` | D3.js force simulation FFI | Default physics engine |
| `hylograph-wasm-kernel` | Rust/WASM force simulation FFI | Performance-critical simulations (1000+ nodes) |
| `hylograph-simulation-halogen` | Halogen integration for simulation events | Simulation inside Halogen components |
| `hylograph-transitions` | Easing functions, interpolation (number, color, point), tick-based animation | Animated transitions between states |
| `hylograph-optics` | Lenses, prisms, traversals for trees and graphs | Functional updates to nested structures |
| `hylograph-music` | Audio/sonification interpreter | Accessibility, data sonification |
| `hylograph-canvas` | Canvas 2D rendering support | High-performance rendering (particles, real-time) |

### Dependency Relationships

```
hylograph-simulation (high-level API)
  +-- hylograph-d3-kernel  OR  hylograph-wasm-kernel
  +-- hylograph-simulation-core
  +-- hylograph-selection
  +-- hylograph-transitions

hylograph-selection (core rendering)
  +-- hylograph-layout
  +-- hylograph-graph
  +-- hylograph-transitions

hylograph-layout (pure algorithms)
  +-- hylograph-graph
```

---

## Universal App Structure

Every Hylograph application follows this architecture:

```
my-app/
+-- spago.yaml              -- Dependencies
+-- src/
|   +-- Main.purs           -- Entry point (~16 lines)
|   +-- Types.purs          -- Pure data types for your domain
|   +-- Component/
|   |   +-- App.purs        -- Main Halogen component (owns state)
|   +-- Data/
|   |   +-- Loader.purs     -- Data fetching (Aff + JSON decode)
|   +-- Viz/
|       +-- MyViz.purs      -- Pure HATS tree builders
+-- public/
    +-- index.html          -- Container div + bundle.js
```

### Main.purs (always this shape)

```purescript
module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Component.App as App

main :: Effect Unit
main = launchAff_ do
  body <- HA.awaitBody
  _ <- runUI App.component unit body
  pure unit
```

### The Halogen Component Pattern

```purescript
module Component.App where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State =
  { phase :: AppPhase       -- Loading | Ready | Error String
  , myData :: Maybe MyData  -- Domain data once loaded
  }

data AppPhase = Loading | Ready | Error String
derive instance Eq AppPhase

data Action = Initialize

component :: forall query input output m. MonadAff m =>
  H.Component query input output m
component = H.mkComponent
  { initialState: \_ -> { phase: Loading, myData: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.phase of
  Loading -> HH.div_ [ HH.text "Loading..." ]
  Error msg -> HH.div_ [ HH.text msg ]
  Ready ->
    HH.div [ HP.class_ (H.ClassName "app") ]
      [ HH.div [ HP.id "viz-container" ] []
        -- HATS renders into this empty div
      ]

handleAction :: forall m. MonadAff m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction Initialize = do
  result <- liftAff loadMyData
  case result of
    Left err -> H.modify_ _ { phase = Error err }
    Right myData -> do
      H.modify_ _ { phase = Ready, myData = Just myData }
      liftEffect $ renderVisualization myData
```

### The Visualization Module Pattern

```purescript
module Viz.MyViz where

import Prelude
import Effect (Effect)
import Hylograph.HATS (Tree, elem, forEach, thunkedNum, thunkedStr)
import Hylograph.HATS.Friendly (viewBox, class_, r)
import Hylograph.HATS.InterpreterTick (rerender, RerenderResult)
import Hylograph.Internal.Element.Types (ElementType(..))

-- | Pure function: data -> HATS tree (no effects)
buildTree :: MyData -> Tree
buildTree myData =
  elem SVG
    [ viewBox 0.0 0.0 800.0 600.0 ]
    [ elem Group [ class_ "data-layer" ]
        [ forEach "items" Circle myData.items (\item -> item.id) \item ->
            elem Circle
              [ thunkedNum "cx" item.x
              , thunkedNum "cy" item.y
              , r 5.0
              , thunkedStr "fill" (colorFor item)
              ]
              []
        ]
    ]

-- | Effectful: render tree into DOM container
-- | rerender returns { selections :: SelectionMap, transitions :: Maybe HATSTransitions }
renderVisualization :: MyData -> Effect Unit
renderVisualization myData = do
  _ <- rerender "#viz-container" (buildTree myData)
  pure unit
```

**Key pattern**: Inside a `forEach` template, use `thunkedStr`/`thunkedNum` for datum-derived values (they capture the value in a closure). For constant values, prefer **Friendly helpers** (`r`, `fill`, `class_`, `viewBox`, etc. from `Hylograph.HATS.Friendly`) -- they're wrappers around `staticStr`/`staticNum` but more readable. Fall back to `staticStr`/`staticNum` for attribute names Friendly doesn't cover. The tree is always re-rendered in full via `rerender` -- there is no incremental update; HATS handles enter/update/exit (GUP) internally.

### The Data Loading Pattern

```purescript
module Data.Loader where

import Prelude
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Affjax as AX
import Affjax.ResponseFormat as RF
import Data.Argonaut.Decode (decodeJson)

loadMyData :: Aff (Either String MyData)
loadMyData = do
  result <- AX.get RF.json "/api/data"
  case result of
    Left err -> pure $ Left $ AX.printError err
    Right response ->
      case decodeJson response.body of
        Left decodeErr -> pure $ Left $ show decodeErr
        Right myData -> pure $ Right myData
```

---

## Decision Matrix: Data -> Visualization

Use this to decide which packages and patterns to reach for.

### By Data Shape

| Your Data | Best Visualization | Key Packages |
|-----------|-------------------|-------------|
| **Flat array of records** (e.g. CSV rows) | Scatterplot, bar chart, beeswarm | `hylograph-selection` (scales + HATS) |
| **Hierarchical / tree-shaped** | Treemap, tree layout, sunburst, circle packing | `hylograph-layout` + `hylograph-selection` |
| **Network / graph** (nodes + edges) | Force-directed graph, adjacency matrix | `hylograph-simulation` + `hylograph-selection` |
| **Flow data** (source -> target + value) | Sankey diagram, chord diagram | `hylograph-layout` (Sankey/Chord modules) |
| **DAG** (directed acyclic graph) | Topological layout, layered graph | `hylograph-graph` (topological sort, layers) |
| **Time series** | Line chart, area chart | `hylograph-selection` (time scales) |
| **Multi-dimensional** | SPLOM, parallel coordinates | `hylograph-selection` (coordinated views) |
| **Geographic** | Map with projected coordinates | `hylograph-selection` (linear scales for projection) |

### By Interaction Need

| Interaction | Pattern | Where It Lives |
|------------|---------|---------------|
| **Hover highlighting** | `withBehaviors [ onMouseEnter ..., onMouseLeave ... ]` | `Hylograph.HATS` |
| **Coordinated multi-view** (simple) | `onCoordinatedHighlight` with classify function | `Hylograph.HATS` |
| **Coordinated multi-view** (full) | `onCoordinatedInteraction` with respond function | `Hylograph.HATS` |
| **Brush selection** | `onBrush` on container + `onCoordinatedInteraction` on elements | `Hylograph.HATS` |
| **Zoom + pan** | `onZoom` behavior | `Hylograph.HATS` |
| **Click navigation** | `onClick` / `onClickWithModifier` | `Hylograph.HATS` |
| **Drag** | `onDrag` behavior | `Hylograph.HATS` |
| **Animation / transition** | `forEachWithGUP` with `TransitionConfig` | `Hylograph.HATS` |
| **Real-time physics** | Force simulation tick -> rerender | `Hylograph.Simulation` |

### By Visual Encoding

| You Want To Show | Visual Encoding | HATS Element |
|-----------------|----------------|-------------|
| **Position** on 2D plane | x, y coordinates | `cx`, `cy` or `x`, `y` attributes on `Circle`/`Rect` |
| **Size** / magnitude | Radius, width, height | `r` on `Circle`, `width`/`height` on `Rect` |
| **Category** / type | Color fill | `fill` attribute with categorical color |
| **Quantity** on scale | Position along axis | Scale (`linear`, `log`, `band`) -> position |
| **Hierarchy** depth | Nesting, indentation | Treemap rectangles, packed circles |
| **Flow** / volume | Link width | Sankey link `width` |
| **Connection** | Lines between elements | `Line` or `Path` elements with source/target coords |
| **Labels** | Text elements | `Text` element with `textContent` |

---

## Cookbook: Concrete Patterns

### Pattern 1: Scatterplot with Scales

For flat data with two numeric dimensions and a category.

```purescript
module Viz.Scatter where

import Prelude
import Data.Maybe (fromMaybe)
import Data.Foldable (minimum, maximum)
import Effect (Effect)
import Hylograph.HATS (Tree, elem, forEach, thunkedNum, thunkedStr)
import Hylograph.HATS.Friendly (viewBox, transform, r, fillOpacity, strokeWidth)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Scale as Scale
import Hylograph.Scale (applyScale, nice)

type DataPoint =
  { label :: String
  , x :: Number
  , y :: Number
  , category :: String
  }

type VizConfig =
  { width :: Number
  , height :: Number
  , margin :: { top :: Number, right :: Number, bottom :: Number, left :: Number }
  }

defaultConfig :: VizConfig
defaultConfig =
  { width: 800.0
  , height: 600.0
  , margin: { top: 40.0, right: 40.0, bottom: 60.0, left: 60.0 }
  }

renderScatter :: String -> VizConfig -> Array DataPoint -> Effect Unit
renderScatter selector config points = do
  let innerWidth = config.width - config.margin.left - config.margin.right
      innerHeight = config.height - config.margin.top - config.margin.bottom

      xValues = points <#> _.x
      yValues = points <#> _.y

      xScale = Scale.linear
        # Scale.domain [ fromMaybe 0.0 (minimum xValues), fromMaybe 1.0 (maximum xValues) ]
        # Scale.range [ 0.0, innerWidth ]
        # nice

      yScale = Scale.linear
        # Scale.domain [ fromMaybe 0.0 (minimum yValues), fromMaybe 1.0 (maximum yValues) ]
        # Scale.range [ innerHeight, 0.0 ]  -- SVG Y is inverted
        # nice

      tree = elem SVG
        [ viewBox 0.0 0.0 config.width config.height ]
        [ elem Group
            [ transform ("translate(" <> show config.margin.left <> "," <> show config.margin.top <> ")") ]
            [ forEach "points" Circle points (\pt -> pt.label) \pt ->
                elem Circle
                  [ thunkedNum "cx" (applyScale xScale pt.x)
                  , thunkedNum "cy" (applyScale yScale pt.y)
                  , r 5.0
                  , thunkedStr "fill" (categoryColor pt.category)
                  , fillOpacity "0.7"
                  , thunkedStr "stroke" (categoryColor pt.category)
                  , strokeWidth 1.0
                  ]
                  []
            ]
        ]

  _ <- rerender selector tree
  pure unit

categoryColor :: String -> String
categoryColor = case _ of
  "A" -> "#4a9eff"
  "B" -> "#ff6b4a"
  "C" -> "#4aff6b"
  _   -> "#888888"
```

### Pattern 2: Force-Directed Graph

For network data with nodes and edges.

```purescript
module Viz.ForceGraph where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Hylograph.HATS (Tree, elem, forEach, staticStr, thunkedNum, thunkedStr, withBehaviors, onMouseEnter, onMouseLeave)
import Hylograph.HATS.Friendly (r, stroke, strokeWidth, strokeOpacity)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Simulation (runSimulation, Engine(..), setup, manyBody, center, collide, link, withStrength, withRadius, withDistance, static, dynamic)
import Hylograph.Simulation.Emitter (SimulationEvent(..), subscribe)
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.Scale (schemeCategory10At)

-- SimulationNode uses extensible row types:
-- type SimulationNode r = { id :: Int, x :: Number, y :: Number
--                         , vx :: Number, vy :: Number
--                         , fx :: Nullable Number, fy :: Nullable Number | r }
type MyNode = SimulationNode (label :: String, group :: Int)

type ForceConfig =
  { width :: Number
  , height :: Number
  , chargeStrength :: Number
  , linkDistance :: Number
  , collideRadius :: Number
  }

defaultForceConfig :: ForceConfig
defaultForceConfig =
  { width: 800.0
  , height: 600.0
  , chargeStrength: -100.0
  , linkDistance: 60.0
  , collideRadius: 10.0
  }

initForceGraph :: String -> ForceConfig -> Array MyNode -> Array { source :: Int, target :: Int } -> Effect Unit
initForceGraph selector config nodes links = do
  -- Setup force configuration
  let mySetup = setup "physics"
        [ manyBody "charge" # withStrength (static config.chargeStrength)
        , center "center"
        , collide "collision" # withRadius (static config.collideRadius)
        , link "links" # withDistance (static config.linkDistance)
        ]

  -- Run simulation
  { handle, events } <- runSimulation
    { engine: D3
    , setup: mySetup
    , nodes: nodes
    , links: links
    , container: selector
    , alphaMin: 0.001
    }

  -- Subscribe to tick events -> re-render ONE combined tree per tick
  -- IMPORTANT: build a single tree containing all visual elements, don't call
  -- rerender multiple times (each call replaces the previous content).
  _ <- subscribe events \event -> case event of
    Tick { alpha, nodeCount } -> do
      currentNodes <- handle.getNodes
      _ <- rerender selector (graphTree currentNodes links)
      pure unit
    Completed -> pure unit
    _ -> pure unit

  pure unit

-- Build ONE combined HATS tree for all visual elements
-- Links go first (drawn behind nodes in SVG paint order)
graphTree :: Array MyNode -> Array { source :: Int, target :: Int } -> Tree
graphTree nodes links =
  elem SVG [ viewBox 0.0 0.0 800.0 600.0 ]
    [ forEach "links" Line links (\l -> show l.source <> "-" <> show l.target) \lnk ->
        elem Line
          [ thunkedNum "x1" (nodeX lnk.source nodes)
          , thunkedNum "y1" (nodeY lnk.source nodes)
          , thunkedNum "x2" (nodeX lnk.target nodes)
          , thunkedNum "y2" (nodeY lnk.target nodes)
          , stroke "#999"
          , strokeOpacity "0.6"
          , strokeWidth 1.5
          ]
          []
    , forEach "nodes" Circle nodes (\n -> show n.id) \node ->
        elem Circle
          [ thunkedNum "cx" node.x
          , thunkedNum "cy" node.y
          , r 8.0
          , thunkedStr "fill" (schemeCategory10At node.group)
          , staticStr "cursor" "pointer"
          ]
          []
    ]
```

### Pattern 3: Treemap from Hierarchical Data

For hierarchical data with a value at each leaf.

```purescript
module Viz.MyTreemap where

import Prelude
import Effect (Effect)
import Data.Array as Array
import DataViz.Layout.Hierarchy.Treemap (treemap, squarify, defaultTreemapConfig, TreemapNode(..))
import DataViz.Layout.Hierarchy.Types (ValuedNode(..))
import Hylograph.HATS (Tree, elem, forEach, thunkedStr, thunkedNum)
import Hylograph.HATS.Friendly (width, height, stroke, strokeWidth, fontSize, fill)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

-- treemap :: forall a. TreemapConfig a -> ValuedNode a -> TreemapNode a
-- Constructor: TNode { data_, depth, height, value, children, x0, y0, x1, y1 }
-- Input: VNode { data_, depth, height, parent, children, value }

-- Flatten leaves manually (no library export for this)
flattenLeaves :: forall a. TreemapNode a -> Array (TreemapNode a)
flattenLeaves (TNode n) =
  if Array.null n.children then [ TNode n ]
  else Array.concatMap flattenLeaves n.children

renderTreemap :: String -> ValuedNode String -> Effect Unit
renderTreemap selector hierarchy = do
  let config = defaultTreemapConfig
        { size = { width: 800.0, height: 600.0 }
        , paddingInner = 2.0
        , paddingOuter = 4.0
        , round = true
        }

  -- Apply treemap layout (pure computation)
  let laid = treemap config hierarchy

  -- Flatten to leaf cells, extract positioned data for rendering
  let cells = flattenLeaves laid <#> \(TNode n) ->
        { name: n.data_, x0: n.x0, y0: n.y0, x1: n.x1, y1: n.y1, depth: n.depth }

  _ <- rerender selector (treemapTree cells)
  pure unit

treemapTree :: Array { name :: String, x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number, depth :: Int } -> Tree
treemapTree cells =
  elem SVG
    [ width 800.0, height 600.0 ]
    [ forEach "cells" Group cells (\c -> c.name) \cell ->
        elem Group []
          [ elem Rect
              [ thunkedNum "x" cell.x0
              , thunkedNum "y" cell.y0
              , thunkedNum "width" (cell.x1 - cell.x0)
              , thunkedNum "height" (cell.y1 - cell.y0)
              , thunkedStr "fill" (depthColor cell.depth)
              , stroke "white"
              , strokeWidth 1.0
              ]
              []
          , elem Text
              [ thunkedNum "x" (cell.x0 + 4.0)
              , thunkedNum "y" (cell.y0 + 16.0)
              , fontSize "12"
              , fill "white"
              , thunkedStr "textContent" cell.name
              ]
              []
          ]
    ]

depthColor :: Int -> String
depthColor = case _ of
  0 -> "#1a1a2e"
  1 -> "#16213e"
  2 -> "#0f3460"
  _ -> "#533483"
```

### Pattern 4: Tree Layout

For tree-structured data (org charts, file trees, taxonomy).

```purescript
module Viz.TreeLayout where

import Prelude
import Effect (Effect)
import DataViz.Layout.Hierarchy.Tree (tree, defaultTreeConfig, withLayerSeparation)
import Hylograph.HATS (Tree, elem, forEach, thunkedNum, thunkedStr)
import Hylograph.HATS.Friendly (width, height, class_, d, fill, stroke, strokeWidth, r, textAnchor, fontSize)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

-- tree :: forall r. TreeConfig {...} -> Tree {...} -> Tree {...}
-- Mutates x, y on input tree nodes. Configure via defaultTreeConfig.

renderTree :: String -> LayoutTree -> Effect Unit
renderTree selector rootNode = do
  let config = defaultTreeConfig
        { size = { width: 800.0, height: 600.0 }
        , minSeparation = 2.0
        }
        # withLayerSeparation 80.0

  -- Layout produces tree with positioned x, y coordinates
  let laid = tree config rootNode

  _ <- rerender selector (treeVizTree laid)
  pure unit

treeVizTree :: { nodes :: Array PositionedNode, links :: Array TreeLink } -> Tree
treeVizTree laid =
  elem SVG
    [ width 800.0, height 600.0 ]
    [ -- Links first (drawn behind nodes)
      elem Group [ class_ "links" ]
        (laid.links <#> \lnk ->
          elem Path
            [ d (curvedLink lnk)
            , fill "none"
            , stroke "#ccc"
            , strokeWidth 1.5
            ]
            []
        )
    , -- Then nodes
      forEach "nodes" Group laid.nodes (\n -> n.label) \node ->
        elem Group
          [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")") ]
          [ elem Circle
              [ r 6.0
              , thunkedStr "fill" (if node.hasChildren then "#555" else "#999")
              , stroke "white"
              , strokeWidth 2.0
              ]
              []
          , elem Text
              [ thunkedNum "dy" (if node.hasChildren then (-12.0) else 4.0)
              , textAnchor "middle"
              , fontSize "11"
              , thunkedStr "textContent" node.label
              ]
              []
          ]
    ]

-- Curved SVG path for tree links
curvedLink :: TreeLink -> String
curvedLink lnk =
  "M" <> show lnk.sourceX <> "," <> show lnk.sourceY
  <> "C" <> show lnk.sourceX <> "," <> show midY
  <> " " <> show lnk.targetX <> "," <> show midY
  <> " " <> show lnk.targetX <> "," <> show lnk.targetY
  where midY = (lnk.sourceY + lnk.targetY) / 2.0
```

### Pattern 5: Sankey Diagram

For flow data (source -> target with value/weight).

```purescript
module Viz.MySankey where

import Prelude
import Effect (Effect)
import DataViz.Layout.Sankey.Compute (computeLayout, computeLayoutWithConfig)
import DataViz.Layout.Sankey.Types (LinkCSVRow, SankeyLayoutResult, SankeyNode, SankeyLink, defaultSankeyConfig, Alignment(..))
import Hylograph.HATS (Tree, elem, forEach, thunkedNum, thunkedStr)
import Hylograph.HATS.Friendly (width, height, class_, d, fill, textAnchor, fontSize, strokeOpacity)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

-- Sankey input: Array LinkCSVRow where LinkCSVRow = { s :: String, t :: String, v :: Number }

renderSankey :: String -> Array LinkCSVRow -> Effect Unit
renderSankey selector flows = do
  -- computeLayout :: Array LinkCSVRow -> Number -> Number -> SankeyLayoutResult
  let result = computeLayout flows 800.0 400.0
  -- result :: { nodes :: Array SankeyNode, links :: Array SankeyLink }
  -- SankeyNode has: name, x0, y0, x1, y1, value, depth, layer, color
  -- SankeyLink has: sourceIndex, targetIndex, value, width, y0, y1, color

  _ <- rerender selector (sankeyTree result)
  pure unit

-- For custom config:
renderSankeyCustom :: String -> Array LinkCSVRow -> Effect Unit
renderSankeyCustom selector flows = do
  -- defaultSankeyConfig :: Number -> Number -> SankeyConfig
  let config = defaultSankeyConfig 800.0 400.0
  -- config has: alignment (Justify), nodeWidth (15.0), nodePadding (10.0),
  --             iterations (6), linkColorMode (SourceTargetGradient), extent
  let result = computeLayoutWithConfig flows config
  _ <- rerender selector (sankeyTree result)
  pure unit

sankeyTree :: SankeyLayoutResult -> Tree
sankeyTree result =
  elem SVG [ width 800.0, height 400.0 ]
    [ -- Links (ribbons)
      elem Group [ class_ "sankey-links" ]
        (result.links <#> \lnk ->
          elem Path
            [ d (sankeyLinkPath lnk result.nodes)
            , fill "none"
            , thunkedStr "stroke" lnk.color
            , thunkedNum "stroke-width" lnk.width
            , strokeOpacity "0.4"
            ]
            []
        )
    , -- Nodes (rectangles)
      forEach "nodes" Group result.nodes (\n -> n.name) \node ->
        elem Group []
          [ elem Rect
              [ thunkedNum "x" node.x0
              , thunkedNum "y" node.y0
              , thunkedNum "width" (node.x1 - node.x0)
              , thunkedNum "height" (node.y1 - node.y0)
              , thunkedStr "fill" node.color
              ]
              []
          , elem Text
              [ thunkedNum "x" (node.x0 - 6.0)
              , thunkedNum "y" ((node.y0 + node.y1) / 2.0)
              , textAnchor "end"
              , fontSize "11"
              , thunkedStr "textContent" node.name
              ]
              []
          ]
    ]
```

### Pattern 6: Simulation inside Halogen Component

For force-directed layouts that need to send events back to Halogen state.

```purescript
module Component.ForceViz where

import Prelude
import Effect.Class (liftEffect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hylograph.Simulation (runSimulation, Engine(..), setup, manyBody, center, collide, withStrength, static)
import Hylograph.Simulation.Emitter (SimulationEvent(..), subscribe)
import Hylograph.ForceEngine.Halogen (toHalogenEmitter)

type State =
  { nodes :: Array SimNode
  , alpha :: Number
  , initialized :: Boolean
  }

data Action
  = Initialize
  | SimEvent SimulationEvent

component :: forall q i o m. MonadAff m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { nodes: [], alpha: 1.0, initialized: false }
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "force-viz") ]
    [ HH.div [ HP.id "force-container" ] [] ]

handleAction :: forall o m. MonadAff m =>
  Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Start simulation
    { handle, events } <- liftEffect $ runSimulation
      { engine: D3
      , setup: setup "physics"
          [ manyBody "charge" # withStrength (static (-80.0))
          , center "center"
          , collide "collision"
          ]
      , nodes: initialNodes
      , links: []
      , container: "#force-container"
      , alphaMin: 0.001
      }

    -- Bridge simulation events -> Halogen actions
    halogenEmitter <- liftEffect $ toHalogenEmitter events
    void $ H.subscribe $ halogenEmitter <#> SimEvent

    H.modify_ _ { initialized = true }

  SimEvent (Tick { alpha, nodeCount }) -> do
    H.modify_ _ { alpha = alpha }
    -- Re-render HATS tree with updated node positions from handle.getNodes

  SimEvent Completed -> do
    H.modify_ _ { alpha = 0.0 }

  SimEvent _ -> pure unit
```

### Pattern 7: Coordinated Multi-View with Highlighting

For linked views where hovering in one view highlights across all views.

```purescript
module Viz.Coordinated where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Hylograph.HATS
  ( Tree, elem, forEach, thunkedNum, thunkedStr
  , withBehaviors, onCoordinatedHighlight
  )
import Hylograph.HATS.Friendly (width, height, r)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Internal.Behavior.Types (HighlightClass(..))

type DataPoint =
  { id :: String
  , x :: Number, y :: Number      -- For scatter view
  , barValue :: Number             -- For bar view
  , category :: String
  , connections :: Array String    -- Related items
  }

-- Two views sharing the same data, coordinated via highlight group
renderCoordinated :: String -> String -> Array DataPoint -> Effect Unit
renderCoordinated scatterSelector barSelector points = do
  _ <- rerender scatterSelector (scatterView points)
  _ <- rerender barSelector (barView points)
  pure unit

scatterView :: Array DataPoint -> Tree
scatterView points =
  elem SVG [ width 400.0, height 400.0 ]
    [ forEach "scatter-pts" Circle points _.id \pt ->
        withBehaviors
          [ onCoordinatedHighlight
              { identify: pt.id
              , classify: \hoveredId ->
                  if pt.id == hoveredId then Primary
                  else if hoveredId `Array.elem` pt.connections then Related
                  else Dimmed
              , group: Nothing  -- global coordination (or Just "my-group" to scope)
              }
          ] $
        elem Circle
          [ thunkedNum "cx" pt.x
          , thunkedNum "cy" pt.y
          , r 5.0
          , thunkedStr "fill" (categoryColor pt.category)
          ]
          []
    ]

barView :: Array DataPoint -> Tree
barView points =
  elem SVG [ width 400.0, height 400.0 ]
    [ forEach "bar-rects" Rect points _.id \pt ->
        withBehaviors
          [ onCoordinatedHighlight
              { identify: pt.id
              , classify: \hoveredId ->
                  if pt.id == hoveredId then Primary
                  else Dimmed
              , group: Nothing
              }
          ] $
        elem Rect
          [ thunkedNum "x" (barX pt)
          , thunkedNum "y" (400.0 - pt.barValue)
          , width 20.0
          , thunkedNum "height" pt.barValue
          , thunkedStr "fill" (categoryColor pt.category)
          ]
          []
    ]
```

**How coordinated highlighting works**: When any element with `onCoordinatedHighlight` is hovered, ALL elements in the same group receive CSS classes based on their `classify` function:
- `.highlight-primary` -- the hovered element
- `.highlight-related` -- related elements
- `.highlight-dimmed` -- unrelated elements
- `.highlight-neutral` -- default state

HighlightClass constructors: `Primary | Related | Dimmed | Neutral | Upstream | Downstream`

### Pattern 8: Chord Diagram

For matrix data showing relationships between groups.

```purescript
module Viz.MyChord where

import Prelude
import Effect (Effect)
import DataViz.Layout.Chord (layout, layoutWithConfig)
import DataViz.Layout.Chord.Types (ChordLayout, ChordGroup, Chord, defaultConfig)
import Hylograph.HATS (Tree, elem, forEach, thunkedStr)
import Hylograph.HATS.Friendly (width, height, transform, fillOpacity)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Scale (schemeCategory10At)

renderChord :: String -> Array String -> Array (Array Number) -> Effect Unit
renderChord selector names matrix = do
  -- layout :: Array (Array Number) -> ChordLayout
  -- ChordLayout = { groups :: Array ChordGroup, chords :: Array Chord }
  -- ChordGroup = { index :: Int, startAngle :: Number, endAngle :: Number, value :: Number }
  -- Chord = { source :: ChordGroup, target :: ChordGroup }
  let result = layout matrix

  _ <- rerender selector (chordTree names result)
  pure unit

chordTree :: Array String -> ChordLayout -> Tree
chordTree names result =
  let radius = 200.0
  in elem SVG [ width 500.0, height 500.0 ]
    [ elem Group [ transform "translate(250,250)" ]
        [ -- Arcs
          forEach "arcs" Path result.groups (\g -> show g.index) \group ->
            elem Path
              [ thunkedStr "d" (arcPath radius (radius + 20.0) group.startAngle group.endAngle)
              , thunkedStr "fill" (schemeCategory10At group.index)
              ]
              []
        , -- Ribbons
          forEach "ribbons" Path result.chords (\c -> show c.source.index <> "-" <> show c.target.index) \chord ->
            elem Path
              [ thunkedStr "d" (ribbonPath radius chord)
              , thunkedStr "fill" (schemeCategory10At chord.source.index)
              , fillOpacity "0.6"
              ]
              []
        ]
    ]
```

---

## Key Types Reference

### HATS Core (Hylograph.HATS)

```purescript
-- The central tree type -- compose freely via <>
data Tree
  = Elem { elemType :: ElementType, attrs :: Array Attr, children :: Array Tree, behaviors :: Array ThunkedBehavior }
  | MkFold SomeFold       -- Existentially-wrapped data iteration
  | Empty

-- Tree is a Semigroup and Monoid.
-- Use <> to compose independent visualization parts:
-- chart = title <> xAxis <> yAxis <> plotArea <> legend
--
-- Or equivalently, use an SVG elem's children array:
-- elem SVG [...] [ title, xAxis, yAxis, plotArea, legend ]
--
-- Both approaches work. Children arrays are more common for spatial grouping;
-- <> is useful when combining trees from different modules or functions.

-- SVG/HTML element types (EXHAUSTIVE)
data ElementType
  -- SVG elements (require SVG namespace)
  = Circle | Rect | Path | Line | Polygon | Text | Group | SVG
  | Defs | LinearGradient | Stop | PatternFill
  -- HTML elements (use default namespace)
  | Div | Span | Table | Tr | Td | Th | Tbody | Thead

-- Attribute type
data Attr
  = StaticAttr String String            -- name, value (fixed)
  | ThunkedAttr String (Unit -> String)  -- name, thunk (captures datum in closure)

-- Attr constructors
staticStr  :: String -> String -> Attr    -- Fixed string value
staticNum  :: String -> Number -> Attr    -- Fixed numeric value (show'd to string)
thunkedStr :: String -> String -> Attr    -- Captures string in closure
thunkedNum :: String -> Number -> Attr    -- Captures number in closure

-- Key constructors
elem     :: ElementType -> Array Attr -> Array Tree -> Tree
forEach  :: forall a. String -> ElementType -> Array a -> (a -> String) -> (a -> Tree) -> Tree
forEachP :: forall source target. Project source target =>
            String -> ElementType -> source -> (target -> String) -> (target -> Tree) -> Tree
forEachWithGUP :: forall a. String -> ElementType -> Array a -> (a -> String) -> (a -> Tree) -> GUPSpec a -> Tree
fromTree :: forall a. String -> ElementType -> a -> (a -> Array a) -> (a -> String) -> TraversalOrder -> Boolean -> (a -> Tree) -> Tree
-- TraversalOrder = DepthFirst | BreadthFirst
-- Boolean = include internal (non-leaf) nodes
-- Note: the template function receives the datum only, not depth/index.
-- To position items, either pre-compute depth on your data type or use a layout algorithm.
preserveTree :: forall a. String -> ElementType -> a -> (a -> Array a) -> (a -> String) -> (a -> Tree) -> Tree

-- Behavior constructors (all take Effect Unit, datum captured via closures)
onMouseEnter :: Effect Unit -> ThunkedBehavior
onMouseLeave :: Effect Unit -> ThunkedBehavior
onClick :: Effect Unit -> ThunkedBehavior
onClickWithModifier :: Effect Unit -> Effect Unit -> ThunkedBehavior  -- plain, modifier+click
onDrag :: DragConfig -> ThunkedBehavior
onZoom :: ZoomConfig -> ThunkedBehavior

-- Coordinated highlighting (simple -- hover only)
onCoordinatedHighlight ::
  { identify :: String, classify :: String -> HighlightClass, group :: Maybe String }
  -> ThunkedBehavior
onCoordinatedHighlightWithTooltip ::
  { identify :: String, classify :: String -> HighlightClass, group :: Maybe String
  , tooltip :: Maybe { content :: String, showWhen :: TooltipTrigger } }
  -> ThunkedBehavior

-- Coordinated interaction (full -- hover + brush + clear)
onCoordinatedInteraction ::
  { identify :: String, respond :: InteractionTrigger -> InteractionState
  , position :: Maybe { x :: Number, y :: Number }, group :: Maybe String }
  -> ThunkedBehavior
onBrush :: { extent :: BoundingBox, group :: Maybe String } -> ThunkedBehavior

-- BoundingBox = { x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number }

-- InteractionTrigger (import from Hylograph.Interaction.Coordinated)
--   = HoverTrigger String | BrushTrigger BoundingBox
--   | SelectionTrigger (Set String) | FocusTrigger (Maybe String) | ClearTrigger

-- InteractionState (import from Hylograph.Interaction.Coordinated)
--   = Primary | Related | Selected | Dimmed | Neutral

-- HighlightClass (import from Hylograph.Internal.Behavior.Types)
--   = Primary | Related | Dimmed | Neutral | Upstream | Downstream

-- TooltipTrigger = OnHover | WhenPrimary | WhenRelated

-- Interpreter
rerender :: String -> Tree -> Effect RerenderResult
-- RerenderResult = { selections :: SelectionMap, transitions :: Maybe HATSTransitions }
clearContainer :: String -> Effect Unit
clearAllHighlights :: Effect Unit
```

### HATS.Friendly (Hylograph.HATS.Friendly)

Named attribute helpers for cleaner code (all produce `Attr`):

```purescript
-- Position: cx, cy, r, x, y, x1, y1, x2, y2, width, height
-- Path: d, points
-- Style: fill, stroke, strokeWidth, opacity, fillOpacity, strokeOpacity
-- Text: textAnchor, fontSize, fontFamily, fontWeight
-- Layout: viewBox (4 args), transform, class_, style, preserveAspectRatio
-- Generic: attr (String -> String -> Attr), attrNum (String -> Number -> Attr)
```

These are static-only convenience wrappers (`staticStr`/`staticNum` under the hood). Inside a `forEach` template, Friendly helpers work fine for **constant** values (the template is called once per datum, so the concrete value is captured at call time). Use `thunkedStr`/`thunkedNum` for **datum-derived** values -- they wrap in a thunk that matters for GUP transitions where attributes are re-evaluated.

### Finally-Tagless Projection (Hylograph.HATS)

```purescript
-- Project a source to an array of targets
class Project source target | source -> target where
  project :: source -> Array target

-- Built-in instances:
instance Project (Array a) a                     -- identity
instance Project (MapKeys k v) k                 -- Map -> keys
instance Project (MapValues k v) v               -- Map -> unique values
instance Project (MapEntries k v) (Tuple k v)    -- Map -> entries

-- MapKeys, MapValues, MapEntries are newtypes wrapping Map.
-- Construct by wrapping your Map:
newtype MapKeys k v = MapKeys (Map k v)
newtype MapValues k v = MapValues (Map k v)
newtype MapEntries k v = MapEntries (Map k v)

-- The killer feature: one Map, three views, one diagram
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Hylograph.HATS (forEachP, MapKeys(..), MapValues(..), MapEntries(..))

mapDiagram :: forall k v. Ord k => Ord v => Show k => Show v => Map k v -> Tree
mapDiagram m =
  elem SVG [...]
    [ forEachP "keys" Circle (MapKeys m) show \key -> keyNode key
    , forEachP "values" Circle (MapValues m) show \value -> valueNode value
    , forEachP "arrows" Path (MapEntries m) (\(Tuple k _) -> show k) \(Tuple k v) ->
        arrow (keyPos k) (valuePos v)
    ]
```

### Scales (Hylograph.Scale)

```purescript
-- Scale is a foreign data type with phantom types: Scale domain range kind
foreign import data Scale :: Type -> Type -> Type -> Type

-- Type aliases
type ContinuousScale = Scale Number Number Continuous
type BandScale d = Scale d Number Band
type TimeScale = Scale JSDate Number Time
type SequentialScale = Scale Number String Sequential
type DivergingScale = Scale Number String Diverging

-- Continuous scale constructors
linear :: ContinuousScale
log    :: ContinuousScale
pow    :: ContinuousScale
sqrt   :: ContinuousScale
symlog :: ContinuousScale

-- Time scale constructors
scaleTime :: TimeScale
scaleUtc  :: TimeScale

-- Sequential/diverging (take an Interpolator argument!)
sequential    :: Interpolator String -> SequentialScale
sequentialLog :: Interpolator String -> SequentialScale
diverging     :: Interpolator String -> DivergingScale

-- Ordinal/categorical
ordinal :: forall d r. OrdinalScale d r
band    :: forall d. BandScale d
point   :: forall d. BandScale d

-- Configuration (chainable via #)
domain      :: forall d r k. Array d -> Scale d r k -> Scale d r k
range       :: forall d r k. Array r -> Scale d r k -> Scale d r k
nice        :: forall r k. Scale Number r k -> Scale Number r k
clamp       :: forall d r k. Boolean -> Scale d r k -> Scale d r k
padding     :: forall d. Number -> BandScale d -> BandScale d
paddingInner :: forall d. Number -> BandScale d -> BandScale d
paddingOuter :: forall d. Number -> BandScale d -> BandScale d

-- Operations
applyScale :: forall d r k. Scale d r k -> d -> r
invert     :: forall r. Scale Number r Continuous -> r -> Maybe Number
ticks      :: forall r k. Int -> Scale Number r k -> Array Number
tickFormat :: forall r k. Int -> String -> Scale Number r k -> (Number -> String)
bandwidth  :: forall d. BandScale d -> Number

-- Functional combinators
andThen  :: forall a b c k1 k2. Scale a b k1 -> Scale b c k2 -> (a -> c)
contramap :: forall a a' b k. (a' -> a) -> Scale a b k -> (a' -> b)
map       :: forall a b b' k. (b -> b') -> Scale a b k -> (a -> b')
dimap     :: forall a a' b b' k. (a' -> a) -> (b -> b') -> Scale a b k -> (a' -> b')

-- Categorical color schemes (Array String or Int -> String)
schemeCategory10   :: Array String
schemeCategory10At :: Int -> String       -- wraps modularly
schemeTableau10    :: Array String
schemeTableau10At  :: Int -> String
schemePaired, schemeSet1, schemeSet2, schemeSet3 :: Array String
schemeAccent, schemeDark2, schemePastel1, schemePastel2 :: Array String

-- Sequential interpolators (Interpolator = Number -> String, maps [0,1] to color)
interpolateViridis, interpolatePlasma, interpolateInferno, interpolateMagma :: Interpolator String
interpolateBlues, interpolateGreens, interpolateReds, interpolateOranges :: Interpolator String
interpolateBuGn, interpolateYlGnBu, interpolateYlOrRd :: Interpolator String

-- Diverging interpolators
interpolateRdBu, interpolateRdYlGn, interpolatePiYG, interpolateSpectral :: Interpolator String
```

### Simulation (Hylograph.Simulation)

```purescript
data Engine = D3 | WASM

-- SimulationNode uses extensible row types
type SimulationNode r = { id :: Int, x :: Number, y :: Number
                        , vx :: Number, vy :: Number
                        , fx :: Nullable Number, fy :: Nullable Number | r }

-- Main entry point
runSimulation :: forall r. SimulationConfig r -> Effect (SimulationResult r)

type SimulationConfig r =
  { engine :: Engine
  , setup :: Setup (SimulationNode r)
  , nodes :: Array (SimulationNode r)
  , links :: Array { source :: Int, target :: Int }
  , container :: String
  , alphaMin :: Number
  }

type SimulationResult r =
  { handle :: SimulationHandle r
  , events :: SimulationEmitter
  }

-- Events
data SimulationEvent
  = Tick { alpha :: Number, nodeCount :: Int }
  | Started | Stopped | Completed

subscribe :: SimulationEmitter -> (SimulationEvent -> Effect Unit) -> Effect Unsubscribe

-- For Halogen bridging
toHalogenEmitter :: SimulationEmitter -> Effect (HS.Emitter SimulationEvent)

-- Force builders (all take a name :: String, return ForceConfig node)
manyBody  :: forall node. String -> ForceConfig node   -- N-body charge
center    :: forall node. String -> ForceConfig node   -- Pull toward center
collide   :: forall node. String -> ForceConfig node   -- Prevent overlap
link      :: forall node. String -> ForceConfig node   -- Spring forces
positionX :: forall node. String -> ForceConfig node   -- Horizontal constraint
positionY :: forall node. String -> ForceConfig node   -- Vertical constraint
radial    :: forall node. String -> ForceConfig node   -- Circular constraint

-- Static vs dynamic values for force modifiers
data Value node a = Static a | Dynamic (node -> a)
static  :: forall node a. a -> Value node a
dynamic :: forall node a. (node -> a) -> Value node a

-- Modifiers (chainable via #)
withStrength    :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withRadius      :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withDistance    :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withX           :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withY           :: forall node. Value node Number -> ForceConfig node -> ForceConfig node
withTheta       :: forall node. Number -> ForceConfig node -> ForceConfig node
withIterations  :: forall node. Int -> ForceConfig node -> ForceConfig node
withFilter      :: forall node. (node -> Boolean) -> ForceConfig node -> ForceConfig node

-- Setup builder
setup :: forall node. String -> Array (ForceConfig node) -> Setup node

-- Handle operations
-- handle.getNodes :: Effect (Array (SimulationNode r))
-- handle.stop :: Effect Unit
-- handle.start :: Effect Unit
-- handle.reheat :: Effect Unit
-- handle.updateData :: Array nodes -> Array links -> Effect GUPResult
```

### Layout (hylograph-layout)

#### Input types

```purescript
-- ValuedNode (DataViz.Layout.Hierarchy.Types) -- input for treemap, pack
-- Constructor: VNode (not ValuedNode)
data ValuedNode a = VNode
  { data_ :: a, depth :: Int, height :: Int
  , parent :: Maybe (ValuedNode a)
  , children :: Array (ValuedNode a)
  , value :: Number
  }
-- Helper accessors: getValuedData, getValue, getValuedDepth, getValuedHeight, getValuedChildren
```

#### Treemap

```purescript
-- DataViz.Layout.Hierarchy.Treemap
treemap :: forall a. TreemapConfig a -> ValuedNode a -> TreemapNode a
-- TileFunction options: squarify phi, sliceDice, binary, dice, slice

-- Constructor: TNode (not TreemapNode)
data TreemapNode a = TNode
  { data_ :: a, depth :: Int, height :: Int, value :: Number
  , children :: Array (TreemapNode a)
  , x0 :: Number, y0 :: Number, x1 :: Number, y1 :: Number  -- positioned bounds
  }

-- No built-in flatten -- recurse through children manually:
flattenLeaves :: forall a. TreemapNode a -> Array (TreemapNode a)
flattenLeaves (TNode n) =
  if Array.null n.children then [ TNode n ]
  else Array.concatMap flattenLeaves n.children
```

#### Tree layout

```purescript
-- DataViz.Layout.Hierarchy.Tree
tree :: forall r. TreeConfig {...} -> Tree {...} -> Tree {...}
-- defaultTreeConfig + withLayerSeparation, withSeparation
-- Returns tree with positioned x, y coordinates on each node
```

#### Circle Packing

```purescript
-- DataViz.Layout.Hierarchy.Pack
pack :: forall a. PackConfig a -> PackNode a -> PackNode a

type PackConfig a =
  { size :: { width :: Number, height :: Number }
  , padding :: Number
  , radius :: Maybe (a -> Number)
  }
-- defaultPackConfig provided. PackNode has: data_, depth, height, value, children, x, y, r
-- Same flattening pattern as TreemapNode (recurse through children).
```

#### Sankey

```purescript
-- DataViz.Layout.Sankey.Compute
computeLayout :: Array LinkCSVRow -> Number -> Number -> SankeyLayoutResult
computeLayoutWithConfig :: Array LinkCSVRow -> SankeyConfig -> SankeyLayoutResult
-- LinkCSVRow = { s :: String, t :: String, v :: Number }
-- SankeyLayoutResult = { nodes :: Array SankeyNode, links :: Array SankeyLink }
-- SankeyNode has: name, x0, y0, x1, y1, value, depth, layer, color
-- SankeyLink has: sourceIndex, targetIndex, value, width, y0, y1, color
-- defaultSankeyConfig :: Number -> Number -> SankeyConfig
-- Alignment = Justify | Left | Right | Center
-- LinkColorMode = SourceColor | TargetColor | SourceTargetGradient | StaticColor String
```

**Sankey ribbon paths** are not library-provided. Write your own cubic bezier:

```purescript
sankeyLinkPath :: SankeyLink -> Array SankeyNode -> String
sankeyLinkPath lnk nodes =
  let src = nodes !! lnk.sourceIndex
      tgt = nodes !! lnk.targetIndex
  in case src, tgt of
    Just s, Just t ->
      let sx = s.x1, tx = t.x0, midX = (sx + tx) / 2.0
      in "M" <> show sx <> "," <> show lnk.y0
         <> "C" <> show midX <> "," <> show lnk.y0
         <> " " <> show midX <> "," <> show lnk.y1
         <> " " <> show tx <> "," <> show lnk.y1
    _, _ -> ""
```

#### Chord

```purescript
-- DataViz.Layout.Chord
layout :: Array (Array Number) -> ChordLayout
layoutWithConfig :: LayoutConfig -> Array (Array Number) -> ChordLayout
-- ChordLayout = { groups :: Array ChordGroup, chords :: Array Chord }
-- ChordGroup = { index :: Int, startAngle :: Number, endAngle :: Number, value :: Number }
-- Chord = { source :: ChordGroup, target :: ChordGroup }
```

**Arc and ribbon paths** are not library-provided. Write your own using SVG arc commands:

```purescript
arcPath :: Number -> Number -> Number -> Number -> String
arcPath innerR outerR startAngle endAngle =
  let sx0 = outerR * sin startAngle, sy0 = negate (outerR * cos startAngle)
      sx1 = outerR * sin endAngle, sy1 = negate (outerR * cos endAngle)
      largeArc = if endAngle - startAngle > pi then "1" else "0"
  in "M" <> show sx0 <> "," <> show sy0
     <> "A" <> show outerR <> "," <> show outerR <> " 0 " <> largeArc <> " 1 " <> show sx1 <> "," <> show sy1
     <> "L" <> show (innerR * sin endAngle) <> "," <> show (negate (innerR * cos endAngle))
     <> "A" <> show innerR <> "," <> show innerR <> " 0 " <> largeArc <> " 0 " <> show (innerR * sin startAngle) <> "," <> show (negate (innerR * cos startAngle))
     <> "Z"
```

#### Partition and Edge Bundle

```purescript
-- DataViz.Layout.Hierarchy.Partition
partition :: forall a. PartitionConfig a -> PartitionNode a -> PartitionNode a
flattenPartition :: forall a. PartitionNode a -> Array (PartitionNode a)  -- pre-order traversal
-- PartitionNode has: data_, depth, height, value, children, x0, y0, x1, y1

-- DataViz.Layout.Hierarchy.EdgeBundle
edgeBundle :: forall a. EdgeBundleConfig a -> Array a -> EdgeBundleResult a
-- EdgeBundleResult = { nodes :: Array (PositionedNode a), links :: Array BundledLink }
-- BundledLink has: source, target, path (SVG path string -- library-provided!)
```

### Graph (hylograph-graph)

```purescript
-- Core types
newtype NodeId = NodeId String
newtype Graph = Graph { nodeSet, edgeList, adjacency, positions }

-- DAG operations
topologicalSort :: DAG node weight -> Array node
layers          :: DAG node weight -> Map node Int
sources         :: DAG node weight -> Array node   -- No incoming
sinks           :: DAG node weight -> Array node   -- No outgoing

-- Pathfinding
findPath       :: NodeId -> NodeId -> Graph -> Maybe Path
findPathTraced :: NodeId -> NodeId -> Graph ->
  { result :: Maybe Path, steps :: Array Step, explored :: Array NodeId }
```

### Transitions (hylograph-transitions)

```purescript
-- Import: Hylograph.Transition.Easing (EasingType(..))
-- Also re-exported from Hylograph.Internal.Attribute in hylograph-selection

data EasingType
  = Linear
  | QuadIn | QuadOut | QuadInOut
  | CubicIn | CubicOut | CubicInOut
  | SinIn | SinOut | SinInOut
  | ExpIn | ExpOut | ExpInOut
  | CircleIn | CircleOut | CircleInOut
  | BackIn | BackOut | BackInOut
  | ElasticIn | ElasticOut | ElasticInOut
  | BounceIn | BounceOut | BounceInOut

-- Interpolation
lerpPoint :: Point -> Point -> Progress -> Point
lerpRGB   :: RGB -> RGB -> Progress -> RGB
lerpHSL   :: HSL -> HSL -> Progress -> HSL
```

---

## Interaction Patterns

All behavior constructors live in `Hylograph.HATS` -- there are no separate interaction modules to import for basic behaviors.

### Hover with Visual Feedback

Behavior callbacks take `Effect Unit`. Common patterns for what goes inside:

```purescript
import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Hylograph.HATS (onMouseEnter, onMouseLeave, withBehaviors, rerender)

-- Pattern A: Use an Effect.Ref to track hover state, then re-render
renderWithHover :: Ref.Ref (Maybe String) -> Array Item -> Effect Unit
renderWithHover hoveredRef items = do
  let tree = forEach "items" Circle items _.id \item ->
        withBehaviors
          [ onMouseEnter do
              Ref.write (Just item.id) hoveredRef
              renderWithHover hoveredRef items  -- re-render with new state
          , onMouseLeave do
              Ref.write Nothing hoveredRef
              renderWithHover hoveredRef items
          ] $
        elem Circle [ ... ] []
  _ <- rerender "#viz" tree
  pure unit

-- Pattern B: Pass callbacks from Halogen (see Click -> Halogen Action below)
-- Pattern C: Use onCoordinatedHighlight for cross-view highlighting (no manual callback needed)
```

Note: The datum is captured in the closure because we're inside the `forEach` template. `onMouseEnter` receives `Effect Unit`, not the datum -- the datum is already in scope.

### Click -> Halogen Action

For click events that need to update Halogen state, use a callback bridge:

```purescript
-- In the Halogen component:
handleAction Initialize = do
  { listener, emitter } <- liftEffect HS.create
  void $ H.subscribe $ emitter

  -- Pass callbacks to visualization
  liftEffect $ renderViz
    { onItemClick: \itemId -> HS.notify listener (ItemClicked itemId)
    , onHover: \itemId -> HS.notify listener (ItemHovered itemId)
    }

-- In the visualization module:
renderViz :: { onItemClick :: String -> Effect Unit, onHover :: String -> Effect Unit } -> Effect Unit
renderViz callbacks = do
  let tree = forEach "items" Circle items _.id \item ->
        withBehaviors
          [ onClick (callbacks.onItemClick item.id)
          , onMouseEnter (callbacks.onHover item.id)
          ] $
        elem Circle [ ... ] []
  _ <- rerender "#viz" tree
  pure unit
```

### Click with Modifier Keys

```purescript
import Hylograph.HATS (onClickWithModifier)

forEach "nodes" Circle nodes _.id \node ->
  withBehaviors
    [ onClickWithModifier
        (callbacks.onDrill node.id)      -- plain click
        (callbacks.onFilter node.id)     -- Cmd/Ctrl/Shift + click
    ] $
  elem Circle [ ... ] []
```

### Coordinated Highlighting (Simple)

For hover-only coordination across views:

```purescript
import Hylograph.HATS (onCoordinatedHighlight)
import Hylograph.Internal.Behavior.Types (HighlightClass(..))

forEach "nodes" Circle nodes _.id \node ->
  withBehaviors
    [ onCoordinatedHighlight
        { identify: node.name
        , classify: \hoveredId ->
            if node.name == hoveredId then Primary
            else if hoveredId `elem` node.connections then Related
            else Dimmed
        , group: Nothing  -- global, or Just "my-group" to scope
        }
    ] $
  elem Circle [ ... ] []
```

### Coordinated Highlighting with Tooltip

```purescript
import Hylograph.HATS (onCoordinatedHighlightWithTooltip)
import Hylograph.Internal.Behavior.Types (HighlightClass(..), TooltipTrigger(..))

forEach "nodes" Circle nodes _.id \node ->
  withBehaviors
    [ onCoordinatedHighlightWithTooltip
        { identify: node.name
        , classify: \hoveredId -> if node.name == hoveredId then Primary else Dimmed
        , group: Nothing
        , tooltip: Just { content: node.description, showWhen: OnHover }
        }
    ] $
  elem Circle [ ... ] []
```

### Brush Selection (Full Coordinated Interaction)

```purescript
import Hylograph.HATS (onCoordinatedInteraction, onBrush, withBehaviors)
import Hylograph.Interaction.Coordinated (InteractionTrigger(..), InteractionState(..))

-- Add brush overlay to container
brushOverlay :: Tree
brushOverlay =
  withBehaviors
    [ onBrush
        { extent: { x0: 0.0, y0: 0.0, x1: 400.0, y1: 300.0 }
        , group: Just "scatter-plot"
        }
    ] $
  elem Group [ staticStr "class" "brush-overlay" ] []

-- Elements respond to both hover and brush
scatterPoints :: Array DataPoint -> Tree
scatterPoints points =
  forEach "points" Circle points _.id \pt ->
    withBehaviors
      [ onCoordinatedInteraction
          { identify: pt.id
          , respond: \trigger -> case trigger of
              HoverTrigger id -> if pt.id == id then Primary else Dimmed
              BrushTrigger box -> if pointInBox pt box then Selected else Dimmed
              ClearTrigger -> Neutral
          , position: Just { x: pt.x, y: pt.y }  -- for brush hit-testing
          , group: Just "scatter-plot"
          }
      ] $
    elem Circle [ ... ] []
```

### Zoom + Pan

```purescript
import Hylograph.HATS (onZoom, withBehaviors)
import Hylograph.Internal.Behavior.Types (ZoomConfig(..), ScaleExtent(..))

withBehaviors
  [ onZoom (ZoomConfig
      { scaleExtent: ScaleExtent 0.5 10.0
      , targetSelector: "#viz-group"
      })
  ] $
elem Group [ staticStr "class" "zoomable" ] [ vizContent ]
```

---

## GUP (General Update Pattern) with Transitions

For animated enter/update/exit when data changes:

```purescript
import Hylograph.HATS (forEachWithGUP, GUPSpec, PhaseSpec, thunkedNum, thunkedStr)
import Hylograph.HATS.Friendly (r, fillOpacity)
import Hylograph.Internal.Transition.Types (TransitionConfig)
import Hylograph.Transition.Easing (EasingType(..))
import Data.Time.Duration (Milliseconds(..))

animatedCircles :: Array DataPoint -> Tree
animatedCircles points =
  forEachWithGUP "circles" Circle points _.id
    -- Template (target state)
    (\pt -> elem Circle
      [ thunkedNum "cx" pt.x
      , thunkedNum "cy" pt.y
      , r 5.0
      , thunkedStr "fill" (colorFor pt)
      ]
      []
    )
    -- GUP specification
    { enter: Just
        { attrs: [ r 0.0, fillOpacity "0" ]
        , transition: Just
            { duration: Milliseconds 500.0
            , delay: Nothing
            , easing: CubicOut
            , staggerDelay: Just 20.0
            }
        }
    , update: Just
        { attrs: []
        , transition: Just
            { duration: Milliseconds 300.0
            , delay: Nothing
            , easing: CubicInOut
            , staggerDelay: Nothing
            }
        }
    , exit: Just
        { attrs: [ r 0.0, fillOpacity "0" ]
        , transition: Just
            { duration: Milliseconds 300.0
            , delay: Nothing
            , easing: CubicIn
            , staggerDelay: Nothing
            }
        }
    }
```

---

## Halogen Integration Patterns

### Change Detection in Receive

When a parent component passes new props, detect what changed and react appropriately:

```purescript
handleAction (Receive input) = do
  state <- H.get
  let oldInput = state.lastInput
      dataChanged = Array.length input.nodes /= Array.length oldInput.nodes
      scopeChanged = input.scope /= oldInput.scope
      themeChanged = input.theme /= oldInput.theme

  H.modify_ _ { lastInput = input }

  if dataChanged then fullRerender input        -- Rebuild everything
  else if scopeChanged then updateScope input   -- GUP: animate in/out
  else if themeChanged then updateColors input  -- Just restyle
  else pure unit
```

### Subscription Bridge (D3 -> Halogen)

```purescript
import Halogen.Subscription as HS

-- Create bridge
{ listener, emitter } <- liftEffect HS.create

-- Subscribe Halogen to the emitter
void $ H.subscribe $ emitter

-- In D3/Effect code, notify through the listener
HS.notify listener (MyAction someData)
```

### Multi-Scene Navigation (CodeExplorer pattern)

For apps with multiple visualization scenes:

```purescript
data Scene
  = Overview         -- High-level summary
  | Detail String    -- Drill into specific item
  | Comparison       -- Side-by-side comparison

type State =
  { scene :: Scene
  , data :: AllData
  , capturedPositions :: Maybe (Array CapturedPosition)  -- For hero transitions
  }

handleAction (NavigateTo targetScene) = do
  -- 1. Optionally capture positions from current view
  capturedPos <- captureCurrentPositions

  -- 2. Clear visualization containers
  liftEffect clearAllVizContainers

  -- 3. Update state
  H.modify_ _ { scene = targetScene, capturedPositions = capturedPos }

  -- 4. Lazy-load data if needed for new scene
  prepareSceneData targetScene

  -- 5. Render new scene
  renderCurrentScene
```

---

## Data Transformation Patterns

### CSV/Tabular Data -> Scatterplot

```purescript
-- Raw data (from JSON/CSV)
type RawRow = { name :: String, income :: Number, happiness :: Number, region :: String }

-- Transform to viz-ready data
toScatterPoints :: Array RawRow -> Array DataPoint
toScatterPoints = map \row ->
  { label: row.name
  , x: row.income
  , y: row.happiness
  , category: row.region
  }
```

### Flat List -> Sankey (flow data)

```purescript
-- Sankey expects LinkCSVRow = { s :: String, t :: String, v :: Number }
import DataViz.Layout.Sankey.Types (LinkCSVRow)

toSankeyInput :: Array { from :: String, to :: String, amount :: Number } -> Array LinkCSVRow
toSankeyInput = map \row -> { s: row.from, t: row.to, v: row.amount }
```

### Adjacency List -> SimulationNode + Link

```purescript
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Data.Nullable as Nullable

-- SimulationNode r = { id :: Int, x, y, vx, vy :: Number, fx, fy :: Nullable Number | r }
type MyNode = SimulationNode (name :: String, group :: Int)

toSimData :: Array { name :: String, group :: Int } -> Array { from :: String, to :: String }
  -> { nodes :: Array MyNode, links :: Array { source :: Int, target :: Int } }
toSimData nodeInputs edges =
  let nodeIndex = Map.fromFoldable $ mapWithIndex (\i n -> Tuple n.name i) nodeInputs
      nodes = mapWithIndex (\i n ->
        { id: i, name: n.name, group: n.group
        , x: 0.0, y: 0.0, vx: 0.0, vy: 0.0
        , fx: Nullable.null, fy: Nullable.null
        }) nodeInputs
      links = Array.mapMaybe (\e -> do
        si <- Map.lookup e.from nodeIndex
        ti <- Map.lookup e.to nodeIndex
        pure { source: si, target: ti }
        ) edges
  in { nodes, links }
```

---

## spago.yaml Template

```yaml
workspace:
  extraPackages: {}  # All from registry

package:
  name: my-visualization
  dependencies:
    # Always needed
    - prelude
    - effect
    - aff
    - halogen
    - hylograph-selection

    # For force-directed layouts
    - hylograph-simulation
    - hylograph-d3-kernel          # or hylograph-wasm-kernel
    - hylograph-simulation-halogen
    - hylograph-simulation-core
    - halogen-subscriptions

    # For hierarchical layouts
    - hylograph-layout

    # For graph algorithms
    - hylograph-graph

    # For animations
    - hylograph-transitions

    # Common utilities
    - arrays
    - maybe
    - either
    - tuples
    - ordered-collections
    - integers
    - numbers
    - strings
    - web-dom
    - web-html

    # For data loading
    - affjax
    - argonaut-codecs

  bundle:
    module: Main
    outfile: public/bundle.js
    platform: browser
```

---

## Style Guide

### Color Palettes

Prefer Hylograph's built-in D3 color schemes over hand-crafted palettes:

```purescript
import Hylograph.Scale (schemeCategory10At, schemeTableau10At, sequential, interpolateViridis, applyScale)

-- Categorical: use schemeCategory10At or schemeTableau10At
fill = schemeCategory10At groupIndex  -- wraps modularly

-- Sequential: use sequential scale with an interpolator
colorScale = sequential interpolateViridis
  # Scale.domain [0.0, maxValue]
-- applyScale colorScale someValue  -- returns CSS color string

-- For custom restrained palettes (Swiss/International style)
categorical :: Int -> String
categorical = case _ of
  0 -> "#2d4057"  -- Dark slate
  1 -> "#c9a227"  -- Gold
  2 -> "#d4644a"  -- Terracotta
  3 -> "#4a8c6f"  -- Forest
  4 -> "#7b5ea7"  -- Plum
  _ -> "#8a8a8a"  -- Neutral grey
```

### Layout Conventions

- Generous whitespace: margins of 40-60px
- SVG viewBox for responsive sizing
- Sans-serif typography (system font stack)
- Strong visual hierarchy through size and weight, not decoration
- Light backgrounds for data areas

### Code Conventions

- Pure visualization functions in `Viz/` modules (no Effect except `rerender`)
- Domain types in `Types.purs` (pure, no framework dependency)
- Data loading in `Data/Loader.purs` (Aff)
- Halogen state management in `Component/` modules
- Prefer `forEach` for data-driven elements (enables GUP/animation)
- Use `thunkedStr`/`thunkedNum` for datum-derived attributes inside `forEach` templates
- Use **Friendly helpers** (`r`, `fill`, `viewBox`, `class_`, etc. from `Hylograph.HATS.Friendly`) for constant attributes -- cleaner than raw `staticStr`/`staticNum`
- Fall back to `staticStr`/`staticNum` for attribute names Friendly doesn't cover (e.g., `staticStr "cursor" "pointer"`)
- Always pass `[]` as the children argument to leaf elements (`elem Circle [...] []`)

---

## Troubleshooting

| Problem | Likely Cause | Fix |
|---------|-------------|-----|
| Nothing renders | Container div doesn't exist yet | Ensure `Initialize` runs after `render` creates the container |
| `rerender` has no effect | Wrong CSS selector | Check `#id` matches the `HP.id` in your Halogen HTML |
| Force simulation doesn't move | Missing forces or wrong alphaMin | Ensure forces are configured and `alphaMin < 1.0` |
| Compilation error with `Tree` composition | Datum type mismatch | HATS trees are unparameterized -- this shouldn't happen. Check you're using `Tree`, not a parameterized type |
| Stale visualization after data change | Not re-rendering | Call `rerender` again with the new tree after state update |
| Simulation events not reaching Halogen | Missing `toHalogenEmitter` | Bridge with `toHalogenEmitter` and `H.subscribe` |
| `forEach` elements not updating | Key function returns duplicates | Ensure key function returns unique strings per datum |
| Attributes not reflecting datum | Using `staticStr` inside template | Use `thunkedStr`/`thunkedNum` inside `forEach` templates to capture datum values |
| Scale returns wrong values | Domain/range not set | Scales need both `domain` and `range` configured |

## Notes on Existing Code Patterns

The existing `.claude/skills/hylograph-spa.md` skill in the selection library uses some older patterns:
- Uses FFI-based click handlers instead of HATS built-in `onClick`
- Uses `<#>` mapping instead of `forEach` for data-driven elements

The modern approach is to use HATS behaviors (`onClick`, `onMouseEnter`, etc.) and `forEach` for data binding, as shown throughout this document.
