# How to Apply Grammar of Graphics Principles to Hylograph Development

## What This Document Is

This is a practical guide for developers who know the Grammar of Graphics (from Wilkinson's *The Grammar of Graphics*, Wickham's ggplot2, or Observable Plot) and want to apply that thinking when building visualizations with Hylograph.

Hylograph isn't a Grammar of Graphics implementation -- it's a lower-level toolkit with D3-lineage. But the GoG decomposition is a powerful *design tool* for thinking about what you're building before you write code. This guide shows how to translate each grammatical concept into Hylograph constructs, and where Hylograph gives you capabilities that GoG doesn't address.

### A Note on Specificity

The examples throughout this guide use **SVG rendering** via the D3 interpreter, embedded in **Halogen** components. This is the most common and well-exercised path today. But a central design goal of Hylograph is that these choices should be *orthogonal*.

The HATS tree you build -- the `Tree` value with its elements, attributes, data bindings, and behaviors -- is an abstract specification. It doesn't know it's being rendered to SVG. The interpreter makes that decision. Today the primary interpreter is `InterpreterTick` (D3/SVG), but the architecture supports others: Canvas 2D, WebGL/Three.js, string-based interpreters for testing, even audio (the `hylograph-music` package exists for sonification).

Similarly, the framework embedding is a separate concern. Halogen is the most developed integration path, but HATS trees are plain PureScript values -- they can be rendered from raw `Effect` code, a React wrapper, Deku, or anything else that can call `rerender`.

We don't give every example as Halogen/React/Deku multiplied by SVG/Canvas/Music/3js because that would obscure the patterns. But when you see `elem Circle [thunkedNum "cx" x] []` in an example, understand that the *specification* is what matters. The rendering target and the framework embedding are decisions you make once, at the boundary, and they don't contaminate the visualization logic.

---

## The Grammar, Briefly

The Grammar of Graphics decomposes any visualization into independent, composable layers:

| Layer | Question It Answers | GoG Term |
|-------|-------------------|----------|
| **Data** | What am I visualizing? | `data` |
| **Aesthetics** | How does data map to visual properties? | `aes(x = income, y = happiness, color = region)` |
| **Geometry** | What shapes represent the data? | `geom_point()`, `geom_bar()`, `geom_line()` |
| **Statistics** | What transformations summarize or reshape the data? | `stat_smooth()`, `stat_bin()` |
| **Scales** | How do data values become pixel positions and colors? | `scale_x_continuous()`, `scale_color_brewer()` |
| **Coordinates** | What spatial system frames the plot? | `coord_cartesian()`, `coord_polar()` |
| **Facets** | How is the data split across sub-plots? | `facet_wrap()`, `facet_grid()` |
| **Guides** | How does the reader decode the mapping? | Axes, legends, annotations |
| **Theme** | What are the non-data visual properties? | Fonts, backgrounds, grid lines |

The power of this decomposition is that each layer is an independent decision. You can change the geometry from points to bars without touching the aesthetic mapping. You can swap a linear scale for a log scale without changing anything else.

---

## Layer-by-Layer Translation

### 1. Data

**GoG**: A data frame / table that flows through the pipeline.

**Hylograph**: Any structure that can be deconstructed. The "hylo" in "Hylomorphic Abstract Trees" refers to *hylomorphisms* -- the composition of an unfold (taking a structure apart) followed by a fold (building new structure from the pieces). The aspiration is that any data shape that can be deconstructed can produce a visualization.

In practice, HATS provides several enumeration strategies that reflect this:

```purescript
-- Flat data (the most common case, analogous to GoG's data frame)
forEach :: forall a. String -> ElementType -> Array a -> (a -> String) -> (a -> Tree) -> Tree

-- Tree-shaped data (root + children function -- no flattening required)
fromTree :: forall a. String -> ElementType -> a -> (a -> Array a)
  -> (a -> String) -> TraversalOrder -> Boolean -> (a -> Tree) -> Tree

-- Tree-shaped data, preserving parent-child nesting in output
preserveTree :: forall a. String -> ElementType -> a -> (a -> Array a)
  -> (a -> String) -> (a -> Tree) -> Tree

-- Projected data (Maps, etc. via the Project type class)
forEachP :: forall source target. Project source target =>
  String -> ElementType -> source -> (target -> String) -> (target -> Tree) -> Tree
```

These correspond to the internal `Enumeration` type:

```purescript
data Enumeration a
  = FromArray (Array a)                     -- Flat collection
  | FromTree { root :: a                    -- Tree-shaped: root value
             , children :: a -> Array a     -- + deconstruction function
             , order :: TraversalOrder      -- depth-first or breadth-first
             , includeInternal :: Boolean   -- internal nodes or just leaves?
             }
  | WithContext (Array { datum :: a, depth :: Int, index :: Int })  -- Pre-annotated
```

So while `Array` is the most common input shape, you can also feed in:
- **Trees** via a root + children accessor (no need to flatten first)
- **Maps** via `forEachP` with `MapKeys`, `MapValues`, or `MapEntries` projections
- **Any `Foldable`** structure in principle -- the fold machinery handles the deconstruction

```purescript
-- Your domain types (pure, no framework dependency)
type DataPoint =
  { country :: String
  , income :: Number
  , lifeExpectancy :: Number
  , population :: Number
  , continent :: String
  }

-- Tree-shaped data works directly, no flattening
type FileNode = { name :: String, size :: Number, children :: Array FileNode }

renderFileTree :: FileNode -> Tree
renderFileTree root =
  fromTree "files" Group root _.children _.name DepthFirst true \node ->
    elem Group [ ... ] [ ... ]

-- Map-shaped data works via projection
renderDependencies :: Map String (Array String) -> Tree
renderDependencies deps =
  forEachP "modules" Group (MapEntries deps) (\(Tuple k _) -> k) \(Tuple name imports) ->
    elem Group [ ... ] [ ... ]
```

**Key difference from GoG**: In GoG, data is always a rectangular table (data frame). In Hylograph, data can be any shape you can deconstruct -- trees, graphs, maps, flat arrays. The hylomorphic fold handles the structural traversal; you provide the deconstruction function and the per-element template.

**Practical rule**: Define your data types in a separate `Types.purs` module. Keep them pure (no `Effect`, no framework types). Choose the right enumeration strategy for your data's natural shape -- don't force everything through an array if it's naturally a tree or a map.

---

### 2. Aesthetics (Mapping Data to Visual Channels)

**GoG**: `aes(x = income, y = lifeExpectancy, size = population, color = continent)` -- a declarative mapping from data fields to visual properties.

**Hylograph**: Aesthetic mapping happens inside `forEach` closures, where you extract fields from the datum and pass them through scales.

```purescript
-- GoG: aes(x = income, y = lifeExpectancy, size = population, color = continent)
-- Hylograph equivalent:

forEach "countries" Circle points (\pt -> pt.country) \pt ->
  elem Circle
    [ thunkedNum "cx" (applyScale xScale pt.income)          -- x = income
    , thunkedNum "cy" (applyScale yScale pt.lifeExpectancy)   -- y = lifeExpectancy
    , thunkedNum "r"  (applyScale sizeScale pt.population)    -- size = population
    , thunkedStr "fill" (applyScale colorScale pt.continent)  -- color = continent
    ]
    []
```

**The pattern**: Each aesthetic mapping becomes one attribute line: `thunkedNum "attr" (applyScale someScale pt.field)`.

**GoG has implicit mapping; Hylograph has explicit mapping.** This means:
- You see every mapping. Nothing is hidden.
- You control the attribute name exactly (`"cx"` vs `"x"`, `"fill"` vs `"stroke"`).
- You can apply arbitrary computation, not just field extraction: `thunkedNum "r" (if pt.population > 1.0e9 then 12.0 else 5.0)`.

**Design practice**: Before writing code, list your aesthetic mappings as a table:

| Visual Channel | Data Field | Scale | SVG Attribute |
|---------------|-----------|-------|---------------|
| Horizontal position | `income` | linear | `cx` |
| Vertical position | `lifeExpectancy` | linear | `cy` |
| Size | `population` | sqrt | `r` |
| Color | `continent` | ordinal/categorical | `fill` |

Then each row becomes one `thunkedNum`/`thunkedStr` line inside your `forEach`.

**Attribute style guide**: Hylograph provides three levels of attribute constructor:

- **Friendly helpers** (`cx`, `r`, `fill`, `viewBox`, `class_`, `transform`, etc. from `Hylograph.HATS.Friendly`) for constant values -- cleanest and most readable
- **`thunkedStr` / `thunkedNum`** for datum-derived values inside `forEach` closures -- captures the value in a thunk
- **`staticStr` / `staticNum`** as the raw fallback, for attribute names that Friendly doesn't cover (e.g., `staticStr "cursor" "pointer"`)

```purescript
import Hylograph.HATS (elem, forEach, thunkedNum, thunkedStr, staticStr)
import Hylograph.HATS.Friendly (r, fill, stroke, class_, viewBox, transform)

-- Inside a forEach template:
forEach "points" Circle data_ _.id \pt ->
  elem Circle
    [ thunkedNum "cx" (applyScale xScale pt.x)  -- datum-derived: thunked
    , thunkedNum "cy" (applyScale yScale pt.y)  -- datum-derived: thunked
    , r 5.0                                      -- constant: Friendly
    , thunkedStr "fill" (colorFor pt.category)   -- datum-derived: thunked
    , stroke "#333"                              -- constant: Friendly
    , staticStr "cursor" "pointer"               -- constant, no Friendly helper: raw
    ]
    []
```

---

### 3. Geometry

**GoG**: `geom_point()`, `geom_bar()`, `geom_line()`, `geom_area()`, etc.

**Hylograph**: `ElementType` constructors + the `elem` function.

| GoG Geometry | Hylograph Element | Notes |
|-------------|------------------|-------|
| `geom_point` | `Circle` | Position with `cx`, `cy`, size with `r` |
| `geom_bar` | `Rect` | Position with `x`, `y`, size with `width`, `height` |
| `geom_line` | `Path` | Build SVG path string from data |
| `geom_segment` | `Line` | Two endpoints: `x1`, `y1`, `x2`, `y2` |
| `geom_area` | `Path` | Closed SVG path with area fill |
| `geom_text` | `Text` | Position + `textContent` attribute |
| `geom_polygon` | `Polygon` | `points` attribute with coordinate string |
| `geom_rect` | `Rect` | Same as bar but possibly different semantics (heatmap cells, treemap tiles) |
| `geom_tile` | `Rect` | For grid/matrix layouts |

**GoG geometry is declarative and abstract; Hylograph geometry is concrete SVG.** This means you have more control but also more responsibility. GoG's `geom_bar()` knows to stack bars and compute widths from a band scale. In Hylograph, you compute the bar positions yourself:

```purescript
-- Bar chart: you compute everything the geom would do for you in GoG
forEach "bars" Rect sortedData (\d -> d.label) \d ->
  elem Rect
    [ thunkedNum "x" (applyScale xBand d.label)
    , thunkedNum "y" (applyScale yScale d.value)
    , staticNum "width" (bandwidth xBand)
    , thunkedNum "height" (innerHeight - applyScale yScale d.value)
    , thunkedStr "fill" (applyScale colorScale d.category)
    ]
    []
```

**Design practice**: Choose your geometry first, then work out which SVG attributes it needs. Group elements if they need to move together (e.g., a bar + its label = a `Group` containing a `Rect` + `Text`).

---

### 4. Statistics (Data Transformations)

**GoG**: `stat_smooth()`, `stat_bin()`, `stat_density()`, `stat_summary()` -- transformations applied to data before rendering.

**Hylograph**: Pure PureScript functions. Statistics are just data transformations you apply before building the tree.

| GoG Stat | Hylograph Equivalent |
|---------|---------------------|
| `stat_identity` | Direct array, no transformation |
| `stat_bin` / `stat_histogram` | Write a `bin :: Array Number -> BinConfig -> Array Bin` function |
| `stat_smooth` | Regression/smoothing as a pure function |
| `stat_summary` | `foldl`, `groupBy`, aggregation functions |
| Layout algorithms | `treemap`, `pack`, `sankey`, `chord`, `tree`, `partition`, `edgeBundle` |
| Force simulation | `runSimulation` (continuous statistical transform) |

This is where Hylograph's layout libraries shine. GoG doesn't really handle:
- **Hierarchical layouts**: `treemap config hierarchy` is a stat that transforms a tree into rectangles with positions.
- **Network layouts**: `runSimulation` is an iterative stat that converges node positions.
- **Flow layouts**: `computeLayout flows width height` transforms flow data into positioned nodes and links.

**The GoG insight still applies**: Separate your statistical transformation from your visual encoding. Don't compute layout positions inside your rendering function.

```purescript
-- GOOD: separate stat from rendering
renderTreemap :: String -> RawHierarchy -> Effect Unit
renderTreemap selector rawData = do
  -- STAT: transform data (pure)
  let hierarchy = buildValuedNode rawData
      laid = treemap config hierarchy           -- stat_treemap
      cells = flattenLeaves laid                -- stat_flatten

  -- RENDER: visual encoding (effectful)
  _ <- rerender selector (treemapTree cells)
  pure unit

-- BAD: interleaving stat and rendering
renderTreemap selector rawData = do
  _ <- rerender selector (elem SVG [...]
    [ forEach "cells" Rect (flattenLeaves (treemap config (buildValuedNode rawData))) ...
    ])
  -- Hard to read, impossible to test the stat independently
```

**Design practice**: Write your stats as pure functions `inputData -> transformedData`. Test them independently. Then pipe the result into your tree builder.

---

### 5. Scales

**GoG**: `scale_x_continuous()`, `scale_y_log()`, `scale_color_brewer()`, `scale_size_area()`.

**Hylograph**: `Hylograph.Scale` -- a direct mapping.

| GoG Scale | Hylograph |
|----------|-----------|
| `scale_x_continuous()` | `Scale.linear # domain [...] # range [0.0, innerWidth]` |
| `scale_y_continuous()` | `Scale.linear # domain [...] # range [innerHeight, 0.0]` (inverted!) |
| `scale_x_log()` | `Scale.log # domain [...] # range [...]` |
| `scale_color_manual(values)` | Pure function: `colorFor :: String -> String` |
| `scale_color_brewer()` | `schemeCategory10At`, `schemeTableau10At` |
| `scale_color_viridis()` | `sequential interpolateViridis # domain [min, max]` |
| `scale_color_diverging()` | `diverging interpolateRdBu # domain [min, mid, max]` |
| `scale_size_area()` | `Scale.sqrt # domain [0.0, maxPop] # range [2.0, 30.0]` |
| `scale_x_band()` | `Scale.band # domain labels # range [0.0, innerWidth]` |

**The critical SVG-ism**: SVG's y-axis points downward. In GoG, `scale_y_continuous()` handles this implicitly. In Hylograph, you must invert the range: `range [innerHeight, 0.0]`.

```purescript
-- Scale construction follows GoG layering: independent of geometry
let xScale = Scale.linear
      # Scale.domain [xMin, xMax]
      # Scale.range [0.0, innerWidth]
      # nice

    yScale = Scale.linear
      # Scale.domain [yMin, yMax]
      # Scale.range [innerHeight, 0.0]  -- INVERTED for SVG
      # nice

    sizeScale = Scale.sqrt
      # Scale.domain [0.0, maxPopulation]
      # Scale.range [2.0, 25.0]

    colorScale = Scale.ordinal
      # Scale.domain continents
      # Scale.range (Array.mapWithIndex (\i _ -> schemeTableau10At i) continents)
```

**GoG's `scale_*_identity()`**: When a data field is already in visual units (e.g., pre-computed pixel coordinates from a layout algorithm), you don't need a scale -- just use `thunkedNum` directly.

**Design practice**: Define all your scales in one `let` block before building the tree. This makes them easy to find and modify, mirroring how GoG treats scales as a separate layer.

---

### 6. Coordinates

**GoG**: `coord_cartesian()`, `coord_polar()`, `coord_flip()`.

**Hylograph**: SVG coordinate transforms via `viewBox` and `transform` attributes.

| GoG Coord | Hylograph Approach |
|----------|-------------------|
| `coord_cartesian()` | Default. Set up `viewBox` and margin conventions |
| `coord_flip()` | Swap which scale maps to x vs y attributes |
| `coord_polar()` | Compute angles/radii manually, use `Path` with arc commands |
| `coord_fixed(ratio=1)` | Set `viewBox` aspect ratio + `preserveAspectRatio` |

The margin convention is your coordinate system setup:

```purescript
-- This IS your coord_cartesian() with margins
let margin = { top: 40.0, right: 40.0, bottom: 60.0, left: 60.0 }
    innerWidth = width - margin.left - margin.right
    innerHeight = height - margin.top - margin.bottom

elem SVG
  [ viewBox 0.0 0.0 width height
  , preserveAspectRatio "xMidYMid meet"
  ]
  [ elem Group
      [ transform ("translate(" <> show margin.left <> "," <> show margin.top <> ")") ]
      [ plotArea ]  -- Everything inside is in the margin-adjusted coordinate space
  ]
```

**For polar coordinates** (pie charts, radar charts, chord diagrams): GoG provides `coord_polar()` to transform Cartesian geoms. In Hylograph, you compute polar positions explicitly:

```purescript
-- Polar coordinate transform (what coord_polar() does internally)
toCartesian :: { angle :: Number, radius :: Number } -> { x :: Number, y :: Number }
toCartesian { angle, radius } =
  { x: radius * cos angle
  , y: radius * sin angle
  }
```

**Design practice**: Set up your coordinate system (margins, viewBox, inner dimensions) at the top of your tree builder. Everything downstream works in the inner coordinate space.

---

### 7. Facets (Small Multiples)

**GoG**: `facet_wrap(~continent)`, `facet_grid(continent ~ year)`.

**Hylograph**: Use `forEach` on the grouping variable, with each group getting its own sub-visualization.

```purescript
-- facet_wrap(~continent): one subplot per continent
facetedScatter :: Map String (Array DataPoint) -> Tree
facetedScatter grouped =
  let groups = Map.toUnfoldable grouped :: Array (Tuple String (Array DataPoint))
      cols = 3
  in
  elem SVG
    [ viewBox 0.0 0.0 totalWidth totalHeight ]
    [ forEach "facets" Group groups (\(Tuple name _) -> name) \(Tuple name points) ->
        let col = facetIndex `mod` cols
            row = facetIndex / cols
            tx = toNumber col * (facetWidth + gapX)
            ty = toNumber row * (facetHeight + gapY)
        in
        elem Group
          [ thunkedStr "transform" ("translate(" <> show tx <> "," <> show ty <> ")") ]
          [ -- Facet title
            elem Text
              [ x (facetWidth / 2.0)
              , y (-8.0)
              , textAnchor "middle"
              , fontSize "12"
              , fontWeight "600"
              , thunkedStr "textContent" name
              ]
              []
          , -- Facet content: a scatterplot of this group's data
            scatterPanel facetWidth facetHeight points
          ]
    ]

-- Each facet renders the same geometry with the same aesthetic mapping
-- but different data -- exactly the GoG faceting principle
scatterPanel :: Number -> Number -> Array DataPoint -> Tree
scatterPanel w h points =
  let xScale = Scale.linear # Scale.domain [...] # Scale.range [0.0, w] # nice
      yScale = Scale.linear # Scale.domain [...] # Scale.range [h, 0.0] # nice
  in
  forEach "pts" Circle points _.label \pt ->
    elem Circle
      [ thunkedNum "cx" (applyScale xScale pt.x)
      , thunkedNum "cy" (applyScale yScale pt.y)
      , r 3.0
      , thunkedStr "fill" (applyScale colorScale pt.category)
      ]
      []
```

**Key GoG principle preserved**: Each facet uses the same aesthetic mapping and geometry. Only the data subset changes. In Hylograph, you enforce this by factoring the common rendering into a shared function (`scatterPanel`).

**SPLOM (Scatterplot Matrix)** is a special case: facet by pairs of variables.

```purescript
-- facet_grid equivalent: forEach on rows × columns
splom :: Array String -> Array Record -> Tree
splom variables data_ =
  forEach "rows" Group variables identity \yVar ->
    forEach "cols" Group variables identity \xVar ->
      scatterPanel (fieldAccessor xVar) (fieldAccessor yVar) data_
```

**For `forEachP`**: When your grouping key is a Map, use `forEachP` with `MapKeys`, `MapValues`, or `MapEntries` projections to iterate over it naturally.

---

### 8. Guides (Axes, Legends, Annotations)

**GoG**: Automatically generated from scales. `scale_x_continuous(name = "Income ($)")` produces an axis.

**Hylograph**: You build axes and legends explicitly as Tree nodes. This is more work but gives you pixel-level control.

```purescript
-- X axis (what GoG generates automatically from scale_x_continuous)
xAxis :: Number -> Number -> ContinuousScale -> Tree
xAxis innerWidth innerHeight xScale =
  let tickValues = ticks 8 xScale
      format = tickFormat 8 "" xScale
  in
  elem Group
    [ transform ("translate(0," <> show innerHeight <> ")") ]
    ( -- Axis line
      [ elem Line
          [ x1 0.0, y1 0.0
          , thunkedNum "x2" innerWidth, y2 0.0
          , stroke "#333"
          ]
          []
      ]
      -- Tick marks + labels
      <> (tickValues <#> \v ->
        let px = applyScale xScale v
        in elem Group
          [ thunkedStr "transform" ("translate(" <> show px <> ",0)") ]
          [ elem Line
              [ x1 0.0, y1 0.0
              , x2 0.0, y2 6.0
              , stroke "#333"
              ]
              []
          , elem Text
              [ x 0.0, y 20.0
              , textAnchor "middle"
              , fontSize "11"
              , thunkedStr "textContent" (format v)
              ]
              []
          ]
      )
    )

-- Categorical legend
legend :: Array { label :: String, color :: String } -> Tree
legend items =
  forEach "legend-items" Group items _.label \item ->
    elem Group []
      [ elem Rect
          [ width 12.0, height 12.0
          , thunkedStr "fill" item.color
          ]
          []
      , elem Text
          [ x 18.0, attrNum "dy" 10.0
          , fontSize "11"
          , thunkedStr "textContent" item.label
          ]
          []
      ]
```

**Design practice**: Build axis and legend functions once, reuse across visualizations. These are your guide layer -- keep them separate from the data geometry. Hylograph's `ticks` and `tickFormat` functions give you the same tick computation that GoG axes use internally.

---

### 9. Theme

**GoG**: `theme_minimal()`, `theme(panel.background = ...)`.

**Hylograph**: CSS classes + style attributes. Theme is the non-data visual layer.

```purescript
-- Theme as CSS (in your HTML or a stylesheet)
-- This is equivalent to theme_minimal() + customization
{-
.viz-container { font-family: system-ui, -apple-system, sans-serif; }
.axis line, .axis path { stroke: #e0e0e0; }
.axis text { fill: #666; font-size: 11px; }
.grid line { stroke: #f0f0f0; stroke-dasharray: 2 4; }
.data-point { transition: opacity 150ms; }
.data-point:hover { opacity: 1 !important; }
.highlight-dimmed { opacity: 0.15; }
.highlight-primary { opacity: 1; stroke-width: 2px; }
-}

-- Apply theme via class attributes
elem Group [ class_ "axis x-axis" ] [ xAxisContent ]
elem Group [ class_ "axis y-axis" ] [ yAxisContent ]
elem Group [ class_ "grid" ] [ gridLines ]
```

**Design practice**: Keep all visual theming in CSS. Use `class_` to tag elements. This cleanly separates the data-encoding layer (Hylograph HATS) from the presentation layer (CSS), just as GoG separates theme from geometry.

---

## Beyond the Grammar: What Hylograph Adds

The Grammar of Graphics was designed for static publication charts. Hylograph goes further in several areas where GoG has no vocabulary:

### Interaction Grammar

GoG has no concept of interaction. Hylograph adds a composable interaction layer:

| Interaction Concept | Hylograph Construct |
|-------------------|-------------------|
| Selection (hover) | `onCoordinatedHighlight` with `classify` function |
| Selection (brush) | `onBrush` + `onCoordinatedInteraction` with `respond` function |
| Navigation (drill-down) | `onClick` / `onClickWithModifier` |
| Direct manipulation | `onDrag`, `onZoom` |
| Linked views | Coordination `group` parameter |

The `classify` and `respond` functions are Hylograph's way of making interaction declarative. Instead of imperative event handlers that manually update styles, you declare a pure function from "what was interacted with" to "what visual state should this element be in":

```purescript
-- Declarative interaction mapping (analogous to aesthetic mapping)
classify: \hoveredId ->
  if myId == hoveredId then Primary          -- I am the focus
  else if hoveredId `elem` myConnections then Related  -- I'm connected
  else Dimmed                                 -- I'm background
```

This is arguably the GoG of interaction: a declarative mapping from interaction events to visual states, independent of the specific visual encoding.

### Animation Grammar

GoG doesn't address animation. Hylograph's GUP (General Update Pattern) with `forEachWithGUP` provides enter/update/exit transitions:

| Transition Phase | What It Animates | GoG Analog |
|-----------------|-----------------|------------|
| **Enter** | New data appearing | No analog (data is static in GoG) |
| **Update** | Existing data changing | No analog |
| **Exit** | Data being removed | No analog |

```purescript
-- Transition specification is separate from geometry -- another independent layer
forEachWithGUP "points" Circle data_ _.id template
  { enter: Just { attrs: [r 0.0], transition: fadeIn }
  , update: Just { attrs: [], transition: smooth }
  , exit: Just { attrs: [r 0.0], transition: fadeOut }
  }
```

### Continuous Statistical Transforms

GoG stats are applied once to produce static output. Hylograph's force simulation is a *continuous* stat -- it produces a stream of positions over time:

```purescript
-- The simulation is a continuously-applied stat
-- Each tick produces new x, y positions for all nodes
subscribe events \event -> case event of
  Tick { alpha } -> do
    nodes <- handle.getNodes  -- Updated positions
    _ <- rerender selector (nodesTree nodes)  -- Re-encode visually
    pure unit
```

### Tree Composition

GoG layers combine via `+` (ggplot2) or the layering spec. Hylograph trees compose via `<>` (Semigroup). The key difference: HATS trees are unparameterized, so you can freely combine trees that internally bind different data types:

```purescript
-- These trees bind different data internally, but compose freely
visualization = axisLayer <> gridLayer <> dataLayer <> annotationLayer <> legendLayer
```

This is more flexible than GoG's layer system, where all layers must share the same data frame (or explicitly override it).

---

## Putting It All Together: The GoG Design Process for Hylograph

When approaching a new visualization, decompose it using GoG layers, then translate each to Hylograph:

### Step 1: Data
Define your PureScript types. What shape is your data? Flat records? A tree with children? A graph? A map?
```purescript
type MyDatum = { ... }                               -- flat (use forEach)
type MyTree = { ..., children :: Array MyTree }       -- recursive (use fromTree)
type MyLookup = Map String (Array Connection)         -- keyed (use forEachP)
```

### Step 2: Stats
What transformations do you need? Layout algorithms? Binning? Aggregation? Write these as pure functions.
```purescript
transformed :: Array LayoutResult
transformed = applyLayout config rawData
```

### Step 3: Aesthetics
List your mappings: data field -> visual channel -> scale -> SVG attribute.

### Step 4: Scales
Build your scales in one `let` block.
```purescript
let xScale = Scale.linear # domain [...] # range [...]
    yScale = ...
    colorScale = ...
```

### Step 5: Geometry
Choose your `ElementType` and build the `forEach` with attribute mappings.
```purescript
forEach "items" Circle transformed _.id \item ->
  elem Circle [ thunkedNum "cx" (applyScale xScale item.x), ... ] []
```

### Step 6: Coordinates
Set up margins, viewBox, and the transform group.

### Step 7: Guides
Build axes from your scales using `ticks` and `tickFormat`.

### Step 8: Theme
Apply CSS classes and styles.

### Step 9: Interactions (Hylograph extension)
Add `withBehaviors` to your geometry for hover, click, brush, and coordination.

### Step 10: Transitions (Hylograph extension)
If data changes over time, use `forEachWithGUP` with transition specs.

### Step 11: Compose
Combine all layers with `<>`:
```purescript
buildTree data_ =
  elem SVG [ viewBox ... ]
    [ elem Group [ transform margin ]
        [ gridLines xScale yScale        -- theme/guide
        , xAxis innerWidth innerHeight xScale    -- guide
        , yAxis innerWidth innerHeight yScale    -- guide
        , dataPoints xScale yScale colorScale data_  -- geometry + aesthetics
        , legend categories                -- guide
        ]
    ]
```

---

## Common Pitfalls When Thinking in GoG Terms

### "Where's my geom_bar?"
There isn't one. You build bars from `Rect` elements, computing positions from a `band` scale and heights from a `linear` scale. This is more work, but you understand exactly what's happening. Extract it into a `barChart` function once and reuse it.

### "Where's the automatic axis?"
You build it. But `Scale.ticks` and `Scale.tickFormat` do the hard computation. The rest is positioning `Line` and `Text` elements. Again, build it once, reuse everywhere.

### "Scales feel inside-out"
In GoG, scales are configured at the layer level: `scale_color_brewer()`. In Hylograph, you create a scale, configure it, then `applyScale` at the point of use. The scale is a value you pass around, not a side-channel configuration. This is more functional but requires you to explicitly thread the scale to where it's needed.

### "I want coord_polar but there's no coordinate transform"
Hylograph gives you SVG, which is Cartesian. For polar coordinates, compute `{ x: r * cos(theta), y: r * sin(theta) }` yourself. The chord and radial force layouts already do this for you -- learn from their output types.

### "facet_wrap was one line, this is twenty"
True. But Hylograph's facets are just `forEach` over groups, so you get full control over layout, spacing, shared vs independent axes, and per-facet interactions. The twenty lines give you capabilities GoG facets don't have.

### "I'm used to + for layers, not building SVG trees"
Think of `elem Group [...] [child1, child2, child3]` as three layers stacked via SVG rendering order. First child = back layer, last child = front layer. The `<>` operator on `Tree` is for combining sibling trees, analogous to GoG's `+`.

---

## Quick Reference: GoG → Hylograph Cheat Sheet

| GoG | Hylograph |
|-----|-----------|
| `ggplot(data, aes(...))` | `forEach` (flat), `fromTree` (tree-shaped), `forEachP` (maps/projections) + `elem ... [thunkedNum "attr" (applyScale scale datum.field)] []` |
| `data` (always a data frame) | Any deconstructable structure: arrays, trees (root + children fn), maps (via `Project`), any `Foldable` |
| `geom_point()` | `Circle` with `cx`, `cy`, `r` |
| `geom_bar()` | `Rect` with `x` (from band scale), `y`, `width` (from `bandwidth`), `height` |
| `geom_line()` | `Path` with computed `d` attribute |
| `geom_text()` | `Text` with `textContent`, `x`, `y` |
| `stat_identity()` | Direct data array |
| `stat_bin()` | Pure binning function |
| Layout stats | `treemap`, `pack`, `sankey`, `chord`, `tree`, `partition` |
| `scale_x_continuous()` | `Scale.linear # domain [...] # range [...]` |
| `scale_color_brewer()` | `schemeCategory10At` / `schemeTableau10At` |
| `scale_color_viridis()` | `sequential interpolateViridis` |
| `coord_cartesian()` | Margin convention + `viewBox` |
| `coord_polar()` | Manual polar → cartesian transform |
| `coord_flip()` | Swap x/y scale assignments |
| `facet_wrap(~var)` | `forEach` over grouped data |
| Axis | `ticks` + `tickFormat` + `Line`/`Text` elements |
| Legend | `forEach` over categories + `Rect`/`Text` |
| `theme_minimal()` | CSS classes |
| `+` (layer) | `<>` (Tree Semigroup) or SVG child ordering |
| — | `withBehaviors [onCoordinatedHighlight ...]` (interaction) |
| — | `forEachWithGUP` (animation) |
| — | `runSimulation` (continuous stat) |
