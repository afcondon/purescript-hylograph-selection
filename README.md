# purescript-hylograph-selection

> **This is the core package of [Hylograph](https://hylograph.net)**, a PureScript library system for building interactive data visualizations: declarative tree construction, scales, layout algorithms, graph algorithms and force simulation. Rendering, scales and layouts are pure PureScript with no JavaScript visualization dependency. Force simulation is the one exception: its force calculations come from `d3-force`, run by the library's own simulation loop (see [hylograph-simulation](https://github.com/afcondon/purescript-hylograph-simulation)). See [hylograph.net](https://hylograph.net) for documentation, guides, and examples.

Declarative DOM tree construction and interpretation for PureScript. This package has no runtime dependency on D3 or any other JavaScript visualization library.

Live interactive demo can be seen as part of the [suite of Hylograph library demos](https://afcondon.github.io/purescript-hylograph-demos/). Click the screenshot below for **The Hylographic Fold** — a seven-chapter guide explaining how HATS works, built entirely with HATS.

[![The Hylographic Fold Demo](https://afcondon.github.io/purescript-hylograph-demos/thumbnails/HATS.jpg)](https://afcondon.github.io/purescript-hylograph-demos/selection/)

## Installation

```bash
spago install hylograph-selection
```

## Overview

Build visualizations as declarative trees using **HATS** (Hylomorphic Abstract Tree Syntax). A HATS tree describes *what* to build; interpreters decide *how*.

```purescript
import Prelude

import Data.Array (mapWithIndex)
import Data.Int (toNumber)
import Effect (Effect)
import Hylograph.HATS (Tree, ElementType(..), elem, forEach)
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (rerender)

type Bar = { label :: String, value :: Number }

barChart :: Array Bar -> Tree
barChart bars =
  elem SVG [ F.viewBox 0.0 0.0 400.0 300.0 ]
    [ forEach "bars" Rect (mapWithIndex (\i bar -> { i, bar }) bars) _.bar.label \d ->
        elem Rect
          [ F.x (toNumber d.i * 50.0)
          , F.y (300.0 - d.bar.value)
          , F.width 40.0
          , F.height d.bar.value
          , F.fill "#C9A962"
          ] []
    ]

main :: Effect Unit
main = void $ rerender "#chart" $ barChart
  [ { label: "a", value: 120.0 }
  , { label: "b", value: 200.0 }
  , { label: "c", value: 80.0 }
  ]
```

A `Tree` is an ordinary data structure, so an interpreter is any function that walks it. Two are in everyday use: the DOM interpreter (`rerender`, which is what draws the SVG above) and the English interpreter (`runEnglish`, a plain-text description of the structure). Both stop at the same place. The template passed to `forEach` is a PureScript function, so an interpreter can run it for each datum but cannot read it: English prints `Fold "bars" over 3 items from array` and then `(Template function defined for creating elements from data)`; it cannot say what a bar looks like.

## Key Features

- **Pure PureScript scales** — linear, log, pow, sqrt. No D3 dependency. The linear and pow scales pass tests translated from D3's own suite. The log scale maps values as D3's does, but its `ticks` and `nice` use the linear algorithm rather than D3's powers-of-the-base one. 37 color interpolators (Viridis, Blues, RdYlGn, etc.).
- **Coordinated highlighting** — hover an element and related elements across multiple views respond. Configurable identity/classify functions.
- **Interpreters** — DOM (SVG and HTML) and English are in use. MetaHATS (a tree that draws another tree's structure), Mermaid and SemiQuine (code generation) ship as experiments: nothing in the published demos calls the library's copies, Mermaid has no test, and SemiQuine's output is not yet valid PureScript.
- **Composable trees** — combine fragments with `siblings`, nest with `forEach`.

## Modules

### Core
- `Hylograph.HATS` — Tree types, `elem`, `forEach`, `withBehaviors`
- `Hylograph.HATS.Friendly` — Attribute helpers (`F.width`, `F.cx`, `F.fill`, etc.)
- `Hylograph.HATS.InterpreterTick` — DOM rendering (`rerender`)

### Scales (pure, no D3)
- `Hylograph.Scale` — Facade re-exporting Pure + ColorSchemes + Interpolation
- `Hylograph.Scale.Pure` — Linear, log, pow, sqrt scales with tick/nice (linear ticks for log; see above)
- `Hylograph.Scale.ColorSchemes` — Categorical palettes (Category10, Tableau10, etc.)
- `Hylograph.Scale.Sequential` — 37 sequential/diverging/cyclical color interpolators
- `Hylograph.Scale.Interpolation` — RGB and HSL color interpolation
- `Hylograph.Scale.FP` — Sampling, modifiers, quantize, threshold

### Interpreters
- `Hylograph.HATS.InterpreterTick` — the DOM interpreter (listed under Core)
- `Hylograph.Interpreter.English` — Plain English description
- `Hylograph.Interpreter.MetaHATS` — Tree structure visualization (experimental)
- `Hylograph.Interpreter.Mermaid` — Mermaid diagram generation (experimental, untested)
- `Hylograph.Interpreter.SemiQuine` — PureScript code generation (experimental; output does not compile)

### Interactions
- `Hylograph.Interaction.Coordinated` — Coordinated highlighting across views
- `Hylograph.Interaction.Zoom` — Zoom and pan
- `Hylograph.Interaction.Brush` — Brush selection
- `Hylograph.Interaction.Pointer` — Pointer events

## The Hylograph Library System

This package is the foundation. The full ecosystem:

| Package | Description |
|---------|-------------|
| **hylograph-selection** | Core: HATS tree construction, the DOM and English interpreters, scales, color schemes, coordinated highlighting, zoom, brush, pointer events. *This package.* |
| **hylograph-graph** | Graph data structures and algorithms — shortest path (Dijkstra), BFS/DFS, connected and biconnected components, topological sort. |
| **hylograph-layout** | Layout algorithms in pure PureScript — tree and cluster layouts, pack, partition, treemap, edge bundling, Sankey, chord, adjacency matrix. |
| **hylograph-simulation** | Force simulation — n-body, collision, link forces, centering. The force calculations are `d3-force`'s; the simulation loop is the library's own, which is what lets data enter and leave a running simulation. |
| **hylograph-simulation-halogen** | Halogen adapter for simulation events, for tick-driven rendering. |
| **hylograph-canvas** | Canvas 2D drawing of circles and lines, with zoom and pan. It does not interpret HATS trees. |
| **hylograph-optics** | Optics (lenses, traversals) over rose trees and graphs. |
| **hylograph-music** | Proof of concept: an audio interpreter that plays a HATS tree as notes. |

These packages are published to the [PureScript Registry](https://pursuit.purescript.org) and documented at [hylograph.net](https://hylograph.net).

## License

MIT
