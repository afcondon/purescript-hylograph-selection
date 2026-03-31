# purescript-hylograph-selection

> **This is the core package of [Hylograph](https://hylograph.net)**, a PureScript library system for building interactive data visualizations. Hylograph provides declarative tree construction, force simulation, graph data structures, layout algorithms, and more — all in pure PureScript with zero JavaScript visualization dependencies. See [hylograph.net](https://hylograph.net) for documentation, guides, and examples.

Declarative DOM tree construction and interpretation for PureScript. Zero runtime dependencies on D3 or any other JavaScript visualization library.

**[Interactive Demo: The Hylographic Fold](https://afcondon.github.io/purescript-hylograph-selection/)** — a seven-chapter guide explaining how HATS works, built entirely with HATS.

## Installation

```bash
spago install hylograph-selection
```

## Overview

Build visualizations as declarative trees using **HATS** (Hylomorphic Abstract Tree Syntax). A HATS tree describes *what* to build; interpreters decide *how*.

```purescript
import Hylograph.HATS (elem, forEach, staticStr)
import Hylograph.HATS.Friendly as F
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

barChart :: Array { label :: String, value :: Number } -> Tree
barChart data =
  elem SVG [ F.viewBox 0.0 0.0 400.0 300.0 ]
    [ forEach "bars" Group data _.label \d ->
        elem Rect
          [ F.x (d.idx * 50.0)
          , F.y (300.0 - d.value)
          , F.width 40.0
          , F.height d.value
          , F.fill "#C9A962"
          ] []
    ]
```

The same tree can be interpreted as SVG (rendered to the DOM), English (plain text description), Mermaid (diagram syntax), or MetaHATS (a visualization of the tree's own structure).

## Key Features

- **Pure PureScript scales** — linear, log, pow, sqrt with D3-compatible tick/nice algorithms. 37 color interpolators (Viridis, Blues, RdYlGn, etc.). No D3 dependency.
- **Coordinated highlighting** — hover an element and related elements across multiple views respond. Configurable identity/classify functions.
- **Multiple interpreters** — SVG, English, Mermaid, MetaHATS, SemiQuine (code generation).
- **Composable trees** — combine fragments with `siblings`, nest with `forEach`.

## Modules

### Core
- `Hylograph.HATS` — Tree types, `elem`, `forEach`, `withBehaviors`
- `Hylograph.HATS.Friendly` — Attribute helpers (`F.width`, `F.cx`, `F.fill`, etc.)
- `Hylograph.HATS.InterpreterTick` — DOM rendering (`rerender`)

### Scales (pure, no D3)
- `Hylograph.Scale` — Facade re-exporting Pure + ColorSchemes + Interpolation
- `Hylograph.Scale.Pure` — Linear, log, pow, sqrt scales with tick/nice
- `Hylograph.Scale.ColorSchemes` — Categorical palettes (Category10, Tableau10, etc.)
- `Hylograph.Scale.Sequential` — 37 sequential/diverging/cyclical color interpolators
- `Hylograph.Scale.Interpolation` — RGB and HSL color interpolation
- `Hylograph.Scale.FP` — Sampling, modifiers, quantize, threshold

### Interpreters
- `Hylograph.Interpreter.English` — Plain English description
- `Hylograph.Interpreter.Mermaid` — Mermaid diagram generation
- `Hylograph.Interpreter.SemiQuine` — PureScript code generation
- `Hylograph.Interpreter.MetaHATS` — Tree structure visualization

### Interactions
- `Hylograph.Interaction.Coordinated` — Coordinated highlighting across views
- `Hylograph.Interaction.Zoom` — Zoom and pan
- `Hylograph.Interaction.Brush` — Brush selection
- `Hylograph.Interaction.Pointer` — Pointer events

## The Hylograph Library System

This package is the foundation. The full ecosystem:

| Package | Description |
|---------|-------------|
| **hylograph-selection** | Core: HATS tree construction, interpreters (SVG, English, Mermaid, MetaHATS), scales, color schemes, coordinated highlighting, zoom, brush, pointer events. *This package.* |
| **hylograph-graph** | Graph data structures — adjacency lists, traversals, connected components, topological sort. |
| **hylograph-layout** | Layout algorithms — tree layouts, edge bundling, Sankey diagrams, treemaps, beeswarm, force-directed placement. |
| **hylograph-simulation** | Force simulation — n-body, collision, link forces, centering. Halogen integration for animated simulations. |
| **hylograph-simulation-halogen** | Halogen component wrapper for force simulations with tick-driven rendering. |
| **hylograph-canvas** | Canvas rendering backend for high-performance visualizations. |
| **hylograph-optics** | Optics (lenses, prisms) for traversing and transforming HATS trees. |
| **hylograph-music** | Proof of concept: interpret data to sound just as readily as data to visuals. Same fold, different output. |

All packages are published to the [PureScript Registry](https://pursuit.purescript.org) and documented at [hylograph.net](https://hylograph.net).

## License

MIT
