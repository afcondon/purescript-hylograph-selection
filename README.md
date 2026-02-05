# purescript-hylograph-selection

[![Tree Builder Demo](../../site/lib-selection/public/demo.jpeg)](/#/tree-builder)

Type-safe D3 selection and attribute library for PureScript.

## Overview

A declarative, type-safe approach to D3.js visualization in PureScript. Instead of imperative D3 method chaining, you build a HATS tree that describes your visualization, then render it.

## Installation

```bash
spago install hylograph-selection
```

## Key Concepts

### HATS (Hylomorphic Abstract Tree Syntax)

Build visualizations as data structures using HATS:

```purescript
import Hylograph.HATS as H
import Hylograph.HATS.Friendly as HF
import Hylograph.Expr.Friendly (width, height, cx, cy, r, fill, num, text)
import Hylograph.HATS.InterpreterTick (rerender)

myViz :: H.Tree
myViz =
  HF.svg [ width $ num 400.0, height $ num 300.0 ]
    [ HF.circle
        [ cx $ num 200.0
        , cy $ num 150.0
        , r $ num 50.0
        , fill $ text "steelblue"
        ]
    ]

main :: Effect Unit
main = void $ rerender "#chart" myViz
```

### Data-Driven Rendering

Generate elements from data with `Array.map`:

```purescript
barChart :: Array Number -> H.Tree
barChart dataset =
  HF.svg [ width $ num 400.0, height $ num 300.0 ]
    (dataset # Array.mapWithIndex \i val ->
      HF.rect
        [ x $ num (toNumber i * 50.0)
        , y $ num (300.0 - val)
        , width $ num 40.0
        , height $ num val
        , fill $ text "steelblue"
        ])
```

### Interpreters

- **D3 Interpreter** (`Hylograph.HATS.InterpreterTick`) - Renders to DOM
- **English Interpreter** - Describes the tree in plain English
- **Mermaid Interpreter** - Generates Mermaid diagrams
- **SemiQuine Interpreter** - Generates PureScript code
- **MetaHATS Interpreter** - Meta-visualization of tree structure

## Modules

### Core
- `Hylograph.HATS` - Tree types and constructors
- `Hylograph.HATS.Friendly` - User-friendly element helpers (svg, circle, rect, etc.)
- `Hylograph.HATS.InterpreterTick` - DOM rendering (rerender, rerenderInto)
- `Hylograph.Render` - Selection-based rendering utilities

### Expressions
- `Hylograph.Expr.Friendly` - Attribute helpers (width, cx, fill, num, text, etc.)
- `Hylograph.Expr.Attr` - Low-level attribute types
- `Hylograph.Expr.Sugar` - Convenience functions

### Interpreters
- `Hylograph.Interpreter.D3` - D3.js selection operations
- `Hylograph.Interpreter.English` - Plain English description
- `Hylograph.Interpreter.Mermaid` - Mermaid diagram generation
- `Hylograph.Interpreter.SemiQuine` - Code generation
- `Hylograph.Interpreter.MetaHATS` - Tree meta-visualization

### Interactions
- `Hylograph.Interaction.Zoom` - Zoom behavior
- `Hylograph.Interaction.Brush` - Brush selection
- `Hylograph.Interaction.Pointer` - Pointer events
- `Hylograph.Interaction.Coordinated` - Coordinated highlighting

### Utilities
- `Hylograph.Scale` - D3-style scales
- `Hylograph.Transform` - SVG transforms
- `Hylograph.Tooltip` - Tooltip helpers

## Part of Hylograph

- **hylograph-selection** - D3 selection library (this package)
- **hylograph-graph** - Graph data structures
- **hylograph-layout** - Layout algorithms
- **hylograph-simulation** - Force simulation

Uses `tree-rose` for rose tree data structures.

## License

MIT
