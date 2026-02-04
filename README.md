# purescript-hylograph-selection

[![Tree Builder Demo](../../site/lib-selection/public/demo.jpeg)](/#/tree-builder)

Type-safe D3 selection and attribute library for PureScript.

## Overview

A declarative, type-safe approach to D3.js visualization in PureScript. Instead of imperative D3 method chaining, you build a tree AST that describes your visualization, then interpret it to render.

## Installation

```bash
spago install hylograph-selection
```

## Key Concepts

### HATS (Hylomorphic Abstract Tree Syntax)

Build visualizations as data structures using HATS:

```purescript
import Hylograph.AST as A
import Hylograph.Expr.Friendly (attr, num, text)
import Hylograph.Internal.Selection.Types (ElementType(..))

myViz :: A.Tree Unit
myViz =
  A.named SVG "chart"
    [ attr "width" $ num 400.0
    , attr "height" $ num 300.0
    ]
    `A.withChild`
      A.elem Circle
        [ attr "cx" $ num 200.0
        , attr "cy" $ num 150.0
        , attr "r" $ num 50.0
        , attr "fill" $ text "steelblue"
        ]
```

### Data Joins

D3-style enter/update/exit with type safety:

```purescript
A.joinData "circles" "circle" myData $ \d ->
  A.elem Circle
    [ fnAttr "cx" (_.x)
    , fnAttr "cy" (_.y)
    , fnAttr "r" (_.radius)
    ]
```

### Interpreters

- **D3 Interpreter** - Renders to DOM via D3.js
- **English Interpreter** - Describes the tree in plain English
- **Mermaid Interpreter** - Generates Mermaid diagrams
- **SemiQuine Interpreter** - Generates PureScript code

## Modules

### Core
- `Hylograph.AST` - Tree AST types and constructors
- `Hylograph.HATS` - HATS DSL helpers
- `Hylograph.Render` - Rendering utilities

### Expressions
- `Hylograph.Expr.Friendly` - Attribute helpers (attr, cx, cy, fill, etc.)
- `Hylograph.Expr.Attr` - Low-level attribute types
- `Hylograph.Expr.Sugar` - Convenience functions

### Interpreters
- `Hylograph.Interpreter.D3` - D3.js rendering
- `Hylograph.Interpreter.English` - Plain English description
- `Hylograph.Interpreter.Mermaid` - Mermaid diagram generation
- `Hylograph.Interpreter.SemiQuine` - Code generation

### Interactions
- `Hylograph.Interaction.Zoom` - Zoom behavior
- `Hylograph.Interaction.Brush` - Brush selection
- `Hylograph.Interaction.Pointer` - Pointer events

### Utilities
- `Hylograph.Scale` - D3-style scales
- `Hylograph.Axis.Axis` - Axis rendering
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
