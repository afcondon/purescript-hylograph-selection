# purescript-psd3-selection

Type-safe D3 selection and attribute library for PureScript.

## Overview

A declarative, type-safe approach to D3.js visualization in PureScript. Instead of imperative D3 method chaining, you build a tree AST that describes your visualization, then interpret it to render.

## Installation

```bash
spago install psd3-selection
```

## Key Concepts

### Tree AST

Build visualizations as data structures:

```purescript
myViz :: T.Tree Unit
myViz =
  T.named SVG "chart"
    [ attr "width" $ num 400.0
    , attr "height" $ num 300.0
    ]
    `T.withChild`
      T.elem Circle
        [ cx $ num 200.0
        , cy $ num 150.0
        , r $ num 50.0
        , fill $ text "steelblue"
        ]
```

### Data Joins

D3-style enter/update/exit with type safety:

```purescript
T.joinData "circles" "circle" myData $ \d ->
  T.elem Circle
    [ fnAttr "cx" (_.x)
    , fnAttr "cy" (_.y)
    , fnAttr "r" (_.radius)
    ]
```

### Interpreters

- **D3 Interpreter** - Renders to DOM via D3.js
- **English Interpreter** - Describes the tree in plain English
- **Mermaid Interpreter** - Generates Mermaid diagrams

## Modules

- `PSD3.AST` - Tree AST types and constructors
- `PSD3.Expr.Friendly` - Attribute helpers (attr, cx, cy, fill, etc.)
- `PSD3.Interpreter.D3` - D3.js rendering
- `PSD3.Internal.Selection` - Low-level selection operations
- `PSD3.Internal.Behavior` - Event handlers, zoom, drag

## Part of PSD3

- **psd3-tree** - Tree data structures (dependency)
- **psd3-selection** - D3 selection library (this package)
- **psd3-layout** - Layout algorithms
- **psd3-simulation** - Force simulation

## License

MIT
