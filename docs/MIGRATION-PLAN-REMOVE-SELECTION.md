# Migration Plan: Remove Selection API from hylograph-selection

## Overview

Remove the legacy Selection API (~4,700 lines) from hylograph-selection,
retaining only shared types (`ElementType`, `createElementWithNS`) that HATS
needs. Three downstream consumers require migration first.

---

## Phase 1: Extract Shared Types

Before removing anything, move the two things HATS actually uses out of the
Selection modules.

### 1a. Move `ElementType` to `Hylograph.HATS`

`ElementType` is the ADT defining SVG/HTML element kinds (`Circle`, `Rect`,
`SVG`, `Group`, etc.). It currently lives in
`Hylograph.Internal.Selection.Types` but is conceptually part of HATS.

**Action**: Add `ElementType(..)`, its `Show`/`Generic` instances, and
`elementContext`/`RenderContext` to `Hylograph.HATS` (or a new
`Hylograph.HATS.Types` if HATS.purs is already large enough).

**Re-export** from the old location temporarily for backwards compatibility:
```purescript
-- Hylograph.Internal.Selection.Types
-- DEPRECATED: Import from Hylograph.HATS instead
module Hylograph.Internal.Selection.Types (module Hylograph.HATS) where
import Hylograph.HATS (ElementType(..))
```

### 1b. Move `createElementWithNS` to `Hylograph.HATS.InterpreterTick` (or a DOM utility module)

This is a ~15-line function that creates a DOM element with the correct
namespace (SVG vs HTML). It's used only by `InterpreterTick`.

**Action**: Inline it into `InterpreterTick.purs` or create a small
`Hylograph.DOM` utility module. The function is:

```purescript
createElementWithNS :: ElementType -> Document -> Effect Element
createElementWithNS elemType doc =
  case elementContext elemType of
    SVGContext ->
      -- SVG elements need the SVG namespace
      createElementNS (Just svgNamespace) (elementTypeToString elemType) doc
    HTMLContext ->
      -- HTML elements use default namespace
      createElement (elementTypeToString elemType) doc
```

Along with `elementTypeToString` (a simple Show-like function mapping
`Circle -> "circle"`, `Rect -> "rect"`, etc.).

---

## Phase 2: Migrate Downstream Consumers

### 2a. CodeExplorer SceneCoordinator (trivial)

**File**: `CodeExplorer/minard/frontend/src/Component/SceneCoordinator.purs`

**Current code**:
```purescript
import Hylograph.Render (runD3, clear)

clearAllVizContainers :: Effect Unit
clearAllVizContainers = void $ runD3 do
  clear "#galaxy-beeswarm-container *"
  clear (C.bubblePackBeeswarmContainer <> " *")
  clear "#pkg-treemap-container *"
  clear "#circlepack-container *"
```

**Migration**: Replace with `clearContainer` from HATS InterpreterTick, or
direct DOM manipulation:

```purescript
import Hylograph.HATS.InterpreterTick (clearContainer)

clearAllVizContainers :: Effect Unit
clearAllVizContainers = do
  clearContainer "#galaxy-beeswarm-container"
  clearContainer "#pkg-treemap-container"
  clearContainer "#circlepack-container"
  -- etc.
```

If `clearContainer` doesn't support the `" *"` wildcard pattern, use a small
FFI helper or `web-dom` directly:

```purescript
import Web.DOM.ParentNode (querySelectorAll)
-- clear children of container
```

**Effort**: ~15 minutes.

### 2b. Simpson's Paradox Visualization (medium)

**Files** (4, in `purescript-polyglot/site/website/src/Viz/Simpsons/`):
- `App.purs`
- `ForceViz.purs`
- `Components.purs`
- `DataTable.purs`

**Current pattern**: Uses `D3v2M` monad with `select`, `renderTree`, `clear`.

**Migration**: Rewrite to use HATS tree construction + `rerender`. The
visualization is a force-directed scatter plot with regression lines and a data
table - all patterns well-supported by HATS.

The key translations:

| Selection API | HATS equivalent |
|---------------|----------------|
| `runD3v2M do select "#chart" >>= ...` | `rerender "#chart" myTree` |
| `renderTree Circle data keyFn attrsFn` | `forEach "circles" Circle data keyFn \d -> elem Circle [attrs] []` |
| `clear "#chart"` | `clearContainer "#chart"` |
| `selectAll "circle" >>= setAttrs` | Just rebuild and `rerender` the tree |

**Effort**: ~2-4 hours. This is the largest migration task.

### 2c. HowTo Transitions page (purescript-polyglot website)

**File**: `purescript-polyglot/site/website/src/Component/HowTo/HowtoTransitions.purs`

**Current**: Imports `TransitionM` type class, likely for documentation/example
code strings rather than actual usage.

**Action**: Check whether it's runtime code or just string literals containing
example code. If the latter, update the example text. If the former, migrate
like Simpson's above.

**Effort**: ~15-30 minutes.

### 2d. hylograph-music WebAudio interpreter (design change)

**File**: `hylograph-music/src/Hylograph/Music/Interpreter/WebAudio.purs`

**Current**: Implements `SelectionM` type class, interpreting selection
operations as audio events.

**Migration**: Rewrite as a HATS tree interpreter, matching the pattern of
`English.purs`, `Mermaid.purs`, `SemiQuine.purs`:

```purescript
-- Current: instance SelectionM WebAudioSelection WebAudioM
-- New pattern:
interpretAudio :: Tree -> Effect AudioResult
interpretAudio (Elem { elemType, attrs, children }) = ...
interpretAudio (MkFold someFold) = runSomeFold someFold \spec -> ...
interpretAudio Empty = ...
```

**Effort**: ~1-2 hours, depending on how complex the audio mapping is.

---

## Phase 3: Remove Selection Modules

Once all consumers are migrated, delete:

### Files to Delete

```
src/Hylograph/Internal/Selection/Types.purs          -- after ElementType moved
src/Hylograph/Internal/Selection/Operations.purs
src/Hylograph/Internal/Selection/Operations.js
src/Hylograph/Internal/Selection/Operations/Selection.purs
src/Hylograph/Internal/Selection/Operations/Selection.js
src/Hylograph/Internal/Selection/Operations/Conversions.purs
src/Hylograph/Internal/Selection/Operations/Conversions.js
src/Hylograph/Internal/Selection/Operations/Helpers.purs
src/Hylograph/Internal/Selection/Join.purs
src/Hylograph/Internal/Selection/Query.purs
src/Hylograph/Internal/Capabilities/Selection.purs
src/Hylograph/Internal/Capabilities/Transition.purs
src/Hylograph/Interpreter/D3.purs
src/Hylograph/Render.purs
```

### Lines Removed: ~4,700

### Dependencies Potentially Removable

Check whether any of these dependencies were only needed by Selection:
- Review if `web-dom` usage reduces (HATS InterpreterTick also uses it, so
  likely stays)
- Review if `transformers`/`free` usage reduces

---

## Phase 4: Update Downstream Imports

Every consumer that does `import Hylograph.Internal.Selection.Types
(ElementType(..))` needs to update to the new location. This is mechanical
but touches many files:

### Affected repos (grep for `Internal.Selection.Types`):
- `purescript-hylograph-showcases/` (~12 files)
- `CodeExplorer/` (~8 files)
- `purescript-hylograph-simulation/` (~2 files)
- `purescript-hylograph-layout/` (~1 file)
- `purescript-polyglot/` (~4 files)

**Mitigation**: Keep a re-export shim for one release cycle:
```purescript
-- Hylograph.Internal.Selection.Types (DEPRECATED)
module Hylograph.Internal.Selection.Types (module Hylograph.HATS) where
import Hylograph.HATS (ElementType(..))
```

This lets downstream code continue working while they update imports at their
own pace.

---

## Phase 5: Bump Version

This is a breaking change (removed modules). Bump to 0.3.0 (or 1.0.0 if this
feels like the right milestone for "HATS is the API").

Update the package description in spago.yaml to reflect that this is now a HATS
package, not a "selection" package. Consider whether to rename the package
itself (e.g. `hylograph-core` or just `hylograph`).

---

## Summary

| Phase | Effort | Blocking? |
|-------|--------|-----------|
| 1. Extract shared types | 30 min | No |
| 2a. CodeExplorer cleanup | 15 min | No |
| 2b. Simpson's Paradox rewrite | 2-4 hours | Yes - most work |
| 2c. HowTo Transitions | 15-30 min | No |
| 2d. Music interpreter | 1-2 hours | Can defer |
| 3. Delete files | 15 min | After phase 2 |
| 4. Update imports | 1 hour | After phase 1 |
| 5. Version bump | 15 min | Last |

**Total: ~5-8 hours of focused work**, dominated by the Simpson's Paradox
rewrite.
