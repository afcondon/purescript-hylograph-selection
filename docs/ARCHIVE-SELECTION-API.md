# The Selection API: Design, Ideas, and Why HATS Superseded It

## Status: Archived

This document preserves the design rationale and interesting patterns from the
Selection API (V1), which was the original rendering interface in
hylograph-selection. It has been superseded by HATS (Hylomorphic Abstract Tree
Syntax) for all production use, but contains ideas worth preserving.

---

## 1. The Core Idea: Phantom-Typed State Machines

The Selection API's central innovation was encoding D3's selection lifecycle as
a compile-time state machine using phantom types. In D3.js, calling `.enter()`
on something that hasn't been joined to data, or `.exit()` on an enter
selection, silently produces garbage. The Selection API made these errors
impossible.

### The State Types

```purescript
-- Uninhabited types used only at the type level
data SEmpty          -- Selection with parent elements but no data bound
data SBoundOwns      -- Elements that own their __data__ binding
data SBoundInherits  -- Elements with data inherited from parent
data SPending        -- Data without elements (the "enter" selection)
data SExiting        -- Elements without matching data (the "exit" selection)
```

### The Selection Type

```purescript
-- Three phantom parameters track state, parent context, and data type
newtype Selection (state :: Type) (parent :: Type) (datum :: Type)
  = Selection SelectionImpl

-- The runtime representation was just arrays of DOM elements + data
data SelectionImpl
  = EmptySelection { parentElements :: Array Element }
  | BoundSelection
      { parentElements :: Array Element
      , elements :: Array Element
      , dataArray :: Array Foreign
      , keyFunction :: Maybe (Foreign -> String)
      }
  | PendingSelection { ... }  -- enter: data without elements
  | ExitingSelection { ... }  -- exit: elements without data
```

### Legal State Transitions

The type class instances enforced this state machine:

```
SEmpty ──[joinData]──> JoinResult { enter :: SPending
                                  , update :: SBoundOwns
                                  , exit   :: SExiting }

SPending ──[append]──────> SBoundOwns    (create elements for entering data)
SBoundOwns ──[setAttrs]──> SBoundOwns    (update attributes)
SBoundOwns ──[selectChild]──> SBoundInherits (drill into children)
SExiting ──[remove]──────> (consumed)     (remove exiting elements)
```

Any attempt to, say, `append` to an `SBoundOwns` selection or `joinData` to an
`SPending` selection would be a compile-time error. This caught an entire class
of bugs that plague D3.js code.

### Example: The Type-Safe GUP

```purescript
-- This is the "General Update Pattern" from D3, but type-safe
updateVisualization :: Array DataPoint -> D3v2M Unit
updateVisualization newData = do
  -- 1. Select existing elements (type: SEmpty)
  svg <- select "#chart"
  circles <- selectAll "circle" svg

  -- 2. Join data (produces three selections simultaneously)
  JoinResult { enter, update, exit } <- joinData newData keyFn circles
  -- enter  :: Selection SPending  Element DataPoint
  -- update :: Selection SBoundOwns Element DataPoint
  -- exit   :: Selection SExiting  Element DataPoint

  -- 3. Handle entering elements
  entered <- append Circle enter         -- SPending -> SBoundOwns
  setAttrs enterAttrs entered

  -- 4. Handle updating elements
  setAttrs updateAttrs update

  -- 5. Handle exiting elements
  remove exit                            -- SExiting consumed

  pure unit
```

The beauty: if you forget `remove exit`, PureScript's exhaustiveness checking
wouldn't catch it (it's an effect, not a pattern match), but the *structure* of
the API made it hard to get wrong. Every branch of the JoinResult demanded
attention.

---

## 2. The Data Join Algorithm

`Selection.Join` implemented D3's data join in pure PureScript. The algorithm
matches existing DOM elements to new data using a key function:

```purescript
joinData :: forall datum.
  Array datum ->                    -- New data
  (datum -> String) ->              -- Key function
  Selection SEmpty Element datum -> -- Existing elements
  D3v2M (JoinResult datum)          -- Three selections
```

### How It Worked

1. **Build key maps**: Hash existing elements by their bound datum's key, and
   new data items by their key.

2. **Three-way partition**:
   - **Enter**: Keys in new data but not in existing elements. These need new
     DOM elements created.
   - **Update**: Keys present in both. These elements stay, attributes update.
   - **Exit**: Keys in existing elements but not in new data. These elements
     should be removed (often with an animation).

3. **Preserve order**: The update selection preserves DOM order to match the new
   data order, handling reordering via DOM node repositioning.

This is essentially the same algorithm React uses for list reconciliation with
`key` props, but exposed as a first-class operation rather than hidden inside a
framework.

---

## 3. The Capabilities Type Classes

Rather than hardcoding DOM operations, the Selection API defined abstract
interfaces:

```purescript
class (Monad m) <= SelectionM sel m | sel -> m where
  select      :: String -> m (sel SEmpty Element Unit)
  selectAll   :: String -> sel SEmpty Element Unit -> m (sel SEmpty Element Unit)
  append      :: ElementType -> sel SPending Element datum -> m (sel SBoundOwns Element datum)
  setAttrs    :: Array (Attribute datum) -> sel SBoundOwns Element datum -> m Unit
  remove      :: sel SExiting Element datum -> m Unit
  joinData    :: Array datum -> (datum -> String) -> sel SEmpty Element datum
                  -> m (JoinResult sel datum)
  renderTree  :: ElementType -> Array datum -> (datum -> String)
                  -> (datum -> Array (Attribute datum)) -> Array (sel SBoundOwns Element datum -> m Unit)
                  -> sel SEmpty Element datum -> m (sel SBoundOwns Element datum)
  clear       :: String -> m Unit

class (Monad m) <= TransitionM sel m | sel -> m where
  transition  :: sel SBoundOwns Element datum -> TransitionConfig -> m Unit
  animate     :: sel SBoundOwns Element datum -> AnimationSpec datum -> m Unit
```

### The Multi-Target Vision

The intent was that `SelectionM` could be implemented for different backends:

- **D3v2M**: The DOM interpreter (the workhorse)
- **WebAudioM**: An audio interpreter in hylograph-music (sounds instead of
  shapes)
- **TestM**: A pure interpreter for testing (never fully built)

This vision partially materialized. The WebAudio interpreter in hylograph-music
did implement `SelectionM`, proving the abstraction worked. But in practice,
only D3v2M saw real use, and the overhead of the type class machinery added
complexity without much payoff for most users.

---

## 4. Why HATS Superseded Selection

The HATS design document (`HATS-DESIGN.md`) articulates this thoroughly, but
the key insights were:

### Problem 1: Conflated Concerns

The Selection API tied together two orthogonal choices:

- **Enumeration** (coalgebra): How to extract elements from input data (iterate
  an array? traverse a tree? walk with context?)
- **Assembly** (algebra): How to structure the output (flat siblings? nested
  hierarchy?)

In Selection, these were fused into specific operations (`selectAll` +
`joinData` = array-to-siblings, `selectChild` = tree-to-nested). You couldn't
mix and match. Want array-to-nested? There was no clean way.

HATS separated these into an explicit `Enumeration x Assembly` matrix, where
any enumeration works with any assembly.

### Problem 2: Imperative Sequencing

Selection required careful ordering of effects:

```purescript
-- Selection: order matters, state threads through
svg <- select "#chart"
g <- append Group svg
circles <- selectAll "circle" g
joined <- joinData data keyFn circles
entered <- append Circle joined.enter
setAttrs attrs entered
```

HATS replaced this with declarative tree construction:

```purescript
-- HATS: pure value, order is structural
myTree =
  elem SVG []
    [ elem Group []
        [ forEach "circles" Circle data _.id \d ->
            elem Circle [thunkedNum "cx" d.x, thunkedNum "cy" d.y] []
        ]
    ]
```

The tree is a pure value. No effects, no ordering constraints, no state to
thread. Interpretation happens in one call (`rerender`).

### Problem 3: The Parameterization Trap

`Selection state parent datum` carried three type parameters. Composing
selections with different data types required careful threading:

```purescript
-- These have incompatible types:
circleSelection :: Selection SBoundOwns Element CircleData
labelSelection  :: Selection SBoundOwns Element LabelData
-- Can't just put them in the same container
```

HATS solved this with existential types. `Tree` has no type parameter:

```purescript
-- These compose freely:
circles :: Tree  -- internally binds CircleData
labels  :: Tree  -- internally binds LabelData
chart = circles <> labels  -- works!
```

Each `forEach` (internally a `MkFold SomeFold`) packs its datum type
existentially. The datum type is in scope inside the template function but
erased at the tree level.

### Problem 4: Single Interpretation

Despite the `SelectionM` type class, the Selection API was fundamentally shaped
around DOM operations. The state machine (SEmpty -> SPending -> SBoundOwns)
modeled DOM element lifecycle, not visualization semantics.

HATS, being a pure AST, naturally supports multiple interpretations:
- **InterpreterTick**: DOM rendering with transitions
- **English**: Natural language description
- **Mermaid**: Diagram generation
- **SemiQuine**: PureScript code generation
- **MetaHATS**: Meta-visualization (visualize the visualization's structure)

---

## 5. Patterns Worth Preserving

### Phantom-Type State Machines

The pattern of using uninhabited types as phantom parameters to encode legal
state transitions is broadly useful. It appears in:

- Protocol state machines (connection handshakes)
- Builder patterns (ensure required fields are set)
- Resource lifecycle (opened -> used -> closed)
- Workflow systems (draft -> review -> approved)

The key technique:

```purescript
-- Uninhabited state types
data Open
data Closed

-- Phantom-parameterized handle
newtype FileHandle (state :: Type) = FileHandle ForeignHandle

-- Functions that change the phantom type
open  :: FilePath -> Effect (FileHandle Open)
read  :: FileHandle Open -> Effect String      -- Only works on Open
close :: FileHandle Open -> Effect (FileHandle Closed)  -- Open -> Closed
-- read on a Closed handle is a compile error
```

### Existential Wrapping for Heterogeneous Composition

HATS's solution to the parameterization problem (CPS-encoded existentials) is a
generally useful pattern:

```purescript
-- Instead of: data Tree a = ...  (parameterized, can't mix types)
-- Use:
newtype SomeFold = SomeFold (forall r. (forall a. FoldSpec a -> r) -> r)

-- Pack: hide the type
mkSomeFold :: forall a. FoldSpec a -> SomeFold
mkSomeFold spec = SomeFold (\k -> k spec)

-- Unpack: work with the hidden type in a callback
runSomeFold :: forall r. SomeFold -> (forall a. FoldSpec a -> r) -> r
runSomeFold (SomeFold f) k = f k
```

### The Enumeration x Assembly Insight

The decomposition of data-to-DOM mapping into orthogonal enumeration and
assembly dimensions is a genuine contribution to visualization theory. It
clarifies why D3's API has so many special cases (`selectAll`, `data`,
`enter`, `append`, `merge`, nested selections, tree layouts...) - they're all
points in a 2D space that D3 addresses with separate mechanisms.

---

## 6. File Inventory (at time of archival)

### Core Selection Modules (~3,700 lines)
| File | Lines | Purpose |
|------|-------|---------|
| `Internal/Selection/Types.purs` | 255 | Phantom types, Selection type |
| `Internal/Selection/Operations.purs` | 1,383 | DOM manipulation, data join |
| `Internal/Selection/Operations/Selection.purs` | 168 | Select/selectAll |
| `Internal/Selection/Operations/Conversions.purs` | 175 | Element type conversions |
| `Internal/Selection/Operations/Helpers.purs` | 79 | DOM utility functions |
| `Internal/Selection/Join.purs` | 203 | Data join algorithm |
| `Internal/Selection/Query.purs` | 698 | Selection queries |

### Capability Type Classes (~570 lines)
| File | Lines | Purpose |
|------|-------|---------|
| `Internal/Capabilities/Selection.purs` | 406 | SelectionM type class |
| `Internal/Capabilities/Transition.purs` | 160 | TransitionM type class |

### D3v2 Interpreter (~350 lines)
| File | Lines | Purpose |
|------|-------|---------|
| `Interpreter/D3.purs` | 354 | D3v2M implementation |

### Supporting FFI
| File | Purpose |
|------|---------|
| `Internal/Selection/Operations.js` | DOM manipulation |
| `Internal/Selection/Operations/Conversions.js` | Namespace handling |
| `Internal/Selection/Operations/Selection.js` | querySelectorAll |

### Convenience Wrappers
| File | Lines | Purpose |
|------|-------|---------|
| `Render.purs` | 62 | High-level render API |

---

## 7. Downstream Consumers at Time of Archival

At the time of this archival, the Selection API had exactly three remaining
consumers:

1. **Simpson's Paradox visualization** (purescript-polyglot website): 4 files
   using D3v2M for a force-directed layout. Migration to HATS planned.

2. **CodeExplorer SceneCoordinator**: 1 file using `Hylograph.Render (runD3,
   clear)` for container cleanup between scene transitions. Trivially
   replaceable with `clearContainer` from HATS.

3. **hylograph-music WebAudio interpreter**: Implements `SelectionM` for audio
   rendering. Planned migration to HATS tree interpretation (matching the
   pattern used by English, Mermaid, SemiQuine, and MetaHATS interpreters).

All showcases, the main CodeExplorer visualizations, the simulation packages,
and ShapedSteer use HATS exclusively.
