# Hylograph SPA Skill

Build Halogen-based single-page applications with Hylograph visualizations.

## Usage

```
/hylograph-spa              # Show this guide
/hylograph-spa scaffold     # Create new project structure
/hylograph-spa component    # Add a new viz component
/hylograph-spa debug        # Help diagnose common issues
```

## Instructions

When this skill is invoked, help the user build or debug a Halogen + Hylograph SPA following the patterns below.

---

## Architecture Overview

```
project/
├── spago.yaml              # Dependencies
├── package.json            # Build tooling
├── public/
│   ├── index.html          # Mount point
│   ├── styles.css          # Styling
│   └── bundle.js           # Built output
└── src/
    ├── Main.purs           # App entry, mounts Halogen
    ├── Component/
    │   └── MyViz.purs      # Halogen component (owns state)
    ├── Viz/
    │   ├── MyViz.purs      # HATS renderer (stateless)
    │   └── MyViz.js        # FFI for DOM events
    └── Data/
        └── Types.purs      # Domain types
```

**Key principle**: Halogen owns state, Viz modules are pure renderers.

---

## Dependencies (spago.yaml)

```yaml
package:
  name: my-hylograph-app
  dependencies:
    - halogen
    - halogen-subscriptions
    - prelude
    - effect
    - aff
    - arrays
    - maybe
    - ordered-collections    # For Set/Map in state
    - hylograph-selection    # HATS, scales, colors
    - hylograph-layout       # Layout algorithms (optional)
    - hylograph-simulation   # Force layouts (optional)

workspace:
  packageSet:
    registry: 67.0.1
  extraPackages:
    hylograph-selection:
      path: /path/to/purescript-hylograph-selection
    # Add other hylograph libs as needed
```

---

## Main.purs (Entry Point)

```purescript
module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Component.App as App

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI App.component unit body
```

---

## Halogen Component Pattern

```purescript
module Component.MyViz where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Viz.MyViz as Viz

-- Types
type Input = MyData
type Output = Void
data Query a = NoQuery a
type Slot = H.Slot Query Output

-- State: Halogen owns ALL mutable state
type State =
  { data :: MyData
  , selected :: Set String    -- UI state example
  , initialized :: Boolean
  }

-- Actions
data Action
  = Initialize
  | Receive Input
  | Select String             -- User interaction

-- Component
component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState data_ = { data: data_, selected: Set.empty, initialized: false }

-- Render: Just a container div for HATS
render :: forall m. State -> H.ComponentHTML Action () m
render _ =
  HH.div
    [ HP.class_ (HH.ClassName "viz-wrapper") ]
    [ HH.div [ HP.id "viz-container" ] [] ]  -- HATS renders here

-- Action handler
handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let config = Viz.defaultConfig { containerSelector = "#viz-container" }

    -- Set up event bridge: FFI → Halogen
    { emitter, listener } <- liftEffect HS.create
    _ <- H.subscribe emitter
    liftEffect $ Viz.setupClickHandlers config \id ->
      HS.notify listener (Select id)

    -- Initial render
    liftEffect $ Viz.render config state.data state.selected
    H.modify_ _ { initialized = true }

  Receive data_ -> do
    state <- H.get
    H.modify_ _ { data = data_ }
    let config = Viz.defaultConfig { containerSelector = "#viz-container" }
    liftEffect $ Viz.render config data_ state.selected

  Select id -> do
    state <- H.get
    let newSelected = if Set.member id state.selected
                      then Set.delete id state.selected
                      else Set.insert id state.selected
    H.modify_ _ { selected = newSelected }
    let config = Viz.defaultConfig { containerSelector = "#viz-container" }
    liftEffect $ Viz.render config state.data newSelected
```

---

## Viz Module Pattern (Stateless)

```purescript
module Viz.MyViz where

import Prelude
import Data.Set (Set)
import Effect (Effect)

-- HATS imports
import Hylograph.HATS (Tree, elem)
import Hylograph.HATS.Friendly (attr, x, y, width, height, fill, stroke, class_, style, viewBox)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Element.Types (ElementType(..))

-- FFI
foreign import addClickHandlers :: String -> (String -> Effect Unit) -> Effect Unit

-- Config
type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  }

defaultConfig :: Config
defaultConfig =
  { containerSelector: "#viz-container"
  , width: 800.0
  , height: 600.0
  }

-- Public API: Pure render function
render :: Config -> MyData -> Set String -> Effect Unit
render config data_ selected = do
  let tree = buildTree config data_ selected
  _ <- rerender config.containerSelector tree
  pure unit

-- Set up click handlers (call once in Initialize)
setupClickHandlers :: Config -> (String -> Effect Unit) -> Effect Unit
setupClickHandlers config callback =
  addClickHandlers config.containerSelector callback

-- Build HATS tree (pure function)
buildTree :: Config -> MyData -> Set String -> Tree
buildTree config data_ selected =
  elem SVG
    [ attr "id" "my-viz"
    , viewBox 0.0 0.0 config.width config.height
    , width config.width
    , height config.height
    , style "display: block;"
    ]
    [ elem Group [ class_ "items" ]
        (data_.items <#> \item -> renderItem item (Set.member item.id selected))
    ]

renderItem :: Item -> Boolean -> Tree
renderItem item isSelected =
  elem Rect
    [ attr "data-id" item.id
    , x item.x
    , y item.y
    , width 50.0
    , height 50.0
    , fill (if isSelected then "#b8860b" else "#ccc")
    , stroke "#333"
    , style "cursor: pointer;"
    ]
    []
```

---

## FFI for DOM Events

**Viz/MyViz.js**:
```javascript
export const addClickHandlers = selector => callback => () => {
  const container = document.querySelector(selector);
  if (!container) return;

  // Remove existing handler to avoid duplicates
  if (container._clickHandler) {
    container.removeEventListener('click', container._clickHandler);
  }

  container._clickHandler = (event) => {
    // Walk up to find element with data-id
    let target = event.target;
    while (target && target !== container) {
      if (target.dataset && target.dataset.id) {
        callback(target.dataset.id)();
        return;
      }
      target = target.parentElement;
    }
  };

  container.addEventListener('click', container._clickHandler);
};
```

---

## HATS Friendly Attributes Reference

```purescript
import Hylograph.HATS.Friendly

-- Positioning
x, y           :: Number -> Attr
x1, y1, x2, y2 :: Number -> Attr      -- Lines
cx, cy, r      :: Number -> Attr      -- Circles
width, height  :: Number -> Attr

-- Styling
fill, stroke   :: String -> Attr
strokeWidth    :: Number -> Attr
opacity        :: String -> Attr

-- Text
textAnchor     :: String -> Attr      -- "start" | "middle" | "end"
fontSize       :: String -> Attr
fontFamily     :: String -> Attr

-- Layout
viewBox        :: Number -> Number -> Number -> Number -> Attr
transform      :: String -> Attr
class_         :: String -> Attr
style          :: String -> Attr

-- Paths
d              :: String -> Attr      -- SVG path data
points         :: String -> Attr      -- Polygon points

-- Generic (for attrs not in Friendly)
attr           :: String -> String -> Attr
attrNum        :: String -> Number -> Attr
```

---

## ElementTypes Reference

```purescript
import Hylograph.Internal.Element.Types (ElementType(..))

-- Container elements
SVG, Group

-- Shape elements
Rect, Circle, Ellipse, Line, Path, Polygon, Polyline

-- Text
Text, TSpan

-- Other
Image, Use, Defs, ClipPath, Mask
```

---

## Common Pitfalls

### 1. Container not found
```
Error: rerender failed - selector not found
```
**Fix**: Ensure the container div is rendered before calling `Viz.render`. In Halogen, the div exists after `render` runs, so call viz render in `Initialize` action.

### 2. Click handlers not working
**Check**:
- Is `data-id` attribute set on clickable elements?
- Is `setupClickHandlers` called in `Initialize`?
- Is the emitter subscribed before setting up handlers?

### 3. State not updating
**Check**:
- Are you calling `Viz.render` after `H.modify_`?
- Is the new state being passed to render (not old state)?

### 4. Duplicate event handlers
**Fix**: The FFI should remove existing handlers before adding new ones:
```javascript
if (container._clickHandler) {
  container.removeEventListener('click', container._clickHandler);
}
```

### 5. Wrong import for elem
```
UnknownName: elem
```
**Fix**: Import from HATS, not Halogen:
```purescript
import Hylograph.HATS (elem)  -- Correct
-- NOT: import Halogen.HTML (elem)
```

---

## Build & Serve

```bash
# Build
spago bundle --bundle-type app --outfile public/bundle.js

# Serve
npx http-server public -p 8080
```

---

## Example: Registry Dashboard

See https://github.com/afcondon/registry-dashboard for a complete working example with:
- Compiler compatibility matrix (collapsible groups)
- Job timeline beeswarm (force simulation)
- Queue depth area chart
- All using this Halogen + HATS pattern
