-- | Map Diagram - HATS Projection Version
-- |
-- | Visualizes Data.Map as a classic math textbook function diagram:
-- | - Domain (keys) on the left
-- | - Codomain (values) on the right
-- | - Arrows showing the mapping
-- | - Value sharing is visible (multiple keys → same value)
-- |
-- | **The Story**: One Map, three projections, one diagram.
-- |
-- | This demonstrates the `Project` type class - a finally tagless extension
-- | that allows any data structure to project into an array. A single Map
-- | yields three different views:
-- | - `MapKeys m` → domain nodes
-- | - `MapValues m` → codomain nodes (auto-deduplicated)
-- | - `MapEntries m` → arrows connecting them
module Examples.MapDiagram
  ( mapDiagramTree
  , MapConfig
  , defaultMapConfig
  , sampleUserAges
  , sampleColorMap
  -- Componentized chrome for reuse
  , functionDiagramChrome
  ) where

import Prelude

import Data.Array (length, sortBy)
import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Hylograph.HATS.Friendly as F
import Hylograph.HATS (Tree, MapKeys(..), MapValues(..), MapEntries(..), elem, forEachP, staticStr, staticNum, thunkedNum, thunkedStr)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- =============================================================================
-- Configuration
-- =============================================================================

type MapConfig =
  { width :: Number
  , height :: Number
  , boxWidth :: Number
  , boxPadding :: Number
  , nodeRadius :: Number
  , fontSize :: Number
  , domainLabel :: String
  , codomainLabel :: String
  }

defaultMapConfig :: MapConfig
defaultMapConfig =
  { width: 500.0
  , height: 300.0
  , boxWidth: 120.0
  , boxPadding: 30.0
  , nodeRadius: 8.0
  , fontSize: 12.0
  , domainLabel: "Keys"
  , codomainLabel: "Values"
  }

-- =============================================================================
-- Sample Data
-- =============================================================================

-- | User ages - demonstrates basic mapping
sampleUserAges :: Map String Int
sampleUserAges = Map.fromFoldable
  [ Tuple "alice" 42
  , Tuple "bob" 17
  , Tuple "carol" 99
  , Tuple "dave" 99   -- same age as carol - shows value sharing
  , Tuple "eve" 42    -- same age as alice
  ]

-- | Color preferences - string to string mapping
sampleColorMap :: Map String String
sampleColorMap = Map.fromFoldable
  [ Tuple "sky" "blue"
  , Tuple "grass" "green"
  , Tuple "ocean" "blue"    -- shares with sky
  , Tuple "fire" "red"
  , Tuple "blood" "red"     -- shares with fire
  , Tuple "sun" "yellow"
  ]

-- =============================================================================
-- Position Computation
-- =============================================================================

-- | Compute Y positions for keys based on their sorted order
-- | Returns a function that maps key string to Y position
computeKeyPositions :: forall k v. Ord k => Show k
                    => MapConfig -> Map k v -> (String -> Number)
computeKeyPositions cfg m =
  let
    keys = Array.fromFoldable (Map.keys m)
    numKeys = length keys
    spacing = (cfg.height - cfg.boxPadding * 2.0) / Int.toNumber (max 1 (numKeys + 1))
    keyPosMap = Map.fromFoldable $
      Array.mapWithIndex (\idx k -> Tuple (show k) (cfg.boxPadding + spacing * Int.toNumber (idx + 1))) keys
  in
    \keyStr -> fromMaybe 0.0 (Map.lookup keyStr keyPosMap)

-- | Compute Y positions for unique values based on their sorted order
-- | Returns a function that maps value string to Y position
computeValuePositions :: forall k v. Ord k => Ord v => Show v
                      => MapConfig -> Map k v -> (String -> Number)
computeValuePositions cfg m =
  let
    entries = Map.toUnfoldable m :: Array (Tuple k v)
    values = Array.nub $ sortBy compare $ map (show <<< snd) entries
    numValues = length values
    spacing = (cfg.height - cfg.boxPadding * 2.0) / Int.toNumber (max 1 (numValues + 1))
    valuePosMap = Map.fromFoldable $
      Array.mapWithIndex (\idx v -> Tuple v (cfg.boxPadding + spacing * Int.toNumber (idx + 1))) values
  in
    \valueStr -> fromMaybe 0.0 (Map.lookup valueStr valuePosMap)

-- =============================================================================
-- HATS Tree - Three Projections, One Diagram
-- =============================================================================

-- | Render a Map as a function diagram using three projections.
-- |
-- | The same Map is projected three ways:
-- | 1. `MapKeys m` → Domain nodes (left side)
-- | 2. `MapValues m` → Codomain nodes (right side, auto-deduplicated)
-- | 3. `MapEntries m` → Arrows connecting keys to values
mapDiagramTree :: forall k v. Ord k => Ord v => Show k => Show v
               => MapConfig -> Map k v -> Tree
mapDiagramTree cfg m =
  let
    -- Position lookup functions (computed once, used in templates)
    keyY = computeKeyPositions cfg m
    valueY = computeValuePositions cfg m

    -- Layout constants
    leftBoxX = cfg.boxPadding
    rightBoxX = cfg.width - cfg.boxPadding - cfg.boxWidth
    arrowStartX = leftBoxX + cfg.boxWidth
    arrowEndX = rightBoxX

    -- Bezier curve path from key to value
    arrowPath :: Tuple k v -> String
    arrowPath (Tuple k v) =
      let
        x1 = arrowStartX
        y1 = keyY (show k)
        x2 = arrowEndX
        y2 = valueY (show v)
        cx = (x1 + x2) / 2.0  -- Control point X
      in
        "M" <> show x1 <> "," <> show y1 <>
        " C" <> show cx <> "," <> show y1 <>
        " " <> show cx <> "," <> show y2 <>
        " " <> show x2 <> "," <> show y2
  in
    elem SVG
      [ F.width cfg.width
      , F.height cfg.height
      , F.attr "viewBox" ("0 0 " <> show cfg.width <> " " <> show cfg.height)
      , F.class_ "map-diagram"
      ]
      -- =========================================================
      -- STATIC CHROME - The diagram frame (componentized)
      -- =========================================================
      ( functionDiagramChrome cfg leftBoxX rightBoxX <>

      -- =========================================================
      -- THE THREE PROJECTIONS - One Map, Three Views
      -- =========================================================

      -- PROJECTION 1: MapEntries → Arrows
      -- Each entry (key, value) becomes an arrow path
      [ forEachP "arrows" Path (MapEntries m) (\(Tuple k _) -> show k) \entry ->
          elem Path
            [ thunkedStr "d" (arrowPath entry)
            , F.fill "none"
            , F.stroke "#888"
            , F.strokeWidth 1.5
            , staticStr "marker-end" "url(#arrowhead)"
            ] []

      -- PROJECTION 2: MapKeys → Domain Nodes
      -- Each key becomes a node in the domain box
      , forEachP "keys" Group (MapKeys m) show \key ->
          elem Group
            [ thunkedStr "transform" ("translate(" <> show (leftBoxX + cfg.boxWidth - 20.0) <> "," <> show (keyY (show key)) <> ")")
            ]
            [ elem Circle
                [ F.r cfg.nodeRadius
                , F.fill "#4a9ebb"
                , F.stroke "#fff"
                , F.strokeWidth 2.0
                ] []
            , elem Text
                [ F.x (-15.0)
                , F.y 4.0
                , F.textAnchor "end"
                , thunkedNum "font-size" cfg.fontSize
                , F.fill "#333"
                , thunkedStr "textContent" (show key)
                ] []
            ]

      -- PROJECTION 3: MapValues → Codomain Nodes
      -- Each unique value becomes a node in the codomain box
      -- Note: MapValues auto-deduplicates - multiple keys → same value = one node
      , forEachP "values" Group (MapValues m) show \value ->
          elem Group
            [ thunkedStr "transform" ("translate(" <> show (rightBoxX + 20.0) <> "," <> show (valueY (show value)) <> ")")
            ]
            [ elem Circle
                [ F.r cfg.nodeRadius
                , F.fill "#bb804a"
                , F.stroke "#fff"
                , F.strokeWidth 2.0
                ] []
            , elem Text
                [ F.x 15.0
                , F.y 4.0
                , F.textAnchor "start"
                , thunkedNum "font-size" cfg.fontSize
                , F.fill "#333"
                , thunkedStr "textContent" (show value)
                ] []
            ]
      ])

-- =============================================================================
-- Static Chrome (Reusable Frame for Function Diagrams)
-- =============================================================================

-- | The static "chrome" of a function diagram: background, boxes, labels, defs.
-- | This is the frame that doesn't change with data - componentized for reuse.
functionDiagramChrome :: MapConfig -> Number -> Number -> Array Tree
functionDiagramChrome cfg leftBoxX rightBoxX =
  [ -- Background
    elem Rect
      [ staticStr "data-label" "background"
      , F.width cfg.width
      , F.height cfg.height
      , F.fill "#fafafa"
      , staticNum "rx" 8.0
      ] []

  -- Domain box (left)
  , elem Rect
      [ staticStr "data-label" "domain-box"
      , F.x leftBoxX
      , F.y cfg.boxPadding
      , F.width cfg.boxWidth
      , F.height (cfg.height - cfg.boxPadding * 2.0)
      , F.fill "#e8f4f8"
      , F.stroke "#4a9ebb"
      , F.strokeWidth 2.0
      , staticNum "rx" 6.0
      ] []

  -- Domain label
  , elem Text
      [ staticStr "data-label" "domain-label"
      , F.x (leftBoxX + cfg.boxWidth / 2.0)
      , F.y (cfg.boxPadding - 8.0)
      , F.textAnchor "middle"
      , staticNum "font-size" 14.0
      , F.fill "#4a9ebb"
      , staticStr "font-weight" "bold"
      , staticStr "textContent" cfg.domainLabel
      ] []

  -- Codomain box (right)
  , elem Rect
      [ staticStr "data-label" "codomain-box"
      , F.x rightBoxX
      , F.y cfg.boxPadding
      , F.width cfg.boxWidth
      , F.height (cfg.height - cfg.boxPadding * 2.0)
      , F.fill "#f8f0e8"
      , F.stroke "#bb804a"
      , F.strokeWidth 2.0
      , staticNum "rx" 6.0
      ] []

  -- Codomain label
  , elem Text
      [ staticStr "data-label" "codomain-label"
      , F.x (rightBoxX + cfg.boxWidth / 2.0)
      , F.y (cfg.boxPadding - 8.0)
      , F.textAnchor "middle"
      , staticNum "font-size" 14.0
      , F.fill "#bb804a"
      , staticStr "font-weight" "bold"
      , staticStr "textContent" cfg.codomainLabel
      ] []

  -- Arrow marker definition
  , elem Defs
      [ staticStr "data-label" "arrow-marker" ]
      [ elem Group
          [ staticStr "id" "arrowhead" ]
          [ elem Path
              [ staticStr "d" "M0,-4L8,0L0,4"
              , F.fill "#666"
              ] []
          ]
      ]
  ]
