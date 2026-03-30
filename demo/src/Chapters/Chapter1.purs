-- | Chapter 1: The Fold
-- |
-- | Shows the most basic fold: an array of items producing an SVG visualization.
-- | Data items and their corresponding SVG elements are linked via
-- | CoordinatedHighlight — hover on a data item and its output lights up.
module Chapters.Chapter1
  ( dataArrayTree
  , svgOutputTree
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Hylograph.HATS (Tree, ThunkedBehavior, elem, forEach, staticStr, withBehaviors, onCoordinatedHighlight)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Behavior.Types (HighlightClass(..))
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Data
-- =============================================================================

type IndexedItem = { name :: String, color :: String, idx :: Int }

indexedItems :: Array IndexedItem
indexedItems = mapWithIndex (\i r -> { name: r.name, color: r.color, idx: i })
  [ { name: "Apples",   color: "#e74c3c" }
  , { name: "Bananas",  color: "#f1c40f" }
  , { name: "Cherries", color: "#c0392b" }
  , { name: "Dates",    color: "#d4a574" }
  , { name: "Figs",     color: "#8e44ad" }
  ]

-- | Build highlight behavior for an item.
-- | identify = this item's name, classify = Primary if same, Dimmed otherwise.
hlFor :: String -> ThunkedBehavior
hlFor name = onCoordinatedHighlight
  { identify: name
  , classify: \hoveredId ->
      if name == hoveredId then Primary
      else Dimmed
  , group: Nothing
  }

-- =============================================================================
-- Data Panel (left): array of labeled boxes
-- =============================================================================

dataArrayTree :: Tree
dataArrayTree =
  let
    boxH = 36.0
    gap = 6.0
    svgW = 180.0
    svgH = 240.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH
      , F.width svgW
      , F.height svgH
      ]
      [ forEach "data" Group indexedItems _.name \item ->
          let y = 10.0 + Int.toNumber item.idx * (boxH + gap)
          in
            withBehaviors [ hlFor item.name ] $
            elem Group [ F.transform ("translate(0," <> show y <> ")") ]
              [ elem Rect
                  [ F.x 4.0, F.y 0.0
                  , F.width (svgW - 8.0), F.height boxH
                  , staticStr "rx" "3"
                  , F.fill "#f5f5f0"
                  , F.stroke "#ddd"
                  , staticStr "stroke-width" "1"
                  ] []
              , elem Text
                  [ F.x 14.0, F.y (boxH / 2.0 + 5.0)
                  , F.fontSize "13"
                  , F.fontFamily "'JetBrains Mono', monospace"
                  , F.fill "#333"
                  , staticStr "textContent" item.name
                  ] []
              ]
      ]

-- =============================================================================
-- SVG Output Panel: colored circles with labels
-- =============================================================================

svgOutputTree :: Tree
svgOutputTree =
  let
    circleR = 18.0
    rowH = circleR * 2.0 + 10.0
    cx = 30.0
    svgW = 280.0
    svgH = 260.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH
      , F.width svgW
      , F.height svgH
      ]
      [ forEach "output" Group indexedItems _.name \item ->
          let y = 10.0 + circleR + Int.toNumber item.idx * rowH
          in
            withBehaviors [ hlFor item.name ] $
            elem Group [ F.transform ("translate(0," <> show y <> ")") ]
              [ elem Circle
                  [ F.cx cx, F.cy 0.0
                  , F.r circleR
                  , F.fill item.color
                  , F.opacity "0.85"
                  ] []
              , elem Text
                  [ F.x (cx + circleR + 12.0), F.y 5.0
                  , F.fontSize "14"
                  , F.fontFamily "'Inter', sans-serif"
                  , F.fill "#333"
                  , staticStr "textContent" item.name
                  ] []
              ]
      ]
