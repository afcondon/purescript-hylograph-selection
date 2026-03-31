-- | Chapter 3: Composing Fragments
-- |
-- | A visualization built step by step. Each stage adds one declarative
-- | fragment to the tree — composed with siblings (<>).
-- |
-- | Stage 1: Domain dots
-- | Stage 2: + Codomain dots
-- | Stage 3: + Arrows
-- | Stage 4: + Chrome (boxes, labels)
module Chapters.Chapter3
  ( Stage(..)
  , stageTree
  , dataTree
  ) where

import Prelude

import Data.Array (mapWithIndex, length)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Hylograph.HATS (Tree, ThunkedBehavior, elem, forEach, staticStr, withBehaviors, onCoordinatedHighlight, siblings)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Behavior.Types (HighlightClass(..))
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Stages
-- =============================================================================

data Stage = Stage1 | Stage2 | Stage3 | Stage4

-- =============================================================================
-- Data — a simple function (name → age)
-- =============================================================================

type Entry = { key :: String, value :: String, keyIdx :: Int, valIdx :: Int }

entries :: Array Entry
entries =
  [ { key: "Alice", value: "42", keyIdx: 0, valIdx: 0 }
  , { key: "Bob",   value: "17", keyIdx: 1, valIdx: 1 }
  , { key: "Carol", value: "99", keyIdx: 2, valIdx: 2 }
  , { key: "Dave",  value: "99", keyIdx: 3, valIdx: 2 }  -- shares value with Carol
  , { key: "Eve",   value: "42", keyIdx: 4, valIdx: 0 }  -- shares value with Alice
  ]

-- Unique values (deduplicated, sorted)
uniqueValues :: Array { value :: String, idx :: Int }
uniqueValues =
  [ { value: "17", idx: 1 }
  , { value: "42", idx: 0 }
  , { value: "99", idx: 2 }
  ]

-- =============================================================================
-- Layout
-- =============================================================================

svgW :: Number
svgW = 440.0

svgH :: Number
svgH = 260.0

leftX :: Number
leftX = 90.0

rightX :: Number
rightX = 350.0

dotR :: Number
dotR = 7.0

topPad :: Number
topPad = 50.0

keyY :: Int -> Number
keyY idx = topPad + Int.toNumber idx * 38.0

valY :: Int -> Number
valY idx = topPad + Int.toNumber idx * 70.0 + 15.0

gold :: String
gold = "#C9A962"

goldDim :: String
goldDim = "#8B7355"

-- =============================================================================
-- Highlight — entries coordinate across stages
-- =============================================================================

hlKey :: String -> ThunkedBehavior
hlKey name = onCoordinatedHighlight
  { identify: "key-" <> name
  , classify: \hoveredId ->
      if hoveredId == "key-" <> name then Primary
      else Neutral
  , group: Just "ch3"
  }

hlVal :: String -> ThunkedBehavior
hlVal v = onCoordinatedHighlight
  { identify: "val-" <> v
  , classify: \hoveredId ->
      if hoveredId == "val-" <> v then Primary
      else Neutral
  , group: Just "ch3"
  }

hlArrow :: String -> String -> ThunkedBehavior
hlArrow k v = onCoordinatedHighlight
  { identify: "arrow-" <> k
  , classify: \hoveredId ->
      if hoveredId == "arrow-" <> k then Primary
      else if hoveredId == "key-" <> k then Primary
      else if hoveredId == "val-" <> v then Primary
      else Neutral
  , group: Just "ch3"
  }

-- =============================================================================
-- Data panel — tuples representation
-- =============================================================================

dataTree :: Tree
dataTree =
  let
    boxH = 28.0
    gap = 3.0
    svgW = 160.0
    n = length entries
    svgH = Int.toNumber n * (boxH + gap) + 16.0
    monoFont = "'JetBrains Mono', monospace"
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ forEach "tuples" Group entries _.key \e ->
          let y = 8.0 + Int.toNumber e.keyIdx * (boxH + gap)
          in withBehaviors [ hlKey e.key ] $
             elem Group [ F.transform ("translate(0," <> show y <> ")") ]
               [ elem Rect
                   [ F.x 2.0, F.y 0.0
                   , F.width (svgW - 4.0), F.height boxH
                   , staticStr "rx" "2"
                   , F.fill "#1a1a1a"
                   , F.stroke goldDim
                   , staticStr "stroke-width" "1"
                   ] []
               , elem Text
                   [ F.x 10.0, F.y (boxH / 2.0 + 4.0)
                   , F.fontSize "10", F.fontFamily monoFont
                   , F.fill gold
                   , staticStr "textContent" ("(" <> e.key <> ", " <> e.value <> ")")
                   ] []
               ]
      ]

-- =============================================================================
-- Stage trees — each builds on the previous
-- =============================================================================

stageTree :: Stage -> Tree
stageTree = case _ of
  Stage1 -> wrap [ domainDots ]
  Stage2 -> wrap [ domainDots, codomainDots ]
  Stage3 -> wrap [ arrows, domainDots, codomainDots ]  -- arrows behind dots
  Stage4 -> wrap [ chrome, arrows, domainDots, codomainDots ]

wrap :: Array Tree -> Tree
wrap children =
  elem SVG
    [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
    [ siblings children ]

-- =============================================================================
-- Fragment 1: Domain dots
-- =============================================================================

domainDots :: Tree
domainDots =
  forEach "keys" Group entries _.key \e ->
    let y = keyY e.keyIdx
    in withBehaviors [ hlKey e.key ] $
       elem Group [ F.transform ("translate(" <> show leftX <> "," <> show y <> ")") ]
         [ elem Circle
             [ F.cx 0.0, F.cy 0.0, F.r dotR
             , F.fill gold, F.opacity "0.85"
             ] []
         , elem Text
             [ F.x (-14.0), F.y 4.0
             , F.textAnchor "end"
             , F.fontSize "11"
             , F.fontFamily "'Josefin Sans', sans-serif"
             , F.fill goldDim
             , staticStr "textContent" e.key
             ] []
         ]

-- =============================================================================
-- Fragment 2: Codomain dots (deduplicated values)
-- =============================================================================

codomainDots :: Tree
codomainDots =
  forEach "values" Group uniqueValues _.value \v ->
    let y = valY v.idx
    in withBehaviors [ hlVal v.value ] $
       elem Group [ F.transform ("translate(" <> show rightX <> "," <> show y <> ")") ]
         [ elem Circle
             [ F.cx 0.0, F.cy 0.0, F.r dotR
             , F.fill gold, F.opacity "0.85"
             ] []
         , elem Text
             [ F.x 14.0, F.y 4.0
             , F.textAnchor "start"
             , F.fontSize "11"
             , F.fontFamily "'Josefin Sans', sans-serif"
             , F.fill goldDim
             , staticStr "textContent" v.value
             ] []
         ]

-- =============================================================================
-- Fragment 3: Arrows (bezier curves)
-- =============================================================================

arrows :: Tree
arrows =
  forEach "arrows" Path entries _.key \e ->
    let
      y1 = keyY e.keyIdx
      y2 = valY e.valIdx
      cx = (leftX + rightX) / 2.0
      pathD = "M" <> show (leftX + dotR + 2.0) <> "," <> show y1
           <> " C" <> show cx <> "," <> show y1
           <> " " <> show cx <> "," <> show y2
           <> " " <> show (rightX - dotR - 2.0) <> "," <> show y2
    in withBehaviors [ hlArrow e.key e.value ] $
       elem Path
         [ F.d pathD
         , F.fill "none"
         , F.stroke goldDim
         , F.strokeWidth 1.0
         , F.opacity "0.6"
         ] []

-- =============================================================================
-- Fragment 4: Chrome (boxes and labels)
-- =============================================================================

chrome :: Tree
chrome =
  let
    boxW = 40.0
    boxH = svgH - 30.0
    boxY = 25.0
  in
    elem Group []
      [ -- Domain box
        elem Rect
          [ F.x (leftX - boxW / 2.0), F.y boxY
          , F.width boxW, F.height boxH
          , staticStr "rx" "3"
          , F.fill "none", F.stroke goldDim
          , staticStr "stroke-width" "0.5"
          , staticStr "stroke-dasharray" "4,3"
          ] []
      , elem Text
          [ F.x leftX, F.y 18.0
          , F.textAnchor "middle"
          , F.fontSize "10"
          , F.fontFamily "'Josefin Sans', sans-serif"
          , staticStr "letter-spacing" "0.1em"
          , F.fill goldDim
          , staticStr "textContent" "DOMAIN"
          ] []
      -- Codomain box
      , elem Rect
          [ F.x (rightX - boxW / 2.0), F.y boxY
          , F.width boxW, F.height boxH
          , staticStr "rx" "3"
          , F.fill "none", F.stroke goldDim
          , staticStr "stroke-width" "0.5"
          , staticStr "stroke-dasharray" "4,3"
          ] []
      , elem Text
          [ F.x rightX, F.y 18.0
          , F.textAnchor "middle"
          , F.fontSize "10"
          , F.fontFamily "'Josefin Sans', sans-serif"
          , staticStr "letter-spacing" "0.1em"
          , F.fill goldDim
          , staticStr "textContent" "CODOMAIN"
          ] []
      ]
