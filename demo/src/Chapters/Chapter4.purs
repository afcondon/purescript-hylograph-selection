-- | Chapter 4: HATS Revealed
-- |
-- | Shows actual PureScript HATS code alongside rendered output.
-- | Small, readable examples demonstrating the embedded DSL.
module Chapters.Chapter4
  ( Example(..)
  , exampleTree
  , exampleCode
  , exampleCaption
  ) where

import Prelude

import Data.Array (mapWithIndex, intercalate)
import Data.Int as Int
import Hylograph.HATS (Tree, elem, forEach, staticStr, siblings)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Examples
-- =============================================================================

data Example = ExBars | ExDots | ExComposed

-- =============================================================================
-- Example 1: A bar chart
-- =============================================================================

type BarDatum = { label :: String, value :: Number, idx :: Int }

barData :: Array BarDatum
barData = mapWithIndex (\i r -> { label: r.label, value: r.value, idx: i })
  [ { label: "Mon", value: 120.0 }
  , { label: "Tue", value: 180.0 }
  , { label: "Wed", value: 90.0 }
  , { label: "Thu", value: 210.0 }
  , { label: "Fri", value: 150.0 }
  ]

barTree :: Tree
barTree =
  let
    svgW = 340.0
    svgH = 260.0
    barW = 50.0
    gap = 12.0
    baseY = 230.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ forEach "bars" Group barData _.label \d ->
          let
            x = 20.0 + Int.toNumber d.idx * (barW + gap)
            h = d.value
            y = baseY - h
          in
            elem Group [ F.transform ("translate(" <> show x <> ",0)") ]
              [ elem Rect
                  [ F.x 0.0, F.y y
                  , F.width barW, F.height h
                  , staticStr "rx" "2"
                  , F.fill "#C9A962"
                  , F.opacity "0.85"
                  ] []
              , elem Text
                  [ F.x (barW / 2.0), F.y (baseY + 16.0)
                  , F.textAnchor "middle"
                  , F.fontSize "11"
                  , F.fontFamily "'Josefin Sans', sans-serif"
                  , F.fill "#8B7355"
                  , staticStr "textContent" d.label
                  ] []
              ]
      ]

barCode :: String
barCode =
  "forEach \"bars\" Group data _.label \\d ->\n\
  \  elem Group [ transform ... ]\n\
  \    [ elem Rect\n\
  \        [ F.x 0.0\n\
  \        , F.y (baseY - d.value)\n\
  \        , F.width 50.0\n\
  \        , F.height d.value\n\
  \        , F.fill \"#C9A962\"\n\
  \        ] []\n\
  \    , elem Text\n\
  \        [ F.textAnchor \"middle\"\n\
  \        , textContent d.label\n\
  \        ] []\n\
  \    ]"

-- =============================================================================
-- Example 2: Labeled dots with varying size
-- =============================================================================

type DotDatum = { name :: String, size :: Number, idx :: Int }

dotData :: Array DotDatum
dotData = mapWithIndex (\i r -> { name: r.name, size: r.size, idx: i })
  [ { name: "Alpha", size: 12.0 }
  , { name: "Beta",  size: 8.0 }
  , { name: "Gamma", size: 18.0 }
  , { name: "Delta", size: 6.0 }
  ]

dotTree :: Tree
dotTree =
  let
    svgW = 340.0
    svgH = 200.0
    gap = 70.0
    startX = 60.0
    cy = 80.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ forEach "dots" Group dotData _.name \d ->
          let x = startX + Int.toNumber d.idx * gap
          in
            elem Group [ F.transform ("translate(" <> show x <> "," <> show cy <> ")") ]
              [ elem Circle
                  [ F.cx 0.0, F.cy 0.0
                  , F.r d.size
                  , F.fill "#C9A962"
                  , F.opacity "0.8"
                  ] []
              , elem Text
                  [ F.x 0.0, F.y (d.size + 16.0)
                  , F.textAnchor "middle"
                  , F.fontSize "10"
                  , F.fontFamily "'Josefin Sans', sans-serif"
                  , F.fill "#8B7355"
                  , staticStr "textContent" d.name
                  ] []
              ]
      ]

dotCode :: String
dotCode =
  "forEach \"dots\" Group data _.name \\d ->\n\
  \  elem Group [ transform ... ]\n\
  \    [ elem Circle\n\
  \        [ F.cx 0.0, F.cy 0.0\n\
  \        , F.r d.size\n\
  \        , F.fill \"#C9A962\"\n\
  \        ] []\n\
  \    , elem Text\n\
  \        [ F.textAnchor \"middle\"\n\
  \        , textContent d.name\n\
  \        ] []\n\
  \    ]"

-- =============================================================================
-- Example 3: Composed fragments (bars + trend line + markers)
-- =============================================================================

composedTree :: Tree
composedTree =
  let
    svgW = 340.0
    svgH = 260.0
    barW = 50.0
    gap = 12.0
    baseY = 230.0

    pointStr d =
      let x = 20.0 + Int.toNumber d.idx * (barW + gap) + barW / 2.0
          y = baseY - d.value
      in show x <> "," <> show y

    polyline = intercalate " " (map pointStr barData)
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ siblings
          [ -- Fragment 1: bars (translucent)
            forEach "bars" Rect barData _.label \d ->
              let
                x = 20.0 + Int.toNumber d.idx * (barW + gap)
                h = d.value
                y = baseY - h
              in
                elem Rect
                  [ F.x x, F.y y
                  , F.width barW, F.height h
                  , staticStr "rx" "2"
                  , F.fill "#C9A962"
                  , F.opacity "0.35"
                  ] []

          -- Fragment 2: trend polyline
          , elem Polygon
              [ F.points polyline
              , F.fill "none"
              , F.stroke "#C9A962"
              , F.strokeWidth 2.0
              ] []

          -- Fragment 3: dot markers
          , forEach "markers" Circle barData _.label \d ->
              let
                x = 20.0 + Int.toNumber d.idx * (barW + gap) + barW / 2.0
                y = baseY - d.value
              in
                elem Circle
                  [ F.cx x, F.cy y
                  , F.r 4.0
                  , F.fill "#E8D5A3"
                  ] []
          ]
      ]

composedCode :: String
composedCode =
  "siblings\n\
  \  [ -- Fragment 1: bars\n\
  \    forEach \"bars\" Rect data _.label \\d ->\n\
  \      elem Rect [ F.x ..., F.height d.value\n\
  \               , F.fill \"#C9A962\" ] []\n\
  \\n\
  \  -- Fragment 2: trend line\n\
  \  , elem Polygon\n\
  \      [ F.points polyline\n\
  \      , F.stroke \"#C9A962\" ] []\n\
  \\n\
  \  -- Fragment 3: markers\n\
  \  , forEach \"markers\" Circle data _.label \\d ->\n\
  \      elem Circle [ F.cx ..., F.cy ...\n\
  \                  , F.r 4.0 ] []\n\
  \  ]"

-- =============================================================================
-- Dispatch
-- =============================================================================

exampleTree :: Example -> Tree
exampleTree = case _ of
  ExBars -> barTree
  ExDots -> dotTree
  ExComposed -> composedTree

exampleCode :: Example -> String
exampleCode = case _ of
  ExBars -> barCode
  ExDots -> dotCode
  ExComposed -> composedCode

exampleCaption :: Example -> String
exampleCaption = case _ of
  ExBars -> "A bar chart: one forEach, each datum becomes a rect and a label."
  ExDots -> "Labeled dots: the template captures datum fields like name and size."
  ExComposed -> "Three fragments composed as siblings: bars, a trend line, and dot markers."
