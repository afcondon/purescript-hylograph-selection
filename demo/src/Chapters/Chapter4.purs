-- | Chapter 4: HATS Revealed
-- |
-- | Shows actual PureScript HATS code alongside rendered output.
-- | Small, readable examples demonstrating the embedded DSL.
module Chapters.Chapter4
  ( Example(..)
  , CodeLine
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

-- | Code lines: { text, comment } — comment rendered dimmer
type CodeLine = { text :: String, comment :: String }

cl :: String -> String -> CodeLine
cl t c = { text: t, comment: c }

barCode :: Array CodeLine
barCode =
  [ cl "forEach \"bars\" Group"  "-- iterate the data"
  , cl "  data"                  "-- your Array, Map, Tree..."
  , cl "  _.label"               "-- key function (a -> String)"
  , cl "  \\d ->"                "-- template receives each datum"
  , cl "  elem Group"            "-- outer element type"
  , cl "    [ transform ... ]"   "-- attrs: normal PureScript"
  , cl "    [ elem Rect"         ""
  , cl "        [ F.y (baseY - d.value)"  "-- d is type-checked!"
  , cl "        , F.width 50.0"  ""
  , cl "        , F.height d.value" "-- datum fields are lambdas"
  , cl "        , F.fill \"#C9A962\"" ""
  , cl "        ] []"            "-- [] = children, behaviors"
  , cl "    , elem Text"         ""
  , cl "        [ F.textAnchor \"middle\"" ""
  , cl "        , textContent d.label"     "-- also type-checked"
  , cl "        ] []"            ""
  , cl "    ]"                   ""
  ]

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

dotCode :: Array CodeLine
dotCode =
  [ cl "forEach \"dots\" Group"  "-- name, element type"
  , cl "  data _.name \\d ->"    "-- key fn, then template"
  , cl "  elem Group [ ... ]"    ""
  , cl "    [ elem Circle"       ""
  , cl "        [ F.cx 0.0"      ""
  , cl "        , F.cy 0.0"      ""
  , cl "        , F.r d.size"    "-- size from datum"
  , cl "        , F.fill \"#C9A962\"" ""
  , cl "        ] []"            ""
  , cl "    , elem Text"         ""
  , cl "        [ textContent d.name" "-- name from datum"
  , cl "        ] []"            "-- no children needed"
  , cl "    ]"                   ""
  ]

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

composedCode :: Array CodeLine
composedCode =
  [ cl "siblings"                "-- compose fragments"
  , cl "  ["                     ""
  , cl "    -- Fragment 1: bars" ""
  , cl "    forEach \"bars\" Rect data _.label \\d ->" ""
  , cl "      elem Rect"         ""
  , cl "        [ F.height d.value"  "-- datum drives height"
  , cl "        , F.fill \"#C9A962\"" ""
  , cl "        ] []"            ""
  , cl ""                        ""
  , cl "  -- Fragment 2: trend"  ""
  , cl "  , elem Polygon"        "-- static element, no fold"
  , cl "      [ F.points polyline" "-- computed from data"
  , cl "      , F.stroke \"#C9A962\"" ""
  , cl "      ] []"              ""
  , cl ""                        ""
  , cl "  -- Fragment 3: markers" ""
  , cl "  , forEach \"markers\" Circle data ..." ""
  , cl "  ]"                     ""
  ]

-- =============================================================================
-- Dispatch
-- =============================================================================

exampleTree :: Example -> Tree
exampleTree = case _ of
  ExBars -> barTree
  ExDots -> dotTree
  ExComposed -> composedTree

exampleCode :: Example -> Array CodeLine
exampleCode = case _ of
  ExBars -> barCode
  ExDots -> dotCode
  ExComposed -> composedCode

exampleCaption :: Example -> String
exampleCaption = case _ of
  ExBars -> "A bar chart: one forEach, each datum becomes a rect and a label."
  ExDots -> "Labeled dots: the template captures datum fields like name and size."
  ExComposed -> "Three fragments composed as siblings: bars, a trend line, and dot markers."
