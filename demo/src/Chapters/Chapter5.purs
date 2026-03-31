-- | Chapter 5: Multiple Interpreters
-- |
-- | The same HATS tree can be interpreted in different ways.
-- | Shows English and SVG interpreters side by side.
module Chapters.Chapter5
  ( sampleTree
  , englishOutput
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Int as Int
import Hylograph.HATS (Tree, elem, forEach, staticStr, siblings)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types (ElementType(..))
import Hylograph.Interpreter.English (runEnglish)

-- =============================================================================
-- A sample tree to interpret multiple ways
-- =============================================================================

type Datum = { label :: String, value :: Number, idx :: Int }

sampleData :: Array Datum
sampleData = mapWithIndex (\i r -> { label: r.label, value: r.value, idx: i })
  [ { label: "Alpha",   value: 160.0 }
  , { label: "Beta",    value: 90.0 }
  , { label: "Gamma",   value: 200.0 }
  , { label: "Delta",   value: 130.0 }
  ]

-- | A bar chart with labels — small enough to read as English
sampleTree :: Tree
sampleTree =
  let
    svgW = 320.0
    svgH = 250.0
    barW = 55.0
    gap = 14.0
    baseY = 220.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ forEach "bars" Group sampleData _.label \d ->
          let
            x = 18.0 + Int.toNumber d.idx * (barW + gap)
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

-- | The English interpretation of the sample tree
englishOutput :: String
englishOutput = runEnglish sampleTree
