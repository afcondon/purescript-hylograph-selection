-- | Chapter 0: Join vs Fold
-- |
-- | Introduces the conceptual space: D3's join is array→array (1:1).
-- | The hylographic fold generalizes both sides — any structure you can
-- | take apart on the left, any structure you can assemble on the right.
-- |
-- | Diagrams drawn with HATS, serving as library examples too.
module Chapters.Chapter0
  ( joinDiagram
  , foldDiagrams
  ) where

import Prelude

import Data.Array (length, mapWithIndex)
import Data.Int as Int
import Hylograph.HATS (Tree, elem, staticStr)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Configuration
-- =============================================================================

type DiagramConfig =
  { dotR :: Number
  , dotGap :: Number
  , arrowLen :: Number
  , labelSize :: String
  , structColor :: String    -- gold for structure dots
  , arrowColor :: String     -- dimmer for arrows
  , labelColor :: String     -- for structure labels
  }

cfg :: DiagramConfig
cfg =
  { dotR: 5.0
  , dotGap: 18.0
  , arrowLen: 60.0
  , labelSize: "11"
  , structColor: "#C9A962"
  , arrowColor: "#8B7355"
  , labelColor: "#888"
  }

-- =============================================================================
-- The Join: Array → Array (D3's model)
-- =============================================================================

-- | D3-style join: flat array on left, flat array on right, 1:1 arrows
joinDiagram :: Tree
joinDiagram =
  let
    n = 5
    h = Int.toNumber n * cfg.dotGap
    svgW = 260.0
    svgH = h + 50.0
    leftX = 50.0
    rightX = svgW - 50.0
    topY = 30.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH
      , F.width svgW
      , F.height svgH
      ]
      [ -- Left label
        elem Text
          [ F.x leftX, F.y 16.0
          , F.textAnchor "middle"
          , F.fontSize cfg.labelSize
          , F.fill cfg.labelColor
          , F.fontFamily "'Josefin Sans', sans-serif"
          , staticStr "textContent" "Data"
          ] []
      -- Right label
      , elem Text
          [ F.x rightX, F.y 16.0
          , F.textAnchor "middle"
          , F.fontSize cfg.labelSize
          , F.fill cfg.labelColor
          , F.fontFamily "'Josefin Sans', sans-serif"
          , staticStr "textContent" "DOM"
          ] []
      -- Left dots (array)
      , arrayDots leftX topY n cfg.structColor
      -- Right dots (array)
      , arrayDots rightX topY n cfg.structColor
      -- 1:1 arrows
      , arrows1to1 leftX rightX topY n
      -- "1 : 1" label centered
      , elem Text
          [ F.x (svgW / 2.0), F.y (topY + h / 2.0 + 4.0)
          , F.textAnchor "middle"
          , F.fontSize "10"
          , F.fill cfg.arrowColor
          , F.fontFamily "'Josefin Sans', sans-serif"
          , staticStr "textContent" "1 : 1"
          ] []
      ]

-- =============================================================================
-- The Fold: Various structure pairings
-- =============================================================================

-- | Array of fold diagrams showing different structural combinations
foldDiagrams :: Array { label :: String, tree :: Tree }
foldDiagrams =
  [ { label: "Array \x2192 Tree", tree: arrayToTreeDiagram }
  , { label: "Tree \x2192 Array", tree: treeToArrayDiagram }
  , { label: "Tree \x2192 Tree", tree: treeToTreeDiagram }
  ]

arrayToTreeDiagram :: Tree
arrayToTreeDiagram =
  let
    svgW = 220.0
    svgH = 120.0
    leftX = 40.0
    rightX = svgW - 60.0
    topY = 20.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ arrayDots leftX topY 4 cfg.structColor
      , foldArrow (leftX + 15.0) (rightX - 35.0) (topY + 25.0)
      , treeShape rightX topY cfg.structColor
      ]

treeToArrayDiagram :: Tree
treeToArrayDiagram =
  let
    svgW = 220.0
    svgH = 120.0
    leftX = 60.0
    rightX = svgW - 40.0
    topY = 20.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ treeShape leftX topY cfg.structColor
      , foldArrow (leftX + 35.0) (rightX - 15.0) (topY + 25.0)
      , arrayDots rightX topY 4 cfg.structColor
      ]

treeToTreeDiagram :: Tree
treeToTreeDiagram =
  let
    svgW = 220.0
    svgH = 120.0
    leftX = 60.0
    rightX = svgW - 60.0
    topY = 20.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ treeShape leftX topY cfg.structColor
      , foldArrow (leftX + 35.0) (rightX - 35.0) (topY + 25.0)
      , treeShape rightX topY cfg.structColor
      ]

-- =============================================================================
-- Shape primitives
-- =============================================================================

-- | Vertical column of dots (an array)
arrayDots :: Number -> Number -> Int -> String -> Tree
arrayDots x topY n color =
  elem Group []
    (mapWithIndex (\i _ ->
      elem Circle
        [ F.cx x
        , F.cy (topY + Int.toNumber i * cfg.dotGap)
        , F.r cfg.dotR
        , F.fill color
        ] []
    ) (arrayOfN n))

-- | Simple tree shape: root with two children, left child has one child
treeShape :: Number -> Number -> String -> Tree
treeShape rootX topY color =
  let
    r = cfg.dotR
    rootY = topY
    childY = topY + 30.0
    grandY = topY + 60.0
    leftX = rootX - 20.0
    rightX = rootX + 20.0
    grandX = leftX - 12.0
  in
    elem Group []
      [ -- Edges
        edge rootX rootY leftX childY
      , edge rootX rootY rightX childY
      , edge leftX childY grandX grandY
      -- Nodes
      , elem Circle [ F.cx rootX, F.cy rootY, F.r r, F.fill color ] []
      , elem Circle [ F.cx leftX, F.cy childY, F.r r, F.fill color ] []
      , elem Circle [ F.cx rightX, F.cy childY, F.r r, F.fill color ] []
      , elem Circle [ F.cx grandX, F.cy grandY, F.r r, F.fill color ] []
      ]

-- | 1:1 arrows between two columns
arrows1to1 :: Number -> Number -> Number -> Int -> Tree
arrows1to1 leftX rightX topY n =
  elem Group []
    (mapWithIndex (\i _ ->
      let y = topY + Int.toNumber i * cfg.dotGap
      in elem Line
        [ F.x1 (leftX + cfg.dotR + 2.0)
        , F.y1 y
        , F.x2 (rightX - cfg.dotR - 2.0)
        , F.y2 y
        , F.stroke cfg.arrowColor
        , staticStr "stroke-width" "1"
        , staticStr "stroke-dasharray" "3,3"
        ] []
    ) (arrayOfN n))

-- | Single fold arrow (→)
foldArrow :: Number -> Number -> Number -> Tree
foldArrow x1 x2 y =
  elem Group []
    [ elem Line
        [ F.x1 x1, F.y1 y, F.x2 x2, F.y2 y
        , F.stroke cfg.arrowColor
        , staticStr "stroke-width" "1.5"
        ] []
    -- arrowhead
    , elem Path
        [ F.d ("M" <> show x2 <> "," <> show y
            <> "l-6,-3.5 l0,7 z")
        , F.fill cfg.arrowColor
        ] []
    ]

-- | Edge line between two points
edge :: Number -> Number -> Number -> Number -> Tree
edge x1 y1 x2 y2 =
  elem Line
    [ F.x1 x1, F.y1 y1, F.x2 x2, F.y2 y2
    , F.stroke cfg.arrowColor
    , staticStr "stroke-width" "1"
    ] []

-- | Create an array of N units (for mapWithIndex)
arrayOfN :: Int -> Array Unit
arrayOfN n = mapWithIndex (\_ _ -> unit) (replicate n unit)
  where
  replicate :: Int -> Unit -> Array Unit
  replicate 0 _ = []
  replicate k _ = [unit] <> replicate (k - 1) unit
