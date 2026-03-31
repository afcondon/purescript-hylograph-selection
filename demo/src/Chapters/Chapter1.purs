-- | Chapter 1: The Fold in Action
-- |
-- | Shows the fold as an equation: data + template = output.
-- | Three tabs (HTML, SVG, Markdown) show different templates.
-- |
-- | Cross-panel CoordinatedHighlight:
-- | - Hover a data item → its output line highlights
-- | - Hover the Li/Circle/Text node in the template → all output items highlight
-- | - Hover the Ul/SVG/G node → the container tags highlight
module Chapters.Chapter1
  ( dataArrayTree
  , FoldExample(..)
  , treeFor
  , templateDiagram
  , outputTree
  ) where

import Prelude

import Data.Array (mapWithIndex, length)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Hylograph.HATS (Tree, ThunkedBehavior, elem, forEach, staticStr, withBehaviors, onCoordinatedHighlight)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Behavior.Types (HighlightClass(..))
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Data
-- =============================================================================

type IndexedItem = { name :: String, idx :: Int }

indexedItems :: Array IndexedItem
indexedItems = mapWithIndex (\i name -> { name, idx: i })
  [ "Apples", "Bananas", "Cherries", "Dates", "Figs" ]

itemCount :: Int
itemCount = length indexedItems

-- =============================================================================
-- Which fold example is selected
-- =============================================================================

data FoldExample = ExHTML | ExSVG | ExMarkdown

containerTag :: FoldExample -> String
containerTag = case _ of
  ExHTML -> "Ul"
  ExSVG -> "SVG"
  ExMarkdown -> "G"

itemTag :: FoldExample -> String
itemTag = case _ of
  ExHTML -> "Li"
  ExSVG -> "Circle"
  ExMarkdown -> "Text"

-- =============================================================================
-- Highlight behaviors
-- =============================================================================

-- | Data items: Primary on own name only
hlItem :: String -> ThunkedBehavior
hlItem name = onCoordinatedHighlight
  { identify: name
  , classify: \hoveredId ->
      if hoveredId == name then Primary
      else Neutral
  , group: Nothing
  }

-- | Container tag lines (<ul>, </ul>): Primary on tag-container hover
hlContainer :: ThunkedBehavior
hlContainer = onCoordinatedHighlight
  { identify: "tag-container"
  , classify: \hoveredId ->
      if hoveredId == "tag-container" then Primary
      else Neutral
  , group: Nothing
  }

-- | Item output text (e.g., "Apples"): Primary on own name
hlOutputText :: String -> ThunkedBehavior
hlOutputText name = onCoordinatedHighlight
  { identify: name
  , classify: \hoveredId ->
      if hoveredId == name then Primary
      else Neutral
  , group: Nothing
  }

-- | Tag markers in output (<li>, </li>): Primary when item template hovered
hlOutputTag :: ThunkedBehavior
hlOutputTag = onCoordinatedHighlight
  { identify: "tag-item-marker"
  , classify: \hoveredId ->
      if hoveredId == "tag-item" then Primary
      else Neutral
  , group: Nothing
  }

-- | Template container node: identifies as tag-container
hlTemplateContainer :: ThunkedBehavior
hlTemplateContainer = onCoordinatedHighlight
  { identify: "tag-container"
  , classify: \hoveredId ->
      if hoveredId == "tag-container" then Primary
      else Neutral
  , group: Nothing
  }

-- | Template item node: identifies as tag-item
hlTemplateItem :: ThunkedBehavior
hlTemplateItem = onCoordinatedHighlight
  { identify: "tag-item"
  , classify: \hoveredId ->
      if hoveredId == "tag-item" then Primary
      else Neutral
  , group: Nothing
  }

-- =============================================================================
-- Rendering config
-- =============================================================================

lineH :: Number
lineH = 24.0

lineGap :: Number
lineGap = 2.0

monoFont :: String
monoFont = "'JetBrains Mono', monospace"

decoFont :: String
decoFont = "'Josefin Sans', sans-serif"

gold :: String
gold = "#C9A962"

goldDim :: String
goldDim = "#8B7355"

goldBright :: String
goldBright = "#E8D5A3"

-- =============================================================================
-- Data Panel
-- =============================================================================

dataArrayTree :: Tree
dataArrayTree =
  let
    boxH = 32.0
    gap = 4.0
    svgW = 160.0
    svgH = Int.toNumber itemCount * (boxH + gap) + 16.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ forEach "data" Group indexedItems _.name \item ->
          let y = 8.0 + Int.toNumber item.idx * (boxH + gap)
          in
            withBehaviors [ hlItem item.name ] $
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
                  [ F.x 12.0, F.y (boxH / 2.0 + 4.0)
                  , F.fontSize "12", F.fontFamily monoFont
                  , F.fill gold
                  , staticStr "textContent" ("\"" <> item.name <> "\"")
                  ] []
              ]
      ]

-- =============================================================================
-- HATS trees for each tab (used by interpretMeta if needed)
-- =============================================================================

treeFor :: FoldExample -> Tree
treeFor = case _ of
  ExHTML ->
    elem Ul []
      [ forEach "items" Li indexedItems _.name \item ->
          elem Li [ staticStr "textContent" item.name ] []
      ]
  ExSVG ->
    elem SVG [ F.viewBox 0.0 0.0 200.0 200.0 ]
      [ forEach "items" Circle indexedItems _.name \item ->
          elem Circle
            [ F.cx 100.0
            , F.cy (20.0 + Int.toNumber item.idx * 36.0)
            , F.r 14.0
            ] []
      ]
  ExMarkdown ->
    elem Group []
      [ forEach "items" Text indexedItems _.name \item ->
          elem Text [ staticStr "textContent" ("- " <> item.name) ] []
      ]

-- =============================================================================
-- Custom template diagram (replaces interpretMeta for this chapter)
-- =============================================================================

-- | A hand-crafted diagram showing: Container → Fold ×5 → Item
-- | with CoordinatedHighlight on the Container and Item nodes.
templateDiagram :: FoldExample -> Tree
templateDiagram ex =
  let
    svgW = 400.0
    svgH = 100.0
    nodeW = 90.0
    nodeH = 36.0
    foldW = 130.0
    foldH = 56.0

    -- Three columns: container, fold, item
    x1 = 20.0
    x2 = (svgW - foldW) / 2.0
    x3 = svgW - nodeW - 20.0
    yNode = (svgH - nodeH) / 2.0
    yFold = (svgH - foldH) / 2.0

    -- Connector lines
    connY = svgH / 2.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 svgW svgH, F.width svgW, F.height svgH ]
      [ -- Connectors
        elem Line
          [ F.x1 (x1 + nodeW), F.y1 connY
          , F.x2 x2, F.y2 connY
          , F.stroke goldDim, staticStr "stroke-width" "1"
          ] []
      , elem Line
          [ F.x1 (x2 + foldW), F.y1 connY
          , F.x2 x3, F.y2 connY
          , F.stroke goldDim, staticStr "stroke-width" "1"
          , staticStr "stroke-dasharray" "4,3"
          ] []

      -- Container node
      , withBehaviors [ hlTemplateContainer ] $
        elem Group [ F.transform ("translate(" <> show x1 <> "," <> show yNode <> ")") ]
          [ elem Rect
              [ F.x 0.0, F.y 0.0, F.width nodeW, F.height nodeH
              , staticStr "rx" "3"
              , F.fill "#111", F.stroke goldDim, staticStr "stroke-width" "1"
              ] []
          , elem Text
              [ F.x (nodeW / 2.0), F.y (nodeH / 2.0 + 5.0)
              , F.textAnchor "middle"
              , F.fontSize "14", F.fontFamily decoFont
              , F.fill gold
              , staticStr "textContent" (containerTag ex)
              ] []
          ]

      -- Fold node (center, taller)
      , elem Group [ F.transform ("translate(" <> show x2 <> "," <> show yFold <> ")") ]
          [ elem Rect
              [ F.x 0.0, F.y 0.0, F.width foldW, F.height foldH
              , staticStr "rx" "3"
              , F.fill "#111", F.stroke gold, staticStr "stroke-width" "1.5"
              ] []
          , elem Text
              [ F.x (foldW / 2.0), F.y 22.0
              , F.textAnchor "middle"
              , F.fontSize "12", F.fontFamily decoFont
              , F.fill gold
              , staticStr "textContent" "Fold \"items\""
              ] []
          , elem Text
              [ F.x (foldW / 2.0), F.y 42.0
              , F.textAnchor "middle"
              , F.fontSize "10", F.fontFamily monoFont
              , F.fill goldDim
              , staticStr "textContent" ("\x00d7" <> show itemCount)
              ] []
          ]

      -- Item node (with stacked card effect)
      , withBehaviors [ hlTemplateItem ] $
        elem Group [ F.transform ("translate(" <> show x3 <> "," <> show yNode <> ")") ]
          [ -- Shadow cards
            elem Rect
              [ F.x 6.0, F.y 6.0, F.width nodeW, F.height nodeH
              , staticStr "rx" "3"
              , F.fill "none", F.stroke goldDim, staticStr "stroke-width" "0.5"
              ] []
          , elem Rect
              [ F.x 3.0, F.y 3.0, F.width nodeW, F.height nodeH
              , staticStr "rx" "3"
              , F.fill "none", F.stroke goldDim, staticStr "stroke-width" "0.5"
              ] []
          -- Front card
          , elem Rect
              [ F.x 0.0, F.y 0.0, F.width nodeW, F.height nodeH
              , staticStr "rx" "3"
              , F.fill "#111", F.stroke goldDim, staticStr "stroke-width" "1"
              ] []
          , elem Text
              [ F.x (nodeW / 2.0), F.y (nodeH / 2.0 + 5.0)
              , F.textAnchor "middle"
              , F.fontSize "14", F.fontFamily decoFont
              , F.fill gold
              , staticStr "textContent" (itemTag ex)
              ] []
          ]
      ]

-- =============================================================================
-- Output panel: SVG text lines with cross-highlighting
-- =============================================================================

outputTree :: FoldExample -> Tree
outputTree ex =
  let
    step = lineH + lineGap
    lines = outputLines ex
    totalLines = length lines
    svgH = Int.toNumber totalLines * step + 16.0
  in
    elem SVG
      [ F.viewBox 0.0 0.0 300.0 svgH, F.width 300.0, F.height svgH ]
      (mapWithIndex (\i line -> line (8.0 + Int.toNumber i * step)) lines)

-- =============================================================================
-- Output line rendering — split spans for granular highlighting
-- =============================================================================

-- | Approximate character width for JetBrains Mono at 11px
charW :: Number
charW = 6.6

-- | A text span with its own highlight behavior and background rect
hlSpan :: ThunkedBehavior -> String -> Number -> Number -> Number -> Tree
hlSpan behavior text xPos yPos spanW =
  withBehaviors [ behavior ] $
  elem Group [ F.transform ("translate(" <> show xPos <> "," <> show yPos <> ")") ]
    [ elem Rect
        [ F.x 0.0, F.y 0.0
        , F.width spanW, F.height lineH
        , F.fill "#111"
        ] []
    , elem Text
        [ F.x 2.0, F.y (lineH - 7.0)
        , F.fontSize "11", F.fontFamily monoFont
        , F.fill goldBright
        , staticStr "textContent" text
        ] []
    ]

-- | A plain text span (no highlight behavior)
plainSpan :: String -> String -> Number -> Number -> Number -> Tree
plainSpan color text xPos yPos spanW =
  elem Group [ F.transform ("translate(" <> show xPos <> "," <> show yPos <> ")") ]
    [ elem Rect
        [ F.x 0.0, F.y 0.0
        , F.width spanW, F.height lineH
        , F.fill "#111"
        ] []
    , elem Text
        [ F.x 2.0, F.y (lineH - 7.0)
        , F.fontSize "11", F.fontFamily monoFont
        , F.fill color
        , staticStr "textContent" text
        ] []
    ]

-- | Container tag line (<ul>, </ul>)
containerLine :: String -> Number -> Tree
containerLine text yPos =
  withBehaviors [ hlContainer ] $
  elem Group [ F.transform ("translate(8," <> show yPos <> ")") ]
    [ elem Rect
        [ F.x 0.0, F.y 0.0
        , F.width (charW * Int.toNumber (strLen text) + 4.0)
        , F.height lineH
        , F.fill "#111"
        ] []
    , elem Text
        [ F.x 2.0, F.y (lineH - 7.0)
        , F.fontSize "11", F.fontFamily monoFont
        , F.fill goldDim
        , staticStr "textContent" text
        ] []
    ]

-- | HTML item line: <li> + name + </li> as three separate spans
htmlItemLine :: String -> Number -> Tree
htmlItemLine name yPos =
  let
    indent = 8.0
    tagOpen = "<li>"
    tagClose = "</li>"
    tagW = charW * 4.0 + 4.0
    nameW = charW * Int.toNumber (strLen name) + 4.0
    x1 = indent
    x2 = x1 + tagW
    x3 = x2 + nameW
  in
    elem Group []
      [ hlSpan hlOutputTag tagOpen x1 yPos tagW
      , hlSpan (hlOutputText name) name x2 yPos nameW
      , hlSpan hlOutputTag tagClose x3 yPos tagW
      ]

-- | SVG item line: <circle .../> with tag and attr highlighting
svgItemLine :: String -> Int -> Number -> Tree
svgItemLine name idx yPos =
  let
    indent = 8.0
    tagOpen = "<circle "
    attrs = "cx=\"100\" cy=\"" <> show (20 + idx * 36) <> "\" r=\"14\""
    tagClose = "/>"
    openW = charW * Int.toNumber (strLen tagOpen) + 4.0
    attrW = charW * Int.toNumber (strLen attrs) + 4.0
    closeW = charW * 2.0 + 4.0
    x1 = indent
    x2 = x1 + openW
    x3 = x2 + attrW
  in
    elem Group []
      [ hlSpan hlOutputTag tagOpen x1 yPos openW
      , hlSpan (hlOutputText name) attrs x2 yPos attrW
      , hlSpan hlOutputTag tagClose x3 yPos closeW
      ]

-- | Markdown item line: "- " + name
mdItemLine :: String -> Number -> Tree
mdItemLine name yPos =
  let
    indent = 8.0
    bullet = "- "
    bulletW = charW * 2.0 + 4.0
    nameW = charW * Int.toNumber (strLen name) + 4.0
    x1 = indent
    x2 = x1 + bulletW
  in
    elem Group []
      [ plainSpan goldDim bullet x1 yPos bulletW
      , hlSpan (hlOutputText name) name x2 yPos nameW
      ]

outputLines :: FoldExample -> Array (Number -> Tree)
outputLines = case _ of
  ExHTML ->
    [ containerLine "<ul>" ]
    <> (indexedItems <#> \item -> htmlItemLine item.name)
    <> [ containerLine "</ul>" ]

  ExSVG ->
    [ containerLine "<svg viewBox=\"0 0 200 200\">" ]
    <> (indexedItems <#> \item -> svgItemLine item.name item.idx)
    <> [ containerLine "</svg>" ]

  ExMarkdown ->
    indexedItems <#> \item -> mdItemLine item.name

-- =============================================================================
-- Helpers
-- =============================================================================

strLen :: String -> Int
strLen = SCU.length
