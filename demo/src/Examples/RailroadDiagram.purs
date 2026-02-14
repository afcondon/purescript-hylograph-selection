-- | Railroad Diagram - HATS Version
-- |
-- | Visualizes grammar rules as railroad diagrams (syntax diagrams).
-- | These show the valid paths through a grammar production.
-- |
-- | Elements:
-- | - Terminal: literal token (rounded rect)
-- | - NonTerminal: reference to another rule (rect)
-- | - Sequence: elements in order (horizontal flow)
-- | - Choice: alternatives (vertical branches)
-- | - Optional: can be skipped (bypass path)
-- | - Repeat: zero or more (loop back)
-- |
-- | "Pass a grammar in, get a diagram out."
module Examples.RailroadDiagram
  ( railroadTree
  , RailroadConfig
  , defaultRailroadConfig
  , Grammar(..)
  , sampleJsonValue
  , sampleExpression
  ) where

import Prelude

import Data.Array (length, foldl)
import Data.Array as Array
import Data.Int as Int
import Data.Number (abs)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as SCU
import Hylograph.HATS.Friendly as F
import Hylograph.HATS (Tree, elem, staticStr, staticNum, forEach)
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Grammar AST
-- =============================================================================

-- | Grammar production rule
data Grammar
  = Terminal String           -- Literal token: "if", "+", "123"
  | NonTerminal String        -- Reference to another rule
  | Sequence (Array Grammar)  -- Elements in order
  | Choice (Array Grammar)    -- One of several alternatives
  | Optional Grammar          -- Can be skipped
  | Repeat Grammar            -- Zero or more (with separator)
  | RepeatOne Grammar         -- One or more

-- =============================================================================
-- Configuration
-- =============================================================================

type RailroadConfig =
  { terminalHeight :: Number
  , terminalPadding :: Number
  , nonTerminalHeight :: Number
  , nonTerminalPadding :: Number
  , verticalGap :: Number      -- Gap between choice branches
  , horizontalGap :: Number    -- Gap between sequence elements
  , fontSize :: Number
  , lineWidth :: Number
  , cornerRadius :: Number
  }

defaultRailroadConfig :: RailroadConfig
defaultRailroadConfig =
  { terminalHeight: 28.0
  , terminalPadding: 12.0
  , nonTerminalHeight: 28.0
  , nonTerminalPadding: 12.0
  , verticalGap: 12.0       -- Compact spacing; nearRail uses straight lines for close branches
  , horizontalGap: 16.0
  , fontSize: 14.0
  , lineWidth: 2.0
  , cornerRadius: 10.0
  }

-- =============================================================================
-- Sample Grammars
-- =============================================================================

-- | JSON value grammar (simplified)
sampleJsonValue :: Grammar
sampleJsonValue = Choice
  [ Terminal "null"
  , Terminal "true"
  , Terminal "false"
  , NonTerminal "number"
  , NonTerminal "string"
  , Sequence [ Terminal "[", Optional (NonTerminal "elements"), Terminal "]" ]
  , Sequence [ Terminal "{", Optional (NonTerminal "members"), Terminal "}" ]
  ]

-- | Simple arithmetic expression
sampleExpression :: Grammar
sampleExpression = Sequence
  [ NonTerminal "term"
  , Repeat (Sequence [ Choice [ Terminal "+", Terminal "-" ], NonTerminal "term" ])
  ]

-- =============================================================================
-- Layout Computation
-- =============================================================================

-- | Computed layout for a grammar element
type Layout =
  { width :: Number
  , height :: Number
  , centerY :: Number  -- Y position of the main rail
  }

-- | Compute layout dimensions for a grammar element
computeLayout :: RailroadConfig -> Grammar -> Layout
computeLayout cfg = case _ of
  Terminal text ->
    let w = estimateTextWidth cfg.fontSize text + cfg.terminalPadding * 2.0
    in { width: w, height: cfg.terminalHeight, centerY: cfg.terminalHeight / 2.0 }

  NonTerminal text ->
    let w = estimateTextWidth cfg.fontSize text + cfg.nonTerminalPadding * 2.0
    in { width: w, height: cfg.nonTerminalHeight, centerY: cfg.nonTerminalHeight / 2.0 }

  Sequence items ->
    let
      layouts = map (computeLayout cfg) items
      totalWidth = foldl (\acc l -> acc + l.width) 0.0 layouts
                 + cfg.horizontalGap * Int.toNumber (max 0 (length items - 1))
      maxHeight = foldl (\acc l -> max acc l.height) 0.0 layouts
      maxCenterY = foldl (\acc l -> max acc l.centerY) 0.0 layouts
    in
      { width: totalWidth, height: maxHeight, centerY: maxCenterY }

  Choice items ->
    let
      layouts = map (computeLayout cfg) items
      maxWidth = foldl (\acc l -> max acc l.width) 0.0 layouts
      -- Add space for the branching lines
      totalWidth = maxWidth + cfg.cornerRadius * 4.0
      totalHeight = foldl (\acc l -> acc + l.height) 0.0 layouts
                  + cfg.verticalGap * Int.toNumber (max 0 (length items - 1))
    in
      { width: totalWidth, height: totalHeight, centerY: totalHeight / 2.0 }

  Optional inner ->
    let
      innerLayout = computeLayout cfg inner
      -- Add bypass path height
      bypassHeight = cfg.cornerRadius * 2.0 + cfg.verticalGap
    in
      { width: innerLayout.width + cfg.cornerRadius * 4.0
      , height: innerLayout.height + bypassHeight
      , centerY: bypassHeight + innerLayout.centerY
      }

  Repeat inner ->
    let
      innerLayout = computeLayout cfg inner
      -- Add loop-back path height
      loopHeight = cfg.cornerRadius * 2.0 + cfg.verticalGap
    in
      { width: innerLayout.width + cfg.cornerRadius * 4.0
      , height: innerLayout.height + loopHeight
      , centerY: innerLayout.centerY
      }

  RepeatOne inner ->
    computeLayout cfg (Repeat inner)  -- Same layout as Repeat

-- | Rough text width estimate (monospace-ish)
estimateTextWidth :: Number -> String -> Number
estimateTextWidth fontSize text =
  Int.toNumber (SCU.length text) * fontSize * 0.6

-- =============================================================================
-- HATS Tree Generation
-- =============================================================================

-- | Render a grammar as a railroad diagram
railroadTree :: RailroadConfig -> Grammar -> Tree
railroadTree cfg grammar =
  let
    layout = computeLayout cfg grammar
    padding = 20.0
    totalWidth = layout.width + padding * 2.0
    totalHeight = layout.height + padding * 2.0
  in
    elem SVG
      [ F.width totalWidth
      , F.height totalHeight
      , F.attr "viewBox" ("0 0 " <> show totalWidth <> " " <> show totalHeight)
      , F.class_ "railroad-diagram"
      ]
      [ -- Background
        elem Rect
          [ F.width totalWidth
          , F.height totalHeight
          , F.fill "#fefefe"
          , staticNum "rx" 4.0
          ] []
      -- Entry line
      , elem Line
          [ F.x1 0.0
          , F.y1 (padding + layout.centerY)
          , F.x2 padding
          , F.y2 (padding + layout.centerY)
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          ] []
      -- Exit line
      , elem Line
          [ F.x1 (padding + layout.width)
          , F.y1 (padding + layout.centerY)
          , F.x2 totalWidth
          , F.y2 (padding + layout.centerY)
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          ] []
      -- Grammar content
      , renderGrammar cfg padding (padding + layout.centerY) grammar
      ]

-- | Render a grammar element at a given position
-- | x: left edge, railY: y position of the main rail
renderGrammar :: RailroadConfig -> Number -> Number -> Grammar -> Tree
renderGrammar cfg x railY = case _ of
  Terminal text ->
    let
      layout = computeLayout cfg (Terminal text)
      boxY = railY - layout.height / 2.0
    in
      elem Group []
        [ -- Rounded rectangle (terminal style)
          elem Rect
            [ F.x x
            , F.y boxY
            , F.width layout.width
            , F.height layout.height
            , staticNum "rx" (layout.height / 2.0)  -- Fully rounded ends
            , F.fill "#d4f0d4"
            , F.stroke "#2d882d"
            , F.strokeWidth cfg.lineWidth
            ] []
        , elem Text
            [ F.x (x + layout.width / 2.0)
            , F.y (railY + cfg.fontSize * 0.35)
            , F.textAnchor "middle"
            , staticNum "font-size" cfg.fontSize
            , F.fontFamily "monospace"
            , F.fill "#1a5c1a"
            , staticStr "textContent" text
            ] []
        ]

  NonTerminal text ->
    let
      layout = computeLayout cfg (NonTerminal text)
      boxY = railY - layout.height / 2.0
    in
      elem Group []
        [ -- Rectangle (non-terminal style)
          elem Rect
            [ F.x x
            , F.y boxY
            , F.width layout.width
            , F.height layout.height
            , staticNum "rx" 4.0
            , F.fill "#e8e8f8"
            , F.stroke "#4444aa"
            , F.strokeWidth cfg.lineWidth
            ] []
        , elem Text
            [ F.x (x + layout.width / 2.0)
            , F.y (railY + cfg.fontSize * 0.35)
            , F.textAnchor "middle"
            , staticNum "font-size" cfg.fontSize
            , staticStr "font-style" "italic"
            , F.fill "#333388"
            , staticStr "textContent" text
            ] []
        ]

  Sequence items ->
    renderSequence cfg x railY items

  Choice items ->
    renderChoice cfg x railY items

  Optional inner ->
    renderOptional cfg x railY inner

  Repeat inner ->
    renderRepeat cfg x railY inner

  RepeatOne inner ->
    renderRepeat cfg x railY inner  -- Same visual as Repeat for now

-- | Render a sequence of grammar elements
-- | Uses forEach to declare iteration over sequence items
renderSequence :: RailroadConfig -> Number -> Number -> Array Grammar -> Tree
renderSequence cfg startX railY items =
  let
    -- Pre-compute positioned items (unfold phase)
    positionedItems = computePositionedSequence cfg startX railY items
  in
    -- forEach over positioned items (fold phase via HATS)
    forEach "sequence" Group positionedItems (\p -> show p.index) \pos ->
      elem Group []
        [ renderGrammar cfg pos.x railY pos.grammar
        -- Connecting line to next (if not last)
        , case pos.nextX of
            Just nx ->
              elem Line
                [ F.x1 (pos.x + pos.width)
                , F.y1 railY
                , F.x2 nx
                , F.y2 railY
                , F.stroke "#333"
                , F.strokeWidth cfg.lineWidth
                ] []
            Nothing -> elem Group [] []
        ]

-- | Positioned sequence item with all layout info
type PositionedSeqItem =
  { index :: Int
  , grammar :: Grammar
  , x :: Number
  , width :: Number
  , nextX :: Maybe Number  -- Nothing for last item
  }

-- | Compute positioned items for a sequence
computePositionedSequence :: RailroadConfig -> Number -> Number -> Array Grammar -> Array PositionedSeqItem
computePositionedSequence cfg startX _ items =
  let
    n = length items
    go :: { x :: Number, acc :: Array PositionedSeqItem, idx :: Int } -> Grammar -> { x :: Number, acc :: Array PositionedSeqItem, idx :: Int }
    go { x, acc, idx } grammar =
      let
        layout = computeLayout cfg grammar
        nextX = x + layout.width + cfg.horizontalGap
        isLast = idx >= n - 1
      in
        { x: nextX
        , idx: idx + 1
        , acc: acc <>
            [ { index: idx
              , grammar
              , x
              , width: layout.width
              , nextX: if isLast then Nothing else Just nextX
              }
            ]
        }
  in
    (foldl go { x: startX, acc: [], idx: 0 } items).acc

-- | Render choice alternatives
-- | Uses forEach to declare iteration over alternatives
renderChoice :: RailroadConfig -> Number -> Number -> Array Grammar -> Tree
renderChoice cfg x railY items =
  let
    -- Pre-compute positioned branches (unfold phase)
    positionedBranches = computePositionedChoice cfg x railY items

    -- Use straight line if branch is too close to rail for a proper curve
    -- (within cornerRadius means we can't draw a clean arc)
    nearRail :: Number -> Boolean
    nearRail branchY = abs (branchY - railY) < cfg.cornerRadius
  in
    -- forEach over positioned branches (fold phase via HATS)
    forEach "alternatives" Group positionedBranches (\b -> show b.index) \branch ->
      elem Group []
        [ -- Entry curve/line to branch
          if nearRail branch.branchY
          then elem Line
            [ F.x1 x
            , F.y1 railY
            , F.x2 branch.itemX
            , F.y2 branch.branchY  -- Go directly to branch Y
            , F.stroke "#333"
            , F.strokeWidth cfg.lineWidth
            ] []
          else renderBranchEntry cfg x railY branch.itemX branch.branchY
        -- The element
        , renderGrammar cfg branch.itemX branch.branchY branch.grammar
        -- Exit curve/line from branch
        , if nearRail branch.branchY
          then elem Line
            [ F.x1 (branch.itemX + branch.width)
            , F.y1 branch.branchY  -- Start from branch Y
            , F.x2 branch.exitX
            , F.y2 railY
            , F.stroke "#333"
            , F.strokeWidth cfg.lineWidth
            ] []
          else renderBranchExit cfg (branch.itemX + branch.width) branch.branchY branch.exitX railY
        ]

-- | Positioned choice branch with all layout info
type PositionedBranch =
  { index :: Int
  , grammar :: Grammar
  , itemX :: Number
  , branchY :: Number
  , width :: Number
  , exitX :: Number
  }

-- | Compute positioned branches for a choice
computePositionedChoice :: RailroadConfig -> Number -> Number -> Array Grammar -> Array PositionedBranch
computePositionedChoice cfg x railY items =
  let
    layouts = map (computeLayout cfg) items
    maxWidth = foldl (\acc l -> max acc l.width) 0.0 layouts
    r = cfg.cornerRadius
    exitX = x + maxWidth + r * 4.0

    -- Compute Y positions
    totalHeight = foldl (\acc l -> acc + l.height) 0.0 layouts
                + cfg.verticalGap * Int.toNumber (max 0 (length layouts - 1))
    startY = railY - totalHeight / 2.0

    go :: { y :: Number, acc :: Array PositionedBranch, idx :: Int } -> { grammar :: Grammar, layout :: Layout }
       -> { y :: Number, acc :: Array PositionedBranch, idx :: Int }
    go { y, acc, idx } { grammar, layout } =
      let
        branchY = y + layout.centerY
        itemX = x + r * 2.0 + (maxWidth - layout.width) / 2.0
      in
        { y: y + layout.height + cfg.verticalGap
        , idx: idx + 1
        , acc: acc <>
            [ { index: idx
              , grammar
              , itemX
              , branchY
              , width: layout.width
              , exitX
              }
            ]
        }

    itemsWithLayouts = Array.zipWith (\g l -> { grammar: g, layout: l }) items layouts
  in
    (foldl go { y: startY, acc: [], idx: 0 } itemsWithLayouts).acc

-- | Render branch entry curve
renderBranchEntry :: RailroadConfig -> Number -> Number -> Number -> Number -> Tree
renderBranchEntry cfg x1 y1 x2 y2 =
  let
    r = cfg.cornerRadius
    goingDown = y2 > y1
    midX = x1 + r
    -- SVG arc: A rx ry angle large-arc sweep x y
    sweep = if goingDown then "1" else "0"
  in
    elem Path
      [ F.d (
          "M" <> show x1 <> "," <> show y1 <>
          " H" <> show midX <>
          " A" <> show r <> "," <> show r <> " 0 0 " <> sweep <> " " <> show (midX + r) <> "," <> show (y1 + (if goingDown then r else -r)) <>
          " V" <> show (y2 + (if goingDown then -r else r)) <>
          " A" <> show r <> "," <> show r <> " 0 0 " <> (if goingDown then "0" else "1") <> " " <> show (midX + r * 2.0) <> "," <> show y2 <>
          " H" <> show x2
        )
      , F.fill "none"
      , F.stroke "#333"
      , F.strokeWidth cfg.lineWidth
      ] []

-- | Render branch exit curve
renderBranchExit :: RailroadConfig -> Number -> Number -> Number -> Number -> Tree
renderBranchExit cfg x1 y1 x2 y2 =
  let
    r = cfg.cornerRadius
    goingUp = y1 > y2
    midX = x2 - r
    -- Sweep flags are opposite to entry: curving back toward the main rail
    sweep1 = if goingUp then "0" else "1"
    sweep2 = if goingUp then "1" else "0"
  in
    elem Path
      [ F.d (
          "M" <> show x1 <> "," <> show y1 <>
          " H" <> show (midX - r) <>
          " A" <> show r <> "," <> show r <> " 0 0 " <> sweep1 <> " " <> show midX <> "," <> show (y1 + (if goingUp then -r else r)) <>
          " V" <> show (y2 + (if goingUp then r else -r)) <>
          " A" <> show r <> "," <> show r <> " 0 0 " <> sweep2 <> " " <> show (midX + r) <> "," <> show y2 <>
          " H" <> show x2
        )
      , F.fill "none"
      , F.stroke "#333"
      , F.strokeWidth cfg.lineWidth
      ] []

-- | Render optional element with bypass
renderOptional :: RailroadConfig -> Number -> Number -> Grammar -> Tree
renderOptional cfg x railY inner =
  let
    innerLayout = computeLayout cfg inner
    r = cfg.cornerRadius
    bypassY = railY - r * 2.0 - cfg.verticalGap
    innerX = x + r * 2.0
    exitX = innerX + innerLayout.width + r * 2.0
  in
    elem Group []
      [ -- Main path through element
        elem Line
          [ F.x1 x
          , F.y1 railY
          , F.x2 innerX
          , F.y2 railY
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          ] []
      , renderGrammar cfg innerX railY inner
      , elem Line
          [ F.x1 (innerX + innerLayout.width)
          , F.y1 railY
          , F.x2 exitX
          , F.y2 railY
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          ] []
      -- Bypass path (skip over)
      , elem Path
          [ F.d (
              "M" <> show x <> "," <> show railY <>
              " A" <> show r <> "," <> show r <> " 0 0 0 " <> show (x + r) <> "," <> show (railY - r) <>
              " V" <> show (bypassY + r) <>
              " A" <> show r <> "," <> show r <> " 0 0 1 " <> show (x + r * 2.0) <> "," <> show bypassY <>
              " H" <> show (exitX - r * 2.0) <>
              " A" <> show r <> "," <> show r <> " 0 0 1 " <> show (exitX - r) <> "," <> show (bypassY + r) <>
              " V" <> show (railY - r) <>
              " A" <> show r <> "," <> show r <> " 0 0 0 " <> show exitX <> "," <> show railY
            )
          , F.fill "none"
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          ] []
      ]

-- | Render repeat element with loop-back
renderRepeat :: RailroadConfig -> Number -> Number -> Grammar -> Tree
renderRepeat cfg x railY inner =
  let
    innerLayout = computeLayout cfg inner
    r = cfg.cornerRadius
    loopY = railY + innerLayout.height / 2.0 + cfg.verticalGap + r
    innerX = x + r * 2.0
    exitX = innerX + innerLayout.width + r * 2.0
  in
    elem Group []
      [ -- Main path through element
        elem Line
          [ F.x1 x
          , F.y1 railY
          , F.x2 innerX
          , F.y2 railY
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          ] []
      , renderGrammar cfg innerX railY inner
      , elem Line
          [ F.x1 (innerX + innerLayout.width)
          , F.y1 railY
          , F.x2 exitX
          , F.y2 railY
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          ] []
      -- Loop-back path
      , elem Path
          [ F.d (
              "M" <> show (innerX + innerLayout.width) <> "," <> show railY <>
              " A" <> show r <> "," <> show r <> " 0 0 1 " <> show (innerX + innerLayout.width + r) <> "," <> show (railY + r) <>
              " V" <> show (loopY - r) <>
              " A" <> show r <> "," <> show r <> " 0 0 1 " <> show (innerX + innerLayout.width) <> "," <> show loopY <>
              " H" <> show innerX <>
              " A" <> show r <> "," <> show r <> " 0 0 1 " <> show (innerX - r) <> "," <> show (loopY - r) <>
              " V" <> show (railY + r) <>
              " A" <> show r <> "," <> show r <> " 0 0 1 " <> show innerX <> "," <> show railY
            )
          , F.fill "none"
          , F.stroke "#333"
          , F.strokeWidth cfg.lineWidth
          , staticStr "marker-mid" "url(#arrow)"
          ] []
      -- Arrow indicator on loop
      , elem Circle
          [ F.cx (innerX + innerLayout.width / 2.0)
          , F.cy loopY
          , F.r 3.0
          , F.fill "#333"
          ] []
      ]
