-- | Chapter 2: Any Foldable, Any Structure
-- |
-- | Same chess board, two folds. The nested fold preserves row grouping:
-- | hover any cell in the nested board and the whole row lights up.
-- | The flat fold has no grouping — only individual cells highlight.
module Chapters.Chapter2
  ( flatBoardTree
  , nestedBoardTree
  ) where

import Prelude

import Prelude

import Data.Array (range)
import Data.Int as Int
import Hylograph.HATS (Tree, ThunkedBehavior, elem, forEach, staticStr, withBehaviors, onCoordinatedHighlight)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Behavior.Types (HighlightClass(..))
import Hylograph.Internal.Element.Types (ElementType(..))
import Data.Maybe (Maybe(..))

-- =============================================================================
-- Config
-- =============================================================================

cellSize :: Number
cellSize = 34.0

padding :: Number
padding = 10.0

fontSize :: Number
fontSize = 22.0

boardN :: Int
boardN = 8

totalSize :: Number
totalSize = Int.toNumber boardN * cellSize + padding * 2.0

-- =============================================================================
-- Chess data
-- =============================================================================

type Cell = { row :: Int, col :: Int, piece :: String, color :: String }

type Row = { rowIndex :: Int, cells :: Array Cell }

flatCells :: Array Cell
flatCells = do
  row <- range 0 7
  col <- range 0 7
  let isLight = (row + col) `mod` 2 == 0
      color = if isLight then "#f0d9b5" else "#b58863"
  pure { row, col, piece: chessPiece row col, color }

nestedRows :: Array Row
nestedRows = map mkRow (range 0 7)
  where
  mkRow row = { rowIndex: row, cells: map (mkCell row) (range 0 7) }
  mkCell row col =
    let isLight = (row + col) `mod` 2 == 0
        color = if isLight then "#f0d9b5" else "#b58863"
    in { row, col, piece: chessPiece row col, color }

chessPiece :: Int -> Int -> String
chessPiece 0 0 = "♜"
chessPiece 0 1 = "♞"
chessPiece 0 2 = "♝"
chessPiece 0 3 = "♛"
chessPiece 0 4 = "♚"
chessPiece 0 5 = "♝"
chessPiece 0 6 = "♞"
chessPiece 0 7 = "♜"
chessPiece 1 _ = "♟"
chessPiece 6 _ = "♙"
chessPiece 7 0 = "♖"
chessPiece 7 1 = "♘"
chessPiece 7 2 = "♗"
chessPiece 7 3 = "♕"
chessPiece 7 4 = "♔"
chessPiece 7 5 = "♗"
chessPiece 7 6 = "♘"
chessPiece 7 7 = "♖"
chessPiece _ _ = ""

-- =============================================================================
-- Highlight behaviors
-- =============================================================================

-- | Flat board: each cell is individual — no row grouping
hlFlatCell :: Int -> Int -> ThunkedBehavior
hlFlatCell row col = onCoordinatedHighlight
  { identify: "flat-" <> show row <> "-" <> show col
  , classify: \hoveredId ->
      if hoveredId == "flat-" <> show row <> "-" <> show col then Primary
      else Neutral
  , group: Just "ch2-flat"
  }

-- | Nested board: cells identify by ROW — hover any cell, whole row lights up
hlNestedCell :: Int -> ThunkedBehavior
hlNestedCell row = onCoordinatedHighlight
  { identify: "row-" <> show row
  , classify: \hoveredId ->
      if hoveredId == "row-" <> show row then Primary
      else Neutral
  , group: Just "ch2-nested"
  }

-- =============================================================================
-- Flat board: forEach over 64 cells
-- =============================================================================

flatBoardTree :: Tree
flatBoardTree =
  elem SVG
    [ F.width totalSize, F.height totalSize
    , F.viewBox 0.0 0.0 totalSize totalSize
    ]
    [ elem Group [ F.transform ("translate(" <> show padding <> "," <> show padding <> ")") ]
        [ border
        , forEach "cells" Group flatCells cellKey \cell ->
            let
              x = Int.toNumber cell.col * cellSize
              y = Int.toNumber cell.row * cellSize
            in
              withBehaviors [ hlFlatCell cell.row cell.col ] $
              cellElem x y cell
        ]
    ]

-- =============================================================================
-- Nested board: forEach rows → forEach cells
-- =============================================================================

nestedBoardTree :: Tree
nestedBoardTree =
  elem SVG
    [ F.width totalSize, F.height totalSize
    , F.viewBox 0.0 0.0 totalSize totalSize
    ]
    [ elem Group [ F.transform ("translate(" <> show padding <> "," <> show padding <> ")") ]
        [ border
        , forEach "rows" Group nestedRows (\r -> "row-" <> show r.rowIndex) \row ->
            let y = Int.toNumber row.rowIndex * cellSize
            in elem Group
                [ staticStr "transform" ("translate(0," <> show y <> ")") ]
                [ forEach "cells" Group row.cells cellKey \cell ->
                    let x = Int.toNumber cell.col * cellSize
                    in
                      withBehaviors [ hlNestedCell cell.row ] $
                      cellElem x 0.0 cell
                ]
        ]
    ]

-- =============================================================================
-- Shared cell rendering
-- =============================================================================

cellElem :: Number -> Number -> Cell -> Tree
cellElem x y cell =
  elem Group [ staticStr "transform" ("translate(" <> show x <> "," <> show y <> ")") ]
    [ elem Rect
        [ F.width cellSize, F.height cellSize
        , staticStr "fill" cell.color
        , F.stroke "#666"
        , F.strokeWidth 0.5
        ] []
    , elem Text
        [ F.x (cellSize / 2.0)
        , F.y (cellSize / 2.0 + fontSize * 0.35)
        , F.textAnchor "middle"
        , staticStr "font-size" (show fontSize)
        , F.fill "#000"
        , F.fontFamily "serif"
        , staticStr "textContent" cell.piece
        ] []
    ]

border :: Tree
border =
  elem Rect
    [ F.x (-2.0), F.y (-2.0)
    , F.width (Int.toNumber boardN * cellSize + 4.0)
    , F.height (Int.toNumber boardN * cellSize + 4.0)
    , F.fill "none"
    , F.stroke "#8B7355"
    , F.strokeWidth 2.0
    ] []

cellKey :: Cell -> String
cellKey cell = show cell.row <> "-" <> show cell.col
