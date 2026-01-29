-- | Board Games - HATS Version
-- |
-- | Chess, Sudoku, and Scrabble boards demonstrating:
-- | - forEach over flattened grid cells
-- | - Nested elements (rect + text per cell)
-- | - Color encoding from data
-- |
-- | Classic example of 2D grid visualization.
module Examples.BoardGames
  ( chessBoardTree
  , chessBoardTreeNested
  , sudokuBoardTree
  , scrabbleBoardTree
  , scrabbleBoardTreeNested
  , ChessCell
  , ChessRow
  , ScrabbleCell
  , ScrabbleRow
  , chessData
  , chessDataNested
  , sudokuData
  , scrabbleData
  , scrabbleFullData
  , scrabbleDataNested
  , BoardConfig
  , defaultChessConfig
  , defaultSudokuConfig
  , defaultScrabbleConfig
  ) where

import Prelude

import Data.Array (range)
import Data.Int as Int
import Hylograph.HATS (Tree, elem, forEach, staticStr, staticNum, thunkedNum, thunkedStr)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Selection.Types (ElementType(..))

-- =============================================================================
-- Data Types
-- =============================================================================

-- | A single cell on any board
type ChessCell =
  { row :: Int
  , col :: Int
  , value :: String  -- piece symbol or number
  , color :: String  -- background color
  }

-- | Board configuration
type BoardConfig =
  { cellSize :: Number
  , padding :: Number
  , fontSize :: Number
  , boardSize :: Int  -- cells per side
  , showBorder :: Boolean
  , borderColor :: String
  }

-- =============================================================================
-- Configurations
-- =============================================================================

defaultChessConfig :: BoardConfig
defaultChessConfig =
  { cellSize: 50.0
  , padding: 20.0
  , fontSize: 32.0
  , boardSize: 8
  , showBorder: true
  , borderColor: "#333"
  }

defaultSudokuConfig :: BoardConfig
defaultSudokuConfig =
  { cellSize: 40.0
  , padding: 20.0
  , fontSize: 20.0
  , boardSize: 9
  , showBorder: true
  , borderColor: "#333"
  }

defaultScrabbleConfig :: BoardConfig
defaultScrabbleConfig =
  { cellSize: 32.0
  , padding: 40.0  -- room for green border
  , fontSize: 16.0
  , boardSize: 15  -- full board
  , showBorder: true
  , borderColor: "#2d5a4a"  -- dark green
  }

-- =============================================================================
-- Chess Board Data
-- =============================================================================

chessData :: Array ChessCell
chessData = do
  row <- range 0 7
  col <- range 0 7
  let
    isLight = (row + col) `mod` 2 == 0
    color = if isLight then "#f0d9b5" else "#b58863"
    piece = chessPiece row col
  pure { row, col, value: piece, color }

-- | Nested data structure: rows containing cells
-- | This demonstrates true nested folds in the visualization
type ChessRow =
  { rowIndex :: Int
  , cells :: Array ChessCell
  }

-- | Chess data as nested rows (for nested fold demonstration)
chessDataNested :: Array ChessRow
chessDataNested = map mkRow (range 0 7)
  where
  mkRow :: Int -> ChessRow
  mkRow row =
    { rowIndex: row
    , cells: map (mkCell row) (range 0 7)
    }
  mkCell :: Int -> Int -> ChessCell
  mkCell row col =
    let
      isLight = (row + col) `mod` 2 == 0
      color = if isLight then "#f0d9b5" else "#b58863"
      piece = chessPiece row col
    in { row, col, value: piece, color }

-- | Chess piece for starting position (Unicode symbols)
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
-- Sudoku Data
-- =============================================================================

sudokuData :: Array ChessCell
sudokuData = do
  row <- range 0 8
  col <- range 0 8
  let
    -- Alternating 3x3 box colors
    boxRow = row / 3
    boxCol = col / 3
    isAltBox = (boxRow + boxCol) `mod` 2 == 0
    color = if isAltBox then "#e8e8e8" else "#ffffff"
    value = sudokuValue row col
  pure { row, col, value, color }

-- | Sudoku puzzle values (classic puzzle)
sudokuValue :: Int -> Int -> String
sudokuValue 0 0 = "5"
sudokuValue 0 1 = "3"
sudokuValue 0 4 = "7"
sudokuValue 1 0 = "6"
sudokuValue 1 3 = "1"
sudokuValue 1 4 = "9"
sudokuValue 1 5 = "5"
sudokuValue 2 1 = "9"
sudokuValue 2 2 = "8"
sudokuValue 2 7 = "6"
sudokuValue 3 0 = "8"
sudokuValue 3 4 = "6"
sudokuValue 3 8 = "3"
sudokuValue 4 0 = "4"
sudokuValue 4 3 = "8"
sudokuValue 4 5 = "3"
sudokuValue 4 8 = "1"
sudokuValue 5 0 = "7"
sudokuValue 5 4 = "2"
sudokuValue 5 8 = "6"
sudokuValue 6 1 = "6"
sudokuValue 6 6 = "2"
sudokuValue 6 7 = "8"
sudokuValue 7 3 = "4"
sudokuValue 7 4 = "1"
sudokuValue 7 5 = "9"
sudokuValue 7 8 = "5"
sudokuValue 8 4 = "8"
sudokuValue 8 7 = "7"
sudokuValue 8 8 = "9"
sudokuValue _ _ = ""

-- =============================================================================
-- Scrabble Data (Full 15x15 board)
-- =============================================================================

-- | Scrabble cell with bonus type for labels
type ScrabbleCell =
  { row :: Int
  , col :: Int
  , value :: String    -- tile letter (if any)
  , color :: String    -- square background
  , bonus :: String    -- "TW", "DW", "TL", "DL", "★", or ""
  }

scrabbleData :: Array ChessCell
scrabbleData = do
  row <- range 0 14
  col <- range 0 14
  let
    bonus = scrabbleBonus row col
    color = bonusToColor bonus
    value = scrabbleTile row col
  pure { row, col, value, color }

-- | Full scrabble data with bonus info
scrabbleFullData :: Array ScrabbleCell
scrabbleFullData = do
  row <- range 0 14
  col <- range 0 14
  let
    bonus = scrabbleBonus row col
    color = bonusToColor bonus
    value = scrabbleTile row col
  pure { row, col, value, color, bonus }

-- | Get bonus type for a square (symmetric 15x15 pattern)
scrabbleBonus :: Int -> Int -> String
scrabbleBonus r c =
  let
    -- Mirror to top-left quadrant for symmetry
    row = if r > 7 then 14 - r else r
    col = if c > 7 then 14 - c else c
  in case row, col of
    -- Triple Word (corners and edges)
    0, 0 -> "TW"
    0, 7 -> "TW"
    7, 0 -> "TW"
    -- Double Word (diagonal)
    1, 1 -> "DW"
    2, 2 -> "DW"
    3, 3 -> "DW"
    4, 4 -> "DW"
    -- Triple Letter
    1, 5 -> "TL"
    5, 1 -> "TL"
    5, 5 -> "TL"
    -- Double Letter
    0, 3 -> "DL"
    3, 0 -> "DL"
    2, 6 -> "DL"
    6, 2 -> "DL"
    3, 7 -> "DL"
    7, 3 -> "DL"
    6, 6 -> "DL"
    -- Center star
    7, 7 -> "★"
    _, _ -> ""

-- | Convert bonus type to color
bonusToColor :: String -> String
bonusToColor "TW" = "#e8a0a0"  -- salmon/pink for Triple Word
bonusToColor "DW" = "#f0c0b0"  -- light pink for Double Word
bonusToColor "TL" = "#7cb8d4"  -- blue for Triple Letter
bonusToColor "DL" = "#a8d8e8"  -- light blue for Double Letter
bonusToColor "★"  = "#f0c0b0"  -- center same as DW
bonusToColor _    = "#e8dcc8"  -- cream/beige for regular

-- | Nested data structure for Scrabble: rows containing cells
type ScrabbleRow =
  { rowIndex :: Int
  , cells :: Array ScrabbleCell
  }

-- | Scrabble data as nested rows (for nested fold demonstration)
scrabbleDataNested :: Array ScrabbleRow
scrabbleDataNested = map mkRow (range 0 14)
  where
  mkRow :: Int -> ScrabbleRow
  mkRow row =
    { rowIndex: row
    , cells: map (mkCell row) (range 0 14)
    }
  mkCell :: Int -> Int -> ScrabbleCell
  mkCell row col =
    let
      bonus = scrabbleBonus row col
      color = bonusToColor bonus
      value = scrabbleTile row col
    in { row, col, value, color, bonus }

-- | Scrabble tiles: POLYGLOT vertical with PURESCRIPT, GRAPHIC, EXPLORATION crossing
-- | All words connect through POLYGLOT at column 4
scrabbleTile :: Int -> Int -> String
-- POLYGLOT vertical (column 4, rows 3-10)
scrabbleTile 3 4 = "P"
scrabbleTile 4 4 = "O"
scrabbleTile 5 4 = "L"
scrabbleTile 6 4 = "Y"
scrabbleTile 7 4 = "G"
scrabbleTile 8 4 = "L"
scrabbleTile 9 4 = "O"
scrabbleTile 10 4 = "T"
-- PURESCRIPT horizontal (row 3, crossing P)
scrabbleTile 3 5 = "U"
scrabbleTile 3 6 = "R"
scrabbleTile 3 7 = "E"
scrabbleTile 3 8 = "S"
scrabbleTile 3 9 = "C"
scrabbleTile 3 10 = "R"
scrabbleTile 3 11 = "I"
scrabbleTile 3 12 = "P"
scrabbleTile 3 13 = "T"
-- GRAPHIC horizontal (row 7, crossing G)
scrabbleTile 7 5 = "R"
scrabbleTile 7 6 = "A"
scrabbleTile 7 7 = "P"
scrabbleTile 7 8 = "H"
scrabbleTile 7 9 = "I"
scrabbleTile 7 10 = "C"
-- EXPLORATION horizontal (row 9, crossing O)
scrabbleTile 9 0 = "E"
scrabbleTile 9 1 = "X"
scrabbleTile 9 2 = "P"
scrabbleTile 9 3 = "L"
-- O at 9,4 is part of POLYGLOT
scrabbleTile 9 5 = "R"
scrabbleTile 9 6 = "A"
scrabbleTile 9 7 = "T"
scrabbleTile 9 8 = "I"
scrabbleTile 9 9 = "O"
scrabbleTile 9 10 = "N"
scrabbleTile _ _ = ""

-- | Get point value for a letter
letterPoints :: String -> Int
letterPoints "A" = 1
letterPoints "B" = 3
letterPoints "C" = 3
letterPoints "D" = 2
letterPoints "E" = 1
letterPoints "F" = 4
letterPoints "G" = 2
letterPoints "H" = 4
letterPoints "I" = 1
letterPoints "J" = 8
letterPoints "K" = 5
letterPoints "L" = 1
letterPoints "M" = 3
letterPoints "N" = 1
letterPoints "O" = 1
letterPoints "P" = 3
letterPoints "Q" = 10
letterPoints "R" = 1
letterPoints "S" = 1
letterPoints "T" = 1
letterPoints "U" = 1
letterPoints "V" = 4
letterPoints "W" = 4
letterPoints "X" = 8
letterPoints "Y" = 4
letterPoints "Z" = 10
letterPoints _ = 0

-- =============================================================================
-- HATS Trees
-- =============================================================================

-- | Chess board as HATS Tree
chessBoardTree :: BoardConfig -> Array ChessCell -> Tree
chessBoardTree = boardTree "chess"

-- | Sudoku board as HATS Tree
sudokuBoardTree :: BoardConfig -> Array ChessCell -> Tree
sudokuBoardTree = boardTree "sudoku"

-- | Scrabble board as HATS Tree
scrabbleBoardTree :: BoardConfig -> Array ChessCell -> Tree
scrabbleBoardTree cfg cells = boardTreeWithTiles cfg cells

-- | Generic board rendering
boardTree :: String -> BoardConfig -> Array ChessCell -> Tree
boardTree name cfg cells =
  let
    totalSize = Int.toNumber cfg.boardSize * cfg.cellSize + cfg.padding * 2.0

    cellKey :: ChessCell -> String
    cellKey cell = show cell.row <> "-" <> show cell.col
  in
    elem SVG
      [ F.width totalSize
      , F.height totalSize
      , F.attr "viewBox" ("0 0 " <> show totalSize <> " " <> show totalSize)
      , F.class_ (name <> "-board")
      ]
      [ -- Board group with padding offset
        elem Group
          [ F.transform ("translate(" <> show cfg.padding <> "," <> show cfg.padding <> ")")
          ]
          [ -- Optional border
            if cfg.showBorder
              then elem Rect
                [ F.x (-2.0)
                , F.y (-2.0)
                , F.width (Int.toNumber cfg.boardSize * cfg.cellSize + 4.0)
                , F.height (Int.toNumber cfg.boardSize * cfg.cellSize + 4.0)
                , F.fill "none"
                , F.stroke cfg.borderColor
                , F.strokeWidth 2.0
                ] []
              else elem Group [] []

          -- Cells
          , forEach "cells" Group cells cellKey \cell ->
              let
                x = Int.toNumber cell.col * cfg.cellSize
                y = Int.toNumber cell.row * cfg.cellSize
              in
                elem Group
                  [ thunkedStr "transform" ("translate(" <> show x <> "," <> show y <> ")")
                  ]
                  [ -- Cell background
                    elem Rect
                      [ F.width cfg.cellSize
                      , F.height cfg.cellSize
                      , thunkedStr "fill" cell.color
                      , F.stroke "#666"
                      , F.strokeWidth 0.5
                      ] []
                  -- Piece/value
                  , elem Text
                      [ F.x (cfg.cellSize / 2.0)
                      , F.y (cfg.cellSize / 2.0 + cfg.fontSize * 0.35)
                      , F.textAnchor "middle"
                      , thunkedNum "font-size" cfg.fontSize
                      , F.fill "#000"
                      , F.fontFamily "serif"
                      , thunkedStr "textContent" cell.value
                      ] []
                  ]
          ]
      ]

-- | Chess board with NESTED FOLDS (rows → cells)
-- | This demonstrates the structural difference between flat and nested iteration
chessBoardTreeNested :: BoardConfig -> Array ChessRow -> Tree
chessBoardTreeNested cfg rows =
  let
    totalSize = Int.toNumber cfg.boardSize * cfg.cellSize + cfg.padding * 2.0
  in
    elem SVG
      [ F.width totalSize
      , F.height totalSize
      , F.attr "viewBox" ("0 0 " <> show totalSize <> " " <> show totalSize)
      , F.class_ "chess-board-nested"
      ]
      [ elem Group
          [ F.transform ("translate(" <> show cfg.padding <> "," <> show cfg.padding <> ")")
          , staticStr "data-label" "board"
          ]
          [ -- Border
            elem Rect
              [ staticStr "data-label" "border"
              , F.x (-2.0)
              , F.y (-2.0)
              , F.width (Int.toNumber cfg.boardSize * cfg.cellSize + 4.0)
              , F.height (Int.toNumber cfg.boardSize * cfg.cellSize + 4.0)
              , F.fill "none"
              , F.stroke cfg.borderColor
              , F.strokeWidth 2.0
              ] []

          -- NESTED FOLDS: rows → cells
          , forEach "rows" Group rows (\r -> "row-" <> show r.rowIndex) \row ->
              let y = Int.toNumber row.rowIndex * cfg.cellSize
              in elem Group
                  [ thunkedStr "transform" ("translate(0," <> show y <> ")")
                  ]
                  [ -- Inner fold: cells within this row
                    forEach "cells" Group row.cells (\c -> show c.row <> "-" <> show c.col) \cell ->
                      let x = Int.toNumber cell.col * cfg.cellSize
                      in elem Group
                          [ thunkedStr "transform" ("translate(" <> show x <> ",0)")
                          ]
                          [ elem Rect
                              [ F.width cfg.cellSize
                              , F.height cfg.cellSize
                              , thunkedStr "fill" cell.color
                              , F.stroke "#666"
                              , F.strokeWidth 0.5
                              ] []
                          , elem Text
                              [ F.x (cfg.cellSize / 2.0)
                              , F.y (cfg.cellSize / 2.0 + cfg.fontSize * 0.35)
                              , F.textAnchor "middle"
                              , thunkedNum "font-size" cfg.fontSize
                              , F.fill "#000"
                              , F.fontFamily "serif"
                              , thunkedStr "textContent" cell.value
                              ] []
                          ]
                  ]
          ]
      ]

-- | Scrabble board with full chrome (green border, bonus labels, tiles)
boardTreeWithTiles :: BoardConfig -> Array ChessCell -> Tree
boardTreeWithTiles cfg _ =
  let
    cells = scrabbleFullData
    boardPixels = Int.toNumber cfg.boardSize * cfg.cellSize
    borderWidth = 28.0
    totalSize = boardPixels + borderWidth * 2.0 + cfg.padding * 2.0

    cellKey :: ScrabbleCell -> String
    cellKey cell = show cell.row <> "-" <> show cell.col

    hasTile :: ScrabbleCell -> Boolean
    hasTile cell = cell.value /= ""

    -- Bonus label text (multi-line would need tspan, simplified here)
    bonusLabel :: String -> String
    bonusLabel "TW" = "TW"
    bonusLabel "DW" = "DW"
    bonusLabel "TL" = "TL"
    bonusLabel "DL" = "DL"
    bonusLabel "★" = "★"
    bonusLabel _ = ""
  in
    elem SVG
      [ F.width totalSize
      , F.height totalSize
      , F.attr "viewBox" ("0 0 " <> show totalSize <> " " <> show totalSize)
      , F.class_ "scrabble-board"
      ]
      [ -- Outer beige background
        elem Rect
          [ F.width totalSize
          , F.height totalSize
          , F.fill "#c4b8a0"
          ] []

      -- Green border frame
      , elem Rect
          [ F.x cfg.padding
          , F.y cfg.padding
          , F.width (boardPixels + borderWidth * 2.0)
          , F.height (boardPixels + borderWidth * 2.0)
          , F.fill "#2d5a4a"
          , staticNum "rx" 4.0
          ] []

      -- "SCRABBLE" text on borders
      , elem Text
          [ F.x (cfg.padding + borderWidth + boardPixels / 2.0)
          , F.y (cfg.padding + 20.0)
          , F.textAnchor "middle"
          , staticNum "font-size" 14.0
          , F.fill "#c4b8a0"
          , F.fontFamily "Arial, sans-serif"
          , F.fontWeight "bold"
          , staticStr "letter-spacing" "8"
          , staticStr "textContent" "SCRABBLE"
          ] []
      , elem Text
          [ F.x (cfg.padding + borderWidth + boardPixels / 2.0)
          , F.y (cfg.padding + borderWidth * 2.0 + boardPixels - 8.0)
          , F.textAnchor "middle"
          , staticNum "font-size" 14.0
          , F.fill "#c4b8a0"
          , F.fontFamily "Arial, sans-serif"
          , F.fontWeight "bold"
          , staticStr "letter-spacing" "8"
          , staticStr "textContent" "SCRABBLE"
          ] []

      -- Board area
      , elem Group
          [ F.transform ("translate(" <> show (cfg.padding + borderWidth) <> "," <> show (cfg.padding + borderWidth) <> ")")
          ]
          [ -- Board background (cream)
            elem Rect
              [ F.width boardPixels
              , F.height boardPixels
              , F.fill "#e8dcc8"
              ] []

          -- Grid lines
          , elem Group
              [ F.class_ "grid-lines" ]
              []  -- Could add grid lines here

          -- Cells with bonus labels and tiles
          , forEach "cells" Group cells cellKey \cell ->
              let
                x = Int.toNumber cell.col * cfg.cellSize
                y = Int.toNumber cell.row * cfg.cellSize
                tileInset = 2.0
                tileSize = cfg.cellSize - tileInset * 2.0
                bonus = cell.bonus
                showBonus = bonus /= "" && bonus /= "★" && not (hasTile cell)
                showStar = bonus == "★" && not (hasTile cell)
              in
                elem Group
                  [ thunkedStr "transform" ("translate(" <> show x <> "," <> show y <> ")")
                  ]
                  ( [ -- Square background
                      elem Rect
                        [ F.width cfg.cellSize
                        , F.height cfg.cellSize
                        , thunkedStr "fill" cell.color
                        , F.stroke "#bbb"
                        , F.strokeWidth 0.5
                        ] []
                    ] <>
                    -- Bonus label (if no tile)
                    ( if showBonus
                        then
                          [ elem Text
                              [ F.x (cfg.cellSize / 2.0)
                              , F.y (cfg.cellSize / 2.0 + 3.0)
                              , F.textAnchor "middle"
                              , staticNum "font-size" 7.0
                              , F.fill "#666"
                              , F.fontFamily "Arial, sans-serif"
                              , thunkedStr "textContent" (bonusLabel bonus)
                              ] []
                          ]
                        else []
                    ) <>
                    -- Center star
                    ( if showStar
                        then
                          [ elem Text
                              [ F.x (cfg.cellSize / 2.0)
                              , F.y (cfg.cellSize / 2.0 + 6.0)
                              , F.textAnchor "middle"
                              , staticNum "font-size" 18.0
                              , F.fill "#333"
                              , staticStr "textContent" "★"
                              ] []
                          ]
                        else []
                    ) <>
                    -- Tile (if has letter)
                    ( if hasTile cell
                        then
                          [ -- Tile shadow
                            elem Rect
                              [ F.x (tileInset + 1.5)
                              , F.y (tileInset + 1.5)
                              , F.width tileSize
                              , F.height tileSize
                              , F.fill "rgba(0,0,0,0.2)"
                              , staticNum "rx" 2.0
                              ] []
                          -- Tile background
                          , elem Rect
                              [ F.x tileInset
                              , F.y tileInset
                              , F.width tileSize
                              , F.height tileSize
                              , F.fill "#f5e6c8"
                              , F.stroke "#c4a35a"
                              , F.strokeWidth 1.0
                              , staticNum "rx" 2.0
                              ] []
                          -- Letter
                          , elem Text
                              [ F.x (cfg.cellSize / 2.0)
                              , F.y (cfg.cellSize / 2.0 + 5.0)
                              , F.textAnchor "middle"
                              , staticNum "font-size" 16.0
                              , F.fill "#1a1a1a"
                              , F.fontWeight "bold"
                              , F.fontFamily "Georgia, serif"
                              , thunkedStr "textContent" cell.value
                              ] []
                          -- Point value (bottom right of tile)
                          , elem Text
                              [ F.x (cfg.cellSize - tileInset - 4.0)
                              , F.y (cfg.cellSize - tileInset - 3.0)
                              , F.textAnchor "end"
                              , staticNum "font-size" 7.0
                              , F.fill "#333"
                              , F.fontFamily "Arial, sans-serif"
                              , thunkedStr "textContent" (show (letterPoints cell.value))
                              ] []
                          ]
                        else []
                    )
                  )
          ]
      ]

-- | Scrabble board with NESTED FOLDS (rows → cells)
-- | Demonstrates nested iteration with 15x15 grid
scrabbleBoardTreeNested :: BoardConfig -> Array ScrabbleRow -> Tree
scrabbleBoardTreeNested cfg rows =
  let
    boardPixels = Int.toNumber cfg.boardSize * cfg.cellSize
    borderWidth = 28.0
    totalSize = boardPixels + borderWidth * 2.0 + cfg.padding * 2.0

    hasTile :: ScrabbleCell -> Boolean
    hasTile cell = cell.value /= ""

    bonusLabel :: String -> String
    bonusLabel "TW" = "TW"
    bonusLabel "DW" = "DW"
    bonusLabel "TL" = "TL"
    bonusLabel "DL" = "DL"
    bonusLabel "★" = "★"
    bonusLabel _ = ""
  in
    elem SVG
      [ F.width totalSize
      , F.height totalSize
      , F.attr "viewBox" ("0 0 " <> show totalSize <> " " <> show totalSize)
      , F.class_ "scrabble-board-nested"
      ]
      [ -- Outer beige background
        elem Rect
          [ F.width totalSize
          , F.height totalSize
          , F.fill "#c4b8a0"
          ] []

      -- Green border frame
      , elem Rect
          [ F.x cfg.padding
          , F.y cfg.padding
          , F.width (boardPixels + borderWidth * 2.0)
          , F.height (boardPixels + borderWidth * 2.0)
          , F.fill "#2d5a4a"
          , staticNum "rx" 4.0
          ] []

      -- Board area with NESTED FOLDS
      , elem Group
          [ F.transform ("translate(" <> show (cfg.padding + borderWidth) <> "," <> show (cfg.padding + borderWidth) <> ")")
          , staticStr "data-label" "board"
          ]
          [ -- Board background
            elem Rect
              [ F.width boardPixels
              , F.height boardPixels
              , F.fill "#e8dcc8"
              ] []

          -- NESTED FOLDS: rows → cells
          , forEach "rows" Group rows (\r -> "row-" <> show r.rowIndex) \row ->
              let y = Int.toNumber row.rowIndex * cfg.cellSize
              in elem Group
                  [ thunkedStr "transform" ("translate(0," <> show y <> ")")
                  ]
                  [ forEach "cells" Group row.cells (\c -> show c.row <> "-" <> show c.col) \cell ->
                      let
                        x = Int.toNumber cell.col * cfg.cellSize
                        tileInset = 2.0
                        tileSize = cfg.cellSize - tileInset * 2.0
                        bonus = cell.bonus
                        showBonus = bonus /= "" && bonus /= "★" && not (hasTile cell)
                        showStar = bonus == "★" && not (hasTile cell)
                      in elem Group
                          [ thunkedStr "transform" ("translate(" <> show x <> ",0)")
                          ]
                          ( [ -- Square background
                              elem Rect
                                [ F.width cfg.cellSize
                                , F.height cfg.cellSize
                                , thunkedStr "fill" cell.color
                                , F.stroke "#bbb"
                                , F.strokeWidth 0.5
                                ] []
                            ] <>
                            -- Bonus label
                            ( if showBonus
                                then [ elem Text
                                    [ F.x (cfg.cellSize / 2.0)
                                    , F.y (cfg.cellSize / 2.0 + 3.0)
                                    , F.textAnchor "middle"
                                    , staticNum "font-size" 7.0
                                    , F.fill "#666"
                                    , thunkedStr "textContent" (bonusLabel bonus)
                                    ] []
                                  ]
                                else []
                            ) <>
                            -- Center star
                            ( if showStar
                                then [ elem Text
                                    [ F.x (cfg.cellSize / 2.0)
                                    , F.y (cfg.cellSize / 2.0 + 6.0)
                                    , F.textAnchor "middle"
                                    , staticNum "font-size" 18.0
                                    , F.fill "#333"
                                    , staticStr "textContent" "★"
                                    ] []
                                  ]
                                else []
                            ) <>
                            -- Tile with letter
                            ( if hasTile cell
                                then
                                  [ elem Rect
                                      [ F.x tileInset
                                      , F.y tileInset
                                      , F.width tileSize
                                      , F.height tileSize
                                      , F.fill "#f5e6c8"
                                      , F.stroke "#c4a35a"
                                      , F.strokeWidth 1.0
                                      , staticNum "rx" 2.0
                                      ] []
                                  , elem Text
                                      [ F.x (cfg.cellSize / 2.0)
                                      , F.y (cfg.cellSize / 2.0 + 5.0)
                                      , F.textAnchor "middle"
                                      , staticNum "font-size" 16.0
                                      , F.fill "#1a1a1a"
                                      , F.fontWeight "bold"
                                      , thunkedStr "textContent" cell.value
                                      ] []
                                  ]
                                else []
                            )
                          )
                  ]
          ]
      ]
