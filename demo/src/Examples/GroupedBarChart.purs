-- | Grouped Bar Chart - HATS Version
-- |
-- | US population by state and age group, demonstrating:
-- | - Nested Fold: states → bars within each state
-- | - Color encoding for age groups
-- | - Axes rendered with HATS
-- |
-- | This is the canonical "pretty but simple" HATS example.
module Examples.GroupedBarChart
  ( groupedBarChartTree
  , GroupedBarData
  , StateGroup
  , BarData
  , Dimensions
  , defaultDims
  , colorForAge
  , sampleData
  ) where

import Prelude

import AxisHATS (Scale, axisLeft, renderAxisHATS)
import Data.Array (filter, nub, mapWithIndex)
import Data.Array as Array
import Data.Foldable (maximum)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Hylograph.HATS (Tree, elem, forEach, staticStr, staticNum, thunkedNum, thunkedStr)
import Hylograph.HATS.Friendly as F
import Hylograph.Internal.Element.Types (ElementType(..))

-- =============================================================================
-- Data Types
-- =============================================================================

-- | Single data point: one bar segment
type GroupedBarData =
  { state :: String
  , age :: String
  , population :: Number
  }

-- | Grouped data: one state with all its bars
type StateGroup =
  { state :: String
  , stateIndex :: Int
  , bars :: Array BarData
  }

-- | Pre-computed bar data with positions
type BarData =
  { age :: String
  , ageIndex :: Int
  , population :: Number
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , color :: String
  }

-- =============================================================================
-- Configuration
-- =============================================================================

type Dimensions =
  { width :: Number
  , height :: Number
  , marginTop :: Number
  , marginRight :: Number
  , marginBottom :: Number
  , marginLeft :: Number
  }

defaultDims :: Dimensions
defaultDims =
  { width: 928.0
  , height: 500.0
  , marginTop: 30.0
  , marginRight: 120.0  -- Room for legend
  , marginBottom: 50.0
  , marginLeft: 80.0
  }

innerWidth :: Dimensions -> Number
innerWidth dims = dims.width - dims.marginLeft - dims.marginRight

innerHeight :: Dimensions -> Number
innerHeight dims = dims.height - dims.marginTop - dims.marginBottom

-- =============================================================================
-- Color Scale (Spectral scheme)
-- =============================================================================

colorForAge :: String -> String
colorForAge "<10" = "#d53e4f"
colorForAge "10-19" = "#f46d43"
colorForAge "20-29" = "#fdae61"
colorForAge "30-39" = "#fee08b"
colorForAge "40-49" = "#e6f598"
colorForAge "50-59" = "#abdda4"
colorForAge "60-69" = "#66c2a5"
colorForAge "70-79" = "#3288bd"
colorForAge "≥80" = "#5e4fa2"
colorForAge _ = "#999"

-- =============================================================================
-- Sample Data (embedded for standalone demo)
-- =============================================================================

-- | Sample US population data (6 states × 9 age groups)
sampleData :: Array GroupedBarData
sampleData =
  [ -- California
    { state: "CA", age: "<10", population: 5000000.0 }
  , { state: "CA", age: "10-19", population: 5200000.0 }
  , { state: "CA", age: "20-29", population: 5800000.0 }
  , { state: "CA", age: "30-39", population: 5500000.0 }
  , { state: "CA", age: "40-49", population: 5100000.0 }
  , { state: "CA", age: "50-59", population: 4800000.0 }
  , { state: "CA", age: "60-69", population: 4200000.0 }
  , { state: "CA", age: "70-79", population: 2800000.0 }
  , { state: "CA", age: "≥80", population: 1800000.0 }
  -- Texas
  , { state: "TX", age: "<10", population: 3800000.0 }
  , { state: "TX", age: "10-19", population: 3900000.0 }
  , { state: "TX", age: "20-29", population: 4100000.0 }
  , { state: "TX", age: "30-39", population: 3900000.0 }
  , { state: "TX", age: "40-49", population: 3500000.0 }
  , { state: "TX", age: "50-59", population: 3200000.0 }
  , { state: "TX", age: "60-69", population: 2700000.0 }
  , { state: "TX", age: "70-79", population: 1800000.0 }
  , { state: "TX", age: "≥80", population: 1100000.0 }
  -- Florida
  , { state: "FL", age: "<10", population: 2200000.0 }
  , { state: "FL", age: "10-19", population: 2300000.0 }
  , { state: "FL", age: "20-29", population: 2500000.0 }
  , { state: "FL", age: "30-39", population: 2600000.0 }
  , { state: "FL", age: "40-49", population: 2700000.0 }
  , { state: "FL", age: "50-59", population: 2900000.0 }
  , { state: "FL", age: "60-69", population: 2800000.0 }
  , { state: "FL", age: "70-79", population: 2100000.0 }
  , { state: "FL", age: "≥80", population: 1500000.0 }
  -- New York
  , { state: "NY", age: "<10", population: 2300000.0 }
  , { state: "NY", age: "10-19", population: 2400000.0 }
  , { state: "NY", age: "20-29", population: 2800000.0 }
  , { state: "NY", age: "30-39", population: 2700000.0 }
  , { state: "NY", age: "40-49", population: 2500000.0 }
  , { state: "NY", age: "50-59", population: 2600000.0 }
  , { state: "NY", age: "60-69", population: 2200000.0 }
  , { state: "NY", age: "70-79", population: 1500000.0 }
  , { state: "NY", age: "≥80", population: 1000000.0 }
  -- Pennsylvania
  , { state: "PA", age: "<10", population: 1400000.0 }
  , { state: "PA", age: "10-19", population: 1500000.0 }
  , { state: "PA", age: "20-29", population: 1600000.0 }
  , { state: "PA", age: "30-39", population: 1500000.0 }
  , { state: "PA", age: "40-49", population: 1600000.0 }
  , { state: "PA", age: "50-59", population: 1800000.0 }
  , { state: "PA", age: "60-69", population: 1600000.0 }
  , { state: "PA", age: "70-79", population: 1100000.0 }
  , { state: "PA", age: "≥80", population: 800000.0 }
  -- Illinois
  , { state: "IL", age: "<10", population: 1600000.0 }
  , { state: "IL", age: "10-19", population: 1700000.0 }
  , { state: "IL", age: "20-29", population: 1800000.0 }
  , { state: "IL", age: "30-39", population: 1700000.0 }
  , { state: "IL", age: "40-49", population: 1600000.0 }
  , { state: "IL", age: "50-59", population: 1700000.0 }
  , { state: "IL", age: "60-69", population: 1400000.0 }
  , { state: "IL", age: "70-79", population: 900000.0 }
  , { state: "IL", age: "≥80", population: 600000.0 }
  ]

-- =============================================================================
-- Data Preparation
-- =============================================================================

-- | Get unique states
getStates :: Array GroupedBarData -> Array String
getStates = nub <<< map _.state

-- | Get unique age groups (in order)
getAges :: Array GroupedBarData -> Array String
getAges _ = ["<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "≥80"]

-- | Prepare data for rendering: group by state, compute positions
prepareData :: Dimensions -> Array GroupedBarData -> Array StateGroup
prepareData dims rawData =
  let
    iWidth = innerWidth dims
    iHeight = innerHeight dims
    states = getStates rawData
    ages = getAges rawData
    numStates = Array.length states
    numAges = Array.length ages

    groupWidth = iWidth / Int.toNumber numStates
    barWidth = groupWidth / Int.toNumber numAges * 0.85
    barGap = groupWidth / Int.toNumber numAges * 0.15 / 2.0

    populationValues = map _.population rawData
    maxPop = fromMaybe 6000000.0 $ maximum populationValues

    mkStateGroup :: Int -> String -> StateGroup
    mkStateGroup stateIndex state =
      let
        stateBars = filter (\d -> d.state == state) rawData
        bars = mapWithIndex (mkBar stateIndex) stateBars
      in
        { state, stateIndex, bars }

    mkBar :: Int -> Int -> GroupedBarData -> BarData
    mkBar stateIndex ageIndex d =
      let
        xPos = Int.toNumber stateIndex * groupWidth + Int.toNumber ageIndex * (barWidth + barGap * 2.0) + barGap
        barHeight = (d.population / maxPop) * iHeight
        yPos = iHeight - barHeight
      in
        { age: d.age
        , ageIndex
        , population: d.population
        , x: xPos
        , y: yPos
        , width: barWidth
        , height: barHeight
        , color: colorForAge d.age
        }
  in
    mapWithIndex mkStateGroup states

-- =============================================================================
-- HATS Tree
-- =============================================================================

-- | Complete grouped bar chart as a HATS Tree
groupedBarChartTree :: Dimensions -> Array GroupedBarData -> Tree
groupedBarChartTree dims rawData =
  let
    iWidth = innerWidth dims
    iHeight = innerHeight dims
    stateGroups = prepareData dims rawData
    states = getStates rawData
    ages = getAges rawData

    populationValues = map _.population rawData
    maxPop = fromMaybe 6000000.0 $ maximum populationValues

    -- Scale for Y axis
    yScale :: Scale
    yScale =
      { domain: { min: 0.0, max: maxPop }
      , range: { min: iHeight, max: 0.0 }
      }

    -- Legend items
    legendItems :: Array { age :: String, color :: String, index :: Int }
    legendItems = mapWithIndex (\i age -> { age, color: colorForAge age, index: i }) ages
  in
    elem SVG
      [ F.width dims.width
      , F.height dims.height
      , F.viewBox 0.0 0.0 dims.width dims.height
      , F.class_ "grouped-bar-chart"
      ]
      [ -- Chart content group (translated by margins)
        elem Group
          [ F.transform ("translate(" <> show dims.marginLeft <> "," <> show dims.marginTop <> ")")
          , F.class_ "chart-content"
          ]
          [ -- Y axis
            elem Group
              [ F.class_ "y-axis" ]
              [ renderAxisHATS (axisLeft yScale)
              , elem Text
                  [ F.transform ("translate(-65," <> show (iHeight / 2.0) <> ") rotate(-90)")
                  , F.textAnchor "middle"
                  , F.fontSize "12"
                  , F.fill "#333"
                  , staticStr "textContent" "Population"
                  ] []
              ]

          -- X axis (just the line, no numeric ticks)
          , elem Group
              [ F.transform ("translate(0," <> show iHeight <> ")")
              , F.class_ "x-axis"
              ]
              [ elem Line
                  [ F.x1 0.0
                  , F.y1 0.0
                  , F.x2 iWidth
                  , F.y2 0.0
                  , F.stroke "#ccc"
                  ] []
              ]

          -- State labels (centered under each group)
          , forEach "stateLabels" Group stateGroups (_.state) \stateGroup ->
              let
                groupWidth = iWidth / Int.toNumber (Array.length states)
                labelX = Int.toNumber stateGroup.stateIndex * groupWidth + groupWidth / 2.0
              in
                elem Text
                  [ thunkedNum "x" labelX
                  , thunkedNum "y" (iHeight + 25.0)
                  , F.textAnchor "middle"
                  , F.fontSize "11"
                  , F.fill "#333"
                  , thunkedStr "textContent" stateGroup.state
                  ] []

          -- Bars: outer forEach for state groups, inner forEach for bars
          , forEach "stateGroups" Group stateGroups (_.state) \stateGroup ->
              elem Group
                [ F.class_ "state-group"
                , thunkedStr "data-state" stateGroup.state
                ]
                [ forEach "bars" Rect stateGroup.bars (\b -> stateGroup.state <> "-" <> b.age) \bar ->
                    elem Rect
                      [ thunkedNum "x" bar.x
                      , thunkedNum "y" bar.y
                      , thunkedNum "width" bar.width
                      , thunkedNum "height" bar.height
                      , thunkedStr "fill" bar.color
                      , F.class_ "bar"
                      ] []
                ]

          -- Legend (right side, outside chart area)
          , elem Group
              [ F.transform ("translate(" <> show (iWidth + 15.0) <> ", 10)")
              , F.class_ "legend"
              ]
              [ -- Background rectangle
                elem Rect
                  [ F.x (-8.0)
                  , F.y (-18.0)
                  , F.width 78.0
                  , F.height (Int.toNumber (Array.length ages) * 18.0 + 28.0)
                  , F.fill "white"
                  , F.stroke "#ddd"
                  , F.strokeWidth 1.0
                  , staticNum "rx" 4.0
                  ] []
              -- Title
              , elem Text
                  [ F.x 0.0
                  , F.y (-4.0)
                  , F.fontSize "11"
                  , F.fontWeight "600"
                  , F.fill "#333"
                  , staticStr "textContent" "Age Group"
                  ] []
              -- Legend items
              , forEach "legendItems" Group legendItems (\l -> l.age) \item ->
                  elem Group
                    [ thunkedStr "transform" ("translate(0," <> show (Int.toNumber item.index * 18.0 + 14.0) <> ")")
                    ]
                    [ elem Rect
                        [ F.width 14.0
                        , F.height 14.0
                        , thunkedStr "fill" item.color
                        , staticNum "rx" 2.0
                        ] []
                    , elem Text
                        [ F.x 20.0
                        , F.y 11.0
                        , F.fontSize "10"
                        , F.fill "#333"
                        , thunkedStr "textContent" item.age
                        ] []
                    ]
              ]
          ]
      ]
