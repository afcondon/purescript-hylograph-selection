-- | HATS Demo
-- |
-- | Demonstrates the HATS (Hylomorphic Abstract Tree Syntax) system
-- | with actual positioned visualizations and behaviors.
module Test.HATSDemo where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

import PSD3.HATS (Tree(..), forEach, forEachWithGUP, fromTree, preserveTree, GUPSpec, TraversalOrder(..), empty)
import PSD3.HATS.Interpreter (render, rerender)
import Data.Tree as Tree
import DataViz.Layout.Hierarchy.Tree as TreeLayout
import Control.Comonad.Cofree (head, tail)
import Data.List (List(..), fromFoldable) as List
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AttrSource(..))
import PSD3.Internal.Behavior.Types (Behavior(..), DragConfig(..), ZoomConfig(..), ScaleExtent(..), onClickWithDatum, onHover)
import PSD3.Internal.Selection.Types (ElementType(..))
import Data.String.CodeUnits as String

-- ============================================================================
-- Attribute Helpers (convenience functions)
-- ============================================================================

staticAttr :: forall a. String -> String -> Attribute a
staticAttr name val = StaticAttr (AttributeName name) (StringValue val)

staticNum :: forall a. String -> Number -> Attribute a
staticNum name val = StaticAttr (AttributeName name) (NumberValue val)

dataNum :: forall a. String -> (a -> Number) -> Attribute a
dataNum name fn = DataAttr (AttributeName name) UnknownSource (NumberValue <<< fn)

dataStr :: forall a. String -> (a -> String) -> Attribute a
dataStr name fn = DataAttr (AttributeName name) UnknownSource (StringValue <<< fn)

indexedNum :: forall a. String -> (a -> Int -> Number) -> Attribute a
indexedNum name fn = IndexedAttr (AttributeName name) UnknownSource (\d i -> NumberValue (fn d i))

staticStr :: forall a. String -> (a -> String) -> Attribute a
staticStr name fn = DataAttr (AttributeName name) UnknownSource (StringValue <<< fn)

-- ============================================================================
-- Tree Helper with behaviors
-- ============================================================================

elemWithAttrs :: forall a. ElementType -> Array (Attribute a) -> Array (Tree a) -> Tree a
elemWithAttrs elemType attrs children = Elem
  { elemType
  , attrs
  , children
  , behaviors: []
  }

elemWithBehaviors
  :: forall a
   . ElementType
  -> Array (Attribute a)
  -> Array (Behavior a)
  -> Array (Tree a)
  -> Tree a
elemWithBehaviors elemType attrs behaviors children = Elem
  { elemType
  , attrs
  , children
  , behaviors
  }

-- ============================================================================
-- Demo 1: Interactive Scatter Plot with click and hover
-- ============================================================================

type Point = { id :: Int, x :: Number, y :: Number, r :: Number, name :: String }

samplePoints :: Array Point
samplePoints =
  [ { id: 1, x: 50.0, y: 50.0, r: 15.0, name: "Alpha" }
  , { id: 2, x: 120.0, y: 80.0, r: 20.0, name: "Beta" }
  , { id: 3, x: 180.0, y: 40.0, r: 12.0, name: "Gamma" }
  , { id: 4, x: 250.0, y: 100.0, r: 25.0, name: "Delta" }
  ]

-- | Demo 1: Interactive scatter plot
demo1 :: Tree Point
demo1 = elemWithAttrs Group []
  [ forEach "circles" samplePoints (\p -> show p.id) \_ ->
      elemWithBehaviors Circle
        [ dataNum "cx" _.x
        , dataNum "cy" _.y
        , dataNum "r" _.r
        , staticAttr "fill" "#0ff"
        , staticAttr "stroke" "#fff"
        , staticNum "stroke-width" 2.0
        ]
        [ onClickWithDatum \pt -> log $ "Clicked: " <> pt.name <> " (id=" <> show pt.id <> ")"
        , onHover
            { enter: [ { attr: "fill", value: "#ff0" }, { attr: "stroke-width", value: "4" } ]
            , leave: [ { attr: "fill", value: "#0ff" }, { attr: "stroke-width", value: "2" } ]
            }
        ]
        []
  ]

-- ============================================================================
-- Demo 2: Bar Chart
-- ============================================================================

type BarData = { label :: String, value :: Number }

sampleBars :: Array BarData
sampleBars =
  [ { label: "A", value: 80.0 }
  , { label: "B", value: 120.0 }
  , { label: "C", value: 60.0 }
  , { label: "D", value: 100.0 }
  , { label: "E", value: 45.0 }
  ]

barWidth :: Number
barWidth = 40.0

barGap :: Number
barGap = 10.0

demo2 :: Tree BarData
demo2 = elemWithAttrs Group [ staticAttr "transform" "translate(20, 130)" ]
  [ forEach "bars" sampleBars _.label \_ ->
      elemWithBehaviors Rect
        [ indexedNum "x" (\_ i -> toNumber i * (barWidth + barGap))
        , staticNum "y" 0.0
        , staticNum "width" barWidth
        , dataNum "height" _.value
        , staticAttr "fill" "#f0f"
        , staticAttr "stroke" "#fff"
        , staticNum "stroke-width" 1.0
        , staticAttr "transform" "scale(1, -1)"
        ]
        [ onClickWithDatum \bar -> log $ "Clicked bar: " <> bar.label <> " = " <> show bar.value
        , onHover
            { enter: [ { attr: "fill", value: "#ff0" } ]
            , leave: [ { attr: "fill", value: "#f0f" } ]
            }
        ]
        []
  ]

-- ============================================================================
-- Demo 3: Sudoku Board - Nested Folds [[Int]]
-- ============================================================================

-- | A 4x4 mini-Sudoku board
sudokuBoard :: Array (Array Int)
sudokuBoard =
  [ [1, 2, 3, 4]
  , [3, 4, 1, 2]
  , [2, 1, 4, 3]
  , [4, 3, 2, 1]
  ]

cellSize :: Number
cellSize = 30.0

-- | Wrapper type for a cell with its position
type SudokuCell = { rowIdx :: Int, colIdx :: Int, value :: Int }

-- | mapWithIndex helper
mapWithIndex :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapWithIndex f arr = go 0 arr
  where
  go _ [] = []
  go i xs = case xs of
    [] -> []
    _ ->
      let h = unsafeIndex xs 0
          t = unsafeSlice 1 (length xs) xs
      in [f i h] <> go (i + 1) t

foreign import unsafeIndex :: forall a. Array a -> Int -> a
foreign import unsafeSlice :: forall a. Int -> Int -> Array a -> Array a
foreign import length :: forall a. Array a -> Int

-- | Flatten board to array of cells with positions
flattenBoard :: Array (Array Int) -> Array SudokuCell
flattenBoard rows = do
  let indexed = mapWithIndex (\ri cells -> mapWithIndex (\ci v -> { rowIdx: ri, colIdx: ci, value: v }) cells) rows
  Array.concat indexed

-- | Demo 3: Sudoku board using flattened data
-- | (Nested folds would require existential types or a unified datum wrapper)
demo3 :: Tree SudokuCell
demo3 = elemWithAttrs Group [ staticAttr "transform" "translate(10, 10)" ]
  [ forEach "cells" (flattenBoard sudokuBoard) (\c -> show c.rowIdx <> "-" <> show c.colIdx) \_ ->
      elemWithBehaviors Group  -- Wrap Rect + Text in a Group
        [ dataNum "transform-translate-x" (\c -> toNumber c.colIdx * cellSize)  -- Won't render, just for structure
        ]
        [ onClickWithDatum \cell ->
            log $ "Cell [" <> show cell.rowIdx <> "," <> show cell.colIdx <> "] = " <> show cell.value
        ]
        [ -- Background rect
          elemWithBehaviors Rect
            [ dataNum "x" (\c -> toNumber c.colIdx * cellSize)
            , dataNum "y" (\c -> toNumber c.rowIdx * cellSize)
            , staticNum "width" (cellSize - 2.0)
            , staticNum "height" (cellSize - 2.0)
            , staticAttr "fill" "#234"
            , staticAttr "stroke" "#567"
            , staticNum "stroke-width" 1.0
            ]
            [ onHover
                { enter: [ { attr: "fill", value: "#456" } ]
                , leave: [ { attr: "fill", value: "#234" } ]
                }
            ]
            []
        , -- Number text
          elemWithAttrs Text
            [ dataNum "x" (\c -> toNumber c.colIdx * cellSize + cellSize / 2.0 - 1.0)
            , dataNum "y" (\c -> toNumber c.rowIdx * cellSize + cellSize / 2.0 + 5.0)
            , staticAttr "text-anchor" "middle"
            , staticAttr "fill" "#fff"
            , staticAttr "font-size" "16"
            , staticAttr "font-family" "monospace"
            , dataStr "textContent" (\c -> show c.value)  -- This won't work directly
            ]
            []
        ]
  ]

-- ============================================================================
-- Demo 4: Simple Line Chart Points
-- ============================================================================

type DataPoint = { x :: Number, y :: Number }

lineData :: Array DataPoint
lineData =
  [ { x: 20.0, y: 100.0 }
  , { x: 60.0, y: 40.0 }
  , { x: 100.0, y: 80.0 }
  , { x: 140.0, y: 20.0 }
  , { x: 180.0, y: 60.0 }
  , { x: 220.0, y: 30.0 }
  , { x: 260.0, y: 70.0 }
  ]

demo4 :: Tree DataPoint
demo4 = elemWithAttrs Group []
  [ forEach "points" lineData (\p -> show p.x <> "," <> show p.y) \_ ->
      elemWithBehaviors Circle
        [ dataNum "cx" _.x
        , dataNum "cy" _.y
        , staticNum "r" 5.0
        , staticAttr "fill" "#0f0"
        , staticAttr "stroke" "#fff"
        , staticNum "stroke-width" 1.0
        ]
        [ onClickWithDatum \pt -> log $ "Point: (" <> show pt.x <> ", " <> show pt.y <> ")"
        , onHover
            { enter: [ { attr: "r", value: "8" }, { attr: "fill", value: "#ff0" } ]
            , leave: [ { attr: "r", value: "5" }, { attr: "fill", value: "#0f0" } ]
            }
        ]
        []
  ]

-- ============================================================================
-- Demo 5: Draggable Circles
-- ============================================================================

type DragPoint = { id :: Int, x :: Number, y :: Number, color :: String }

dragPoints :: Array DragPoint
dragPoints =
  [ { id: 1, x: 50.0, y: 75.0, color: "#f00" }
  , { id: 2, x: 150.0, y: 75.0, color: "#0f0" }
  , { id: 3, x: 250.0, y: 75.0, color: "#00f" }
  ]

-- | Demo 5: Draggable circles using SimpleDrag
-- | SimpleDrag adds transform to the element on drag
demo5 :: Tree DragPoint
demo5 = elemWithAttrs Group []
  [ forEach "draggable" dragPoints (\p -> show p.id) \_ ->
      elemWithBehaviors Circle
        [ dataNum "cx" _.x
        , dataNum "cy" _.y
        , staticNum "r" 25.0
        , dataStr "fill" _.color
        , staticAttr "stroke" "#fff"
        , staticNum "stroke-width" 3.0
        ]
        [ Drag SimpleDrag  -- SimpleDrag adds transform offset on drag
        , onClickWithDatum \pt -> log $ "Circle " <> show pt.id <> " clicked"
        , onHover
            { enter: [ { attr: "stroke-width", value: "5" } ]
            , leave: [ { attr: "stroke-width", value: "3" } ]
            }
        ]
        []
  ]

-- ============================================================================
-- Demo 6: Zoom/Pan Container
-- ============================================================================

type ZoomPoint = { id :: Int, x :: Number, y :: Number }

zoomPoints :: Array ZoomPoint
zoomPoints =
  [ { id: 1, x: 40.0, y: 40.0 }
  , { id: 2, x: 100.0, y: 70.0 }
  , { id: 3, x: 160.0, y: 50.0 }
  , { id: 4, x: 70.0, y: 100.0 }
  , { id: 5, x: 130.0, y: 110.0 }
  ]

-- | Demo 6: Zoom/pan on container
-- | The zoom behavior is on the outer group, transforms inner content
-- | A transparent background rect catches wheel/pointer events
demo6 :: Tree ZoomPoint
demo6 = elemWithBehaviors Group
  [ staticAttr "class" "zoom-container" ]
  [ Zoom (ZoomConfig { scaleExtent: ScaleExtent 0.5 4.0, targetSelector: ".zoom-content" }) ]
  [ -- Background rect to catch events (transparent but pointer-events enabled)
    elemWithAttrs Rect
      [ staticNum "x" 0.0
      , staticNum "y" 0.0
      , staticNum "width" 300.0
      , staticNum "height" 150.0
      , staticAttr "fill" "transparent"
      , staticAttr "class" "zoom-background"
      ]
      []
  , elemWithAttrs Group [ staticAttr "class" "zoom-content" ]
      [ forEach "zoom-circles" zoomPoints (\p -> show p.id) \_ ->
          elemWithBehaviors Circle
            [ dataNum "cx" _.x
            , dataNum "cy" _.y
            , staticNum "r" 12.0
            , staticAttr "fill" "#f80"
            , staticAttr "stroke" "#fff"
            , staticNum "stroke-width" 2.0
            ]
            [ onClickWithDatum \pt -> log $ "Zoom circle " <> show pt.id
            , onHover
                { enter: [ { attr: "r", value: "15" }, { attr: "fill", value: "#ff0" } ]
                , leave: [ { attr: "r", value: "12" }, { attr: "fill", value: "#f80" } ]
                }
            ]
            []
      ]
  ]

-- ============================================================================
-- Demo 7: GUP (General Update Pattern)
-- ============================================================================

type GUPItem = { id :: Int, x :: Number, y :: Number }

-- | Initial data for GUP demo
initialGUPData :: Array GUPItem
initialGUPData =
  [ { id: 1, x: 50.0, y: 75.0 }
  , { id: 2, x: 100.0, y: 75.0 }
  , { id: 3, x: 150.0, y: 75.0 }
  ]

-- | GUP spec with enter/update/exit styling
gupSpec :: GUPSpec GUPItem
gupSpec =
  { enter: Just
      { attrs: [ staticAttr "fill" "#0f0" ]  -- Green for entering
      , transition: Nothing
      }
  , update: Just
      { attrs: [ staticAttr "fill" "#00f" ]  -- Blue for updating
      , transition: Nothing
      }
  , exit: Just
      { attrs: [ staticAttr "fill" "#f00" ]  -- Red before removing
      , transition: Nothing
      }
  }

-- | Create a GUP tree from data
makeGUPTree :: Array GUPItem -> Tree GUPItem
makeGUPTree items = elemWithAttrs Group []
  [ forEachWithGUP "gup-circles" items (\p -> show p.id) gupTemplate gupSpec
  ]
  where
  gupTemplate _ = elemWithBehaviors Circle
    [ dataNum "cx" _.x
    , dataNum "cy" _.y
    , staticNum "r" 20.0
    , staticAttr "fill" "#888"  -- Default gray, overridden by GUP phases
    , staticAttr "stroke" "#fff"
    , staticNum "stroke-width" 2.0
    ]
    [ onClickWithDatum \pt -> log $ "GUP circle " <> show pt.id
    , onHover
        { enter: [ { attr: "stroke-width", value: "4" } ]
        , leave: [ { attr: "stroke-width", value: "2" } ]
        }
    ]
    []

-- | Demo 7: GUP demonstration
-- | Uses mutable state and rerender to show enter/update/exit
demo7 :: Effect Unit
demo7 = do
  -- Create mutable state for the data
  dataRef <- Ref.new initialGUPData
  nextIdRef <- Ref.new 4  -- Next ID to assign

  -- Initial render
  _ <- render "#demo7" (makeGUPTree initialGUPData)

  -- Set up button handlers via FFI
  setupGUPButtons
    -- Add handler
    (\_ -> do
      currentData <- Ref.read dataRef
      nextId <- Ref.read nextIdRef
      -- Find rightmost x position and place new circle to the right
      let maxX = Array.foldl (\acc item -> max acc item.x) 0.0 currentData
      let newItem = { id: nextId, x: maxX + 50.0, y: 75.0 }
      let newData = currentData <> [newItem]
      Ref.write newData dataRef
      Ref.write (nextId + 1) nextIdRef
      _ <- rerender "#demo7" (makeGUPTree newData)
      log $ "Added circle " <> show nextId <> ", now have " <> show (Array.length newData)
    )
    -- Remove handler
    (\_ -> do
      currentData <- Ref.read dataRef
      case Array.unsnoc currentData of
        Just { init, last } -> do
          Ref.write init dataRef
          _ <- rerender "#demo7" (makeGUPTree init)
          log $ "Removed circle " <> show last.id <> ", now have " <> show (Array.length init)
        Nothing ->
          log "No circles to remove"
    )
    -- Shuffle handler
    (\_ -> do
      currentData <- Ref.read dataRef
      -- Simple shuffle: reverse and shift x positions
      let shuffled = map (\item -> item { x = 250.0 - item.x }) (Array.reverse currentData)
      Ref.write shuffled dataRef
      _ <- rerender "#demo7" (makeGUPTree shuffled)
      log "Shuffled circles"
    )

  pure unit

foreign import setupGUPButtons
  :: (Unit -> Effect Unit)  -- Add handler
  -> (Unit -> Effect Unit)  -- Remove handler
  -> (Unit -> Effect Unit)  -- Shuffle handler
  -> Effect Unit

-- ============================================================================
-- Demo 8: Tree Layout Visualization
-- ============================================================================

-- | Tree node with layout fields
type TreeNode =
  { name :: String
  , x :: Number
  , y :: Number
  , depth :: Int
  }

-- | Link between parent and child
type TreeLink =
  { source :: TreeNode
  , target :: TreeNode
  }

-- | Union type for tree elements (needed because Tree a has single type)
data TreeElement = TreeNodeEl TreeNode | TreeLinkEl TreeLink

-- | Key function for tree elements
treeElementKey :: TreeElement -> String
treeElementKey = case _ of
  TreeNodeEl n -> "node-" <> n.name
  TreeLinkEl l -> "link-" <> l.source.name <> "->" <> l.target.name

-- | Create sample tree using Data.Tree
-- | mkTree expects List, so we convert from arrays
sampleTree :: Tree.Tree TreeNode
sampleTree = Tree.mkTree root $ List.fromFoldable
  [ Tree.mkTree child1 $ List.fromFoldable
      [ Tree.mkTree grandchild1 List.Nil
      , Tree.mkTree grandchild2 List.Nil
      ]
  , Tree.mkTree child2 $ List.fromFoldable
      [ Tree.mkTree grandchild3 List.Nil
      ]
  , Tree.mkTree child3 List.Nil
  ]
  where
  -- Initial positions don't matter - layout will compute them
  root = { name: "Root", x: 0.0, y: 0.0, depth: 0 }
  child1 = { name: "Child1", x: 0.0, y: 0.0, depth: 0 }
  child2 = { name: "Child2", x: 0.0, y: 0.0, depth: 0 }
  child3 = { name: "Child3", x: 0.0, y: 0.0, depth: 0 }
  grandchild1 = { name: "GC1", x: 0.0, y: 0.0, depth: 0 }
  grandchild2 = { name: "GC2", x: 0.0, y: 0.0, depth: 0 }
  grandchild3 = { name: "GC3", x: 0.0, y: 0.0, depth: 0 }

-- | Tree layout config
treeConfig :: TreeLayout.TreeConfig TreeNode
treeConfig = TreeLayout.defaultTreeConfig
  { size = { width: 260.0, height: 120.0 }
  , minSeparation = 1.0
  }

-- | Apply layout and extract positioned tree
positionedTree :: Tree.Tree TreeNode
positionedTree = TreeLayout.tree treeConfig sampleTree

-- | Flatten tree to array of nodes
flattenTree :: Tree.Tree TreeNode -> Array TreeNode
flattenTree t = [head t] <> Array.concatMap flattenTree (Array.fromFoldable (tail t))

-- | Extract links (parent-child pairs) from tree
extractLinks :: Tree.Tree TreeNode -> Array TreeLink
extractLinks t =
  let parent = head t
      children = Array.fromFoldable (tail t)
      directLinks = map (\child -> { source: parent, target: head child }) children
      childLinks = Array.concatMap extractLinks children
  in directLinks <> childLinks

-- | Positioned nodes for Demo 8
treeNodes :: Array TreeNode
treeNodes = flattenTree positionedTree

-- | Links for Demo 8
treeLinks :: Array TreeLink
treeLinks = extractLinks positionedTree

-- | Combined elements (links first so they render behind nodes)
treeElements :: Array TreeElement
treeElements = map TreeLinkEl treeLinks <> map TreeNodeEl treeNodes

-- | Demo 8: Tree visualization
-- | Shows nodes as circles, links as lines
demo8 :: Tree TreeElement
demo8 = elemWithAttrs Group [ staticAttr "transform" "translate(20, 15)" ]
  [ forEach "tree-elements" treeElements treeElementKey \_ ->
      -- Template renders differently based on element type
      -- But since we can't pattern match inside Tree, we need a different approach
      elemWithAttrs Group [] []
  ]

-- | Actually, let's render links and nodes separately with proper types
demo8Links :: Tree TreeLink
demo8Links = elemWithAttrs Group [ staticAttr "transform" "translate(20, 15)" ]
  [ forEach "tree-links" treeLinks (\link -> link.source.name <> "->" <> link.target.name) \_ ->
      elemWithAttrs Line
        [ dataNum "x1" (_.source >>> _.x)
        , dataNum "y1" (_.source >>> _.y)
        , dataNum "x2" (_.target >>> _.x)
        , dataNum "y2" (_.target >>> _.y)
        , staticAttr "stroke" "#666"
        , staticNum "stroke-width" 2.0
        ]
        []
  ]

demo8Nodes :: Tree TreeNode
demo8Nodes = elemWithAttrs Group [ staticAttr "transform" "translate(20, 15)" ]
  [ forEach "tree-nodes" treeNodes _.name \_ ->
      elemWithBehaviors Group []
        [ onClickWithDatum \node -> log $ "Node: " <> node.name <> " at (" <> show node.x <> ", " <> show node.y <> ")"
        , onHover
            { enter: [ { attr: "opacity", value: "0.8" } ]
            , leave: [ { attr: "opacity", value: "1" } ]
            }
        ]
        [ elemWithAttrs Circle
            [ dataNum "cx" _.x
            , dataNum "cy" _.y
            , staticNum "r" 12.0
            , staticAttr "fill" "#f0f"
            , staticAttr "stroke" "#fff"
            , staticNum "stroke-width" 2.0
            ]
            []
        , elemWithAttrs Text
            [ dataNum "x" _.x
            , dataNum "y" (\n -> n.y + 4.0)
            , staticAttr "text-anchor" "middle"
            , staticAttr "fill" "#fff"
            , staticAttr "font-size" "8"
            , staticAttr "font-family" "sans-serif"
            , staticAttr "pointer-events" "none"
            , dataStr "textContent" (_.name >>> String.take 4)
            ]
            []
        ]
  ]

-- ============================================================================
-- Demo 9: Force-Directed Graph
-- ============================================================================

-- | Node for force simulation
type ForceNode =
  { id :: String
  , x :: Number
  , y :: Number
  }

-- | Initial nodes for force demo
forceNodes :: Array ForceNode
forceNodes =
  [ { id: "A", x: 80.0, y: 75.0 }
  , { id: "B", x: 120.0, y: 50.0 }
  , { id: "C", x: 160.0, y: 75.0 }
  , { id: "D", x: 200.0, y: 100.0 }
  , { id: "E", x: 120.0, y: 100.0 }
  ]

-- | Demo 9: Force-directed graph
-- | HATS renders initial structure, simulation updates positions via D3
demo9 :: Tree ForceNode
demo9 = elemWithAttrs Group []
  [ forEach "force-nodes" forceNodes _.id \_ ->
      elemWithBehaviors Circle
        [ dataNum "cx" _.x
        , dataNum "cy" _.y
        , staticNum "r" 15.0
        , staticAttr "fill" "#0ff"
        , staticAttr "stroke" "#fff"
        , staticNum "stroke-width" 2.0
        , staticAttr "class" "force-node"
        ]
        [ onClickWithDatum \node -> log $ "Force node: " <> node.id
        , onHover
            { enter: [ { attr: "r", value: "18" }, { attr: "fill", value: "#ff0" } ]
            , leave: [ { attr: "r", value: "15" }, { attr: "fill", value: "#0ff" } ]
            }
        ]
        []
  ]

-- | Start force simulation using D3 directly via FFI
-- | Demonstrates HATS + simulation integration
foreign import startForceSimulation :: Effect Unit

-- ============================================================================
-- Demo 10: FromTree with Siblings (Coalgebraic Enumeration)
-- ============================================================================

-- | A simple tree node type (newtype needed for recursion)
newtype HierNode = HierNode
  { name :: String
  , value :: Number
  , children :: Array HierNode
  }

-- | Accessor for name
hierName :: HierNode -> String
hierName (HierNode r) = r.name

-- | Accessor for value
hierValue :: HierNode -> Number
hierValue (HierNode r) = r.value

-- | Accessor for children
hierChildren :: HierNode -> Array HierNode
hierChildren (HierNode r) = r.children

-- | Sample hierarchical data
hierData :: HierNode
hierData = HierNode
  { name: "root"
  , value: 100.0
  , children:
      [ HierNode { name: "A", value: 40.0
        , children:
            [ HierNode { name: "A1", value: 15.0, children: [] }
            , HierNode { name: "A2", value: 25.0, children: [] }
            ]
        }
      , HierNode { name: "B", value: 35.0
        , children:
            [ HierNode { name: "B1", value: 20.0, children: [] }
            , HierNode { name: "B2", value: 15.0, children: [] }
            ]
        }
      , HierNode { name: "C", value: 25.0, children: [] }
      ]
  }

-- | Check if a node is a leaf
isLeaf :: HierNode -> Boolean
isLeaf n = Array.null (hierChildren n)

-- | Demo 10: FromTree with Siblings assembly
-- | The coalgebra (hierChildren) unfolds the tree; output is flat siblings.
-- | This is Pattern 2 from the design doc: "Tree to Flat DOM"
demo10 :: Tree HierNode
demo10 = elemWithAttrs Group []
  [ fromTree "hier-nodes" hierData hierChildren hierName DepthFirst true \node ->
      let
        -- Position based on DFS order (simplified - real layout would use algorithm)
        idx = nodeIndex (hierName node)
        col = idx `mod` 5
        row = idx / 5
      in
        elemWithBehaviors Circle
          [ staticNum "cx" (30.0 + toNumber col * 50.0)
          , staticNum "cy" (30.0 + toNumber row * 50.0)
          , staticNum "r" (if isLeaf node then 15.0 else 10.0)
          , staticAttr "fill" (if isLeaf node then "#0f0" else "#f80")
          , staticAttr "stroke" "#fff"
          , staticNum "stroke-width" 2.0
          ]
          [ onClickWithDatum \n -> log $ "Node: " <> hierName n <> " (value=" <> show (hierValue n) <> ")"
          , onHover
              { enter: [ { attr: "r", value: if isLeaf node then "18" else "12" } ]
              , leave: [ { attr: "r", value: if isLeaf node then "15" else "10" } ]
              }
          ]
          []
  ]
  where
  -- Simple index lookup (for demo positioning)
  nodeIndex name = case name of
    "root" -> 0
    "A" -> 1
    "B" -> 2
    "C" -> 3
    "A1" -> 4
    "A2" -> 5
    "B1" -> 6
    "B2" -> 7
    _ -> 0

-- ============================================================================
-- Demo 11: FromTree with Nested (Preserved Hierarchy)
-- ============================================================================

-- | Demo 11: FromTree with Nested assembly
-- | The tree structure is preserved in the DOM - children become DOM children.
-- | This is Pattern 3 from the design doc: "Tree to Nested DOM"
demo11 :: Tree HierNode
demo11 = elemWithAttrs Group [ staticAttr "transform" "translate(10, 10)" ]
  [ preserveTree "nested-nodes" hierData hierChildren hierName \node ->
      let
        nodeName = hierName node
        isRoot = nodeName == "root"
        depth = nodeDepth nodeName
      in
        elemWithBehaviors Group
          [ staticAttr "transform" ("translate(" <> show (toNumber depth * 15.0) <> ", 0)")
          ]
          [ onClickWithDatum \n -> log $ "Nested node: " <> hierName n
          ]
          [ elemWithAttrs Rect
              [ staticNum "x" 0.0
              , staticNum "y" (toNumber (nodeYIndex nodeName) * 18.0)
              , staticNum "width" (if isRoot then 120.0 else 100.0 - toNumber depth * 15.0)
              , staticNum "height" 16.0
              , staticAttr "fill" (depthColor depth)
              , staticAttr "stroke" "#333"
              , staticNum "stroke-width" 1.0
              ]
              []
          , elemWithAttrs Text
              [ staticNum "x" 4.0
              , staticNum "y" (toNumber (nodeYIndex nodeName) * 18.0 + 12.0)
              , staticAttr "fill" "#fff"
              , staticAttr "font-size" "10"
              , staticAttr "font-family" "monospace"
              , dataStr "textContent" hierName
              ]
              []
          ]
  ]
  where
  nodeDepth name = case name of
    "root" -> 0
    "A" -> 1
    "B" -> 1
    "C" -> 1
    _ -> 2
  nodeYIndex name = case name of
    "root" -> 0
    "A" -> 1
    "A1" -> 2
    "A2" -> 3
    "B" -> 4
    "B1" -> 5
    "B2" -> 6
    "C" -> 7
    _ -> 0
  depthColor d = case d of
    0 -> "#666"
    1 -> "#888"
    _ -> "#aaa"

-- ============================================================================
-- Demo 12: Heterogeneous Nodes (Different Shapes)
-- ============================================================================

-- | Shape type for heterogeneous rendering
data NodeShape = CircleShape | SquareShape | TriangleShape

-- | Node with shape information
type ShapedNode =
  { id :: String
  , x :: Number
  , y :: Number
  , shape :: NodeShape
  , size :: Number
  }

-- | Sample nodes with different shapes
shapedNodes :: Array ShapedNode
shapedNodes =
  [ { id: "c1", x: 40.0, y: 50.0, shape: CircleShape, size: 20.0 }
  , { id: "s1", x: 100.0, y: 50.0, shape: SquareShape, size: 30.0 }
  , { id: "t1", x: 160.0, y: 50.0, shape: TriangleShape, size: 25.0 }
  , { id: "c2", x: 220.0, y: 50.0, shape: CircleShape, size: 15.0 }
  , { id: "s2", x: 40.0, y: 110.0, shape: SquareShape, size: 25.0 }
  , { id: "t2", x: 100.0, y: 110.0, shape: TriangleShape, size: 30.0 }
  , { id: "c3", x: 160.0, y: 110.0, shape: CircleShape, size: 25.0 }
  , { id: "s3", x: 220.0, y: 110.0, shape: SquareShape, size: 20.0 }
  ]

-- | Demo 12: Heterogeneous nodes - template produces different elements based on data
-- | This demonstrates the "chimera" pattern where the same fold produces
-- | different visual elements based on node properties.
demo12 :: Tree ShapedNode
demo12 = elemWithAttrs Group []
  [ forEach "shaped-nodes" shapedNodes _.id \node ->
      case node.shape of
        CircleShape ->
          elemWithBehaviors Circle
            [ dataNum "cx" _.x
            , dataNum "cy" _.y
            , dataNum "r" _.size
            , staticAttr "fill" "#f0f"
            , staticAttr "stroke" "#fff"
            , staticNum "stroke-width" 2.0
            ]
            [ onClickWithDatum \n -> log $ "Circle: " <> n.id
            , onHover
                { enter: [ { attr: "fill", value: "#ff0" } ]
                , leave: [ { attr: "fill", value: "#f0f" } ]
                }
            ]
            []

        SquareShape ->
          elemWithBehaviors Rect
            [ dataNum "x" (\n -> n.x - n.size / 2.0)
            , dataNum "y" (\n -> n.y - n.size / 2.0)
            , dataNum "width" _.size
            , dataNum "height" _.size
            , staticAttr "fill" "#0ff"
            , staticAttr "stroke" "#fff"
            , staticNum "stroke-width" 2.0
            ]
            [ onClickWithDatum \n -> log $ "Square: " <> n.id
            , onHover
                { enter: [ { attr: "fill", value: "#ff0" } ]
                , leave: [ { attr: "fill", value: "#0ff" } ]
                }
            ]
            []

        TriangleShape ->
          elemWithBehaviors Polygon
            [ dataStr "points" (\n ->
                let s = n.size
                    x = n.x
                    y = n.y
                in show x <> "," <> show (y - s) <> " " <>
                   show (x - s) <> "," <> show (y + s * 0.7) <> " " <>
                   show (x + s) <> "," <> show (y + s * 0.7))
            , staticAttr "fill" "#0f0"
            , staticAttr "stroke" "#fff"
            , staticNum "stroke-width" 2.0
            ]
            [ onClickWithDatum \n -> log $ "Triangle: " <> n.id
            , onHover
                { enter: [ { attr: "fill", value: "#ff0" } ]
                , leave: [ { attr: "fill", value: "#0f0" } ]
                }
            ]
            []
  ]

-- ============================================================================
-- Demo 13: Compositional Chart Building (Monoid)
-- ============================================================================

-- | Chart data
type BarDatum = { value :: Number, label :: String }

chartBarsData :: Array BarDatum
chartBarsData =
  [ { value: 80.0, label: "A" }
  , { value: 120.0, label: "B" }
  , { value: 60.0, label: "C" }
  , { value: 100.0, label: "D" }
  , { value: 45.0, label: "E" }
  ]

-- | Title component (polymorphic - works with any datum type)
chartTitle :: forall a. String -> Tree a
chartTitle titleText = elemWithAttrs Text
  [ staticNum "x" 140.0
  , staticNum "y" 15.0
  , staticAttr "text-anchor" "middle"
  , staticAttr "fill" "#fff"
  , staticAttr "font-size" "14"
  , staticAttr "font-weight" "bold"
  , staticAttr "textContent" titleText
  ]
  []

-- | Y-axis component (polymorphic)
chartYAxis :: forall a. Tree a
chartYAxis = elemWithAttrs Line
  [ staticNum "x1" 30.0
  , staticNum "y1" 25.0
  , staticNum "x2" 30.0
  , staticNum "y2" 120.0
  , staticAttr "stroke" "#666"
  , staticNum "stroke-width" 1.0
  ]
  []

-- | X-axis labels component
chartXAxis :: Array BarDatum -> Tree BarDatum
chartXAxis dat = elemWithAttrs Group [ staticAttr "transform" "translate(30, 125)" ]
  [ forEach "x-labels" dat _.label \_ ->
      elemWithAttrs Text
        [ indexedNum "x" (\_ i -> toNumber i * 50.0 + 20.0)
        , staticNum "y" 0.0
        , staticAttr "text-anchor" "middle"
        , staticAttr "fill" "#888"
        , staticAttr "font-size" "10"
        , dataStr "textContent" _.label
        ]
        []
  ]

-- | Bars component
chartBars :: Array BarDatum -> Tree BarDatum
chartBars dat = elemWithAttrs Group [ staticAttr "transform" "translate(30, 120)" ]
  [ forEach "bars" dat _.label \_ ->
      elemWithBehaviors Rect
        [ indexedNum "x" (\_ i -> toNumber i * 50.0 + 5.0)
        , staticNum "y" 0.0
        , staticNum "width" 30.0
        , dataNum "height" _.value
        , staticAttr "fill" "#0ff"
        , staticAttr "transform" "scale(1, -1)"
        ]
        [ onClickWithDatum \d -> log $ "Bar " <> d.label <> " = " <> show d.value
        , onHover
            { enter: [ { attr: "fill", value: "#ff0" } ]
            , leave: [ { attr: "fill", value: "#0ff" } ]
            }
        ]
        []
  ]

-- | Optional legend component (polymorphic)
chartLegend :: forall a. Boolean -> Tree a
chartLegend showIt = if showIt
  then elemWithAttrs Group [ staticAttr "transform" "translate(240, 40)" ]
    [ elemWithAttrs Rect
        [ staticNum "x" 0.0, staticNum "y" 0.0
        , staticNum "width" 12.0, staticNum "height" 12.0
        , staticAttr "fill" "#0ff"
        ]
        []
    , elemWithAttrs Text
        [ staticNum "x" 18.0, staticNum "y" 10.0
        , staticAttr "fill" "#888"
        , staticAttr "font-size" "10"
        , staticAttr "textContent" "Value"
        ]
        []
    ]
  else empty  -- Monoid identity - contributes nothing

-- | Demo 13: Composed chart using Monoid instance
-- |
-- | Components with compatible types can be combined directly with <>
-- | The polymorphic components (title, yAxis, legend) adapt to any datum type.
-- | This shows: chart = title <> yAxis <> xAxis <> bars <> legend
demo13 :: Tree BarDatum
demo13 =
  chartTitle "Compositional Chart"
  <> chartYAxis
  <> chartXAxis chartBarsData
  <> chartBars chartBarsData
  <> chartLegend true

-- ============================================================================
-- Main
-- ============================================================================

main :: Effect Unit
main = do
  log "HATS Demo - Interactive Visualizations"
  log "======================================="

  log "\nDemo 1: Interactive Scatter Plot (click circles, hover to highlight)"
  selectionMap1 <- render "#demo1" demo1
  log $ "  Created selections: " <> show (Map.keys selectionMap1)

  log "\nDemo 2: Bar Chart (click bars, hover to highlight)"
  selectionMap2 <- render "#demo2" demo2
  log $ "  Created selections: " <> show (Map.keys selectionMap2)

  log "\nDemo 3: Sudoku Board (flattened [[Int]] - click cells)"
  selectionMap3 <- render "#demo3" demo3
  log $ "  Created selections: " <> show (Map.keys selectionMap3)

  log "\nDemo 4: Line Chart Points (click points)"
  selectionMap4 <- render "#demo4" demo4
  log $ "  Created selections: " <> show (Map.keys selectionMap4)

  log "\nDemo 5: Draggable Circles (drag to move)"
  selectionMap5 <- render "#demo5" demo5
  log $ "  Created selections: " <> show (Map.keys selectionMap5)

  log "\nDemo 6: Zoom/Pan (scroll to zoom, drag to pan)"
  selectionMap6 <- render "#demo6" demo6
  log $ "  Created selections: " <> show (Map.keys selectionMap6)

  log "\nDemo 7: GUP (add/remove/shuffle circles)"
  demo7
  log "  Use buttons to add, remove, or shuffle circles"
  log "  Enter=green, Update=blue, Exit=red (briefly visible)"

  log "\nDemo 8: Tree Layout (Reingold-Tilford algorithm)"
  -- Render links first (behind), then nodes (on top)
  selectionMap8a <- render "#demo8" demo8Links
  selectionMap8b <- render "#demo8" demo8Nodes
  log $ "  Link selections: " <> show (Map.keys selectionMap8a)
  log $ "  Node selections: " <> show (Map.keys selectionMap8b)
  log $ "  Nodes: " <> show (Array.length treeNodes) <> ", Links: " <> show (Array.length treeLinks)

  log "\nDemo 9: Force-Directed Graph (nodes repel, settle into equilibrium)"
  selectionMap9 <- render "#demo9" demo9
  log $ "  Created selections: " <> show (Map.keys selectionMap9)
  startForceSimulation
  log "  Simulation running..."

  log "\nDemo 10: FromTree with Siblings (coalgebraic enumeration, flat output)"
  selectionMap10 <- render "#demo10" demo10
  log $ "  Created selections: " <> show (Map.keys selectionMap10)
  log "  Tree unfolds via coalgebra, renders as flat siblings"

  log "\nDemo 11: FromTree with Nested (preserved hierarchy in DOM)"
  selectionMap11 <- render "#demo11" demo11
  log $ "  Created selections: " <> show (Map.keys selectionMap11)
  log "  Tree structure preserved in DOM nesting"

  log "\nDemo 12: Heterogeneous Nodes (circles, squares, triangles)"
  selectionMap12 <- render "#demo12" demo12
  log $ "  Created selections: " <> show (Map.keys selectionMap12)
  log "  Template produces different elements based on data"

  log "\nDemo 13: Compositional Chart Building (Monoid)"
  selectionMap13 <- render "#demo13" demo13
  log $ "  Created selections: " <> show (Map.keys selectionMap13)
  log "  Chart = title <> yAxis <> xAxis <> bars <> legend"

  log "\nDone! Try interacting with all the demos."
