-- | HATS: Hylomorphic Abstract Tree Syntax
-- |
-- | A principled AST for data visualization based on recursion schemes.
-- |
-- | The core insight: a visualization is a **transduction** from data to DOM,
-- | composed of two orthogonal choices:
-- |
-- | - **Enumeration**: How to traverse/extract elements from input (coalgebra)
-- | - **Assembly**: How to structure the output DOM (algebra)
-- |
-- | This is a hylomorphism with heterogeneous functors:
-- |
-- | ```
-- | Input ──(enumerate)──▶ [a] ──(template)──▶ [Tree] ──(assemble)──▶ DOM
-- | ```
-- |
-- | Key types:
-- | - `Tree a` - The visual specification (what to render)
-- | - `Enumeration a` - How to get elements from input
-- | - `Assembly` - How to structure output
-- | - `Fold` - The universal iteration primitive
module PSD3.HATS
  ( Tree(..)
  , Enumeration(..)
  , Assembly(..)
  , TraversalOrder(..)
  , GUPSpec(..)
  , PhaseSpec(..)
  -- Smart constructors
  , elem
  , fold
  , empty
  , siblings
  -- Convenience combinators
  , forEach
  , forEachWithGUP
  , fromTree
  , preserveTree
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.Internal.Attribute (Attribute)
import PSD3.Internal.Behavior.Types (Behavior)
import PSD3.Internal.Transition.Types (TransitionConfig)

-- ============================================================================
-- Core AST
-- ============================================================================

-- | The HATS tree type - a specification for visual structure.
-- |
-- | Only three constructors:
-- | - `Elem` - A DOM element with attributes, children, and behaviors
-- | - `Fold` - The universal iteration primitive (enumerate, transform, assemble)
-- | - `Empty` - Nothing to render
data Tree a
  = Elem
      { elemType :: ElementType
      , attrs :: Array (Attribute a)
      , children :: Array (Tree a)
      , behaviors :: Array (Behavior a)
      }

  | Fold
      { name :: String                    -- Selection name for retrieval
      , enumerate :: Enumeration a        -- How to get elements from input
      , assemble :: Assembly              -- How to structure output
      , keyFn :: a -> String              -- Identity function for diffing
      , template :: a -> Tree a           -- How to visualize one element
      , gup :: Maybe (GUPSpec a)          -- Optional enter/update/exit
      }

  | Empty

-- | Semigroup instance: trees combine as siblings
-- |
-- | This enables compositional chart building:
-- | ```purescript
-- | chart = title <> xAxis <> yAxis <> plotArea <> legend
-- | ```
-- |
-- | Empty acts as identity, and bare Groups are flattened for associativity:
-- | `(a <> b) <> c = a <> (b <> c) = siblings [a, b, c]`
instance Semigroup (Tree a) where
  append Empty t = t
  append t Empty = t
  append t1 t2 = siblings [t1, t2]

-- | Monoid instance with Empty as identity
-- |
-- | Enables `foldMap` for building visualizations:
-- | ```purescript
-- | dashboard = foldMap renderWidget widgets
-- | annotations = foldMap annotate (filter significant points)
-- | ```
instance Monoid (Tree a) where
  mempty = Empty

-- ============================================================================
-- Enumeration (Coalgebra - how to unfold/traverse input)
-- ============================================================================

-- | How to extract elements from a data structure.
-- |
-- | This is the coalgebraic part of the hylomorphism - it "unfolds"
-- | the input structure into a sequence of elements to be transformed.
data Enumeration a
  -- | From a flat collection
  = FromArray (Array a)

  -- | From a tree/recursive structure
  | FromTree
      { root :: a
      , children :: a -> Array a          -- The coalgebra
      , order :: TraversalOrder
      , includeInternal :: Boolean        -- Include non-leaf nodes?
      }

  -- | With explicit context (depth, index, etc.)
  -- | Useful when enumeration happens outside HATS
  | WithContext (Array { datum :: a, depth :: Int, index :: Int })

-- | Traversal order for tree enumeration
data TraversalOrder
  = DepthFirst
  | BreadthFirst

-- ============================================================================
-- Assembly (Algebra - how to fold/structure output)
-- ============================================================================

-- | How to structure the output DOM.
-- |
-- | This is the algebraic part of the hylomorphism - it "folds"
-- | the transformed elements into the final DOM structure.
data Assembly
  -- | All elements as siblings at the same level (flat)
  = Siblings

  -- | Preserve input structure in output (nested)
  -- | Only meaningful with FromTree enumeration
  | Nested

-- ============================================================================
-- GUP (General Update Pattern)
-- ============================================================================

-- | Specification for differential updates (enter/update/exit).
-- |
-- | This is orthogonal to enumeration and assembly - it describes
-- | how to handle changes when data updates.
type GUPSpec a =
  { enter :: Maybe (PhaseSpec a)
  , update :: Maybe (PhaseSpec a)
  , exit :: Maybe (PhaseSpec a)
  }

-- | Specification for one phase of GUP
type PhaseSpec a =
  { attrs :: Array (Attribute a)
  , transition :: Maybe TransitionConfig
  }

-- ============================================================================
-- Smart Constructors
-- ============================================================================

-- | Create an element node
elem
  :: forall a
   . ElementType
  -> Array (Attribute a)
  -> Array (Tree a)
  -> Tree a
elem elemType attrs children = Elem
  { elemType
  , attrs
  , children
  , behaviors: []
  }

-- | Create a fold node
fold
  :: forall a
   . { name :: String
     , enumerate :: Enumeration a
     , assemble :: Assembly
     , keyFn :: a -> String
     , template :: a -> Tree a
     }
  -> Tree a
fold spec = Fold
  { name: spec.name
  , enumerate: spec.enumerate
  , assemble: spec.assemble
  , keyFn: spec.keyFn
  , template: spec.template
  , gup: Nothing
  }

-- | Empty tree
empty :: forall a. Tree a
empty = Empty

-- | Combine multiple trees as siblings
-- |
-- | This is the core of the Monoid instance. It flattens "bare" Groups
-- | (those with no attributes or behaviors) to maintain associativity.
-- |
-- | ```purescript
-- | -- These are equivalent:
-- | siblings [a, b, c]
-- | a <> b <> c
-- |
-- | -- Conditional inclusion via Empty:
-- | siblings [plotArea, if showLegend then legend else empty, annotations]
-- | ```
siblings :: forall a. Array (Tree a) -> Tree a
siblings trees =
  case flatten trees of
    [] -> Empty
    [t] -> t
    ts -> Elem
      { elemType: Group
      , attrs: []
      , children: ts
      , behaviors: []
      }
  where
  -- Flatten bare Groups and remove Emptys for associativity
  flatten :: Array (Tree a) -> Array (Tree a)
  flatten = Array.concatMap \t -> case t of
    Empty -> []
    Elem e
      | e.elemType == Group
      , Array.null e.attrs
      , Array.null e.behaviors
      -> flatten e.children  -- Recursively flatten bare Groups
    _ -> [t]

-- ============================================================================
-- Convenience Combinators
-- ============================================================================

-- | Simple iteration over an array (most common case)
-- |
-- | ```purescript
-- | forEach "circles" points _.id \pt ->
-- |   elem Circle [ cx pt.x, cy pt.y, r 5.0 ] []
-- | ```
forEach
  :: forall a
   . String                    -- Name
  -> Array a                   -- Data
  -> (a -> String)             -- Key function
  -> (a -> Tree a)             -- Template
  -> Tree a
forEach name items keyFn template = Fold
  { name
  , enumerate: FromArray items
  , assemble: Siblings
  , keyFn
  , template
  , gup: Nothing
  }

-- | Iteration with GUP (enter/update/exit transitions)
forEachWithGUP
  :: forall a
   . String
  -> Array a
  -> (a -> String)
  -> (a -> Tree a)
  -> GUPSpec a
  -> Tree a
forEachWithGUP name items keyFn template gupSpec = Fold
  { name
  , enumerate: FromArray items
  , assemble: Siblings
  , keyFn
  , template
  , gup: Just gupSpec
  }

-- | Unfold a tree structure to flat output (Siblings assembly)
-- |
-- | This is the hylomorphic pattern: unfold with coalgebra, render flat.
-- | Use for force layouts of hierarchies, where tree structure informs
-- | the simulation but output is flat circles.
-- |
-- | ```purescript
-- | fromTree "nodes" rootNode _.children _.id DepthFirst true \node ->
-- |   elem Circle [ cx node.x, cy node.y, r (if isLeaf node then 10.0 else 5.0) ] []
-- | ```
fromTree
  :: forall a
   . String                    -- Name
  -> a                         -- Root node
  -> (a -> Array a)            -- Children coalgebra
  -> (a -> String)             -- Key function
  -> TraversalOrder            -- DFS or BFS
  -> Boolean                   -- Include internal nodes?
  -> (a -> Tree a)             -- Template
  -> Tree a
fromTree name root children keyFn order includeInternal template = Fold
  { name
  , enumerate: FromTree { root, children, order, includeInternal }
  , assemble: Siblings
  , keyFn
  , template
  , gup: Nothing
  }

-- | Unfold a tree structure preserving hierarchy in output (Nested assembly)
-- |
-- | This preserves the tree structure in the DOM - each node's children
-- | become DOM children. Use for treemaps, sunbursts, indented trees.
-- |
-- | ```purescript
-- | preserveTree "cells" rootNode _.children _.id \node ->
-- |   elem Group []
-- |     [ elem Rect [ width node.w, height node.h ] []
-- |     , elem Text [ text node.name ] []
-- |     ]
-- | ```
preserveTree
  :: forall a
   . String                    -- Name
  -> a                         -- Root node
  -> (a -> Array a)            -- Children coalgebra
  -> (a -> String)             -- Key function
  -> (a -> Tree a)             -- Template
  -> Tree a
preserveTree name root children keyFn template = Fold
  { name
  , enumerate: FromTree { root, children, order: DepthFirst, includeInternal: true }
  , assemble: Nested
  , keyFn
  , template
  , gup: Nothing
  }
