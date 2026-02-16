-- | Element types and rendering contexts.
-- |
-- | Shared types used by both HATS and the legacy Selection API.
-- | Extracted from Internal.Selection.Types to decouple HATS from Selection.
module Hylograph.Internal.Element.Types
  ( ElementType(..)
  , RenderContext(..)
  , elementContext
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | Output context for rendering
-- | Determines which namespace to use when creating elements
data RenderContext
  = SVGContext -- SVG namespace (for graphics)
  | HTMLContext -- HTML namespace (for DOM elements)

derive instance Eq RenderContext
derive instance Ord RenderContext
derive instance Generic RenderContext _
instance Show RenderContext where
  show = genericShow

-- | Element types organized by rendering context
-- |
-- | This ADT makes the distinction between SVG and HTML elements explicit,
-- | allowing the type system to guide proper namespace handling.
data ElementType
  -- SVG elements (require SVG namespace)
  = Circle
  | Rect
  | Path
  | Line
  | Polygon
  | Text
  | Group
  | SVG
  | Defs
  | LinearGradient
  | Stop
  | PatternFill  -- SVG pattern element for fills (used in fast/slow visual treatment)
  -- HTML elements (use default namespace)
  | Div
  | Span
  | Table
  | Tr
  | Td
  | Th
  | Tbody
  | Thead
  -- Semantic HTML elements
  | Code      -- <code> inline code
  | Var       -- <var> variable (mathematical/programming)
  | Dfn       -- <dfn> term being defined
  | Dl        -- <dl> definition list
  | Dt        -- <dt> definition term
  | Dd        -- <dd> definition description
  | Ol        -- <ol> ordered list
  | Ul        -- <ul> unordered list
  | Li        -- <li> list item
  | Small     -- <small> side comment / fine print
  | Em        -- <em> emphasis
  | Strong    -- <strong> strong importance
  | Anchor    -- <a> hyperlink
  | P         -- <p> paragraph
  | Pre       -- <pre> preformatted text
  | Section   -- <section> thematic grouping
  | Mark      -- <mark> highlighted text
  | Abbr      -- <abbr> abbreviation (title attr for tooltip)

derive instance Eq ElementType
derive instance Ord ElementType
derive instance Generic ElementType _
instance Show ElementType where
  show = genericShow

-- | Determine which rendering context an element belongs to
elementContext :: ElementType -> RenderContext
elementContext Circle = SVGContext
elementContext Rect = SVGContext
elementContext Path = SVGContext
elementContext Line = SVGContext
elementContext Polygon = SVGContext
elementContext Text = SVGContext
elementContext Group = SVGContext
elementContext SVG = SVGContext
elementContext Defs = SVGContext
elementContext LinearGradient = SVGContext
elementContext Stop = SVGContext
elementContext PatternFill = SVGContext
elementContext Div = HTMLContext
elementContext Span = HTMLContext
elementContext Table = HTMLContext
elementContext Tr = HTMLContext
elementContext Td = HTMLContext
elementContext Th = HTMLContext
elementContext Tbody = HTMLContext
elementContext Thead = HTMLContext
elementContext Code = HTMLContext
elementContext Var = HTMLContext
elementContext Dfn = HTMLContext
elementContext Dl = HTMLContext
elementContext Dt = HTMLContext
elementContext Dd = HTMLContext
elementContext Ol = HTMLContext
elementContext Ul = HTMLContext
elementContext Li = HTMLContext
elementContext Small = HTMLContext
elementContext Em = HTMLContext
elementContext Strong = HTMLContext
elementContext Anchor = HTMLContext
elementContext P = HTMLContext
elementContext Pre = HTMLContext
elementContext Section = HTMLContext
elementContext Mark = HTMLContext
elementContext Abbr = HTMLContext
