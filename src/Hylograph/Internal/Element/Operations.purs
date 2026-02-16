-- | Element creation and type conversion operations.
-- |
-- | Shared operations used by both HATS and the legacy Selection API.
-- | Extracted from Internal.Selection.Operations to decouple HATS from Selection.
module Hylograph.Internal.Element.Operations
  ( createElementWithNS
  , elementTypeToString
  , stringToElementType
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Hylograph.Internal.Element.Types (ElementType(..), RenderContext(..), elementContext)
import Web.DOM.Document (Document)
import Web.DOM.Document as Document
import Web.DOM.Element (Element)

-- | Create an element with the appropriate namespace
-- | Uses the element's rendering context to determine namespace
createElementWithNS :: ElementType -> Document -> Effect Element
createElementWithNS elemType doc =
  case elementContext elemType of
    SVGContext ->
      Document.createElementNS (Just "http://www.w3.org/2000/svg") (elementTypeToString elemType) doc
    HTMLContext ->
      Document.createElement (elementTypeToString elemType) doc

-- | Convert ElementType to its string representation for DOM creation
elementTypeToString :: ElementType -> String
elementTypeToString Circle = "circle"
elementTypeToString Rect = "rect"
elementTypeToString Path = "path"
elementTypeToString Line = "line"
elementTypeToString Polygon = "polygon"
elementTypeToString Text = "text"
elementTypeToString Group = "g"
elementTypeToString SVG = "svg"
elementTypeToString Defs = "defs"
elementTypeToString LinearGradient = "linearGradient"
elementTypeToString Stop = "stop"
elementTypeToString PatternFill = "pattern"
elementTypeToString Div = "div"
elementTypeToString Span = "span"
elementTypeToString Table = "table"
elementTypeToString Tr = "tr"
elementTypeToString Td = "td"
elementTypeToString Th = "th"
elementTypeToString Tbody = "tbody"
elementTypeToString Thead = "thead"
elementTypeToString Code = "code"
elementTypeToString Var = "var"
elementTypeToString Dfn = "dfn"
elementTypeToString Dl = "dl"
elementTypeToString Dt = "dt"
elementTypeToString Dd = "dd"
elementTypeToString Ol = "ol"
elementTypeToString Ul = "ul"
elementTypeToString Li = "li"
elementTypeToString Small = "small"
elementTypeToString Em = "em"
elementTypeToString Strong = "strong"
elementTypeToString Anchor = "a"
elementTypeToString P = "p"
elementTypeToString Pre = "pre"
elementTypeToString Section = "section"
elementTypeToString Mark = "mark"
elementTypeToString Abbr = "abbr"

-- | Convert string to ElementType (inverse of elementTypeToString)
stringToElementType :: String -> ElementType
stringToElementType "circle" = Circle
stringToElementType "rect" = Rect
stringToElementType "path" = Path
stringToElementType "line" = Line
stringToElementType "polygon" = Polygon
stringToElementType "text" = Text
stringToElementType "g" = Group
stringToElementType "svg" = SVG
stringToElementType "defs" = Defs
stringToElementType "linearGradient" = LinearGradient
stringToElementType "stop" = Stop
stringToElementType "pattern" = PatternFill
stringToElementType "div" = Div
stringToElementType "span" = Span
stringToElementType "table" = Table
stringToElementType "tr" = Tr
stringToElementType "td" = Td
stringToElementType "th" = Th
stringToElementType "tbody" = Tbody
stringToElementType "thead" = Thead
stringToElementType "code" = Code
stringToElementType "var" = Var
stringToElementType "dfn" = Dfn
stringToElementType "dl" = Dl
stringToElementType "dt" = Dt
stringToElementType "dd" = Dd
stringToElementType "ol" = Ol
stringToElementType "ul" = Ul
stringToElementType "li" = Li
stringToElementType "small" = Small
stringToElementType "em" = Em
stringToElementType "strong" = Strong
stringToElementType "a" = Anchor
stringToElementType "p" = P
stringToElementType "pre" = Pre
stringToElementType "section" = Section
stringToElementType "mark" = Mark
stringToElementType "abbr" = Abbr
stringToElementType _ = Group -- Default to Group for unknown types
