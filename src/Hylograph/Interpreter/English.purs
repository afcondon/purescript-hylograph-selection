module Hylograph.Interpreter.English
  ( runEnglish
  , describeTree
  , describeAttr
  , showElemType
  , showEnumeration
  , showAssembly
  , indent
  ) where

-- | English description interpreter for HATS trees
-- | Translates tree structure into English description of HOW it's built

import Prelude

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (length, null)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (snd)
import Hylograph.HATS (Tree(..), Attr(..), Enumeration(..), Assembly(..), runSomeFold)
import Hylograph.Internal.Element.Types (ElementType(..))

-- | Run the English interpreter on a tree
runEnglish :: Tree -> String
runEnglish tree = snd $ runWriter $ describeTree tree 0

-- | Describe a tree with indentation level
describeTree :: Tree -> Int -> Writer String Unit
describeTree tree level = case tree of
  Elem { elemType, attrs, children } -> do
    tell $ indent level <> "Create a " <> showElemType elemType
    when (not $ null attrs) do
      tell " with attributes:\n"
      for_ attrs \attr -> tell $ indent (level + 1) <> describeAttr attr <> "\n"
    when (not $ null children) do
      tell $ indent level <> "Then add " <> show (length children) <> " child element(s):\n"
      for_ children \child -> describeTree child (level + 1)

  MkFold someFold ->
    runSomeFold someFold \spec -> do
      let enumDesc = showEnumeration spec.enumerate
      let assemblyDesc = showAssembly spec.assemble
      tell $ indent level <> "Fold \"" <> spec.name <> "\" over " <> enumDesc
      tell $ " → " <> assemblyDesc <> "\n"

      -- Describe GUP if present
      case spec.gup of
        Just gup -> do
          tell $ indent (level + 1) <> "With General Update Pattern:\n"
          when (isJust gup.enter)
            $ tell $ indent (level + 2) <> "- Enter: elements appear with initial attributes and transition\n"
          when (isJust gup.update)
            $ tell $ indent (level + 2) <> "- Update: elements transition to new state\n"
          when (isJust gup.exit)
            $ tell $ indent (level + 2) <> "- Exit: elements transition out then removed\n"
        Nothing -> pure unit

      tell $ indent (level + 1) <> "(Template function defined for creating elements from data)\n"

  Empty ->
    tell $ indent level <> "(Empty - nothing to render)\n"

-- | Describe an attribute in English
describeAttr :: Attr -> String
describeAttr = case _ of
  StaticAttr name value ->
    "Set " <> name <> " to \"" <> value <> "\""
  ThunkedAttr name _ ->
    "Set " <> name <> " from data (dynamic)"

-- | Show element type as English
showElemType :: ElementType -> String
showElemType SVG = "SVG container"
showElemType Group = "group"
showElemType Circle = "circle"
showElemType Rect = "rectangle"
showElemType Path = "path"
showElemType Line = "line"
showElemType Polygon = "polygon"
showElemType Text = "text"
showElemType Div = "div"
showElemType Span = "span"
showElemType Table = "table"
showElemType Tbody = "table body"
showElemType Thead = "table head"
showElemType Tr = "table row"
showElemType Td = "table cell"
showElemType Th = "table header"
showElemType Defs = "definitions"
showElemType LinearGradient = "linear gradient"
showElemType Stop = "gradient stop"
showElemType PatternFill = "fill pattern"
showElemType Code = "code block"
showElemType Var = "variable"
showElemType Dfn = "definition"
showElemType Dl = "definition list"
showElemType Dt = "term"
showElemType Dd = "description"
showElemType Ol = "ordered list"
showElemType Ul = "unordered list"
showElemType Li = "list item"
showElemType Small = "annotation"
showElemType Em = "emphasis"
showElemType Strong = "strong"
showElemType Anchor = "link"
showElemType P = "paragraph"
showElemType Pre = "preformatted"
showElemType Section = "section"
showElemType Mark = "highlight"
showElemType Abbr = "abbreviation"

-- | Show enumeration strategy as English
showEnumeration :: forall a. Enumeration a -> String
showEnumeration = case _ of
  FromArray arr -> show (length arr) <> " items from array"
  FromTree _ -> "tree nodes"
  WithContext arr -> show (length arr) <> " items with context (depth, index)"

-- | Show assembly strategy as English
showAssembly :: Assembly -> String
showAssembly = case _ of
  Siblings -> "siblings at same level"
  Nested -> "nested structure"

-- | Create indentation
indent :: Int -> String
indent 0 = ""
indent n = "  " <> indent (n - 1)
