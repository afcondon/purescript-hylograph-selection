-- | Pretty-printer for HATS Trees
-- |
-- | Renders a Tree as readable pseudo-code showing the structure.
module TreePretty where

import Prelude

import Data.Array (length, head, tail)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits as SCU
import Data.String.Common (toUpper)
import Hylograph.HATS (Tree(..), Attr(..), Enumeration(..), Assembly(..), runSomeFold)
import Hylograph.Internal.Element.Types (ElementType(..))

-- | Pretty-print a Tree as HATS pseudo-code
prettyTree :: Tree -> String
prettyTree = go 0
  where
  go :: Int -> Tree -> String
  go _ Empty = ""

  go indent (Elem node) =
    let
      ind = indentStr indent
      elemName = showElemType node.elemType
      hasChildren = length node.children > 0
      hasAttrs = length node.attrs > 0
    in
      if hasChildren
        then
          ind <> "elem " <> elemName <> "\n" <>
          (if hasAttrs
            then ind <> "  " <> prettyAttrsMultiline (ind <> "  ") node.attrs <> "\n"
            else ind <> "  []\n") <>
          ind <> "  [ " <>
          (if length node.children == 1
            then trimStart (go 0 (fromMaybe Empty (head node.children))) <> ind <> "  ]\n"
            else "\n" <> foldMap (go (indent + 2)) node.children <> ind <> "  ]\n")
        else
          ind <> "elem " <> elemName <> " " <> prettyAttrs node.attrs <> " []\n"

  go indent (MkFold someFold) =
    runSomeFold someFold \spec ->
      let
        ind = indentStr indent
        elemType = showElemType spec.elementType
        countStr = case spec.enumerate of
          FromArray arr -> show (length arr)
          WithContext arr -> show (length arr)
          FromTree _ -> "n"
      in
        ind <> "forEach \"" <> spec.name <> "\" " <> elemType <> " data  -- ×" <> countStr <> "\n" <>
        ind <> "  \\datum -> ... -- template\n"

  indentStr :: Int -> String
  indentStr n = repeatStr n "  "

  repeatStr :: Int -> String -> String
  repeatStr 0 _ = ""
  repeatStr n s = s <> repeatStr (n - 1) s

-- | Show element type
showElemType :: ElementType -> String
showElemType = case _ of
  SVG -> "SVG"
  Group -> "G"
  Rect -> "Rect"
  Circle -> "Circle"
  Path -> "Path"
  Line -> "Line"
  Text -> "Text"
  Polygon -> "Polygon"
  Defs -> "Defs"
  LinearGradient -> "LinearGradient"
  Stop -> "Stop"
  PatternFill -> "Pattern"
  Div -> "Div"
  Span -> "Span"
  Table -> "Table"
  Tr -> "Tr"
  Td -> "Td"
  Th -> "Th"
  Tbody -> "Tbody"
  Thead -> "Thead"
  Code -> "Code"
  Var -> "Var"
  Dfn -> "Dfn"
  Dl -> "Dl"
  Dt -> "Dt"
  Dd -> "Dd"
  Ol -> "Ol"
  Ul -> "Ul"
  Li -> "Li"
  Small -> "Small"
  Em -> "Em"
  Strong -> "Strong"
  Anchor -> "Anchor"
  P -> "P"
  Pre -> "Pre"
  Section -> "Section"
  Mark -> "Mark"
  Abbr -> "Abbr"

-- | Pretty-print attributes on multiple lines
prettyAttrsMultiline :: String -> Array Attr -> String
prettyAttrsMultiline indent attrs =
  case length attrs of
    0 -> "[]"
    _ -> "[ " <> joinAttrs (indent <> ", ") (map showAttr attrs) <> "\n" <> indent <> "]"

-- | Join strings with separator
joinAttrs :: String -> Array String -> String
joinAttrs sep arr =
  case head arr, tail arr of
    Nothing, _ -> ""
    Just h, Nothing -> h
    Just h, Just t -> h <> foldMap (\s -> "\n" <> sep <> s) t

-- | Pretty-print attributes inline (for compact view)
prettyAttrs :: Array Attr -> String
prettyAttrs attrs =
  case length attrs of
    0 -> "[]"
    _ -> "[ " <> joinWith ", " (map showAttr attrs) <> " ]"

joinWith :: String -> Array String -> String
joinWith sep arr =
  case head arr, tail arr of
    Nothing, _ -> ""
    Just h, Nothing -> h
    Just h, Just t -> h <> foldMap (\s -> sep <> s) t

showAttr :: Attr -> String
showAttr (StaticAttr name val) =
  case friendlyName name of
    Just fn | isNumeric val -> "F." <> fn <> " " <> val
    Just fn -> "F." <> fn <> " " <> show val
    Nothing | isNumeric val -> "staticNum \"" <> name <> "\" " <> val
    Nothing -> "staticStr \"" <> name <> "\" " <> show val
showAttr (ThunkedAttr name _) =
  case friendlyName name of
    Just fn -> "thunked" <> titleCase fn <> " datum." <> name
    Nothing -> "thunked \"" <> name <> "\" datum." <> name

-- | Map attribute names to Friendly module function names
friendlyName :: String -> Maybe String
friendlyName = case _ of
  -- Dimensions
  "width" -> Just "width"
  "height" -> Just "height"
  -- Position
  "x" -> Just "x"
  "y" -> Just "y"
  "x1" -> Just "x1"
  "y1" -> Just "y1"
  "x2" -> Just "x2"
  "y2" -> Just "y2"
  "cx" -> Just "cx"
  "cy" -> Just "cy"
  "r" -> Just "r"
  "rx" -> Just "rx"
  "ry" -> Just "ry"
  -- Style
  "fill" -> Just "fill"
  "stroke" -> Just "stroke"
  "stroke-width" -> Just "strokeWidth"
  "opacity" -> Just "opacity"
  -- Text
  "text-anchor" -> Just "textAnchor"
  "font-family" -> Just "fontFamily"
  "font-size" -> Just "fontSize"
  "font-weight" -> Just "fontWeight"
  -- Transform/path
  "transform" -> Just "transform"
  "d" -> Just "d"
  -- Other common
  "class" -> Just "class_"
  "viewBox" -> Just "viewBox"
  "id" -> Just "id"
  _ -> Nothing

-- | Simple title case for thunked names
titleCase :: String -> String
titleCase s = toUpper (SCU.take 1 s) <> SCU.drop 1 s

-- | Check if string looks like a number
isNumeric :: String -> Boolean
isNumeric s =
  let c = SCU.take 1 s
  in c >= "0" && c <= "9" || c == "-"

-- | Show enumeration type
showEnum :: forall a. Enumeration a -> String
showEnum = case _ of
  FromArray _ -> "Array"
  FromTree _ -> "Tree"
  WithContext _ -> "Context"

-- | Show assembly type
showAssembly :: Assembly -> String
showAssembly Siblings = "Siblings"
showAssembly Nested = "Nested"

-- | Trim leading whitespace (no stdlib equivalent)
foreign import trimStart :: String -> String
