-- | SemiQuine Tree Interpreter for HATS
-- |
-- | Converts a HATS Tree directly to PureScript code.
-- |
-- | The generated code uses HATS DSL syntax:
-- | - `elem Circle [staticNum "r" 5.0, staticStr "fill" "blue"] []`
-- | - `forEach "circles" data_ \d -> elem Circle [...]`
module Hylograph.Interpreter.SemiQuine.TreeToCode
  ( treeToCode
  ) where

import Prelude

import Data.Array (concat, drop, head, length, mapWithIndex, null, replicate) as Array
import Data.Maybe (Maybe(..), isJust)
import Data.String (joinWith) as String
import Hylograph.HATS (Tree(..), Attr(..), Enumeration(..), Assembly(..), runSomeFold)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- | Convert a HATS Tree to PureScript code
treeToCode :: Tree -> String
treeToCode tree = String.joinWith "\n" $ treeLines tree 0

-- | Generate code lines for a tree node
treeLines :: Tree -> Int -> Array String
treeLines tree indentLevel = case tree of

  Elem { elemType, attrs, children } ->
    let
      -- Element constructor
      elemCode = "elem " <> showElemType elemType

      -- Attributes as array
      attrsLines = attrsToLines attrs (indentLevel + 1)

      -- Children
      childrenCode = case Array.length children of
        0 -> [ indent indentLevel <> "[]" ]
        1 -> case Array.head children of
          Just child ->
            [ indent indentLevel <> "[" ] <>
            treeLines child (indentLevel + 1) <>
            [ indent indentLevel <> "]" ]
          Nothing -> [ indent indentLevel <> "[]" ]
        _ ->
          [ indent indentLevel <> "["
          ] <>
          childrenWithCommas children (indentLevel + 1) <>
          [ indent indentLevel <> "]" ]
    in
      [ indent indentLevel <> elemCode ] <>
      attrsLines <>
      childrenCode

  MkFold someFold ->
    runSomeFold someFold \spec ->
      let
        enumCode = showEnumerationType spec.enumerate
        assemblyCode = showAssemblyType spec.assemble
        gupCode = if isJust spec.gup then " -- with GUP" else ""

        -- Try to evaluate template with sample datum
        templateCode = case getSampleDatum spec.enumerate of
          Just sampleDatum ->
            let templateTree = spec.template sampleDatum
            in treeLines templateTree (indentLevel + 1)
          Nothing ->
            [ indent (indentLevel + 1) <> "-- (no data to evaluate template)" ]
      in
        [ indent indentLevel <> "fold"
        , indent (indentLevel + 1) <> "{ name: \"" <> spec.name <> "\""
        , indent (indentLevel + 1) <> ", enumerate: " <> enumCode
        , indent (indentLevel + 1) <> ", assemble: " <> assemblyCode
        , indent (indentLevel + 1) <> ", template: \\d ->" <> gupCode
        ] <> templateCode <>
        [ indent (indentLevel + 1) <> "}" ]

  Empty ->
    [ indent indentLevel <> "empty" ]

-- | Render children with proper comma formatting
childrenWithCommas :: Array Tree -> Int -> Array String
childrenWithCommas children ind =
  Array.concat $ Array.mapWithIndex renderChild children
  where
  renderChild idx child =
    let
      childLines = treeLines child 0
      prefix = if idx == 0 then "  " else ", "
    in case Array.head childLines of
      Just firstLine ->
        [ indent ind <> prefix <> firstLine ] <>
        map (\l -> indent (ind + 1) <> l) (dropFirst childLines)
      Nothing -> []

  dropFirst arr = case Array.length arr of
    0 -> []
    1 -> []
    _ -> Array.drop 1 arr

-- | Convert attributes array to multi-line code
attrsToLines :: Array Attr -> Int -> Array String
attrsToLines attrs indentLevel =
  if Array.null attrs
    then [ indent indentLevel <> "[]" ]
    else
      let attrStrings = map attrToCode attrs
      in case Array.head attrStrings of
        Just first ->
          [ indent indentLevel <> "[ " <> first ] <>
          map (\a -> indent indentLevel <> ", " <> a) (Array.drop 1 attrStrings) <>
          [ indent indentLevel <> "]" ]
        Nothing -> [ indent indentLevel <> "[]" ]

-- | Convert a single HATS attribute to code
attrToCode :: Attr -> String
attrToCode = case _ of
  StaticAttr name value ->
    "staticStr \"" <> name <> "\" \"" <> value <> "\""
  ThunkedAttr name thunk ->
    let evaluated = thunk unit
    in "thunkedStr \"" <> name <> "\" (\\_ -> \"" <> evaluated <> "\")"

-- | Try to get a sample datum from an enumeration
getSampleDatum :: forall a. Enumeration a -> Maybe a
getSampleDatum = case _ of
  FromArray arr -> Array.head arr
  FromTree { root } -> Just root
  WithContext arr -> map _.datum (Array.head arr)

-- | Show enumeration as code
showEnumerationType :: forall a. Enumeration a -> String
showEnumerationType = case _ of
  FromArray arr -> "FromArray [...] -- " <> show (Array.length arr) <> " items"
  FromTree _ -> "FromTree {...}"
  WithContext arr -> "WithContext [...] -- " <> show (Array.length arr) <> " items"

-- | Show assembly as code
showAssemblyType :: Assembly -> String
showAssemblyType = case _ of
  Siblings -> "Siblings"
  Nested -> "Nested"

-- | Show element type as constructor
showElemType :: ElementType -> String
showElemType = case _ of
  SVG -> "SVG"
  Group -> "Group"
  Circle -> "Circle"
  Rect -> "Rect"
  Path -> "Path"
  Line -> "Line"
  Polygon -> "Polygon"
  Text -> "Text"
  Div -> "Div"
  Span -> "Span"
  Table -> "Table"
  Tbody -> "Tbody"
  Thead -> "Thead"
  Tr -> "Tr"
  Td -> "Td"
  Th -> "Th"
  Defs -> "Defs"
  LinearGradient -> "LinearGradient"
  Stop -> "Stop"
  PatternFill -> "PatternFill"

-- | Generate indentation (2 spaces per level)
indent :: Int -> String
indent n = String.joinWith "" (Array.replicate n "  ")
