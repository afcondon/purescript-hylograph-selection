-- | MetaHATS Interpreter
-- |
-- | A Tree → Tree interpreter that visualizes HATS tree structure.
-- | Takes any HATS tree and produces a horizontal diagram showing
-- | Elem nodes, Fold nodes (with enum×assembly glyphs), and structure.
-- |
-- | Usage:
-- |   let vizTree = interpretMeta defaultMetaConfig myTree
-- |   rerender "#container" vizTree
module Hylograph.Interpreter.MetaHATS
  ( interpretMeta
  , MetaConfig
  , defaultMetaConfig
  ) where

import Prelude

import Data.Array (length, mapWithIndex, foldl, head, range)
import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Hylograph.HATS.Friendly as F
import Hylograph.HATS (Tree(..), Attr(..), Enumeration(..), Assembly(..), runSomeFold, elem, staticStr, staticNum)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- =============================================================================
-- Configuration
-- =============================================================================

type MetaConfig =
  { nodeWidth :: Number
  , nodeHeight :: Number
  , foldHeight :: Number      -- Taller for compound glyph
  , horizontalGap :: Number
  , verticalGap :: Number
  , fontSize :: Number
  , smallFontSize :: Number
  }

defaultMetaConfig :: MetaConfig
defaultMetaConfig =
  { nodeWidth: 120.0
  , nodeHeight: 40.0
  , foldHeight: 70.0
  , horizontalGap: 40.0
  , verticalGap: 20.0
  , fontSize: 12.0
  , smallFontSize: 10.0
  }

-- =============================================================================
-- Layout Computation
-- =============================================================================

newtype Layout = Layout
  { width :: Number
  , height :: Number
  , childLayouts :: Array Layout
  }

-- | Compute layout for a HATS tree
computeLayout :: MetaConfig -> Tree -> Layout
computeLayout cfg = case _ of
  Empty ->
    Layout { width: 0.0, height: 0.0, childLayouts: [] }

  Elem { children } ->
    let
      childLayouts = map (computeLayout cfg) children
      childrenHeight = foldl (\acc (Layout l) -> acc + l.height) 0.0 childLayouts
                     + cfg.verticalGap * Int.toNumber (max 0 (length children - 1))
      maxChildWidth = foldl (\acc (Layout l) -> max acc l.width) 0.0 childLayouts
      totalWidth = cfg.nodeWidth + (if length children > 0 then cfg.horizontalGap + maxChildWidth else 0.0)
      totalHeight = max cfg.nodeHeight childrenHeight
    in
      Layout { width: totalWidth, height: max totalHeight cfg.nodeHeight, childLayouts }

  MkFold someFold ->
    -- For Fold, we show the fold node plus template structure (if available)
    runSomeFold someFold \spec ->
      let
        -- Try to get a sample datum and compute template layout
        mTemplateLayout = case spec.enumerate of
          FromArray arr -> case Array.head arr of
            Just sample -> Just (computeLayout cfg (spec.template sample))
            Nothing -> Nothing
          WithContext arr -> case Array.head arr of
            Just sample -> Just (computeLayout cfg (spec.template sample.datum))
            Nothing -> Nothing
          FromTree _ -> Nothing  -- Can't easily get sample from tree

        -- Layout depends on whether we have template structure
        templateWidth = case mTemplateLayout of
          Just (Layout tl) -> cfg.horizontalGap + tl.width
          Nothing -> cfg.horizontalGap + 60.0  -- deck width fallback

        templateHeight = case mTemplateLayout of
          Just (Layout tl) -> max cfg.foldHeight tl.height
          Nothing -> cfg.foldHeight
      in
        Layout
          { width: cfg.nodeWidth + templateWidth
          , height: templateHeight
          , childLayouts: case mTemplateLayout of
              Just tl -> [tl]
              Nothing -> []
          }

-- =============================================================================
-- The Interpreter
-- =============================================================================

-- | Interpret a HATS tree into a visualization of its structure
interpretMeta :: MetaConfig -> Tree -> Tree
interpretMeta cfg tree =
  let
    layout@(Layout l) = computeLayout cfg tree
    padding = 20.0
    totalWidth = max (l.width + padding * 2.0) 200.0
    totalHeight = max (l.height + padding * 2.0) 100.0
  in
    elem SVG
      [ F.width totalWidth
      , F.height totalHeight
      , F.attr "viewBox" ("0 0 " <> show totalWidth <> " " <> show totalHeight)
      , F.class_ "meta-hats-diagram"
      ]
      [ -- Zoom-able group (subtle backdrop - code shows through)
        elem Group
          [ F.class_ "zoom-group" ]
          [ -- Subtle dark backdrop to separate from code
            elem Rect
              [ F.width totalWidth
              , F.height totalHeight
              , F.fill "#000"
              , F.attrNum "opacity" 0.5
              , F.attrNum "rx" 4.0
              , F.class_ "zoom-background"
              ] []
          , -- Tree content
            renderTree cfg padding (padding + l.height / 2.0) tree layout
          ]
      ]

-- | Render a tree node at position (x is left edge, y is center)
-- | The repetition parameter indicates if we're inside a Fold template (Just count) or not (Nothing)
renderTree :: MetaConfig -> Number -> Number -> Tree -> Layout -> Tree
renderTree cfg x y tree layout = renderTreeWithRepetition cfg x y tree layout Nothing

-- | Internal: render with optional repetition context
renderTreeWithRepetition :: MetaConfig -> Number -> Number -> Tree -> Layout -> Maybe Int -> Tree
renderTreeWithRepetition cfg x y tree (Layout layout) mRepetition = case tree of
  Empty ->
    elem Group [] []

  Elem node ->
    let
      nodeY = y - cfg.nodeHeight / 2.0
      childrenStartX = x + cfg.nodeWidth + cfg.horizontalGap
      label = extractLabel node.attrs
      attrs = countAttrs node.attrs
    in
      elem Group []
        [ -- The elem node box (with optional stacking for repetition)
          case mRepetition of
            Just count -> renderStackedElemNode cfg x nodeY (showElementType node.elemType) label count attrs
            Nothing -> renderElemNode cfg x nodeY (showElementType node.elemType) label attrs
        , -- Children with connecting lines (propagate repetition context)
          renderChildrenWithRepetition cfg x y childrenStartX node.children layout.childLayouts mRepetition
        ]

  MkFold someFold ->
    runSomeFold someFold \spec ->
      let
        nodeY = y - cfg.foldHeight / 2.0
        enumStr = showEnumeration spec.enumerate
        assemblyStr = showAssembly spec.assemble
        innerCount = enumerationCount spec.enumerate
        templateX = x + cfg.nodeWidth + cfg.horizontalGap

        -- Multiply inner count with outer repetition context
        -- e.g., ×8 rows containing ×8 cells = ×64 for leaf elements
        combinedCount = case { outer: mRepetition, inner: innerCount } of
          { outer: Just o, inner: Just i } -> Just (o * i)
          { outer: Nothing, inner: i } -> i
          { outer: o, inner: Nothing } -> o

        -- Try to get template tree by applying to sample datum
        mTemplateTree = case spec.enumerate of
          FromArray arr -> map spec.template (head arr)
          WithContext arr -> map (\s -> spec.template s.datum) (head arr)
          FromTree _ -> Nothing

        -- Get template layout from childLayouts (computed earlier)
        mTemplateLayout = head layout.childLayouts
      in
        elem Group []
          [ renderFoldNode cfg x nodeY spec.name enumStr assemblyStr innerCount
          , -- Connector line to template
            elem Line
              [ F.x1 (x + cfg.nodeWidth)
              , F.y1 y
              , F.x2 templateX
              , F.y2 y
              , F.stroke "#8B7355"
              , F.strokeWidth 1.5
              , staticStr "stroke-dasharray" "4,2"
              ] []
          , -- Template structure (recursive with repetition context!) or fallback deck
            case { tree: mTemplateTree, layout: mTemplateLayout } of
              { tree: Just templateTree, layout: Just templateLayout } ->
                -- Render the template tree recursively WITH combined count
                renderTreeWithRepetition cfg templateX y templateTree templateLayout combinedCount
              _ ->
                -- Fallback: show deck of cards
                renderDeck cfg templateX (y - 25.0) combinedCount
          ]

-- | Render children with connecting lines (with optional repetition context)
renderChildrenWithRepetition :: MetaConfig -> Number -> Number -> Number -> Array Tree -> Array Layout -> Maybe Int -> Tree
renderChildrenWithRepetition cfg parentX parentY childX children childLayouts mRepetition =
  let
    totalHeight = foldl (\acc (Layout l) -> acc + l.height) 0.0 childLayouts
                + cfg.verticalGap * Int.toNumber (max 0 (length children - 1))
    startY = parentY - totalHeight / 2.0

    -- Compute Y positions for each child
    childPositions = computeChildPositions cfg startY childLayouts

    -- Render each child
    renderedChildren = mapWithIndex (\i child ->
      case { cl: childLayouts !! i, pos: childPositions !! i } of
        { cl: Just childLayout, pos: Just childY } ->
          elem Group []
            [ -- Connector line from parent
              elem Path
                [ F.d (connectorPath parentX parentY childX childY cfg.nodeWidth)
                , F.fill "none"
                , F.stroke "#8B7355"
                , F.strokeWidth 1.5
                ] []
            , -- Child node (propagate repetition context)
              renderTreeWithRepetition cfg childX childY child childLayout mRepetition
            ]
        _ -> elem Group [] []
      ) children
  in
    elem Group [] renderedChildren

-- | Array indexing helper (uses safe Array.index)
infixl 8 arrayIndex as !!

arrayIndex :: forall a. Array a -> Int -> Maybe a
arrayIndex = Array.index

-- | Compute Y center positions for children
computeChildPositions :: MetaConfig -> Number -> Array Layout -> Array Number
computeChildPositions cfg startY layouts =
  let
    go :: { y :: Number, acc :: Array Number } -> Layout -> { y :: Number, acc :: Array Number }
    go { y, acc } (Layout l) =
      let centerY = y + l.height / 2.0
      in { y: y + l.height + cfg.verticalGap, acc: acc <> [centerY] }
  in
    (foldl go { y: startY, acc: [] } layouts).acc

-- | SVG path for connector (with elbow)
connectorPath :: Number -> Number -> Number -> Number -> Number -> String
connectorPath x1 y1 x2 y2 nodeWidth =
  let
    startX = x1 + nodeWidth
    midX = startX + (x2 - startX) / 2.0
  in
    "M" <> show startX <> "," <> show y1 <>
    " H" <> show midX <>
    " V" <> show y2 <>
    " H" <> show x2

-- =============================================================================
-- Node Rendering
-- =============================================================================

-- | Render an Elem node with optional semantic label and attribute counts
renderElemNode :: MetaConfig -> Number -> Number -> String -> Maybe String -> AttrCounts -> Tree
renderElemNode cfg x y elemType mLabel attrCounts =
  let
    -- If we have a label, show "Type: label" on two lines
    hasLabel = case mLabel of
      Just _ -> true
      Nothing -> false
    attrStr = formatAttrCounts attrCounts
  in
    elem Group []
      [ -- Box
        elem Rect
          [ F.x x
          , F.y y
          , F.width cfg.nodeWidth
          , F.height cfg.nodeHeight
          , F.attrNum "rx" 4.0
          , F.fill "#1a1a1a"
          , F.stroke "#C9A962"
          , F.strokeWidth 1.5
          ] []
      , -- Element type (top line if has label, centered if not)
        elem Text
          [ F.x (x + cfg.nodeWidth / 2.0)
          , F.y (y + (if hasLabel then cfg.nodeHeight / 3.0 + 2.0 else cfg.nodeHeight / 2.0 + 4.0))
          , F.textAnchor "middle"
          , staticNum "font-size" cfg.fontSize
          , F.fontFamily "monospace"
          , F.fill "#E8D5A3"
          , staticStr "textContent" elemType
          ] []
      , -- Label (second line, dimmer)
        case mLabel of
          Just label ->
            elem Text
              [ F.x (x + cfg.nodeWidth / 2.0)
              , F.y (y + cfg.nodeHeight * 2.0 / 3.0 + 4.0)
              , F.textAnchor "middle"
              , staticNum "font-size" cfg.smallFontSize
              , F.fontFamily "monospace"
              , F.fill "#C9A962"
              , staticStr "textContent" label
              ] []
          Nothing ->
            elem Group [] []
      , -- Attribute counts (bottom right, small)
        if attrStr == "" then elem Group [] []
        else elem Text
          [ F.x (x + cfg.nodeWidth - 4.0)
          , F.y (y + cfg.nodeHeight - 4.0)
          , F.textAnchor "end"
          , staticNum "font-size" 9.0
          , F.fontFamily "monospace"
          , F.fill "#8B7355"
          , staticStr "textContent" attrStr
          ] []
      ]

-- | Render a stacked Elem node (inside a Fold template - shows repetition)
renderStackedElemNode :: MetaConfig -> Number -> Number -> String -> Maybe String -> Int -> AttrCounts -> Tree
renderStackedElemNode cfg x y elemType mLabel count attrCounts =
  let
    offset = 3.0  -- Stacking offset
    numStacks = min 3 count  -- Show up to 3 stacked boxes
    attrStr = formatAttrCounts attrCounts

    -- Render a single stacked box at offset
    renderStackBox :: Int -> Tree
    renderStackBox i =
      let
        boxX = x + offset * Int.toNumber (numStacks - 1 - i)
        boxY = y + offset * Int.toNumber i
        opacity = 0.4 + 0.3 * Int.toNumber i
      in
        elem Rect
          [ F.x boxX
          , F.y boxY
          , F.width cfg.nodeWidth
          , F.height cfg.nodeHeight
          , F.attrNum "rx" 4.0
          , F.fill "#1a1a1a"
          , F.stroke "#C9A962"
          , F.strokeWidth 1.5
          , F.attrNum "opacity" opacity
          ] []

    -- Compute positions for text (on front box)
    hasLabel = case mLabel of
      Just _ -> true
      Nothing -> false

    frontX = x + offset * Int.toNumber (numStacks - 1)
    frontY = y + offset * Int.toNumber (numStacks - 1)
  in
    elem Group []
      ( -- Stacked boxes (back to front)
        map renderStackBox (range 0 (numStacks - 1)) <>
        [ -- Element type (on front box)
          elem Text
            [ F.x (frontX + cfg.nodeWidth / 2.0)
            , F.y (frontY + (if hasLabel then cfg.nodeHeight / 3.0 + 2.0 else cfg.nodeHeight / 2.0 + 4.0))
            , F.textAnchor "middle"
            , staticNum "font-size" cfg.fontSize
            , F.fontFamily "monospace"
            , F.fill "#E8D5A3"
            , staticStr "textContent" elemType
            ] []
        , -- Label (second line)
          case mLabel of
            Just label ->
              elem Text
                [ F.x (frontX + cfg.nodeWidth / 2.0)
                , F.y (frontY + cfg.nodeHeight * 2.0 / 3.0 + 4.0)
                , F.textAnchor "middle"
                , staticNum "font-size" cfg.smallFontSize
                , F.fontFamily "monospace"
                , F.fill "#C9A962"
                , staticStr "textContent" label
                ] []
            Nothing -> elem Group [] []
        , -- Attribute counts (inside front box, bottom right)
          if attrStr == "" then elem Group [] []
          else elem Text
            [ F.x (frontX + cfg.nodeWidth - 4.0)
            , F.y (frontY + cfg.nodeHeight - 4.0)
            , F.textAnchor "end"
            , staticNum "font-size" 9.0
            , F.fontFamily "monospace"
            , F.fill "#8B7355"
            , staticStr "textContent" attrStr
            ] []
        , -- Count badge (below)
          elem Text
            [ F.x (frontX + cfg.nodeWidth - 4.0)
            , F.y (frontY + cfg.nodeHeight + 12.0)
            , F.textAnchor "end"
            , staticNum "font-size" cfg.smallFontSize
            , F.fill "#C9A962"
            , F.fontWeight "bold"
            , staticStr "textContent" ("×" <> show count)
            ] []
        ]
      )

-- | Render a Fold node (compound glyph with count)
renderFoldNode :: MetaConfig -> Number -> Number -> String -> String -> String -> Maybe Int -> Tree
renderFoldNode cfg x y name enumStr assemblyStr mCount =
  let
    enumIcon = case enumStr of
      "Array" -> "≡"
      "Tree" -> "△"
      "Keys" -> "K"
      "Values" -> "V"
      "Entries" -> "⟼"
      "Context" -> "#"
      _ -> "?"

    assemblyIcon = case assemblyStr of
      "Siblings" -> "|||"
      "Nested" -> "⊏⊐"
      _ -> "?"
  in
    elem Group []
      [ -- Box with accent border
        elem Rect
          [ F.x x
          , F.y y
          , F.width cfg.nodeWidth
          , F.height cfg.foldHeight
          , F.attrNum "rx" 4.0
          , F.fill "#0a0a0a"
          , F.stroke "#C9A962"
          , F.strokeWidth 2.0
          ] []
      , -- "Fold" label + name + count
        let
          countStr = case mCount of
            Just n -> " ×" <> show n
            Nothing -> ""
        in elem Text
          [ F.x (x + cfg.nodeWidth / 2.0)
          , F.y (y + 16.0)
          , F.textAnchor "middle"
          , staticNum "font-size" cfg.smallFontSize
          , F.fill "#C9A962"
          , staticStr "textContent" ("Fold \"" <> name <> "\"" <> countStr)
          ] []
      , -- Enum icon (left)
        elem Text
          [ F.x (x + 25.0)
          , F.y (y + 38.0)
          , F.textAnchor "middle"
          , staticNum "font-size" 16.0
          , F.fill "#E8D5A3"
          , staticStr "textContent" enumIcon
          ] []
      , -- Arrow
        elem Text
          [ F.x (x + cfg.nodeWidth / 2.0)
          , F.y (y + 38.0)
          , F.textAnchor "middle"
          , staticNum "font-size" 14.0
          , F.fill "#8B7355"
          , staticStr "textContent" "→"
          ] []
      , -- Assembly icon (right)
        elem Text
          [ F.x (x + cfg.nodeWidth - 25.0)
          , F.y (y + 38.0)
          , F.textAnchor "middle"
          , staticNum "font-size" 16.0
          , F.fill "#C9A962"
          , staticStr "textContent" assemblyIcon
          ] []
      , -- Enum label
        elem Text
          [ F.x (x + 25.0)
          , F.y (y + cfg.foldHeight - 8.0)
          , F.textAnchor "middle"
          , staticNum "font-size" cfg.smallFontSize
          , F.fill "#E8D5A3"
          , staticStr "textContent" enumStr
          ] []
      , -- Assembly label
        elem Text
          [ F.x (x + cfg.nodeWidth - 25.0)
          , F.y (y + cfg.foldHeight - 8.0)
          , F.textAnchor "middle"
          , staticNum "font-size" cfg.smallFontSize
          , F.fill "#C9A962"
          , staticStr "textContent" assemblyStr
          ] []
      ]

-- | Render deck of cards (stacked boxes showing repetition)
renderDeck :: MetaConfig -> Number -> Number -> Maybe Int -> Tree
renderDeck cfg x y mCount =
  let
    cardW = 50.0
    cardH = 35.0
    offset = 4.0  -- Offset for stacking effect
    numCards = 3  -- Show 3 stacked cards

    -- Render a single card at offset
    renderCard :: Int -> Tree
    renderCard i =
      let
        cardX = x + offset * Int.toNumber (numCards - 1 - i)
        cardY = y + offset * Int.toNumber i
        opacity = 0.3 + 0.35 * Int.toNumber i  -- Back cards more transparent
      in
        elem Rect
          [ F.x cardX
          , F.y cardY
          , F.width cardW
          , F.height cardH
          , F.attrNum "rx" 3.0
          , F.fill "#1a1a1a"
          , F.stroke "#8B7355"
          , F.strokeWidth 1.0
          , F.attrNum "opacity" opacity
          ] []

    -- Count label
    countLabel = case mCount of
      Just n -> "×" <> show n
      Nothing -> "×N"
  in
    elem Group []
      [ -- Stacked cards (back to front)
        renderCard 0
      , renderCard 1
      , renderCard 2
      , -- Count label
        elem Text
          [ F.x (x + cardW / 2.0 + offset)
          , F.y (y + cardH + 16.0)
          , F.textAnchor "middle"
          , staticNum "font-size" cfg.smallFontSize
          , F.fill "#C9A962"
          , F.fontWeight "bold"
          , staticStr "textContent" countLabel
          ] []
      ]

-- =============================================================================
-- Show Helpers
-- =============================================================================

showElementType :: ElementType -> String
showElementType = case _ of
  Circle -> "Circle"
  Rect -> "Rect"
  Path -> "Path"
  Line -> "Line"
  Polygon -> "Polygon"
  Text -> "Text"
  Group -> "G"
  SVG -> "SVG"
  Defs -> "Defs"
  LinearGradient -> "Gradient"
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

showEnumeration :: forall a. Enumeration a -> String
showEnumeration = case _ of
  FromArray _ -> "Array"
  FromTree _ -> "Tree"
  WithContext _ -> "Context"

-- | Get count from enumeration (if available)
enumerationCount :: forall a. Enumeration a -> Maybe Int
enumerationCount = case _ of
  FromArray arr -> Just (length arr)
  FromTree _ -> Nothing  -- Can't easily count tree nodes
  WithContext arr -> Just (length arr)

showAssembly :: Assembly -> String
showAssembly = case _ of
  Siblings -> "Siblings"
  Nested -> "Nested"

-- | Extract a semantic label from attributes.
-- | Prefers "data-label", falls back to "class", then Nothing.
extractLabel :: Array Attr -> Maybe String
extractLabel attrs =
  let
    findAttr :: String -> Array Attr -> Maybe String
    findAttr name arr = case Array.uncons arr of
      Nothing -> Nothing
      Just { head: StaticAttr n v, tail }
        | n == name -> Just v
        | otherwise -> findAttr name tail
      Just { tail } -> findAttr name tail  -- Skip ThunkedAttr
  in
    case findAttr "data-label" attrs of
      Just label -> Just label
      Nothing -> findAttr "class" attrs

-- | Count static vs thunked attributes
type AttrCounts = { static :: Int, thunked :: Int }

countAttrs :: Array Attr -> AttrCounts
countAttrs = foldl go { static: 0, thunked: 0 }
  where
  go acc (StaticAttr _ _) = acc { static = acc.static + 1 }
  go acc (ThunkedAttr _ _) = acc { thunked = acc.thunked + 1 }

-- | Format attribute counts as compact string
-- | e.g., "4s 2λ" for 4 static, 2 thunked
formatAttrCounts :: AttrCounts -> String
formatAttrCounts { static, thunked } =
  case { static, thunked } of
    { static: 0, thunked: 0 } -> ""
    { static: n, thunked: 0 } -> show n <> "s"
    { static: 0, thunked: n } -> show n <> "λ"
    _ -> show static <> "s " <> show thunked <> "λ"
