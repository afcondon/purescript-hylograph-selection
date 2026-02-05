module Hylograph.Interpreter.Mermaid
  ( NodeID
  , MermaidTreeState
  , MermaidTreeM(..)
  , escapeLabel
  , addNode
  , addEdge
  , showElement
  , formatAttr
  , formatAttrList
  , renderTree
  , generateStyleDefinitions
  , runMermaidTree
  ) where

import Prelude

import Control.Monad.State (StateT, get, modify_, runStateT)
import Data.Array (foldl, length, head)
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), isJust)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.HATS (Tree(..), Attr(..), Enumeration(..), Assembly(..), runSomeFold)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- | Node ID for tracking connections in Mermaid diagram
type NodeID = Int

-- | State for generating Mermaid diagram from Tree API
type MermaidTreeState =
  { nodeCounter :: Int
  , mermaidCode :: String
  }

newtype MermaidTreeM a = MermaidTreeM (StateT MermaidTreeState Effect a)

derive newtype instance functorMermaidTreeM :: Functor MermaidTreeM
derive newtype instance applyMermaidTreeM :: Apply MermaidTreeM
derive newtype instance applicativeMermaidTreeM :: Applicative MermaidTreeM
derive newtype instance bindMermaidTreeM :: Bind MermaidTreeM
derive newtype instance monadMermaidTreeM :: Monad MermaidTreeM

-- | Escape quotes in labels for Mermaid
escapeLabel :: String -> String
escapeLabel = replaceAll (Pattern "\"") (Replacement "'")

-- | Add a node to the Mermaid diagram with a style class
addNode :: String -> String -> MermaidTreeM NodeID
addNode label styleClass = MermaidTreeM do
  state <- get
  let nodeId = state.nodeCounter
      nodeName = "n" <> show nodeId
      escapedLabel = escapeLabel label
      line = "    " <> nodeName <> "[\"" <> escapedLabel <> "\"]:::" <> styleClass <> "\n"
  modify_ (\s -> s { nodeCounter = s.nodeCounter + 1, mermaidCode = s.mermaidCode <> line })
  pure nodeId

-- | Add an edge between two nodes
addEdge :: NodeID -> NodeID -> MermaidTreeM Unit
addEdge fromId toId = MermaidTreeM do
  let fromName = "n" <> show fromId
      toName = "n" <> show toId
      line = "    " <> fromName <> " --> " <> toName <> "\n"
  modify_ (\s -> s { mermaidCode = s.mermaidCode <> line })

-- | Format element type for display
showElement :: ElementType -> String
showElement SVG = "svg"
showElement Group = "g"
showElement Circle = "circle"
showElement Rect = "rect"
showElement Line = "line"
showElement Polygon = "polygon"
showElement Path = "path"
showElement Text = "text"
showElement Div = "div"
showElement Span = "span"
showElement Table = "table"
showElement Tr = "tr"
showElement Td = "td"
showElement Th = "th"
showElement Thead = "thead"
showElement Tbody = "tbody"
showElement Defs = "defs"
showElement LinearGradient = "linearGradient"
showElement Stop = "stop"
showElement PatternFill = "pattern"

-- | Format a single attribute for display
formatAttr :: Attr -> String
formatAttr = case _ of
  StaticAttr name _ -> name
  ThunkedAttr name _ -> name <> "(d)"

-- | Format a list of attributes as a comma-separated string
formatAttrList :: Array Attr -> String
formatAttrList attrs =
  case attrs of
    [] -> ""
    _ -> foldl (\acc attr -> if acc == "" then formatAttr attr else acc <> ", " <> formatAttr attr) "" attrs

-- | Render a tree node to Mermaid
renderTree :: Tree -> Maybe NodeID -> MermaidTreeM NodeID
renderTree tree parentId = case tree of
  Elem { elemType, attrs, children } -> do
    -- Create node
    let nodeLabel = showElement elemType
        attrSuffix = if length attrs > 0
          then " + [" <> formatAttrList attrs <> "]"
          else ""
        fullLabel = nodeLabel <> attrSuffix

    nodeId <- addNode fullLabel "elementNode"

    -- Connect to parent if present
    case parentId of
      Just pid -> addEdge pid nodeId
      Nothing -> pure unit

    -- Render children
    _ <- traverseWithParent nodeId children
    pure nodeId

  MkFold someFold ->
    runSomeFold someFold \spec -> do
      -- Create fold node with enumeration and assembly info
      let enumStr = showEnumeration spec.enumerate
          assemblyStr = showAssembly spec.assemble
          gupStr = if isJust spec.gup then " {GUP}" else ""
          foldLabel = "FOLD \"" <> spec.name <> "\" [" <> enumStr <> " → " <> assemblyStr <> "]" <> gupStr

      foldId <- addNode foldLabel "foldNode"

      -- Connect to parent if present
      case parentId of
        Just pid -> addEdge pid foldId
        Nothing -> pure unit

      -- Create template node
      templateId <- addNode "template(datum) →" "templateNode"
      addEdge foldId templateId

      -- Try to render template structure with sample datum
      case getSampleDatum spec.enumerate of
        Just sampleDatum -> do
          let templateTree = spec.template sampleDatum
          _ <- renderTree templateTree (Just templateId)
          pure unit
        Nothing ->
          pure unit

      pure foldId

  Empty -> do
    nodeId <- addNode "(empty)" "emptyNode"
    case parentId of
      Just pid -> addEdge pid nodeId
      Nothing -> pure unit
    pure nodeId

  where
    traverseWithParent :: NodeID -> Array Tree -> MermaidTreeM Unit
    traverseWithParent pid children = do
      _ <- traverse (\child -> renderTree child (Just pid)) children
      pure unit

-- | Try to get a sample datum from an enumeration
getSampleDatum :: forall a. Enumeration a -> Maybe a
getSampleDatum = case _ of
  FromArray arr -> head arr
  FromTree { root } -> Just root
  WithContext arr -> map _.datum (head arr)

-- | Show enumeration type
showEnumeration :: forall a. Enumeration a -> String
showEnumeration = case _ of
  FromArray arr -> show (length arr) <> " items"
  FromTree _ -> "tree"
  WithContext arr -> show (length arr) <> " ctx"

-- | Show assembly type
showAssembly :: Assembly -> String
showAssembly = case _ of
  Siblings -> "siblings"
  Nested -> "nested"

-- | Generate style class definitions for Mermaid
generateStyleDefinitions :: String
generateStyleDefinitions =
  "\n    %% Style definitions\n" <>
  "    classDef elementNode fill:#e6f598,stroke:#abdda4,stroke-width:2px\n" <>
  "    classDef foldNode fill:#f46d43,stroke:#d53e4f,stroke-width:2px\n" <>
  "    classDef templateNode fill:#abdda4,stroke:#66c2a5,stroke-width:2px\n" <>
  "    classDef emptyNode fill:#eeeeee,stroke:#999999,stroke-width:1px\n"

-- | Run the Mermaid tree interpreter and generate diagram code
runMermaidTree :: Tree -> Effect String
runMermaidTree tree = do
  let (MermaidTreeM state) = renderTree tree Nothing
  Tuple _ finalState <- runStateT state initialState
  let themeConfig = "%%{init: {'theme':'base', 'themeVariables': {'fontFamily':'monospace'}, 'look':'handDrawn', 'flowchart':{'curve':'basis'}}}%%\n"
  pure $ themeConfig <> "graph TD\n" <> finalState.mermaidCode <> generateStyleDefinitions
  where
    initialState = { nodeCounter: 0, mermaidCode: "" }
