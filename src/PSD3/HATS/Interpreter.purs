-- | HATS Interpreter
-- |
-- | Renders HATS trees to DOM via native DOM APIs.
module PSD3.HATS.Interpreter
  ( render
  , renderTo
  , rerender
  , SelectionMap
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)

import PSD3.HATS (Tree(..), Enumeration(..), Assembly(..), TraversalOrder(..), GUPSpec)
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..))
import PSD3.Internal.Behavior.Types (Behavior(..), DragConfig(..), ZoomConfig(..), ScaleExtent(..))
import PSD3.Internal.Selection.Types (ElementType(..))
import PSD3.Internal.Selection.Operations (createElementWithNS)
import Web.DOM.Element (Element)
import Web.DOM.Element (toNode) as Element
import Web.DOM.Node (appendChild) as Node
import Web.DOM.Document (Document)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument as HTMLDocument

-- | Selection map - named selections for later access
type SelectionMap = Map String (Array Element)

-- | Render a HATS tree to a container selected by CSS selector
render :: forall a. String -> Tree a -> Effect SelectionMap
render selector tree = do
  doc <- window >>= document >>= pure <<< HTMLDocument.toDocument
  container <- selectElement selector doc
  interpretTree doc container tree

-- | Render a HATS tree to a specific element
renderTo :: forall a. Element -> Tree a -> Effect SelectionMap
renderTo container tree = do
  doc <- window >>= document >>= pure <<< HTMLDocument.toDocument
  interpretTree doc container tree

-- | The main interpreter - all helpers in where clause to share type variable
interpretTree :: forall a. Document -> Element -> Tree a -> Effect SelectionMap
interpretTree doc parent tree = go parent tree
  where
  -- Main recursive interpreter
  go :: Element -> Tree a -> Effect SelectionMap
  go p = case _ of
    Empty -> pure Map.empty

    Elem spec -> do
      el <- createElementWithNS spec.elemType doc
      appendTo p el
      -- Apply static attributes (no datum context)
      applyAttrsStatic el spec.attrs
      -- Apply behaviors (static, no datum)
      applyBehaviorsStatic el spec.behaviors
      childMaps <- traverse (go el) spec.children
      pure $ Array.foldl Map.union Map.empty childMaps

    Fold spec -> do
      let items = runEnumeration spec.enumerate
      case spec.assemble of
        Siblings -> renderSiblings p spec items
        Nested -> renderNested p spec

  -- Apply attributes without datum (for static Elem nodes)
  applyAttrsStatic :: Element -> Array (Attribute a) -> Effect Unit
  applyAttrsStatic el attrs = do
    _ <- traverse (applyAttrStatic el) attrs
    pure unit

  applyAttrStatic :: Element -> Attribute a -> Effect Unit
  applyAttrStatic el = case _ of
    StaticAttr (AttributeName name) val ->
      setAttribute el name (attrValueToString val)
    DataAttr _ _ _ ->
      -- Can't apply data-driven attr without datum - skip
      pure unit
    IndexedAttr _ _ _ ->
      pure unit
    AnimatedAttr _ ->
      pure unit
    AnimatedCompound _ ->
      pure unit

  -- Apply attributes with datum and index
  applyAttrsWithDatum :: Element -> Array (Attribute a) -> a -> Int -> Effect Unit
  applyAttrsWithDatum el attrs datum idx = do
    _ <- traverse (applyAttrWithDatum el datum idx) attrs
    pure unit

  applyAttrWithDatum :: Element -> a -> Int -> Attribute a -> Effect Unit
  applyAttrWithDatum el datum idx = case _ of
    StaticAttr (AttributeName name) val ->
      setAttribute el name (attrValueToString val)
    DataAttr (AttributeName name) _ fn ->
      setAttribute el name (attrValueToString (fn datum))
    IndexedAttr (AttributeName name) _ fn ->
      setAttribute el name (attrValueToString (fn datum idx))
    AnimatedAttr _ ->
      -- TODO: handle animated attributes
      pure unit
    AnimatedCompound _ ->
      -- TODO: handle compound animations
      pure unit

  -- Convert attribute value to string for DOM
  attrValueToString :: AttributeValue -> String
  attrValueToString = case _ of
    StringValue s -> s
    NumberValue n -> show n
    BooleanValue b -> if b then "true" else "false"

  -- Apply behaviors without datum (for static Elem nodes)
  applyBehaviorsStatic :: Element -> Array (Behavior a) -> Effect Unit
  applyBehaviorsStatic el behaviors = do
    _ <- traverse (applyBehaviorStatic el) behaviors
    pure unit

  applyBehaviorStatic :: Element -> Behavior a -> Effect Unit
  applyBehaviorStatic el = case _ of
    Click handler -> attachClick el handler
    Zoom (ZoomConfig cfg) ->
      let ScaleExtent minScale maxScale = cfg.scaleExtent
      in attachZoom el minScale maxScale cfg.targetSelector
    Drag SimpleDrag -> attachSimpleDrag el
    Drag (SimulationDrag simId) -> attachSimulationDragById el simId
    Drag (SimulationDragNested simId) -> attachSimulationDragNestedById el simId
    -- Behaviors requiring datum - skip in static context
    ClickWithDatum _ -> pure unit
    MouseEnter _ -> pure unit
    MouseLeave _ -> pure unit
    Highlight _ -> pure unit
    CoordinatedHighlight _ -> pure unit
    MouseMoveWithInfo _ -> pure unit
    MouseEnterWithInfo _ -> pure unit
    MouseLeaveWithInfo _ -> pure unit
    MouseDown _ -> pure unit
    MouseDownWithInfo _ -> pure unit

  -- Apply behaviors with datum context
  applyBehaviorsWithDatum :: Element -> Array (Behavior a) -> Effect Unit
  applyBehaviorsWithDatum el behaviors = do
    _ <- traverse (applyBehaviorWithDatum el) behaviors
    pure unit

  applyBehaviorWithDatum :: Element -> Behavior a -> Effect Unit
  applyBehaviorWithDatum el = case _ of
    Click handler -> attachClick el handler
    ClickWithDatum handler -> attachClickWithDatum el handler
    MouseEnter handler -> attachMouseEnter el handler
    MouseLeave handler -> attachMouseLeave el handler
    Highlight style -> attachHighlight el style.enter style.leave
    Zoom (ZoomConfig cfg) ->
      let ScaleExtent minScale maxScale = cfg.scaleExtent
      in attachZoom el minScale maxScale cfg.targetSelector
    Drag SimpleDrag -> attachSimpleDrag el
    Drag (SimulationDrag simId) -> attachSimulationDragById el simId
    Drag (SimulationDragNested simId) -> attachSimulationDragNestedById el simId
    MouseDown handler -> attachMouseDown el handler
    -- Complex behaviors - TODO
    CoordinatedHighlight _ -> pure unit
    MouseMoveWithInfo _ -> pure unit
    MouseEnterWithInfo _ -> pure unit
    MouseLeaveWithInfo _ -> pure unit
    MouseDownWithInfo _ -> pure unit

  -- Render with Siblings assembly (flat output)
  renderSiblings
    :: Element
    -> { name :: String
       , enumerate :: Enumeration a
       , assemble :: Assembly
       , keyFn :: a -> String
       , template :: a -> Tree a
       , gup :: Maybe _
       }
    -> Array a
    -> Effect SelectionMap
  renderSiblings p spec items = do
    results <- traverseWithIndex (renderItemWithKey p spec.keyFn spec.template) items
    let elements = map _.element results
    let childMaps = Array.foldl Map.union Map.empty (map _.childSelections results)
    pure $ Map.insert spec.name elements childMaps

  -- Render a single item with key storage
  renderItemWithKey
    :: Element
    -> (a -> String)
    -> (a -> Tree a)
    -> Int
    -> a
    -> Effect { element :: Element, childSelections :: SelectionMap }
  renderItemWithKey p keyFn template idx datum = do
    let itemTree = template datum
    let key = keyFn datum
    case itemTree of
      Elem spec -> do
        el <- createElementWithNS spec.elemType doc
        appendTo p el
        -- Store key for GUP diffing
        setKey el key
        -- Bind datum to element for later retrieval
        bindDatum el datum
        -- Apply attributes with datum context
        applyAttrsWithDatum el spec.attrs datum idx
        -- Apply behaviors with datum context
        applyBehaviorsWithDatum el spec.behaviors
        -- Process children WITH datum context (inherited from parent)
        childMaps <- traverse (goWithDatum el datum idx) spec.children
        pure { element: el, childSelections: Array.foldl Map.union Map.empty childMaps }

      Fold _ -> do
        -- Nested fold - create a group wrapper
        wrapper <- createElementWithNS Group doc
        appendTo p wrapper
        setKey wrapper key
        bindDatum wrapper datum
        innerMap <- go wrapper itemTree
        pure { element: wrapper, childSelections: innerMap }

      Empty -> do
        -- Return a dummy element (shouldn't happen in practice)
        el <- createElementWithNS Group doc
        pure { element: el, childSelections: Map.empty }

  -- Process a tree node with inherited datum context
  -- Used for children of Fold items that should inherit parent's datum
  goWithDatum :: Element -> a -> Int -> Tree a -> Effect SelectionMap
  goWithDatum p datum idx = case _ of
    Empty -> pure Map.empty

    Elem spec -> do
      el <- createElementWithNS spec.elemType doc
      appendTo p el
      -- Bind same datum to child element
      bindDatum el datum
      -- Apply attributes with inherited datum context
      applyAttrsWithDatum el spec.attrs datum idx
      -- Apply behaviors with datum context
      applyBehaviorsWithDatum el spec.behaviors
      -- Recursively process children with same datum
      childMaps <- traverse (goWithDatum el datum idx) spec.children
      pure $ Array.foldl Map.union Map.empty childMaps

    Fold spec -> do
      -- Nested fold starts fresh enumeration
      let items = runEnumeration spec.enumerate
      case spec.assemble of
        Siblings -> renderSiblings p spec items
        Nested -> renderNested p spec

  -- Render with Nested assembly (preserving structure)
  renderNested
    :: Element
    -> { name :: String
       , enumerate :: Enumeration a
       , assemble :: Assembly
       , keyFn :: a -> String
       , template :: a -> Tree a
       , gup :: Maybe _
       }
    -> Effect SelectionMap
  renderNested p spec = do
    case spec.enumerate of
      FromTree treeSpec -> do
        allElements <- goNested p treeSpec.root treeSpec.children 0
        pure $ Map.singleton spec.name allElements

      _ -> do
        -- Fall back to Siblings for non-tree enumeration
        let items = runEnumeration spec.enumerate
        renderSiblings p spec items
    where
    -- Recursive helper for nested tree rendering
    goNested :: Element -> a -> (a -> Array a) -> Int -> Effect (Array Element)
    goNested parentEl node getChildren idx = do
      let itemTree = spec.template node
      el <- case itemTree of
        Elem elemSpec -> do
          e <- createElementWithNS elemSpec.elemType doc
          appendTo parentEl e
          bindDatum e node
          applyAttrsWithDatum e elemSpec.attrs node idx
          applyBehaviorsWithDatum e elemSpec.behaviors
          _ <- traverse (go e) elemSpec.children
          pure e
        _ -> do
          e <- createElementWithNS Group doc
          appendTo parentEl e
          bindDatum e node
          pure e

      let children = getChildren node
      childElements <- traverseWithIndex
        (\i child -> goNested el child getChildren (idx + i + 1))
        children
      pure $ [el] <> Array.concat childElements

-- ============================================================================
-- Enumeration
-- ============================================================================

-- | Execute an enumeration to get array of elements
runEnumeration :: forall a. Enumeration a -> Array a
runEnumeration = case _ of
  FromArray arr -> arr

  FromTree spec ->
    case spec.order of
      DepthFirst -> enumerateDFS spec.root spec.children spec.includeInternal
      BreadthFirst -> enumerateBFS spec.root spec.children spec.includeInternal

  WithContext items -> map _.datum items

-- | Depth-first enumeration
enumerateDFS :: forall a. a -> (a -> Array a) -> Boolean -> Array a
enumerateDFS root getChildren includeInternal = go root
  where
  go node =
    let children = getChildren node
        childResults = Array.concatMap go children
        isLeaf = Array.null children
    in if isLeaf || includeInternal
       then [node] <> childResults
       else childResults

-- | Breadth-first enumeration
enumerateBFS :: forall a. a -> (a -> Array a) -> Boolean -> Array a
enumerateBFS root getChildren includeInternal = go [root]
  where
  go queue = case Array.uncons queue of
    Nothing -> []
    Just { head: node, tail: rest } ->
      let children = getChildren node
          isLeaf = Array.null children
          include = isLeaf || includeInternal
          thisNode = if include then [node] else []
      in thisNode <> go (rest <> children)

-- ============================================================================
-- FFI
-- ============================================================================

-- | Append child element to parent
appendTo :: Element -> Element -> Effect Unit
appendTo p child = do
  _ <- Node.appendChild (Element.toNode child) (Element.toNode p)
  pure unit

-- | Select element by CSS selector
foreign import selectElement :: String -> Document -> Effect Element

-- | Set attribute on element
foreign import setAttribute :: Element -> String -> String -> Effect Unit

-- | Bind datum to element (for later retrieval by behaviors/tick)
foreign import bindDatum :: forall a. Element -> a -> Effect Unit

-- Behavior FFI
foreign import attachClick :: Element -> Effect Unit -> Effect Unit
foreign import attachClickWithDatum :: forall a. Element -> (a -> Effect Unit) -> Effect Unit
foreign import attachMouseEnter :: forall a. Element -> (a -> Effect Unit) -> Effect Unit
foreign import attachMouseLeave :: forall a. Element -> (a -> Effect Unit) -> Effect Unit
foreign import attachMouseDown :: Element -> Effect Unit -> Effect Unit
foreign import attachHighlight
  :: Element
  -> Array { attr :: String, value :: String }
  -> Array { attr :: String, value :: String }
  -> Effect Unit
foreign import attachZoom :: Element -> Number -> Number -> String -> Effect Unit
foreign import attachSimpleDrag :: Element -> Effect Unit
foreign import attachSimulationDragById :: Element -> String -> Effect Unit
foreign import attachSimulationDragNestedById :: Element -> String -> Effect Unit

-- GUP FFI
foreign import setKey :: Element -> String -> Effect Unit
foreign import getKey :: Element -> Effect String
foreign import getChildElements :: Element -> Effect (Array Element)
foreign import removeElement :: Element -> Effect Unit
foreign import removeElementDelayed :: Element -> Int -> Effect Unit

-- ============================================================================
-- GUP (General Update Pattern)
-- ============================================================================

-- | Re-render a HATS tree with GUP (enter/update/exit)
-- | This compares new data against existing elements and processes accordingly.
rerender :: forall a. String -> Tree a -> Effect SelectionMap
rerender selector tree = do
  doc <- window >>= document >>= pure <<< HTMLDocument.toDocument
  container <- selectElement selector doc
  rerenderTree doc container tree

-- | The GUP-aware interpreter
rerenderTree :: forall a. Document -> Element -> Tree a -> Effect SelectionMap
rerenderTree doc parent tree = goWithIndex parent 0 tree
  where
  -- Track child index to find existing elements by position
  goWithIndex :: Element -> Int -> Tree a -> Effect SelectionMap
  goWithIndex p childIdx = case _ of
    Empty -> pure Map.empty

    Elem spec -> do
      -- For static Elem nodes, try to reuse existing element at this position
      existingChildren <- getChildElements p
      el <- case Array.index existingChildren childIdx of
        Just existing -> do
          -- Reuse existing element (just update attrs if needed)
          applyAttrsStatic existing spec.attrs
          pure existing
        Nothing -> do
          -- No existing element, create new one
          el <- createElementWithNS spec.elemType doc
          appendTo p el
          applyAttrsStatic el spec.attrs
          applyBehaviorsStatic el spec.behaviors
          pure el
      -- Process children recursively with their indices
      childMaps <- traverseWithIndex (\i child -> goWithIndex el i child) spec.children
      pure $ Array.foldl Map.union Map.empty childMaps

    Fold spec -> do
      let items = runEnumeration spec.enumerate
      let newKeys = Set.fromFoldable (map spec.keyFn items)

      -- Get existing elements and their keys
      existingEls <- getChildElements p
      existingKeysAndEls <- traverse (\el -> do
        k <- getKey el
        pure { key: k, element: el }
      ) existingEls

      let existingKeys = Set.fromFoldable (map _.key existingKeysAndEls)

      -- Compute enter/update/exit sets
      let enterKeys = Set.difference newKeys existingKeys
      let updateKeys = Set.intersection newKeys existingKeys
      let exitKeys = Set.difference existingKeys newKeys

      -- Process EXIT (remove elements with delay to show exit styling)
      _ <- traverse (\{ key, element } ->
        if Set.member key exitKeys
          then do
            -- Apply exit phase if specified
            case spec.gup of
              Just gupSpec -> case gupSpec.exit of
                Just phaseSpec -> do
                  applyPhaseAttrs element phaseSpec.attrs
                  -- Delay removal so exit styling is visible
                  removeElementDelayed element 300
                Nothing -> removeElement element
              Nothing -> removeElement element
          else pure unit
      ) existingKeysAndEls

      -- Process UPDATE (update existing elements)
      _ <- traverse (\{ key, element } ->
        if Set.member key updateKeys
          then do
            -- Find the datum for this key
            case Array.find (\d -> spec.keyFn d == key) items of
              Just datum -> do
                bindDatum element datum
                -- Apply template attrs
                case spec.template datum of
                  Elem elemSpec -> applyAttrsWithDatum element elemSpec.attrs datum 0
                  _ -> pure unit
                -- Apply update phase if specified
                case spec.gup of
                  Just gupSpec -> case gupSpec.update of
                    Just phaseSpec -> applyPhaseAttrs element phaseSpec.attrs
                    Nothing -> pure unit
                  Nothing -> pure unit
              Nothing -> pure unit
          else pure unit
      ) existingKeysAndEls

      -- Process ENTER (create new elements)
      let enterItems = Array.filter (\d -> Set.member (spec.keyFn d) enterKeys) items
      enterResults <- traverseWithIndex (renderItemWithKeyAndPhase p spec.keyFn spec.template spec.gup) enterItems

      -- Get all current elements (updated + entered)
      let updateElements = map _.element (Array.filter (\{ key } -> Set.member key updateKeys) existingKeysAndEls)
      let enterElements = map _.element enterResults
      let allElements = updateElements <> enterElements

      let childMaps = Array.foldl Map.union Map.empty (map _.childSelections enterResults)
      pure $ Map.insert spec.name allElements childMaps

  -- Render item with enter phase
  renderItemWithKeyAndPhase
    :: Element
    -> (a -> String)
    -> (a -> Tree a)
    -> Maybe (GUPSpec a)
    -> Int
    -> a
    -> Effect { element :: Element, childSelections :: SelectionMap }
  renderItemWithKeyAndPhase p keyFn template gupSpec idx datum = do
    let itemTree = template datum
    let key = keyFn datum
    case itemTree of
      Elem spec -> do
        el <- createElementWithNS spec.elemType doc
        appendTo p el
        setKey el key
        bindDatum el datum
        applyAttrsWithDatum el spec.attrs datum idx
        applyBehaviorsWithDatum el spec.behaviors
        -- Apply enter phase if specified
        case gupSpec of
          Just gs -> case gs.enter of
            Just phaseSpec -> applyPhaseAttrs el phaseSpec.attrs
            Nothing -> pure unit
          Nothing -> pure unit
        childMaps <- traverse (goWithDatum el datum idx) spec.children
        pure { element: el, childSelections: Array.foldl Map.union Map.empty childMaps }

      _ -> do
        el <- createElementWithNS Group doc
        pure { element: el, childSelections: Map.empty }

  -- Apply phase-specific attributes
  applyPhaseAttrs :: Element -> Array (Attribute a) -> Effect Unit
  applyPhaseAttrs el attrs = do
    _ <- traverse (applyAttrStatic el) attrs
    pure unit

  -- Apply attributes without datum (for static Elem nodes)
  applyAttrsStatic :: Element -> Array (Attribute a) -> Effect Unit
  applyAttrsStatic el attrs = do
    _ <- traverse (applyAttrStatic el) attrs
    pure unit

  applyAttrStatic :: Element -> Attribute a -> Effect Unit
  applyAttrStatic el = case _ of
    StaticAttr (AttributeName name) val ->
      setAttribute el name (attrValueToString val)
    DataAttr _ _ _ -> pure unit
    IndexedAttr _ _ _ -> pure unit
    AnimatedAttr _ -> pure unit
    AnimatedCompound _ -> pure unit

  -- Apply attributes with datum and index
  applyAttrsWithDatum :: Element -> Array (Attribute a) -> a -> Int -> Effect Unit
  applyAttrsWithDatum el attrs datum idx = do
    _ <- traverse (applyAttrWithDatum el datum idx) attrs
    pure unit

  applyAttrWithDatum :: Element -> a -> Int -> Attribute a -> Effect Unit
  applyAttrWithDatum el datum idx = case _ of
    StaticAttr (AttributeName name) val ->
      setAttribute el name (attrValueToString val)
    DataAttr (AttributeName name) _ fn ->
      setAttribute el name (attrValueToString (fn datum))
    IndexedAttr (AttributeName name) _ fn ->
      setAttribute el name (attrValueToString (fn datum idx))
    AnimatedAttr _ -> pure unit
    AnimatedCompound _ -> pure unit

  attrValueToString :: AttributeValue -> String
  attrValueToString = case _ of
    StringValue s -> s
    NumberValue n -> show n
    BooleanValue b -> if b then "true" else "false"

  -- Apply behaviors without datum
  applyBehaviorsStatic :: Element -> Array (Behavior a) -> Effect Unit
  applyBehaviorsStatic el behaviors = do
    _ <- traverse (applyBehaviorStatic el) behaviors
    pure unit

  applyBehaviorStatic :: Element -> Behavior a -> Effect Unit
  applyBehaviorStatic el = case _ of
    Click handler -> attachClick el handler
    Zoom (ZoomConfig cfg) ->
      let ScaleExtent minScale maxScale = cfg.scaleExtent
      in attachZoom el minScale maxScale cfg.targetSelector
    Drag SimpleDrag -> attachSimpleDrag el
    Drag (SimulationDrag simId) -> attachSimulationDragById el simId
    Drag (SimulationDragNested simId) -> attachSimulationDragNestedById el simId
    _ -> pure unit

  -- Apply behaviors with datum context
  applyBehaviorsWithDatum :: Element -> Array (Behavior a) -> Effect Unit
  applyBehaviorsWithDatum el behaviors = do
    _ <- traverse (applyBehaviorWithDatum el) behaviors
    pure unit

  applyBehaviorWithDatum :: Element -> Behavior a -> Effect Unit
  applyBehaviorWithDatum el = case _ of
    Click handler -> attachClick el handler
    ClickWithDatum handler -> attachClickWithDatum el handler
    MouseEnter handler -> attachMouseEnter el handler
    MouseLeave handler -> attachMouseLeave el handler
    Highlight style -> attachHighlight el style.enter style.leave
    Zoom (ZoomConfig cfg) ->
      let ScaleExtent minScale maxScale = cfg.scaleExtent
      in attachZoom el minScale maxScale cfg.targetSelector
    Drag SimpleDrag -> attachSimpleDrag el
    Drag (SimulationDrag simId) -> attachSimulationDragById el simId
    Drag (SimulationDragNested simId) -> attachSimulationDragNestedById el simId
    MouseDown handler -> attachMouseDown el handler
    _ -> pure unit

  goWithDatum :: Element -> a -> Int -> Tree a -> Effect SelectionMap
  goWithDatum p datum idx = case _ of
    Empty -> pure Map.empty
    Elem spec -> do
      el <- createElementWithNS spec.elemType doc
      appendTo p el
      bindDatum el datum
      applyAttrsWithDatum el spec.attrs datum idx
      applyBehaviorsWithDatum el spec.behaviors
      childMaps <- traverse (goWithDatum el datum idx) spec.children
      pure $ Array.foldl Map.union Map.empty childMaps
    Fold foldSpec -> do
      let items = runEnumeration foldSpec.enumerate
      case foldSpec.assemble of
        Siblings -> do
          results <- traverseWithIndex (renderItemWithKeyAndPhase p foldSpec.keyFn foldSpec.template foldSpec.gup) items
          let elements = map _.element results
          let childMaps = Array.foldl Map.union Map.empty (map _.childSelections results)
          pure $ Map.insert foldSpec.name elements childMaps
        Nested -> pure Map.empty
