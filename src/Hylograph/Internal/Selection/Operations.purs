-- | Internal: Implementation of selection operations using web-dom.
-- |
-- | Provides the concrete implementations for `SelectionM` operations:
-- | - Selecting: `select`, `selectAll`, `selectElement`
-- | - Appending: `append`, `appendChild`, `appendChildInheriting`
-- | - Data binding: `joinData`, `joinDataWithKey`, `renderData`
-- | - Attributes: `setAttrs`, `setAttrsExit`
-- | - Lifecycle: `remove`, `clear`, `merge`
-- | - HATS rendering: See Hylograph.HATS.InterpreterTick for the new API
-- |
-- | Uses `web-dom` for DOM manipulation, not D3's selection API.
-- |
-- | **Internal module** - use `Hylograph.Selection` for the public API.
-- |
-- | ## SAFETY: Phantom Type Guards for unsafePartial
-- |
-- | This module uses `unsafePartial` extensively to pattern-match on Selection
-- | constructors. These are safe because the phantom type parameter (SEmpty,
-- | SPending, SBoundOwns, SBoundInherits, SExiting) is set at construction time
-- | and enforced by the type system:
-- |
-- | - `SEmpty` guarantees `EmptySelection` constructor
-- | - `SPending` guarantees `PendingSelection` constructor
-- | - `SBoundOwns` guarantees `BoundSelection` with owned data
-- | - `SBoundInherits` guarantees `BoundSelection` with inherited data
-- | - `SExiting` guarantees `ExitingSelection` constructor
-- |
-- | The phantom types make illegal states unrepresentable at compile time.
-- | A function taking `Selection SPending elem datum` can only receive a
-- | PendingSelection, so the partial match is actually total for that type.
-- |
-- | Similarly, `Array.unsafeIndex` is used where indices come from internal
-- | bookkeeping (e.g., join algorithms) that maintains valid bounds.
module Hylograph.Internal.Selection.Operations
  ( select
  , selectElement
  , selectAll
  , selectAllWithData
  , selectChildInheriting
  , append
  , appendChild
  , appendChildInheriting
  , setAttrs
  , setAttrsExit
  , remove
  , clear
  , syncDOMToData
  , merge
  , joinData
  , joinDataWithKey
  , renderData
  , appendData
  , on
  , elementTypeToString
  , createElementWithNS
  , getDocument
  ) where

import Prelude hiding (append)

import Data.Array as Array
import Data.Foldable (class Foldable, traverse_)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (mkEffectFn2)
import Hylograph.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..))
import Hylograph.Internal.Behavior.FFI as BehaviorFFI
import Hylograph.Internal.Behavior.Types (Behavior(..), DragConfig(..), ZoomConfig(..), ScaleExtent(..), HighlightClass(..), TooltipTrigger(..))
import Hylograph.Internal.Selection.Join as Join
import Hylograph.Internal.Selection.Types (ElementType(..), JoinResult(..), RenderContext(..), SBoundInherits, SBoundOwns, SEmpty, SExiting, SPending, Selection(..), SelectionImpl(..), elementContext)
import Hylograph.Internal.Transition.FFI as TransitionFFI
import Hylograph.Internal.Transition.Types (TransitionConfig)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document)
import Web.DOM.Document as Document
import Web.DOM.Element (Element, fromNode, toNode, toParentNode)
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, clientY, pageX, pageY)

-- FFI for offsetX/offsetY (not yet in purescript-web-uievents - PR candidate)
foreign import offsetX :: MouseEvent -> Number
foreign import offsetY :: MouseEvent -> Number

-- | Select a single element matching the CSS selector
-- |
-- | Returns an empty selection (no data bound).
-- | The datum type is polymorphic and will be inferred from usage.
-- | This is typically the starting point for data binding.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | circles <- renderData Circle [1, 2, 3] "circle" svg ...
-- | ```
select
  :: forall m datum
   . MonadEffect m
  => String -- CSS selector
  -> m (Selection SEmpty Element datum)
select selector = liftEffect do
  htmlDoc <- window >>= document
  let doc = toDocument htmlDoc
  let parentNode = HTMLDocument.toParentNode htmlDoc
  maybeElement <- querySelector (QuerySelector selector) parentNode
  case maybeElement of
    Nothing -> pure $ Selection $ EmptySelection
      { parentElements: []
      , document: doc
      }
    Just element -> pure $ Selection $ EmptySelection
      { parentElements: [ element ]
      , document: doc
      }

-- | Select from a DOM element directly
-- |
-- | This is useful for framework integration (React, Vue, etc.) where you have
-- | a reference to a DOM element rather than a CSS selector.
-- |
-- | Example:
-- | ```purescript
-- | selectElement element >>= renderTree myVisualization
-- | ```
selectElement
  :: forall m datum
   . MonadEffect m
  => Element -- DOM element
  -> m (Selection SEmpty Element datum)
selectElement element = liftEffect do
  htmlDoc <- window >>= document
  let doc = toDocument htmlDoc
  pure $ Selection $ EmptySelection
    { parentElements: [ element ]
    , document: doc
    }

-- | Select all elements matching the CSS selector within a parent selection
-- |
-- | Returns an empty selection (no data bound yet).
-- | The datum type is polymorphic and will be inferred from usage.
-- | Use this for nested selections.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | groups <- selectAll "g" svg
-- | ```
selectAll
  :: forall state parent parentDatum datum m
   . MonadEffect m
  => String -- CSS selector
  -> Selection state parent parentDatum
  -> m (Selection SEmpty Element datum)
selectAll selector (Selection impl) = liftEffect do
  doc <- getDocument impl
  -- Get the parent elements (where new children will be appended)
  -- NOT the query results - those are fetched by joinData when needed
  let
    parentElems = case impl of
      EmptySelection { parentElements } -> parentElements
      BoundSelection { elements } -> elements
      PendingSelection { parentElements } -> parentElements
      ExitingSelection { elements } -> elements

  pure $ Selection $ EmptySelection
    { parentElements: parentElems -- Preserve parents for appending
    , document: doc
    }

-- | Select all elements matching selector and extract their bound data
-- |
-- | Use this when selecting child elements that have inherited data from their parent.
-- | This is necessary when you want to use the selection with transitions that need
-- | access to the bound data (like withTransitionStaggered).
-- |
-- | Example:
-- | ```purescript
-- | -- After creating nodes with appendChildInheriting
-- | groups <- selectSimulationGroups
-- | circles <- selectAllWithData "circle" groups.nodes
-- | withTransitionStaggered config delayFn circles [fill colorByDepth]
-- | ```
selectAllWithData
  :: forall state parent parentDatum datum m
   . MonadEffect m
  => String -- CSS selector
  -> Selection state parent parentDatum
  -> m (Selection SBoundOwns Element datum)
selectAllWithData selector (Selection impl) = liftEffect do
  doc <- getDocument impl
  elements <- case impl of
    EmptySelection { parentElements } ->
      querySelectorAllElements selector parentElements
    BoundSelection { elements: parentElems } ->
      querySelectorAllElements selector parentElems
    PendingSelection { parentElements } ->
      querySelectorAllElements selector parentElements
    ExitingSelection { elements: exitElems } ->
      querySelectorAllElements selector exitElems

  -- Extract __data__ from each element
  dataArray <- traverse
    ( \el -> do
        nullableDatum <- getElementData_ el
        pure $ case toMaybe nullableDatum of
          Just d -> d
          Nothing -> unsafeCoerce unit -- Fallback if no data (shouldn't happen with appendChildInheriting)
    )
    elements

  pure $ Selection $ BoundSelection
    { elements: elements
    , data: dataArray
    , indices: Nothing
    , document: doc
    }

-- | Append new elements to a pending (enter) selection
-- |
-- | This materializes the data into DOM elements.
-- | Returns a bound selection that can be further modified.
-- |
-- | Example:
-- | ```purescript
-- | enterSelection <- append Circle
-- |   [ fill "green"
-- |   , radius 10.0
-- |   , cx (\d -> d.x)
-- |   ]
-- |   pendingSelection
-- | ```
append
  :: forall parent datum m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datum)
  -> Selection SPending parent datum
  -> m (Selection SBoundOwns Element datum)
append elemType attrs (Selection impl) = liftEffect do
  let
    { parentElements, pendingData, indices, document: doc } = unsafePartial case impl of
      PendingSelection r -> r
  -- Create elements for each datum
  -- In D3, if there's one parent, all elements go to that parent
  -- If there are multiple parents, they're distributed (but that's rare)
  let
    parent = case Array.head parentElements of
      Just p -> p
      Nothing -> unsafePartial $ Array.unsafeIndex parentElements 0 -- Should never happen

  elements <- pendingData # traverseWithIndex \arrayIndex datum -> do
    -- Use logical index from indices array if present (for enter selections from joins)
    let
      logicalIndex = case indices of
        Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
        Nothing -> arrayIndex

    element <- createElementWithNS elemType doc
    -- Set attributes on the new element using logical index
    applyAttributes element datum logicalIndex attrs
    -- Bind data to element (CRITICAL for data joins!)
    setElementData_ datum element
    -- Append to parent
    let elementNode = toNode element
    let parentNode = toNode parent
    Node.appendChild elementNode parentNode
    pure element

  pure $ Selection $ BoundSelection
    { elements
    , data: pendingData
    , indices -- Preserve indices from pending selection (for enter selections from joins)
    , document: doc
    }

-- | Set attributes on a bound selection
-- |
-- | Updates existing elements with new attribute values.
-- | This is used for the "update" part of enter-update-exit.
-- |
-- | Example:
-- | ```purescript
-- | updated <- setAttrs
-- |   [ fill "orange"
-- |   , cx (\d -> d.x)
-- |   ]
-- |   boundSelection
-- | ```
setAttrs
  :: forall datum m
   . MonadEffect m
  => Array (Attribute datum)
  -> Selection SBoundOwns Element datum
  -> m (Selection SBoundOwns Element datum)
setAttrs attrs (Selection impl) = liftEffect do
  let
    { elements, data: datumArray, indices, document: doc } = unsafePartial case impl of
      BoundSelection r -> r
  -- Apply attributes to each element, using logical indices if present
  let paired = Array.zipWith Tuple datumArray elements
  paired # traverseWithIndex_ \arrayIndex (Tuple datum element) -> do
    let
      logicalIndex = case indices of
        Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
        Nothing -> arrayIndex
    applyAttributes element datum logicalIndex attrs

  pure $ Selection $ BoundSelection
    { elements
    , data: datumArray
    , indices -- Preserve indices from input selection
    , document: doc
    }

-- | Set attributes on an exiting selection
-- |
-- | Similar to setAttrs but for selections in the exit phase.
-- | Useful for styling elements before they are removed.
-- |
-- | Example:
-- | ```purescript
-- | setAttrsExit [fill "brown", class_ "exiting"] exitSelection
-- | ```
setAttrsExit
  :: forall datum m
   . MonadEffect m
  => Array (Attribute datum)
  -> Selection SExiting Element datum
  -> m (Selection SExiting Element datum)
setAttrsExit attrs (Selection impl) = liftEffect do
  let
    { elements, data: datumArray, document: doc } = unsafePartial case impl of
      ExitingSelection r -> r
  -- Apply attributes to each element
  let paired = Array.zipWith Tuple datumArray elements
  paired # traverseWithIndex_ \index (Tuple datum element) ->
    applyAttributes element datum index attrs

  pure $ Selection $ ExitingSelection
    { elements
    , data: datumArray
    , document: doc
    }

-- | Remove elements from an exit selection
-- |
-- | Removes the elements from the DOM.
-- | This is the final step for exiting data.
-- |
-- | Example:
-- | ```purescript
-- | remove exitSelection
-- | ```
remove
  :: forall datum m
   . MonadEffect m
  => Selection SExiting Element datum
  -> m Unit
remove (Selection impl) = liftEffect do
  let
    { elements } = unsafePartial case impl of
      ExitingSelection r -> r
  elements # traverse_ \element -> do
    let node = toNode element
    maybeParent <- Node.parentNode node
    case maybeParent of
      Just parent -> Node.removeChild node parent
      Nothing -> pure unit -- Element not in DOM, nothing to remove

-- | Clear all children from an element
-- |
-- | Selects the element and removes all its children.
-- | Useful for clearing a container before rendering new content.
-- |
-- | Example:
-- | ```purescript
-- | clear "#viz"
-- | svg <- appendChild SVG [...] container
-- | ```
clear
  :: forall m
   . MonadEffect m
  => String -- CSS selector
  -> m Unit
clear selector = liftEffect $ clearElement_ selector

-- FFI for clearing an element's children
foreign import clearElement_ :: String -> Effect Unit

-- | Sync DOM transform positions back to __data__.x and __data__.y
-- |
-- | Reads the current transform attribute from each element matching the selector
-- | and updates the bound data's x/y coordinates. Essential for transitioning from
-- | CSS animations to force simulation - ensures simulation sees current positions.
-- |
-- | Example:
-- | ```purescript
-- | -- After tree reveal animation completes:
-- | syncDOMToData "g.nodes > g"  -- Sync group positions to node data
-- | start  -- Simulation continues from current positions
-- | ```
syncDOMToData
  :: forall m
   . MonadEffect m
  => String -- CSS selector for elements to sync
  -> m Unit
syncDOMToData selector = liftEffect $ syncDOMToData_ selector

-- FFI for syncing DOM transforms to __data__
foreign import syncDOMToData_ :: String -> Effect Unit

-- | Append a single child element to a parent selection
-- |
-- | Creates one new element and appends it to each parent in the selection.
-- | Returns an empty selection of the newly created element(s).
-- |
-- | This is different from `append` which creates elements for each datum
-- | in a pending (enter) selection. `appendChild` is for structural elements
-- | like creating an SVG container.
-- |
-- | Example:
-- | ```purescript
-- | container <- select "#viz"
-- | svg <- appendChild SVG [width 400.0, height 150.0] container
-- | circles <- renderData Circle [1, 2, 3] "circle" svg ...
-- | ```
appendChild
  :: forall parent datum datumOut m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datumOut)
  -> Selection SEmpty parent datum
  -> m (Selection SEmpty Element datumOut)
appendChild elemType attrs (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r

  -- Create one element for each parent
  elements <- parentElements # traverse \parent -> do
    element <- createElementWithNS elemType doc
    -- Apply attributes with a dummy datum (attributes should be static for appendChild)
    -- We use unit as the datum since structural elements typically don't need data
    let dummyDatum = unsafeCoerce unit :: datumOut
    applyAttributes element dummyDatum 0 attrs
    -- Append to parent
    let elementNode = toNode element
    let parentNode = toNode parent
    Node.appendChild elementNode parentNode
    pure element

  pure $ Selection $ EmptySelection
    { parentElements: elements
    , document: doc
    }

-- | Append child elements with an optional explicit datum for attribute functions
-- | When datumOpt is Nothing, uses a dummy datum (like appendChild)
-- | When datumOpt is Just d, uses d for all datum-dependent attributes
appendChildWithDatum
  :: forall parent datum datumOut m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datumOut)
  -> Maybe datumOut -- Optional datum for attribute functions
  -> Int -- Logical index for IndexedAttr
  -> Selection SEmpty parent datum
  -> m (Selection SEmpty Element datumOut)
appendChildWithDatum elemType attrs datumOpt logicalIndex (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r

  -- Create one element for each parent
  elements <- parentElements # traverse \parent -> do
    element <- createElementWithNS elemType doc
    -- Apply attributes with the provided datum or a dummy if none
    let
      datum = case datumOpt of
        Just d -> d
        Nothing -> unsafeCoerce unit :: datumOut
    applyAttributes element datum logicalIndex attrs
    -- Append to parent
    let elementNode = toNode element
    let parentNode = toNode parent
    Node.appendChild elementNode parentNode
    pure element

  pure $ Selection $ EmptySelection
    { parentElements: elements
    , document: doc
    }

-- | Append child elements to a data-bound selection, inheriting parent's data
-- |
-- | This is the key function for creating nested SVG structures where children
-- | need access to their parent's bound data. The children don't own the data
-- | binding - they inherit it from their parent.
-- |
-- | Semantics: Each parent element gets one child. The child inherits the parent's
-- | data by copying the __data__ reference (a performance optimization vs. traversing
-- | the DOM tree on every attribute access).
-- |
-- | Type signature documents the data flow:
-- | - Parent must be SBoundOwns (owns the data to inherit from)
-- | - Child is SBoundInherits (borrows parent's data)
-- | - Both parent and child have same datum type
-- |
-- | Example:
-- | ```purescript
-- | -- Create groups with data
-- | groups <- append Group [] enterSelection  -- SBoundOwns
-- |
-- | -- Add circles that inherit group's data
-- | circles <- appendChildInheriting Circle [radius 5.0] groups  -- SBoundInherits
-- |
-- | -- Add text that also inherits group's data
-- | labels <- appendChildInheriting Text [textContent _.name] groups  -- SBoundInherits
-- | ```
appendChildInheriting
  :: forall parent datum m
   . MonadEffect m
  => ElementType
  -> Array (Attribute datum)
  -> Selection SBoundOwns parent datum
  -> m (Selection SBoundInherits Element datum)
appendChildInheriting elemType attrs (Selection impl) = liftEffect do
  let
    { elements: parentElements, data: dataArray, document: doc } = unsafePartial case impl of
      BoundSelection r -> r

  -- Create one child for each parent, inheriting the parent's data
  childElements <- Array.zipWith Tuple parentElements dataArray # traverseWithIndex \idx (Tuple parent datum) -> do
    -- Create child element
    child <- createElementWithNS elemType doc

    -- Copy parent's __data__ to child (the "caching" optimization)
    -- This allows child to access data without DOM traversal
    setElementData_ datum child

    -- Apply attributes using the inherited data
    applyAttributes child datum idx attrs

    -- Append child to parent
    let childNode = toNode child
    let parentNode = toNode parent
    Node.appendChild childNode parentNode

    pure child

  -- Return as SBoundInherits selection
  pure $ Selection $ BoundSelection
    { elements: childElements
    , data: dataArray -- Same data as parent
    , indices: Nothing
    , document: doc
    }

-- | Select child elements, inheriting parent's data
-- |
-- | Like D3's `selection.select()`: for each parent element, selects the first
-- | child matching the selector and copies the parent's `__data__` to the child.
-- |
-- | Use when you have groups with bound data and want to update their children.
-- |
-- | Example:
-- | ```purescript
-- | groups <- selectAllWithData ".treemap-package" container
-- | circles <- selectChildInheriting "circle" groups
-- | setAttrs [ fill (colorByData _.topoLayer) ] circles
-- | ```
selectChildInheriting
  :: forall parent datum m
   . MonadEffect m
  => String -- CSS selector for child elements
  -> Selection SBoundOwns parent datum
  -> m (Selection SBoundOwns Element datum)
selectChildInheriting selector (Selection impl) = liftEffect do
  let
    { elements: parentElements, data: dataArray, document: doc } = unsafePartial case impl of
      BoundSelection r -> r

  -- For each parent, select the first matching child and inherit data
  results <- Array.zipWith Tuple parentElements dataArray # traverse \(Tuple parent datum) -> do
    let parentNode = toParentNode parent
    maybeChild <- querySelector (QuerySelector selector) parentNode
    case maybeChild of
      Nothing -> pure Nothing
      Just child -> do
        -- Copy parent's __data__ to child
        setElementData_ datum child
        pure $ Just (Tuple child datum)

  -- Filter to only successful selections
  let paired = Array.catMaybes results
  let childElements = map fst paired
  let childData = map snd paired

  pure $ Selection $ BoundSelection
    { elements: childElements
    , data: childData
    , indices: Nothing
    , document: doc
    }

-- | Merge two bound selections
-- |
-- | Follows D3 semantics: concatenates in document order.
-- | Useful for combining enter and update selections.
-- |
-- | Example:
-- | ```purescript
-- | allCircles <- merge enterSelection updateSelection
-- | ```
merge
  :: forall datum m
   . MonadEffect m
  => Selection SBoundOwns Element datum
  -> Selection SBoundOwns Element datum
  -> m (Selection SBoundOwns Element datum)
merge (Selection impl1) (Selection impl2) = do
  let
    { elements: els1, data: data1, document: doc } = unsafePartial case impl1 of
      BoundSelection r -> r
  let
    { elements: els2, data: data2 } = unsafePartial case impl2 of
      BoundSelection r -> r
  pure $ Selection $ BoundSelection
    { elements: els1 <> els2
    , data: data1 <> data2
    , indices: Nothing -- Merged selections lose index information
    , document: doc
    }

-- | Low-level data join for power users
-- |
-- | Explicitly returns enter, update, and exit selections.
-- | Users must handle each set manually.
-- |
-- | Example:
-- | ```purescript
-- | JoinResult { enter, update, exit } <- joinData [1, 2, 3] "circle" svg
-- | enterEls <- append Circle [...] enter
-- | updateEls <- setAttrs [...] update
-- | remove exit
-- | ```
joinData
  :: forall f parent parentDatum datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  => f datum
  -> String -- Element selector for existing elements
  -> Selection SEmpty parent parentDatum
  -> m (JoinResult Selection parent datum)
joinData foldableData selector (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r
  -- Query for existing elements within parents
  existingElements <- querySelectorAllElements selector parentElements

  -- Get old bindings (elements with their bound data)
  oldBindings <- existingElements # traverse \element -> do
    nullableDatum <- getElementData_ element
    let maybeDatum = toMaybe nullableDatum
    pure $ { element, datum: maybeDatum }

  -- Log what data we found on existing elements
  -- liftEffect $ log $ "joinData: found " <> show (Array.length existingElements) <> " existing elements with selector '" <> selector <> "'"
  -- liftEffect $ log $ "joinData: " <> show (Array.length (Array.mapMaybe _.datum oldBindings)) <> " of those have data bound"

  -- Filter to only elements that have data bound
  let
    validOldBindings = oldBindings # Array.mapMaybe \{ element, datum } ->
      datum <#> \d -> { element, datum: d }

  -- Convert foldable to array
  let newDataArray = Array.fromFoldable foldableData

  -- Run pure join algorithm
  let joinSets = Join.computeJoin newDataArray validOldBindings

  -- Build typed selections for each set
  -- Sort enter bindings by newIndex to match the order in the new data
  let sortedEnter = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.enter

  let
    enterSelection = Selection $ PendingSelection
      { parentElements
      , pendingData: sortedEnter <#> _.datum
      , indices: Just (sortedEnter <#> _.newIndex) -- Preserve logical positions for element creation
      , document: doc
      }

  -- Sort update bindings by newIndex to match the order in the new data
  let sortedUpdate = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.update

  let
    updateSelection = Selection $ BoundSelection
      { elements: sortedUpdate <#> _.element
      , data: sortedUpdate <#> _.newDatum
      , indices: Just (sortedUpdate <#> _.newIndex) -- Preserve logical positions for transitions
      , document: doc
      }

  let
    exitSelection = Selection $ ExitingSelection
      { elements: joinSets.exit <#> _.element
      , data: joinSets.exit <#> _.datum
      , document: doc
      }

  pure $ JoinResult
    { enter: enterSelection
    , update: updateSelection
    , exit: exitSelection
    }

-- | Low-level data join with custom key function
-- |
-- | Like joinData, but uses a key function to extract comparable keys
-- | instead of requiring Ord on the data itself.
-- |
-- | This is essential for data types that don't have lawful Ord instances
-- | (e.g., opaque foreign types like D3Link_Swizzled).
-- |
-- | Example:
-- | ```purescript
-- | JoinResult { enter, update, exit } <- joinDataWithKey links (\l -> l.id) "line" svg
-- | enterEls <- append Line [...] enter
-- | updateEls <- setAttrs [...] update
-- | remove exit
-- | ```
joinDataWithKey
  :: forall f parent parentDatum datum key m
   . MonadEffect m
  => Foldable f
  => Eq key
  => f datum
  -> (datum -> key) -- Key extraction function
  -> String -- Element selector for existing elements
  -> Selection SEmpty parent parentDatum
  -> m (JoinResult Selection parent datum)
joinDataWithKey foldableData keyFn selector (Selection impl) = liftEffect do
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r
  -- Query for existing elements within parents
  existingElements <- querySelectorAllElements selector parentElements

  -- Get old bindings (elements with their bound data)
  oldBindings <- existingElements # traverse \element -> do
    nullableDatum <- getElementData_ element
    let maybeDatum = toMaybe nullableDatum
    pure $ { element, datum: maybeDatum }

  -- Log what data we found on existing elements
  -- liftEffect $ log $ "joinDataWithKey: found " <> show (Array.length existingElements) <> " existing elements with selector '" <> selector <> "'"
  -- liftEffect $ log $ "joinDataWithKey: " <> show (Array.length (Array.mapMaybe _.datum oldBindings)) <> " of those have data bound"

  -- Filter to only elements that have data bound
  let
    validOldBindings = oldBindings # Array.mapMaybe \{ element, datum } ->
      datum <#> \d -> { element, datum: d }

  -- Convert foldable to array
  let newDataArray = Array.fromFoldable foldableData

  -- Run pure join algorithm with key function
  let joinSets = Join.computeJoinWithKey newDataArray validOldBindings keyFn

  -- Build typed selections for each set
  -- Sort enter bindings by newIndex to match the order in the new data
  let sortedEnter = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.enter

  let
    enterSelection = Selection $ PendingSelection
      { parentElements
      , pendingData: sortedEnter <#> _.datum
      , indices: Just (sortedEnter <#> _.newIndex) -- Preserve logical positions for element creation
      , document: doc
      }

  -- Sort update bindings by newIndex to match the order in the new data
  let sortedUpdate = Array.sortBy (\a b -> compare a.newIndex b.newIndex) joinSets.update

  let
    updateSelection = Selection $ BoundSelection
      { elements: sortedUpdate <#> _.element
      , data: sortedUpdate <#> _.newDatum
      , indices: Just (sortedUpdate <#> _.newIndex) -- Preserve logical positions for transitions
      , document: doc
      }

  let
    exitSelection = Selection $ ExitingSelection
      { elements: joinSets.exit <#> _.element
      , data: joinSets.exit <#> _.datum
      , document: doc
      }

  pure $ JoinResult
    { enter: enterSelection
    , update: updateSelection
    , exit: exitSelection
    }

-- | High-level data rendering for most users
-- |
-- | Manages the entire enter-update-exit cycle automatically.
-- | Users provide Maybe callbacks for each phase.
-- |
-- | This is the recommended API for 90% of use cases.
-- |
-- | Example:
-- | ```purescript
-- | circles <- renderData Circle [1, 2, 3] "circle" svg
-- |   (Just \d -> [fill "green", cx (\_ -> d * 100.0)])  -- Enter
-- |   (Just \d -> [fill "orange"])                        -- Update
-- |   Nothing                                             -- Exit (just remove)
-- | ```
renderData
  :: forall f parent datum m
   . MonadEffect m
  => Foldable f
  => Ord datum
  => ElementType
  -> f datum
  -> String -- Element selector
  -> Selection SEmpty parent datum
  -> Maybe (datum -> Array (Attribute datum)) -- Enter attributes
  -> Maybe (datum -> Array (Attribute datum)) -- Update attributes
  -> Maybe (datum -> Array (Attribute datum)) -- Exit attributes (applied before removal)
  -> m (Selection SBoundOwns Element datum)
renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs = do
  -- Perform the join
  JoinResult { enter, update, exit } <- joinData foldableData selector emptySelection

  -- Handle enter: append elements then apply per-datum attributes if provided
  enterBound <- do
    -- First append all elements with no attributes
    bound <- append elemType [] enter
    -- Then apply per-datum attributes if provided
    case enterAttrs of
      Nothing -> pure bound
      Just mkAttrs -> applyPerDatumAttrs mkAttrs bound

  -- Handle update: apply per-datum attributes if provided
  updateBound <- case updateAttrs of
    Nothing -> pure update
    Just mkAttrs -> applyPerDatumAttrs mkAttrs update

  -- Handle exit: apply attributes then remove
  case exitAttrs of
    Nothing -> remove exit
    Just mkAttrs -> do
      _ <- applyPerDatumAttrs mkAttrs exit
      remove exit

  -- Merge enter and update
  merge enterBound updateBound

-- | Simple data append for initial renders
-- |
-- | A simplified variant of renderData for when you just want to create
-- | elements from data without worrying about enter/update/exit cycles.
-- |
-- | This is perfect for initial renders where you know there are no
-- | existing elements to update or remove.
-- |
-- | Example:
-- | ```purescript
-- | svg <- select "svg"
-- | circles <- appendData Circle [1, 2, 3]
-- |   [radius 10.0, fill "steelblue", cx (\d _ -> d * 100.0)]
-- |   svg
-- | ```
appendData
  :: forall f parent parentDatum datum m
   . MonadEffect m
  => Foldable f
  => ElementType
  -> f datum
  -> Array (Attribute datum)
  -> Selection SEmpty parent parentDatum
  -> m (Selection SBoundOwns Element datum)
appendData elemType foldableData attrs emptySelection = liftEffect do
  -- Extract parent elements and document from the empty selection
  let Selection impl = emptySelection
  let
    { parentElements, document: doc } = unsafePartial case impl of
      EmptySelection r -> r

  -- Convert data to array
  let dataArray = Array.fromFoldable foldableData

  -- Create a pending selection with the data
  let
    pendingSelection = Selection $ PendingSelection
      { parentElements: parentElements
      , pendingData: dataArray
      , indices: Just (Array.range 0 (Array.length dataArray - 1))
      , document: doc
      }

  -- Append elements with attributes
  append elemType attrs pendingSelection

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Apply per-datum attributes to a bound selection
-- |
-- | Takes a function that generates attributes for each datum,
-- | and applies those attributes to the corresponding elements.
applyPerDatumAttrs
  :: forall datum m state
   . MonadEffect m
  => (datum -> Array (Attribute datum))
  -> Selection state Element datum
  -> m (Selection state Element datum)
applyPerDatumAttrs mkAttrs (Selection impl) = liftEffect do
  case impl of
    BoundSelection { elements, data: datumArray } -> do
      -- Apply attributes to each (element, datum) pair
      let paired = Array.zipWith Tuple datumArray elements
      paired # traverseWithIndex_ \index (Tuple datum element) -> do
        let attrs = mkAttrs datum
        applyAttributes element datum index attrs
      -- Return the selection unchanged
      pure $ Selection impl
    _ -> pure $ Selection impl -- Non-bound selections: no-op

getDocument :: forall parent datum. SelectionImpl parent datum -> Effect Document
getDocument (EmptySelection { document: doc }) = pure doc
getDocument (BoundSelection { document: doc }) = pure doc
getDocument (PendingSelection { document: doc }) = pure doc
getDocument (ExitingSelection { document: doc }) = pure doc

-- | Create an element with the appropriate namespace
-- | Uses the element's rendering context to determine namespace
createElementWithNS :: ElementType -> Document -> Effect Element
createElementWithNS elemType doc =
  case elementContext elemType of
    SVGContext ->
      -- SVG elements need the SVG namespace
      Document.createElementNS (Just "http://www.w3.org/2000/svg") (elementTypeToString elemType) doc
    HTMLContext ->
      -- HTML elements use default namespace
      Document.createElement (elementTypeToString elemType) doc

-- | Query all matching elements within parent elements
-- | Uses web-dom library functions instead of custom FFI
querySelectorAllElements :: String -> Array Element -> Effect (Array Element)
querySelectorAllElements selector parents = do
  -- Convert each parent to ParentNode and query
  nodeArrays <- parents # traverse \parent -> do
    let parentNode = toParentNode parent
    nodeList <- querySelectorAll (QuerySelector selector) parentNode
    nodes <- NodeList.toArray nodeList
    -- Filter and convert Nodes to Elements
    pure $ Array.mapMaybe fromNode nodes
  -- Flatten the array of arrays
  pure $ Array.concat nodeArrays

-- | Convert HighlightClass to Int for FFI
-- | Must match the constants in FFI.js: HC_PRIMARY=0, HC_RELATED=1, HC_DIMMED=2, HC_NEUTRAL=3, HC_UPSTREAM=4, HC_DOWNSTREAM=5
highlightClassToInt :: HighlightClass -> Int
highlightClassToInt Primary = 0
highlightClassToInt Related = 1
highlightClassToInt Dimmed = 2
highlightClassToInt Neutral = 3
highlightClassToInt Upstream = 4
highlightClassToInt Downstream = 5

-- | Convert TooltipTrigger to Int for FFI
-- | Must match the constants in FFI.js: TT_ON_HOVER=0, TT_WHEN_PRIMARY=1, TT_WHEN_RELATED=2
tooltipTriggerToInt :: TooltipTrigger -> Int
tooltipTriggerToInt OnHover = 0
tooltipTriggerToInt WhenPrimary = 1
tooltipTriggerToInt WhenRelated = 2

-- | Apply a behavior to a single DOM element
-- |
-- | This is the core function that attaches D3 behaviors to elements.
-- | Used by both `on` (for Selection-based API) and `renderTree` (for TreeAPI).
applyBehaviorToElement :: forall datum. Behavior datum -> Element -> Effect Unit
applyBehaviorToElement (Zoom (ZoomConfig { scaleExtent: ScaleExtent scaleMin scaleMax, targetSelector })) element =
  void $ BehaviorFFI.attachZoom_ element scaleMin scaleMax targetSelector
applyBehaviorToElement (Drag SimpleDrag) element =
  void $ BehaviorFFI.attachSimpleDrag_ element unit
applyBehaviorToElement (Drag (SimulationDrag simId)) element =
  -- Look up simulation by ID in the global registry
  void $ BehaviorFFI.attachSimulationDragById_ element simId
applyBehaviorToElement (Drag (SimulationDragNested simId)) element =
  -- For nested datum structure (datum.node is the simulation node)
  void $ BehaviorFFI.attachSimulationDragNestedById_ element simId
applyBehaviorToElement (Click handler) element =
  void $ BehaviorFFI.attachClick_ element handler
applyBehaviorToElement (ClickWithDatum handler) element =
  void $ BehaviorFFI.attachClickWithDatum_ element handler
applyBehaviorToElement (MouseEnter handler) element =
  void $ BehaviorFFI.attachMouseEnter_ element handler
applyBehaviorToElement (MouseLeave handler) element =
  void $ BehaviorFFI.attachMouseLeave_ element handler
applyBehaviorToElement (Highlight { enter, leave }) element =
  void $ BehaviorFFI.attachHighlight_ element enter leave
applyBehaviorToElement (MouseMoveWithInfo handler) element =
  void $ BehaviorFFI.attachMouseMoveWithEvent_ element $ mkEffectFn2 \d evt ->
    handler
      { datum: d
      , clientX: toNumber $ clientX evt
      , clientY: toNumber $ clientY evt
      , pageX: toNumber $ pageX evt
      , pageY: toNumber $ pageY evt
      , offsetX: offsetX evt
      , offsetY: offsetY evt
      }
applyBehaviorToElement (MouseEnterWithInfo handler) element =
  void $ BehaviorFFI.attachMouseEnterWithEvent_ element $ mkEffectFn2 \d evt ->
    handler
      { datum: d
      , clientX: toNumber $ clientX evt
      , clientY: toNumber $ clientY evt
      , pageX: toNumber $ pageX evt
      , pageY: toNumber $ pageY evt
      , offsetX: offsetX evt
      , offsetY: offsetY evt
      }
applyBehaviorToElement (MouseLeaveWithInfo handler) element =
  void $ BehaviorFFI.attachMouseLeaveWithEvent_ element $ mkEffectFn2 \d evt ->
    handler
      { datum: d
      , clientX: toNumber $ clientX evt
      , clientY: toNumber $ clientY evt
      , pageX: toNumber $ pageX evt
      , pageY: toNumber $ pageY evt
      , offsetX: offsetX evt
      , offsetY: offsetY evt
      }
applyBehaviorToElement (MouseDown handler) element =
  void $ BehaviorFFI.attachMouseDown_ element handler
applyBehaviorToElement (MouseDownWithInfo handler) element =
  void $ BehaviorFFI.attachMouseDownWithEvent_ element $ mkEffectFn2 \d evt ->
    handler
      { datum: d
      , clientX: toNumber $ clientX evt
      , clientY: toNumber $ clientY evt
      , pageX: toNumber $ pageX evt
      , pageY: toNumber $ pageY evt
      , offsetX: offsetX evt
      , offsetY: offsetY evt
      }
applyBehaviorToElement (CoordinatedHighlight config) element =
  -- Convert classify function to return Int for FFI
  let
    classifyAsInt :: String -> _ -> Int
    classifyAsInt hoveredId datum = highlightClassToInt (config.classify hoveredId datum)

    -- Extract tooltip content function and trigger
    tooltipContentFn = config.tooltip <#> _.content
    tooltipTrigger = case config.tooltip of
      Just tc -> tooltipTriggerToInt tc.showWhen
      Nothing -> 0  -- Default, but won't be used since contentFn is null
  in
    void $ BehaviorFFI.attachCoordinatedHighlight_
      element
      config.identify
      classifyAsInt
      (toNullable config.group)
      (toNullable tooltipContentFn)
      tooltipTrigger

-- | Attach a behavior (zoom, drag, etc.) to a selection
-- |
-- | Works with any selection type - extracts elements and applies D3 behavior.
-- | Returns the selection unchanged to allow chaining.
-- |
-- | Example:
-- | ```purescript
-- | svg <- appendChild SVG [...] container
-- | zoomGroup <- appendChild Group [...] svg
-- | _ <- on (Drag defaultDrag) zoomGroup
-- | _ <- on (Zoom $ defaultZoom (ScaleExtent 0.5 4.0) ".zoom-group") svg
-- | ```
on :: forall state elem datum. Behavior datum -> Selection state elem datum -> Effect (Selection state elem datum)
on behavior selection@(Selection impl) = do
  -- Extract elements from the selection
  let elements = getElements impl

  -- Apply the behavior to each element using the top-level helper
  traverse_ (applyBehaviorToElement behavior) elements

  -- Return selection unchanged
  pure selection
  where
  -- Extract elements from any selection type
  getElements :: SelectionImpl elem datum -> Array Element
  getElements (EmptySelection { parentElements }) = parentElements
  getElements (BoundSelection { elements: els }) = els
  getElements (PendingSelection { parentElements }) = parentElements
  getElements (ExitingSelection { elements: els }) = els

-- | Attach a behavior with simulation access (for SimulationDrag)
-- |
-- | Apply attributes to an element
applyAttributes :: forall datum. Element -> datum -> Int -> Array (Attribute datum) -> Effect Unit
applyAttributes element datum index attrs =
  attrs # traverse_ \attr -> case attr of
    StaticAttr (AttributeName name) value ->
      if name == "textContent" then setTextContent_ (attributeValueToString value) element
      else Element.setAttribute name (attributeValueToString value) element

    DataAttr (AttributeName name) _src f ->
      let
        val = attributeValueToString (f datum)
      in
        if name == "textContent" then setTextContent_ val element
        else Element.setAttribute name val element

    IndexedAttr (AttributeName name) _src f ->
      let
        val = attributeValueToString (f datum index)
      in
        if name == "textContent" then setTextContent_ val element
        else Element.setAttribute name val element

    AnimatedAttr _ ->
      pure unit -- AnimatedAttr handled separately via animation system

    AnimatedCompound _ ->
      pure unit -- AnimatedCompound handled separately via animation system

attributeValueToString :: AttributeValue -> String
attributeValueToString (StringValue s) = s
attributeValueToString (NumberValue n) = show n
attributeValueToString (BooleanValue b) = show b

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

-- | FFI function to set textContent property
foreign import setTextContent_ :: String -> Element -> Effect Unit

-- ============================================================================
-- Transition Helpers for Tree API
-- ============================================================================

-- | Apply a transition with attributes to an array of elements
-- | Used for enter/update phases - elements are animated but not removed
-- |
-- | Supports staggered delays: if config.staggerDelay is set, each element's
-- | delay is: baseDelay + (index * staggerDelay)
applyTransitionToElements
  :: forall datum
   . TransitionConfig
  -> Array (Tuple Element datum) -- Elements with their data
  -> Array (Attribute datum) -- Attributes to transition to
  -> Effect Unit
applyTransitionToElements config elementDatumPairs attrs = do
  let Milliseconds duration = config.duration
  let baseDelay = maybe 0.0 unwrap config.delay
  let stagger = fromMaybe 0.0 config.staggerDelay
  let easingNullable = TransitionFFI.maybeEasingToNullable config.easing

  elementDatumPairs # traverseWithIndex_ \index (Tuple element datum) -> do
    -- Calculate effective delay: baseDelay + (index * staggerDelay)
    let effectiveDelay = baseDelay + (toNumber index * stagger)
    let delayNullable = toNullable (Just effectiveDelay)

    -- Create transition for this element
    transition <- TransitionFFI.createTransition_ duration delayNullable easingNullable element

    -- Apply each attribute to the transition (animates to target value)
    attrs # traverse_ \attr -> case attr of
      StaticAttr (AttributeName name) value ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

      DataAttr (AttributeName name) _src f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

      IndexedAttr (AttributeName name) _src f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

      AnimatedAttr _ ->
        pure unit -- AnimatedAttr handled separately via animation system

      AnimatedCompound _ ->
        pure unit -- AnimatedCompound handled separately via animation system

-- | Apply a transition to a single element with an explicit index
-- | Used when elements need per-element attrs but shared stagger timing
applyTransitionToSingleElement
  :: forall datum
   . TransitionConfig
  -> Int -- Explicit index for stagger calculation
  -> Element
  -> datum
  -> Array (Attribute datum)
  -> Effect Unit
applyTransitionToSingleElement config index element datum attrs = do
  let Milliseconds duration = config.duration
  let baseDelay = maybe 0.0 unwrap config.delay
  let stagger = fromMaybe 0.0 config.staggerDelay
  let effectiveDelay = baseDelay + (toNumber index * stagger)
  let delayNullable = toNullable (Just effectiveDelay)
  let easingNullable = TransitionFFI.maybeEasingToNullable config.easing

  -- Create transition for this element
  transition <- TransitionFFI.createTransition_ duration delayNullable easingNullable element

  -- Apply each attribute to the transition
  attrs # traverse_ \attr -> case attr of
    StaticAttr (AttributeName name) value ->
      TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

    DataAttr (AttributeName name) _src f ->
      TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

    IndexedAttr (AttributeName name) _src f ->
      TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

    AnimatedAttr _ ->
      pure unit -- AnimatedAttr handled separately via animation system

    AnimatedCompound _ ->
      pure unit -- AnimatedCompound handled separately via animation system

-- | Apply a transition with removal to an array of elements
-- | Used for exit phase - elements animate out then are removed from DOM
-- |
-- | Supports staggered delays: if config.staggerDelay is set, each element's
-- | delay is: baseDelay + (index * staggerDelay)
applyExitTransitionToElements
  :: forall datum
   . TransitionConfig
  -> Array (Tuple Element datum) -- Elements with their data
  -> Array (Attribute datum) -- Attributes to transition to before removal
  -> Effect Unit
applyExitTransitionToElements config elementDatumPairs attrs = do
  let Milliseconds duration = config.duration
  let baseDelay = maybe 0.0 unwrap config.delay
  let stagger = fromMaybe 0.0 config.staggerDelay
  let easingNullable = TransitionFFI.maybeEasingToNullable config.easing

  elementDatumPairs # traverseWithIndex_ \index (Tuple element datum) -> do
    -- Calculate effective delay: baseDelay + (index * staggerDelay)
    let effectiveDelay = baseDelay + (toNumber index * stagger)
    let delayNullable = toNullable (Just effectiveDelay)

    -- Create transition for this element
    transition <- TransitionFFI.createTransition_ duration delayNullable easingNullable element

    -- Apply each attribute to the transition
    attrs # traverse_ \attr -> case attr of
      StaticAttr (AttributeName name) value ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

      DataAttr (AttributeName name) _src f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

      IndexedAttr (AttributeName name) _src f ->
        TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

      AnimatedAttr _ ->
        pure unit -- AnimatedAttr handled separately via animation system

      AnimatedCompound _ ->
        pure unit -- AnimatedCompound handled separately via animation system

    -- Schedule removal after transition completes
    TransitionFFI.transitionRemove_ transition

-- | Extract element-datum pairs from an exiting selection
getExitingElementDatumPairs
  :: forall datum
   . Selection SExiting Element datum
  -> Array (Tuple Element datum)
getExitingElementDatumPairs (Selection impl) =
  let
    { elements, data: datumArray } = unsafePartial case impl of
      ExitingSelection r -> r
  in
    Array.zipWith Tuple elements datumArray

-- | Extract element-datum pairs from a bound selection
getBoundElementDatumPairs
  :: forall datum
   . Selection SBoundOwns Element datum
  -> Array (Tuple Element datum)
getBoundElementDatumPairs (Selection impl) =
  let
    { elements, data: datumArray } = unsafePartial case impl of
      BoundSelection r -> r
  in
    Array.zipWith Tuple elements datumArray

-- | Extract just the elements from a bound selection
getElementsFromBoundSelection
  :: forall datum
   . Selection SBoundOwns Element datum
  -> Array Element
getElementsFromBoundSelection (Selection impl) =
  unsafePartial case impl of
    BoundSelection r -> r.elements

-- ============================================================================
-- FFI Declarations (D3-specific data binding)
-- ============================================================================

-- | Get data bound to an element (D3-style __data__ property)
-- | Returns Nullable which we convert to Maybe using Data.Nullable.toMaybe
foreign import getElementData_ :: forall datum. Element -> Effect (Nullable datum)

-- | Set data on an element (D3-style __data__ property)
foreign import setElementData_ :: forall datum. datum -> Element -> Effect Unit

-- | JSON stringify for use as key function in joins
-- | This allows comparing objects by their JSON representation
-- | Used when types are erased and we can't rely on Eq instances
foreign import jsonStringify_ :: forall a. a -> String
