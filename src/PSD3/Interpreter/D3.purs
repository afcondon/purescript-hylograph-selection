module PSD3.Interpreter.D3
  ( D3v2Selection_
  , D3v2M
  , runD3v2M
  , reselectD3v2
  , queryAllD3v2
  , getElementsD3v2
  ) where

import Prelude

import Partial.Unsafe (unsafePartial)
import Data.Array as Array
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Data.Number as Number
import PSD3.Internal.Attribute (Attribute(..), AttributeName(..), AttributeValue(..), AnimatedValue(..), EasingType(..))
import PSD3.Internal.Capabilities.Selection (class SelectionM)
import PSD3.Internal.Capabilities.Transition (class TransitionM)
import PSD3.Internal.Selection.Operations as Ops
import PSD3.Internal.Selection.Operations (getTransitionContext)
import PSD3.Internal.Selection.Query as Query
import PSD3.Internal.Selection.Types (Selection(..), SelectionImpl(..), SBoundOwns, SEmpty, JoinResult(..))
import PSD3.Internal.Transition.FFI as TransitionFFI
import PSD3.Internal.Transition.Manager as Manager
import PSD3.Internal.Transition.Types as TransitionTypes
import PSD3.Transition.Coordinator as Coordinator
import Web.DOM.Element (Element)

-- | Selection type for D3v2 interpreter
-- |
-- | This is just a newtype wrapper around PSD3.Internal.Selection.Types.Selection
-- | to distinguish it from other interpreter's selection types.
newtype D3v2Selection_ (state :: Type) (parent :: Type) (datum :: Type)
  = D3v2Selection_ (Selection state parent datum)

-- | The D3v2 interpreter monad (without simulation state)
-- |
-- | Wraps Effect to allow for DOM manipulation.
newtype D3v2M a = D3v2M (Effect a)

derive newtype instance Functor D3v2M
derive newtype instance Apply D3v2M
derive newtype instance Applicative D3v2M
derive newtype instance Bind D3v2M
derive newtype instance Monad D3v2M
derive newtype instance MonadEffect D3v2M

-- | Run the D3v2 interpreter
runD3v2M :: D3v2M ~> Effect
runD3v2M (D3v2M eff) = eff

-- | SelectionM instance for D3v2 interpreter
-- |
-- | Delegates all operations to PSD3.Internal.Selection.Operations,
-- | which uses the phantom types with unsafePartial for safe pattern matching.
instance SelectionM D3v2Selection_ D3v2M where

  select selector = D3v2M do
    sel <- Ops.select selector
    pure $ D3v2Selection_ sel

  selectElement element = D3v2M do
    sel <- Ops.selectElement element
    pure $ D3v2Selection_ sel

  selectAll selector (D3v2Selection_ sel) = D3v2M do
    result <- Ops.selectAll selector sel
    pure $ D3v2Selection_ result

  openSelection (D3v2Selection_ sel) selector = D3v2M do
    result <- Ops.selectAll selector sel
    pure $ D3v2Selection_ result

  selectAllWithData selector (D3v2Selection_ sel) = D3v2M do
    result <- Ops.selectAllWithData selector sel
    pure $ D3v2Selection_ result

  renderData elemType foldableData selector (D3v2Selection_ emptySelection) enterAttrs updateAttrs exitAttrs = D3v2M do
    result <- Ops.renderData elemType foldableData selector emptySelection enterAttrs updateAttrs exitAttrs
    pure $ D3v2Selection_ result

  appendData elemType foldableData attrs (D3v2Selection_ emptySelection) = D3v2M do
    result <- Ops.appendData elemType foldableData attrs emptySelection
    pure $ D3v2Selection_ result

  joinData foldableData selector (D3v2Selection_ emptySelection) = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinData foldableData selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  joinDataWithKey foldableData keyFn selector (D3v2Selection_ emptySelection) = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  updateJoin (D3v2Selection_ emptySelection) _elemType foldableData keyFn selector = D3v2M do
    JoinResult { enter, update, exit } <- Ops.joinDataWithKey foldableData keyFn selector emptySelection
    pure $ JoinResult
      { enter: D3v2Selection_ enter
      , update: D3v2Selection_ update
      , exit: D3v2Selection_ exit
      }

  append elemType attrs (D3v2Selection_ pendingSelection) = D3v2M do
    result <- Ops.append elemType attrs pendingSelection
    pure $ D3v2Selection_ result

  setAttrs attrs (D3v2Selection_ boundSelection) = D3v2M do
    result <- Ops.setAttrs attrs boundSelection
    pure $ D3v2Selection_ result

  setAttrsExit attrs (D3v2Selection_ exitingSelection) = D3v2M do
    result <- Ops.setAttrsExit attrs exitingSelection
    pure $ D3v2Selection_ result

  remove (D3v2Selection_ exitingSelection) = D3v2M do
    Ops.remove exitingSelection

  clear selector = D3v2M do
    Ops.clear selector

  merge (D3v2Selection_ sel1) (D3v2Selection_ sel2) = D3v2M do
    result <- Ops.merge sel1 sel2
    pure $ D3v2Selection_ result

  appendChild elemType attrs (D3v2Selection_ emptySelection) = D3v2M do
    result <- Ops.appendChild elemType attrs emptySelection
    pure $ D3v2Selection_ result

  appendChildInheriting elemType attrs (D3v2Selection_ boundSelection) = D3v2M do
    result <- Ops.appendChildInheriting elemType attrs boundSelection
    pure $ D3v2Selection_ result

  on behavior (D3v2Selection_ selection) = D3v2M do
    result <- Ops.on behavior selection
    pure $ D3v2Selection_ result

  renderTree (D3v2Selection_ parent) tree = D3v2M do
    selectionsMap <- Ops.renderTree parent tree
    pure $ map D3v2Selection_ selectionsMap

-- | TransitionM instance for D3v2 interpreter
-- |
-- | Implements animated transitions using D3's transition engine.
-- | Applies transitions to each element in the bound selection.
instance TransitionM D3v2Selection_ D3v2M where

  withTransition config (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements, data, and indices from the bound selection
    let { elements, data: datumArray, indices } = unsafePartial case selection of
          Selection (BoundSelection r) -> r

    -- Check for pure transition context
    maybeCtx <- getTransitionContext

    case maybeCtx of
      -- Pure transition path using Manager
      Just ctx -> do
        let Milliseconds duration = config.duration
        let Milliseconds delay = case config.delay of
              Just d -> d
              Nothing -> Milliseconds 0.0
        let easing = case config.easing of
              Just e -> transitionEasingToAnimEasing e
              Nothing -> Linear

        -- Apply transition to each element
        let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
        paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
          let logicalIndex = case indices of
                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                Nothing -> arrayIndex

          -- Register each attribute as a transition
          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value -> do
              let targetValue = attributeValueToNumber value
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex name
                Nothing  -- Read from DOM
                (StaticAnimValue targetValue)
                { duration, easing, delay }
                (pure unit)
              pure unit

            DataAttr (AttributeName name) _src f -> do
              let targetValue = attributeValueToNumber (f datum)
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex name
                Nothing
                (StaticAnimValue targetValue)
                { duration, easing, delay }
                (pure unit)
              pure unit

            IndexedAttr (AttributeName name) _src f -> do
              let targetValue = attributeValueToNumber (f datum logicalIndex)
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex name
                Nothing
                (StaticAnimValue targetValue)
                { duration, easing, delay }
                (pure unit)
              pure unit

            AnimatedAttr rec -> do
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex
                (let (AttributeName n) = rec.name in n)
                rec.fromValue
                rec.toValue
                rec.config
                (pure unit)
              pure unit

        -- Ensure coordinator is running
        _ <- Coordinator.register ctx.coordinator
          { tick: Manager.toCoordinatorConsumer ctx.manager
          , onComplete: pure unit
          }
        Coordinator.start ctx.coordinator

      -- D3 transition path (legacy)
      Nothing -> do
        let Milliseconds duration = config.duration

        let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
        paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
          let logicalIndex = case indices of
                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                Nothing -> arrayIndex

          -- Create a D3 transition for this element
          transition <- TransitionFFI.createTransition_
            duration
            (TransitionFFI.maybeMillisecondsToNullable config.delay)
            (TransitionFFI.maybeEasingToNullable config.easing)
            element

          -- Apply each attribute to the transition
          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            DataAttr (AttributeName name) _src f ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

            IndexedAttr (AttributeName name) _src f ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum logicalIndex)) transition

            AnimatedAttr rec -> do
              let (AttributeName name) = rec.name
              let toValue = evalAnimValue rec.toValue datum logicalIndex
              TransitionFFI.transitionSetAttribute_ name (show toValue) transition

  withTransitionExit config (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements and data from the exiting selection
    let { elements, data: datumArray } = unsafePartial case selection of
          Selection (ExitingSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \index { datum, element } -> do
      -- Create a D3 transition for this element
      transition <- TransitionFFI.createTransition_
        duration
        (TransitionFFI.maybeMillisecondsToNullable config.delay)
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) _src f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) _src f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

        AnimatedAttr rec -> do
          let (AttributeName name) = rec.name
          let toValue = evalAnimValue rec.toValue datum index
          TransitionFFI.transitionSetAttribute_ name (show toValue) transition

      -- Remove element after transition completes (D3 pattern: transition.remove())
      TransitionFFI.transitionRemove_ transition

  withTransitionStaggered config delayFn (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements, data, and indices from the bound selection
    let { elements, data: datumArray, indices } = unsafePartial case selection of
          Selection (BoundSelection r) -> r

    -- Check for pure transition context
    maybeCtx <- getTransitionContext

    case maybeCtx of
      -- Pure transition path using Manager
      Just ctx -> do
        let Milliseconds duration = config.duration
        let easing = case config.easing of
              Just e -> transitionEasingToAnimEasing e
              Nothing -> Linear

        -- Apply transition to each element
        let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
        paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
          let logicalIndex = case indices of
                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                Nothing -> arrayIndex

          -- Compute the delay for this element using the delay function
          let Milliseconds elementDelay = delayFn datum logicalIndex

          -- Register each attribute as a transition with per-element delay
          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value -> do
              let targetValue = attributeValueToNumber value
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex name
                Nothing
                (StaticAnimValue targetValue)
                { duration, easing, delay: elementDelay }
                (pure unit)
              pure unit

            DataAttr (AttributeName name) _src f -> do
              let targetValue = attributeValueToNumber (f datum)
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex name
                Nothing
                (StaticAnimValue targetValue)
                { duration, easing, delay: elementDelay }
                (pure unit)
              pure unit

            IndexedAttr (AttributeName name) _src f -> do
              let targetValue = attributeValueToNumber (f datum logicalIndex)
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex name
                Nothing
                (StaticAnimValue targetValue)
                { duration, easing, delay: elementDelay }
                (pure unit)
              pure unit

            AnimatedAttr rec -> do
              -- Override delay with per-element stagger
              _ <- Manager.registerAnimatedAttr ctx.manager element datum logicalIndex
                (let (AttributeName n) = rec.name in n)
                rec.fromValue
                rec.toValue
                (rec.config { delay = elementDelay })
                (pure unit)
              pure unit

        -- Ensure coordinator is running
        _ <- Coordinator.register ctx.coordinator
          { tick: Manager.toCoordinatorConsumer ctx.manager
          , onComplete: pure unit
          }
        Coordinator.start ctx.coordinator

      -- D3 transition path (legacy)
      Nothing -> do
        let Milliseconds duration = config.duration

        let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
        paired # traverseWithIndex_ \arrayIndex { datum, element } -> do
          let logicalIndex = case indices of
                Just indexArray -> unsafePartial $ Array.unsafeIndex indexArray arrayIndex
                Nothing -> arrayIndex

          let Milliseconds elementDelay = delayFn datum logicalIndex

          transition <- TransitionFFI.createTransition_
            duration
            (toNullable (Just elementDelay))
            (TransitionFFI.maybeEasingToNullable config.easing)
            element

          attrs # traverse_ \attr -> case attr of
            StaticAttr (AttributeName name) value ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

            DataAttr (AttributeName name) _src f ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

            IndexedAttr (AttributeName name) _src f ->
              TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum logicalIndex)) transition

            AnimatedAttr rec -> do
              let (AttributeName name) = rec.name
              let toValue = evalAnimValue rec.toValue datum logicalIndex
              TransitionFFI.transitionSetAttribute_ name (show toValue) transition

  withTransitionExitStaggered config delayFn (D3v2Selection_ selection) attrs = D3v2M do
    -- Extract elements and data from the exiting selection
    let { elements, data: datumArray } = unsafePartial case selection of
          Selection (ExitingSelection r) -> r

    -- Get transition configuration
    let Milliseconds duration = config.duration

    -- Apply transition to each element with its corresponding datum and index
    let paired = Array.zipWith (\d e -> {datum: d, element: e}) datumArray elements
    paired # traverseWithIndex_ \index { datum, element } -> do
      -- Compute the delay for this element using the delay function
      let Milliseconds elementDelay = delayFn datum index

      -- Create a D3 transition for this element with computed delay
      transition <- TransitionFFI.createTransition_
        duration
        (toNullable (Just elementDelay))
        (TransitionFFI.maybeEasingToNullable config.easing)
        element

      -- Apply each attribute to the transition
      attrs # traverse_ \attr -> case attr of
        StaticAttr (AttributeName name) value ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString value) transition

        DataAttr (AttributeName name) _src f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum)) transition

        IndexedAttr (AttributeName name) _src f ->
          TransitionFFI.transitionSetAttribute_ name (attributeValueToString (f datum index)) transition

        AnimatedAttr rec -> do
          let (AttributeName name) = rec.name
          let toValue = evalAnimValue rec.toValue datum index
          TransitionFFI.transitionSetAttribute_ name (show toValue) transition

      -- Remove element after transition completes (D3 pattern: transition.remove())
      TransitionFFI.transitionRemove_ transition

-- Helper function to evaluate AnimatedValue
evalAnimValue :: forall datum. AnimatedValue datum -> datum -> Int -> Number
evalAnimValue (StaticAnimValue n) _ _ = n
evalAnimValue (DataAnimValue f) d _ = f d
evalAnimValue (IndexedAnimValue f) d i = f d i

-- Helper function to convert AttributeValue to String
attributeValueToString :: AttributeValue -> String
attributeValueToString (StringValue s) = s
attributeValueToString (NumberValue n) = show n
attributeValueToString (BooleanValue b) = show b

-- | Convert an AttributeValue to a Number (for pure transitions)
-- | Strings are parsed as numbers, booleans become 0.0/1.0
attributeValueToNumber :: AttributeValue -> Number
attributeValueToNumber (NumberValue n) = n
attributeValueToNumber (StringValue s) = parseNumber s
attributeValueToNumber (BooleanValue true) = 1.0
attributeValueToNumber (BooleanValue false) = 0.0

-- | Parse a string to a number, returning 0.0 on failure
parseNumber :: String -> Number
parseNumber s = case Number.fromString s of
  Just n -> n
  Nothing -> 0.0

-- | Convert from Transition.Types Easing to Attribute EasingType
transitionEasingToAnimEasing :: TransitionTypes.Easing -> EasingType
transitionEasingToAnimEasing TransitionTypes.Linear = Linear
transitionEasingToAnimEasing TransitionTypes.QuadIn = QuadIn
transitionEasingToAnimEasing TransitionTypes.QuadOut = QuadOut
transitionEasingToAnimEasing TransitionTypes.QuadInOut = QuadInOut
transitionEasingToAnimEasing TransitionTypes.CubicIn = CubicIn
transitionEasingToAnimEasing TransitionTypes.CubicOut = CubicOut
transitionEasingToAnimEasing TransitionTypes.CubicInOut = CubicInOut
transitionEasingToAnimEasing TransitionTypes.SinIn = SinIn
transitionEasingToAnimEasing TransitionTypes.SinOut = SinOut
transitionEasingToAnimEasing TransitionTypes.SinInOut = SinInOut
transitionEasingToAnimEasing TransitionTypes.ExpIn = ExpIn
transitionEasingToAnimEasing TransitionTypes.ExpOut = ExpOut
transitionEasingToAnimEasing TransitionTypes.ExpInOut = ExpInOut
transitionEasingToAnimEasing TransitionTypes.ElasticOut = ElasticOut
transitionEasingToAnimEasing TransitionTypes.BounceOut = BounceOut
-- Fallback for other easing types (Quad, Cubic, Sin, Exp, Circle, Elastic, Bounce without In/Out suffix)
transitionEasingToAnimEasing _ = Linear

-- | Helper function for reselecting from D3v2 renderTree results
-- |
-- | This wraps the `reselect` function from Operations to work with D3v2Selection_ newtype.
reselectD3v2
  :: forall datum datumOut
   . String
  -> Map.Map String (D3v2Selection_ SBoundOwns Element datum)
  -> Effect (D3v2Selection_ SEmpty Element datumOut)
reselectD3v2 name selectionsMap = do
  -- Unwrap D3v2Selection_ to Selection
  let unwrappedMap = map (\(D3v2Selection_ sel) -> sel) selectionsMap
  -- Call reselect
  result <- Ops.reselect name unwrappedMap
  -- Wrap result back in D3v2Selection_
  pure $ D3v2Selection_ result

-- | Query using CSS selector across all named selections
-- |
-- | Like `queryAll` but works with D3v2Selection_ wrapper.
-- | Properly unwraps the newtype before querying.
queryAllD3v2
  :: forall datum datumOut
   . String  -- ^ CSS selector
  -> Map.Map String (D3v2Selection_ SBoundOwns Element datum)
  -> Effect (D3v2Selection_ SEmpty Element datumOut)
queryAllD3v2 selector selectionsMap = do
  -- Unwrap D3v2Selection_ to Selection - CRITICAL!
  let unwrappedMap = map (\(D3v2Selection_ sel) -> sel) selectionsMap
  -- Call queryAll from Query module
  result <- Query.queryAll selector unwrappedMap
  -- Wrap result back in D3v2Selection_
  pure $ D3v2Selection_ result

-- | Extract DOM elements from a D3v2Selection_
-- |
-- | Useful for attaching behaviors that need raw DOM elements,
-- | such as drag handlers for force simulations.
getElementsD3v2 :: forall state parent datum. D3v2Selection_ state parent datum -> Array Element
getElementsD3v2 (D3v2Selection_ sel) = Query.toArray sel
