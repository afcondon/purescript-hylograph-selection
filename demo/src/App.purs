-- | Hylograph Interactive Guide
-- |
-- | An interactive explorer showing HATS visualizations alongside their
-- | MetaHATS structural diagrams.
module App where

import Prelude

import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import PrismJS (highlightAll)
import Hylograph.HATS (Tree)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Interaction.Zoom (attachZoomNative)
import Web.DOM.ParentNode (querySelector, QuerySelector(..))
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener) as ET
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode, toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, clientX, fromEvent)

import Examples.GroupedBarChart as GroupedBar
import Examples.BoardGames as BoardGames
import Examples.MapDiagram as MapDiagram
import Examples.RailroadDiagram as Railroad
import Examples.MetaHATS as Meta
import TreePretty (prettyTree)

-- =============================================================================
-- Types
-- =============================================================================

data Visualization
  = GroupedBarChart
  | ChessBoardFlat
  | ScrabbleBoardNested
  | MapDiagram
  | RailroadJSON
  | RailroadExpr

derive instance eqVisualization :: Eq Visualization

type State =
  { selected :: Visualization
  , hatsCode :: String
  , splitRatio :: Number    -- 0.0 to 1.0, left panel width ratio
  , isDragging :: Boolean
  }

data Action
  = Initialize
  | SelectViz Visualization
  | StartDrag MouseEvent
  | DragMove MouseEvent
  | EndDrag

-- =============================================================================
-- Component
-- =============================================================================

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ ->
        { selected: GroupedBarChart
        , hatsCode: ""
        , splitRatio: 0.33  -- 1:2 ratio by default
        , isDragging: false
        }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- | Convert split ratio to CSS grid-template-columns value
gridColumns :: Number -> String
gridColumns ratio =
  let
    leftPercent = ratio * 100.0
    rightPercent = (1.0 - ratio) * 100.0
  in
    show leftPercent <> "% 6px " <> show rightPercent <> "%"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (H.ClassName "app") ]
    [ -- Header with selector
      HH.header
        [ HP.class_ (H.ClassName "app-header") ]
        [ HH.h1_
            [ HH.img [ HP.src "hylo-twins.png", HP.alt "Hylograph logo" ]
            , HH.text "Hylograph"
            ]
        , HH.p
            [ HP.class_ (H.ClassName "subtitle") ]
            [ HH.text "Interactive HATS Explorer" ]
        , renderSelector state.selected
        ]
    -- Main content: Visualization (left) + divider + overlaid AST/meta (right)
    , HH.main
        [ HP.class_ (H.ClassName $ "app-main" <> if state.isDragging then " dragging" else "")
        , HP.style $ "grid-template-columns: " <> gridColumns state.splitRatio
        ]
        [ -- Left panel: rendered visualization
          HH.div
            [ HP.class_ (H.ClassName "panel viz-panel") ]
            [ HH.h2_ [ HH.text "Visualization" ]
            , HH.div
                [ HP.id "viz-container"
                , HP.class_ (H.ClassName "viz-container")
                ] []
            ]
        , -- Draggable divider
          HH.div
            [ HP.class_ (H.ClassName "divider")
            , HE.onMouseDown StartDrag
            ]
            []
        , -- Right panel: AST code (watermark) with MetaHATS overlaid
          HH.div
            [ HP.class_ (H.ClassName "panel overlay-panel") ]
            [ HH.h2_ [ HH.text "HATS Structure" ]
            , HH.div
                [ HP.id "overlay-container"
                , HP.class_ (H.ClassName "overlay-container")
                ]
                [ -- AST code layer (full background, syntax highlighted)
                  HH.div
                    [ HP.id "ast-layer"
                    , HP.class_ (H.ClassName "ast-layer")
                    ]
                    [ HH.pre_
                        [ HH.code
                            [ HP.class_ (H.ClassName "language-hats") ]
                            [ HH.text state.hatsCode ]
                        ]
                    ]
                , -- Meta layer (on top, interactive - zoom/pan with mouse wheel and drag)
                  HH.div
                    [ HP.id "meta-layer"
                    , HP.class_ (H.ClassName "meta-layer")
                    ] []
                ]
            ]
        ]
    -- Description
    , HH.footer
        [ HP.class_ (H.ClassName "app-footer") ]
        [ renderDescription state.selected ]
    ]

renderSelector :: forall m. Visualization -> H.ComponentHTML Action () m
renderSelector selected =
  HH.div
    [ HP.class_ (H.ClassName "selector") ]
    [ renderOption GroupedBarChart "Grouped Bar Chart" selected
    , renderOption ChessBoardFlat "Chess (Flat ×64)" selected
    , renderOption ScrabbleBoardNested "Scrabble (Nested ×15×15)" selected
    , renderOption MapDiagram "Map Diagram" selected
    , renderOption RailroadJSON "Railroad: JSON" selected
    , renderOption RailroadExpr "Railroad: Expression" selected
    ]

renderOption :: forall m. Visualization -> String -> Visualization -> H.ComponentHTML Action () m
renderOption viz label selected =
  HH.button
    [ HP.class_ (H.ClassName if viz == selected then "option selected" else "option")
    , HE.onClick \_ -> SelectViz viz
    ]
    [ HH.text label ]

renderDescription :: forall m. Visualization -> H.ComponentHTML Action () m
renderDescription = case _ of
  GroupedBarChart ->
    HH.p_
      [ HH.text "Nested folds: "
      , HH.code_ [ HH.text "forEach states" ]
      , HH.text " containing "
      , HH.code_ [ HH.text "forEach ageGroups" ]
      , HH.text ". Each state has bars for each age group."
      ]
  ChessBoardFlat ->
    HH.p_
      [ HH.text "Flat structure: single "
      , HH.code_ [ HH.text "forEach" ]
      , HH.text " over 64 cells. Simple but no structural hierarchy visible in MetaHATS."
      ]
  ScrabbleBoardNested ->
    HH.p_
      [ HH.text "Nested structure: "
      , HH.code_ [ HH.text "forEach rows" ]
      , HH.text " containing "
      , HH.code_ [ HH.text "forEach cells" ]
      , HH.text ". MetaHATS shows ×15 rows × ×15 cells = ×225 total elements."
      ]
  MapDiagram ->
    HH.p_
      [ HH.text "Three parallel folds over the same Map: "
      , HH.code_ [ HH.text "keys" ]
      , HH.text ", "
      , HH.code_ [ HH.text "values" ]
      , HH.text ", and "
      , HH.code_ [ HH.text "arrows" ]
      , HH.text ". Shows function diagram pattern."
      ]
  RailroadJSON ->
    HH.p_
      [ HH.text "Grammar as data: "
      , HH.code_ [ HH.text "forEach alternatives" ]
      , HH.text " for Choice, "
      , HH.code_ [ HH.text "forEach sequence" ]
      , HH.text " for Sequence. JSON value has 7 alternatives."
      ]
  RailroadExpr ->
    HH.p_
      [ HH.text "Recursive grammar structure: Sequence containing Repeat containing Choice. "
      , HH.text "MetaHATS reveals the nesting with multiplicative counts."
      ]

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    renderVisualization state.selected

  SelectViz viz -> do
    H.modify_ _ { selected = viz }
    renderVisualization viz

  StartDrag _ -> do
    H.modify_ _ { isDragging = true }
    -- Subscribe to document-level mouse events for drag tracking
    doc <- liftEffect $ window >>= document
    let target = toEventTarget doc
    { emitter, listener } <- H.liftEffect HS.create

    -- Create event listeners
    moveListener <- liftEffect $ ET.eventListener \e ->
      for_ (fromEvent e) \mouseEvent ->
        HS.notify listener (DragMove mouseEvent)

    upListener <- liftEffect $ ET.eventListener \_ ->
      HS.notify listener EndDrag

    -- Attach listeners
    liftEffect do
      ET.addEventListener (EventType "mousemove") moveListener false target
      ET.addEventListener (EventType "mouseup") upListener false target

    -- Subscribe and store cleanup
    void $ H.subscribe emitter

  DragMove me -> do
    state <- H.get
    when state.isDragging do
      -- Get container width and calculate new ratio
      mContainer <- liftEffect $ do
        doc <- window >>= document
        querySelector (QuerySelector ".app-main") (toParentNode doc)
      for_ mContainer \container -> do
        containerWidth <- liftEffect $ getElementWidth container
        let
          mouseX = Int.toNumber (clientX me)
          -- Account for some padding/offset
          newRatio = clampNum 0.15 0.85 (mouseX / containerWidth)
        H.modify_ _ { splitRatio = newRatio }

  EndDrag -> do
    H.modify_ _ { isDragging = false }
    -- Note: Event listeners will be cleaned up when subscription ends

-- | Clamp a value between min and max
clampNum :: Number -> Number -> Number -> Number
clampNum minVal maxVal val = max minVal (min maxVal val)

renderVisualization :: forall output m. MonadAff m => Visualization -> H.HalogenM State Action () output m Unit
renderVisualization viz = do
  let { tree, meta } = getTreesForViz viz
  let code = prettyTree tree
  H.modify_ _ { hatsCode = code }
  liftEffect do
    -- Clear containers before re-rendering
    clearElement "#viz-container"
    clearElement "#meta-layer"
    -- Render viz on left, meta overlaid on right
    _ <- rerender "#viz-container" tree
    _ <- rerender "#meta-layer" meta
    -- Attach native zoom to the meta diagram SVG
    attachZoomToMetaSVG
    -- Trigger syntax highlighting
    highlightAll
    pure unit

-- | Attach native zoom behavior to the MetaHATS SVG diagram
attachZoomToMetaSVG :: Effect Unit
attachZoomToMetaSVG = do
  doc <- window >>= document
  mSvg <- querySelector (QuerySelector "#meta-layer .meta-hats-diagram") (toParentNode doc)
  case mSvg of
    Just svg -> do
      -- Attach zoom: scale 0.25x to 4x, target the .zoom-group inside
      _ <- attachZoomNative svg 0.25 4.0 ".zoom-group"
      pure unit
    Nothing ->
      pure unit

foreign import clearElement :: String -> Effect Unit
foreign import getElementWidth :: forall e. e -> Effect Number

-- =============================================================================
-- Visualization Data
-- =============================================================================

getTreesForViz :: Visualization -> { tree :: Tree, meta :: Tree }
getTreesForViz = case _ of
  GroupedBarChart ->
    let tree = GroupedBar.groupedBarChartTree GroupedBar.defaultDims GroupedBar.sampleData
    in { tree, meta: Meta.interpretMeta Meta.defaultMetaConfig tree }

  ChessBoardFlat ->
    let tree = BoardGames.chessBoardTree BoardGames.defaultChessConfig BoardGames.chessData
    in { tree, meta: Meta.interpretMeta Meta.defaultMetaConfig tree }

  ScrabbleBoardNested ->
    let tree = BoardGames.scrabbleBoardTreeNested BoardGames.defaultScrabbleConfig BoardGames.scrabbleDataNested
    in { tree, meta: Meta.interpretMeta Meta.defaultMetaConfig tree }

  MapDiagram ->
    let tree = MapDiagram.mapDiagramTree MapDiagram.defaultMapConfig MapDiagram.sampleUserAges
    in { tree, meta: Meta.interpretMeta Meta.defaultMetaConfig tree }

  RailroadJSON ->
    let tree = Railroad.railroadTree Railroad.defaultRailroadConfig Railroad.sampleJsonValue
    in { tree, meta: Meta.interpretMeta Meta.defaultMetaConfig tree }

  RailroadExpr ->
    let tree = Railroad.railroadTree Railroad.defaultRailroadConfig Railroad.sampleExpression
    in { tree, meta: Meta.interpretMeta Meta.defaultMetaConfig tree }
