-- | App — Chapter-based demo for the hylographic fold
module App where

import Prelude

import Data.Array (mapWithIndex)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Hylograph.HATS.InterpreterTick (rerender)
import Chapters.Chapter0 as Ch0
import Chapters.Chapter1 as Ch1
import Chapters.Chapter2 as Ch2
import Chapters.Chapter3 as Ch3
import Chapters.Chapter4 as Ch4
import Examples.MetaHATS as Meta

-- =============================================================================
-- FFI
-- =============================================================================

foreign import clearElement :: String -> Effect Unit

-- =============================================================================
-- Types
-- =============================================================================

type State =
  { selectedTab :: Ch1.FoldExample
  , selectedStage :: Ch3.Stage
  , selectedExample :: Ch4.Example
  }

data Action
  = Initialize
  | SelectTab Ch1.FoldExample
  | SelectStage Ch3.Stage
  | SelectExample Ch4.Example

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { selectedTab: Ch1.ExHTML, selectedStage: Ch3.Stage1, selectedExample: Ch4.ExBars }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- =============================================================================
-- Render
-- =============================================================================

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ renderNav
    , renderChapter0
    , renderChapter1 state
    , renderChapter2
    , renderChapter3 state
    , renderChapter4 state
    ]

renderNav :: forall w i. HH.HTML w i
renderNav =
  HH.nav [ HP.class_ (HH.ClassName "chapter-nav") ]
    [ navDot "#ch0"
    , navDot "#ch1"
    , navDot "#ch2"
    , navDot "#ch3"
    , navDot "#ch4"
    ]

navDot :: forall w i. String -> HH.HTML w i
navDot href =
  HH.a [ HP.href href ] []

-- =============================================================================
-- Chapter 0: Join vs Fold
-- =============================================================================

renderChapter0 :: forall w i. HH.HTML w i
renderChapter0 =
  HH.section
    [ HP.class_ (HH.ClassName "chapter")
    , HP.id "ch0"
    ]
    [ HH.div [ HP.class_ (HH.ClassName "chapter-header") ]
        [ HH.img [ HP.src "hylo-twins.png", HP.class_ (HH.ClassName "logo") ]
        , HH.div_
            [ HH.div [ HP.class_ (HH.ClassName "chapter-number") ]
                [ HH.text "Hylograph" ]
            , HH.h1_ [ HH.text "The Hylographic Fold" ]
            ]
        ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "D3\x2019s join binds data to the DOM one\x2011to\x2011one: an array of values produces an array of elements. It\x2019s powerful, but it\x2019s a special case." ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "The hylographic fold generalizes both sides. The left side can be any structure you can take apart \x2014 an array, a tree, a map. The right side can be any structure you can assemble. The fold is the bridge between them." ]

    -- Join diagram (D3's model)
    , HH.div [ HP.class_ (HH.ClassName "diagram-section") ]
        [ HH.div [ HP.class_ (HH.ClassName "diagram-label") ]
            [ HH.text "The Join" ]
        , HH.div [ HP.class_ (HH.ClassName "diagram-sublabel") ]
            [ HH.text "Array \x2192 Array, one\x2011to\x2011one" ]
        , HH.div [ HP.class_ (HH.ClassName "diagram-box"), HP.id "ch0-join" ] []
        ]

    -- Fold diagrams (Hylograph's generalization)
    , HH.div [ HP.class_ (HH.ClassName "diagram-section") ]
        [ HH.div [ HP.class_ (HH.ClassName "diagram-label") ]
            [ HH.text "The Fold" ]
        , HH.div [ HP.class_ (HH.ClassName "diagram-sublabel") ]
            [ HH.text "Any structure in, any structure out" ]
        , HH.div [ HP.class_ (HH.ClassName "diagram-row") ]
            (mapWithIndex (\i fd ->
              HH.div [ HP.class_ (HH.ClassName "diagram-cell") ]
                [ HH.div [ HP.class_ (HH.ClassName "diagram-cell-label") ]
                    [ HH.text fd.label ]
                , HH.div [ HP.class_ (HH.ClassName "diagram-box"), HP.id ("ch0-fold-" <> show i) ] []
                ]
            ) Ch0.foldDiagrams)
        ]
    ]

-- =============================================================================
-- Chapter 1: The Fold in Action
-- =============================================================================

renderChapter1 :: forall m. State -> H.ComponentHTML Action () m
renderChapter1 state =
  HH.section
    [ HP.class_ (HH.ClassName "chapter")
    , HP.id "ch1"
    ]
    [ HH.div [ HP.class_ (HH.ClassName "chapter-number") ]
        [ HH.text "Chapter 1" ]
    , HH.h1_ [ HH.text "The Fold" ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "The same five items, the same structural pattern, three different outputs. Switch tabs and watch the template change while the structure barely moves." ]

    , HH.div [ HP.class_ (HH.ClassName "hover-cta") ]
        [ HH.text "\x25C8  Hover the data, the template nodes, and the output to see how they connect  \x25C8" ]

    -- Tabs
    , HH.div [ HP.class_ (HH.ClassName "tabs") ]
        [ tab state.selectedTab Ch1.ExHTML "HTML"
        , tab state.selectedTab Ch1.ExSVG "SVG"
        , tab state.selectedTab Ch1.ExMarkdown "Markdown"
        ]

    -- Equation: data + tree = output
    , HH.div [ HP.class_ (HH.ClassName "equation-row") ]
        [ -- Data
          HH.div [ HP.class_ (HH.ClassName "eq-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "Data" ]
            , HH.div [ HP.class_ (HH.ClassName "eq-content"), HP.id "ch1-data" ] []
            ]
        , HH.div [ HP.class_ (HH.ClassName "eq-op") ] [ HH.text "+" ]
        -- Tree (metatree)
        , HH.div [ HP.class_ (HH.ClassName "eq-panel eq-wide") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "Template" ]
            , HH.div [ HP.class_ (HH.ClassName "eq-content"), HP.id "ch1-tree" ] []
            ]
        , HH.div [ HP.class_ (HH.ClassName "eq-op") ] [ HH.text "=" ]
        -- Output
        , HH.div [ HP.class_ (HH.ClassName "eq-panel eq-wide") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "Output" ]
            , HH.div [ HP.class_ (HH.ClassName "eq-content"), HP.id "ch1-output" ] []
            ]
        ]
    ]

-- =============================================================================
-- Chapter 2: Any Foldable, Any Structure
-- =============================================================================

renderChapter2 :: forall w i. HH.HTML w i
renderChapter2 =
  HH.section
    [ HP.class_ (HH.ClassName "chapter")
    , HP.id "ch2"
    ]
    [ HH.div [ HP.class_ (HH.ClassName "chapter-number") ]
        [ HH.text "Chapter 2" ]
    , HH.h1_ [ HH.text "Any Structure" ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "The flat fold loses the grouping \x2014 64 cells, all siblings. The nested fold preserves it: eight rows, each containing eight cells. The output is identical. The structure is not." ]
    , HH.div [ HP.class_ (HH.ClassName "hover-cta") ]
        [ HH.text "\x25C8  Hover the boards to see the difference: the nested fold remembers how the data was organised  \x25C8" ]

    , HH.div [ HP.class_ (HH.ClassName "side-by-side") ]
        [ -- Flat fold
          HH.div [ HP.class_ (HH.ClassName "side-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "side-label") ]
                [ HH.text "Flat Fold" ]
            , HH.div [ HP.class_ (HH.ClassName "side-sublabel") ]
                [ HH.text "forEach over 64 cells" ]
            , HH.div [ HP.class_ (HH.ClassName "side-content") ]
                [ HH.div [ HP.class_ (HH.ClassName "board-box"), HP.id "ch2-flat-board" ] []
                , HH.div [ HP.class_ (HH.ClassName "meta-box"), HP.id "ch2-flat-meta" ] []
                ]
            ]
        -- Nested fold
        , HH.div [ HP.class_ (HH.ClassName "side-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "side-label") ]
                [ HH.text "Nested Fold" ]
            , HH.div [ HP.class_ (HH.ClassName "side-sublabel") ]
                [ HH.text "forEach rows \x2192 forEach cells" ]
            , HH.div [ HP.class_ (HH.ClassName "side-content") ]
                [ HH.div [ HP.class_ (HH.ClassName "board-box"), HP.id "ch2-nested-board" ] []
                , HH.div [ HP.class_ (HH.ClassName "meta-box"), HP.id "ch2-nested-meta" ] []
                ]
            ]
        ]
    ]

-- =============================================================================
-- Chapter 3: Composing Fragments
-- =============================================================================

renderChapter3 :: forall m. State -> H.ComponentHTML Action () m
renderChapter3 state =
  HH.section
    [ HP.class_ (HH.ClassName "chapter")
    , HP.id "ch3"
    ]
    [ HH.div [ HP.class_ (HH.ClassName "chapter-number") ]
        [ HH.text "Chapter 3" ]
    , HH.h1_ [ HH.text "Composing Fragments" ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "So far we\x2019ve looked at simple source data. Now: a Map, which lets us show "
        , HH.em_ [ HH.text "composing" ]
        , HH.text " different bits of visualisation together into a coherent whole."
        ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "A Map can be represented as a list of tuples \x2014 and in PureScript this representation is easily recoverable. To make a reusable visualiser for a Map we compose three individual folds over the same data: one extracts the keys for the left side, one extracts the values for the right, and a third uses both to draw the arrows." ]

    -- Stepper
    , HH.div [ HP.class_ (HH.ClassName "tabs") ]
        [ stageBtn state.selectedStage Ch3.Stage1 "1 \x2014 Keys"
        , stageBtn state.selectedStage Ch3.Stage2 "2 \x2014 + Values"
        , stageBtn state.selectedStage Ch3.Stage3 "3 \x2014 + Arrows"
        , stageBtn state.selectedStage Ch3.Stage4 "4 \x2014 + Chrome"
        ]

    -- Data + visualization + metatree
    , HH.div [ HP.class_ (HH.ClassName "equation-row") ]
        [ -- Data tuples
          HH.div [ HP.class_ (HH.ClassName "eq-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "Map as Tuples" ]
            , HH.div [ HP.class_ (HH.ClassName "eq-content"), HP.id "ch3-data" ] []
            ]
        -- Visualization
        , HH.div [ HP.class_ (HH.ClassName "eq-panel eq-wide") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "Visualization" ]
            , HH.div [ HP.class_ (HH.ClassName "eq-content"), HP.id "ch3-viz" ] []
            ]
        -- Structure
        , HH.div [ HP.class_ (HH.ClassName "eq-panel eq-wide") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "Structure" ]
            , HH.div [ HP.class_ (HH.ClassName "eq-content"), HP.id "ch3-meta" ] []
            ]
        ]
    ]

stageBtn :: forall m. Ch3.Stage -> Ch3.Stage -> String -> H.ComponentHTML Action () m
stageBtn current this label =
  HH.button
    [ HP.class_ (HH.ClassName (if sameStage current this then "tab selected" else "tab"))
    , HE.onClick \_ -> SelectStage this
    ]
    [ HH.text label ]

sameStage :: Ch3.Stage -> Ch3.Stage -> Boolean
sameStage Ch3.Stage1 Ch3.Stage1 = true
sameStage Ch3.Stage2 Ch3.Stage2 = true
sameStage Ch3.Stage3 Ch3.Stage3 = true
sameStage Ch3.Stage4 Ch3.Stage4 = true
sameStage _ _ = false

-- =============================================================================
-- Chapter 4: HATS Revealed
-- =============================================================================

renderChapter4 :: forall m. State -> H.ComponentHTML Action () m
renderChapter4 state =
  HH.section
    [ HP.class_ (HH.ClassName "chapter")
    , HP.id "ch4"
    ]
    [ HH.div [ HP.class_ (HH.ClassName "chapter-number") ]
        [ HH.text "Chapter 4" ]
    , HH.h1_ [ HH.text "HATS" ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "Now that you understand the structure of the transformation from data to visualisation, let\x2019s look at how we actually do this in code. The trees you\x2019ve been seeing are representations of a fully declarative abstract syntax tree we call "
        , HH.strong_ [ HH.text "HATS" ]
        , HH.text " \x2014 Hylomorphic Abstract Tree Syntax."
        ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "HATS is an embedded DSL with full access to PureScript. The tree is evaluated by an interpreter at runtime using the data you provide \x2014 but it\x2019s type-checked at compile time, so you get type safety across the entire pipeline. Here\x2019s what some simple HATS visualisations look like:" ]

    -- Example selector
    , HH.div [ HP.class_ (HH.ClassName "tabs") ]
        [ exBtn state.selectedExample Ch4.ExBars "Bar Chart"
        , exBtn state.selectedExample Ch4.ExDots "Dots"
        , exBtn state.selectedExample Ch4.ExComposed "Composed"
        ]

    -- Caption
    , HH.div [ HP.class_ (HH.ClassName "example-caption") ]
        [ HH.text (Ch4.exampleCaption state.selectedExample) ]

    -- Code + rendered output
    , HH.div [ HP.class_ (HH.ClassName "code-output-row") ]
        [ -- Code
          HH.div [ HP.class_ (HH.ClassName "code-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "HATS Code" ]
            , HH.pre [ HP.class_ (HH.ClassName "hats-code") ]
                [ HH.text (Ch4.exampleCode state.selectedExample) ]
            ]
        -- Rendered output
        , HH.div [ HP.class_ (HH.ClassName "output-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "eq-label") ] [ HH.text "Rendered" ]
            , HH.div [ HP.class_ (HH.ClassName "output-content"), HP.id "ch4-output" ] []
            ]
        ]
    ]

exBtn :: forall m. Ch4.Example -> Ch4.Example -> String -> H.ComponentHTML Action () m
exBtn current this label =
  HH.button
    [ HP.class_ (HH.ClassName (if sameExample current this then "tab selected" else "tab"))
    , HE.onClick \_ -> SelectExample this
    ]
    [ HH.text label ]

sameExample :: Ch4.Example -> Ch4.Example -> Boolean
sameExample Ch4.ExBars Ch4.ExBars = true
sameExample Ch4.ExDots Ch4.ExDots = true
sameExample Ch4.ExComposed Ch4.ExComposed = true
sameExample _ _ = false

-- =============================================================================
-- Chapter 1 helpers
-- =============================================================================

tab :: forall m. Ch1.FoldExample -> Ch1.FoldExample -> String -> H.ComponentHTML Action () m
tab current this label =
  HH.button
    [ HP.class_ (HH.ClassName (if sameTab current this then "tab selected" else "tab"))
    , HE.onClick \_ -> SelectTab this
    ]
    [ HH.text label ]

sameTab :: Ch1.FoldExample -> Ch1.FoldExample -> Boolean
sameTab Ch1.ExHTML Ch1.ExHTML = true
sameTab Ch1.ExSVG Ch1.ExSVG = true
sameTab Ch1.ExMarkdown Ch1.ExMarkdown = true
sameTab _ _ = false

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    H.liftEffect do
      renderChapter0Trees
      renderChapter1Trees state.selectedTab
      renderChapter2Trees
      renderChapter3Trees state.selectedStage
      renderChapter4Trees state.selectedExample

  SelectTab t -> do
    H.modify_ _ { selectedTab = t }
    H.liftEffect (renderChapter1Trees t)

  SelectStage s -> do
    H.modify_ _ { selectedStage = s }
    H.liftEffect (renderChapter3Trees s)

  SelectExample e -> do
    H.modify_ _ { selectedExample = e }
    H.liftEffect (renderChapter4Trees e)

-- =============================================================================
-- Tree rendering
-- =============================================================================

renderChapter0Trees :: Effect Unit
renderChapter0Trees = do
  clearElement "#ch0-join"
  _ <- rerender "#ch0-join" Ch0.joinDiagram
  -- Render each fold diagram
  let indexed = mapWithIndex (\i fd -> { i, tree: fd.tree }) Ch0.foldDiagrams
  for_ indexed \fd -> do
    let sel = "#ch0-fold-" <> show fd.i
    clearElement sel
    _ <- rerender sel fd.tree
    pure unit

renderChapter2Trees :: Effect Unit
renderChapter2Trees = do
  let flatMeta = Meta.interpretMeta Meta.defaultMetaConfig Ch2.flatBoardTree
  let nestedMeta = Meta.interpretMeta Meta.defaultMetaConfig Ch2.nestedBoardTree
  clearElement "#ch2-flat-board"
  clearElement "#ch2-flat-meta"
  clearElement "#ch2-nested-board"
  clearElement "#ch2-nested-meta"
  _ <- rerender "#ch2-flat-board" Ch2.flatBoardTree
  _ <- rerender "#ch2-flat-meta" flatMeta
  _ <- rerender "#ch2-nested-board" Ch2.nestedBoardTree
  _ <- rerender "#ch2-nested-meta" nestedMeta
  pure unit

renderChapter4Trees :: Ch4.Example -> Effect Unit
renderChapter4Trees ex = do
  clearElement "#ch4-output"
  _ <- rerender "#ch4-output" (Ch4.exampleTree ex)
  pure unit

renderChapter3Trees :: Ch3.Stage -> Effect Unit
renderChapter3Trees stage = do
  let vizTree = Ch3.stageTree stage
  let metaTree = Meta.interpretMeta Meta.defaultMetaConfig vizTree
  clearElement "#ch3-data"
  clearElement "#ch3-viz"
  clearElement "#ch3-meta"
  _ <- rerender "#ch3-data" Ch3.dataTree
  _ <- rerender "#ch3-viz" vizTree
  _ <- rerender "#ch3-meta" metaTree
  pure unit

renderChapter1Trees :: Ch1.FoldExample -> Effect Unit
renderChapter1Trees selectedTab = do
  let tmplTree = Ch1.templateDiagram selectedTab
  let outTree = Ch1.outputTree selectedTab
  clearElement "#ch1-data"
  clearElement "#ch1-tree"
  clearElement "#ch1-output"
  _ <- rerender "#ch1-data" Ch1.dataArrayTree
  _ <- rerender "#ch1-tree" tmplTree
  _ <- rerender "#ch1-output" outTree
  pure unit
