-- | App — Chapter-based demo for the hylographic fold
-- |
-- | Full-screen scrolling chapters that progressively reveal
-- | how the fold works, building from arrays to HATS to meta-visualisation.
module App where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hylograph.HATS.InterpreterTick (rerender)
import Examples.MetaHATS as Meta
import Chapters.Chapter1 as Ch1

-- =============================================================================
-- FFI
-- =============================================================================

foreign import clearElement :: String -> Effect Unit

-- =============================================================================
-- Types
-- =============================================================================

type State =
  { rendered :: Boolean
  }

data Action
  = Initialize

-- =============================================================================
-- Component
-- =============================================================================

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { rendered: false }
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
render _state =
  HH.div_
    [ renderNav
    , renderChapter1
    ]

renderNav :: forall w i. HH.HTML w i
renderNav =
  HH.nav [ HP.class_ (HH.ClassName "chapter-nav") ]
    [ navDot "#ch1"
    ]

navDot :: forall w i. String -> HH.HTML w i
navDot href =
  HH.a [ HP.href href ] []

-- =============================================================================
-- Chapter 1: The Fold
-- =============================================================================

renderChapter1 :: forall w. HH.HTML w Action
renderChapter1 =
  HH.section
    [ HP.class_ (HH.ClassName "chapter")
    , HP.id "ch1"
    ]
    [ HH.div [ HP.class_ (HH.ClassName "chapter-number") ]
        [ HH.text "Chapter 1" ]
    , HH.h1_ [ HH.text "The Fold" ]
    , HH.p [ HP.class_ (HH.ClassName "chapter-subtitle") ]
        [ HH.text "A fold takes something apart on one side and puts something together on the other. The same data can produce entirely different structures \x2014 an HTML list, an SVG diagram, a text summary. The fold doesn\x2019t care what it builds. It only needs to know how to take one step." ]
    , HH.div [ HP.class_ (HH.ClassName "viz-row") ]
        [ -- Data column
          HH.div [ HP.class_ (HH.ClassName "viz-panel narrow") ]
            [ HH.div [ HP.class_ (HH.ClassName "viz-panel-label") ] [ HH.text "Data" ]
            , HH.div [ HP.class_ (HH.ClassName "viz-panel-content") ]
                [ HH.div [ HP.id "ch1-data" ] [] ]
            ]
        , HH.div [ HP.class_ (HH.ClassName "fold-arrow") ] [ HH.text "\x2192" ]
        -- SVG output
        , HH.div [ HP.class_ (HH.ClassName "viz-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "viz-panel-label") ] [ HH.text "SVG" ]
            , HH.div [ HP.class_ (HH.ClassName "viz-panel-content") ]
                [ HH.div [ HP.id "ch1-svg" ] [] ]
            ]
        , HH.div [ HP.class_ (HH.ClassName "fold-arrow") ] [ HH.text "\x2192" ]
        -- Structure (metatree, unexplained)
        , HH.div [ HP.class_ (HH.ClassName "viz-panel") ]
            [ HH.div [ HP.class_ (HH.ClassName "viz-panel-label") ] [ HH.text "Structure" ]
            , HH.div [ HP.class_ (HH.ClassName "viz-panel-content") ]
                [ HH.div [ HP.id "ch1-meta" ] [] ]
            ]
        ]
    ]

-- =============================================================================
-- Actions
-- =============================================================================

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    H.liftEffect renderChapter1Trees
    H.modify_ _ { rendered = true }

-- =============================================================================
-- Tree rendering
-- =============================================================================

renderChapter1Trees :: Effect Unit
renderChapter1Trees = do
  let svgTree = Ch1.svgOutputTree
  let metaTree = Meta.interpretMeta Meta.defaultMetaConfig svgTree
  clearElement "#ch1-data"
  clearElement "#ch1-svg"
  clearElement "#ch1-meta"
  _ <- rerender "#ch1-data" Ch1.dataArrayTree
  _ <- rerender "#ch1-svg" svgTree
  _ <- rerender "#ch1-meta" metaTree
  pure unit
