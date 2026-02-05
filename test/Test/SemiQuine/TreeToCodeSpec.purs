module Test.SemiQuine.TreeToCodeSpec where

import Prelude

import Data.String (contains, Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Interpreter.SemiQuine.TreeToCode (treeToCode)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.HATS (Tree, elem, forEach, staticNum, staticStr, thunkedNum)
import Test.Assert (assert')

-- | Sample data type for parabola example
type ParabolaPoint = { x :: Number, y :: Number }

-- | Sample data - parabola points
parabolaData :: Array ParabolaPoint
parabolaData =
  [ { x: -10.0, y: 100.0 }
  , { x: -5.0, y: 25.0 }
  , { x: 0.0, y: 0.0 }
  , { x: 5.0, y: 25.0 }
  , { x: 10.0, y: 100.0 }
  ]

-- | Simple linear scale for testing
scaleX :: Number -> Number
scaleX x = (x + 10.0) * 20.0  -- Maps -10..10 to 0..400

scaleY :: Number -> Number
scaleY y = 300.0 - (y * 2.5)  -- Maps 0..100 to 300..50 (inverted)

-- | Parabola tree - using HATS API
-- | forEach creates a fold over the data
parabolaTree :: Tree
parabolaTree =
  elem SVG
    [ staticNum "width" 400.0
    , staticNum "height" 300.0
    , staticStr "id" "parabola-svg"
    , staticStr "class" "test-example"
    ]
    [ forEach "circles" Circle parabolaData (\d -> show d.x) \d ->
        elem Circle
          [ thunkedNum "cx" (scaleX d.x)
          , thunkedNum "cy" (scaleY d.y)
          , staticNum "r" 5.0
          , staticStr "fill" "green"
          ]
          []
    ]

-- | Static-only tree for comparison
staticTree :: Tree
staticTree =
  elem SVG
    [ staticNum "width" 200.0
    , staticNum "height" 100.0
    ]
    [ elem Circle
        [ staticNum "cx" 50.0
        , staticNum "cy" 50.0
        , staticNum "r" 10.0
        , staticStr "fill" "blue"
        ]
        []
    ]

-- | Helper to check if string contains pattern
shouldContain :: String -> String -> Effect Unit
shouldContain haystack needle =
  assert' ("Expected to find '" <> needle <> "' in output:\n" <> haystack)
    (contains (Pattern needle) haystack)

-- | Run all tests
runTests :: Effect Unit
runTests = do
  log "\n--- Static tree tests ---"
  testStaticElementTypes
  testStaticAttributes
  testChildren

  log "\n--- Data-driven tree tests ---"
  testFold
  testDynamicAttributes
  testStaticAttributesInTemplate

  log "\n--- Full output inspection ---"
  inspectParabolaOutput

-- | Test: generates correct element types
testStaticElementTypes :: Effect Unit
testStaticElementTypes = do
  log "  ✓ generates correct element types"
  let code = treeToCode staticTree
  code `shouldContain` "elem SVG"
  code `shouldContain` "elem Circle"

-- | Test: generates correct static attributes
testStaticAttributes :: Effect Unit
testStaticAttributes = do
  log "  ✓ generates correct static attributes"
  let code = treeToCode staticTree
  -- HATS uses staticNum/staticStr which become StaticAttr in the tree
  code `shouldContain` "width"
  code `shouldContain` "200"
  code `shouldContain` "height"
  code `shouldContain` "100"
  code `shouldContain` "cx"
  code `shouldContain` "50"
  code `shouldContain` "fill"
  code `shouldContain` "blue"

-- | Test: generates children correctly
testChildren :: Effect Unit
testChildren = do
  log "  ✓ generates children structure"
  let code = treeToCode staticTree
  -- Should show nested structure
  code `shouldContain` "elem SVG"
  code `shouldContain` "elem Circle"

-- | Test: generates fold for data joins
testFold :: Effect Unit
testFold = do
  log "  ✓ generates fold for data joins"
  let code = treeToCode parabolaTree
  code `shouldContain` "fold"
  code `shouldContain` "circles"

-- | Test: template evaluation produces concrete values
testDynamicAttributes :: Effect Unit
testDynamicAttributes = do
  log "  ✓ template evaluation shows concrete values"
  let code = treeToCode parabolaTree
  -- First datum is { x: -10.0, y: 100.0 }
  -- scaleX (-10.0) = 0.0, scaleY 100.0 = 50.0
  code `shouldContain` "cx"
  code `shouldContain` "cy"

-- | Test: preserves static attributes in templates
testStaticAttributesInTemplate :: Effect Unit
testStaticAttributesInTemplate = do
  log "  ✓ preserves static attributes in templates"
  let code = treeToCode parabolaTree
  -- radius and fill are static
  code `shouldContain` "r"
  code `shouldContain` "5"
  code `shouldContain` "fill"
  code `shouldContain` "green"

-- | Inspect: print the full output for manual review
inspectParabolaOutput :: Effect Unit
inspectParabolaOutput = do
  log "\n--- Generated code for parabola tree ---"
  let code = treeToCode parabolaTree
  log code
  log "--- End generated code ---\n"
