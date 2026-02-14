# PureScript Language Skill

You are writing PureScript code. This skill covers the patterns, idioms, and pitfalls you need to produce code that compiles cleanly on the first try.

---

## Module Structure

A PureScript module has a **fixed, ordered** structure. These sections must appear in exactly this order. Imports MUST come before any type or value declarations -- never after.

```purescript
module MyModule where          -- 1. Module declaration (required, must match filename)

import Prelude                 -- 2. ALL imports go here, BEFORE any declarations
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)

-- 3. Type declarations
type MyRecord = { name :: String, value :: Number }
data MyADT = Foo | Bar Int
newtype Wrapper = Wrapper String

-- 4. Value declarations (functions, constants)
myFunction :: MyRecord -> String
myFunction r = r.name <> ": " <> show r.value

-- WRONG: imports cannot appear here at the bottom of the file!
-- import Data.Int (toNumber)  -- ERROR: imports after declarations
```

If you realize mid-file that you need another import, go back and add it to section 2 (with the other imports), never at the bottom.

### CRITICAL: Imports are top-level only

Unlike Python, Rust, and Scala, PureScript does **not** support local or scoped imports.
Unlike Java, C#, and Rust, PureScript does **not** support fully-qualified function calls (`Data.Int.toNumber n`).
All imports must appear at the module top level, period.

**WRONG** -- local import in `where` clause:
```purescript
-- THIS WILL NOT COMPILE
myFn x = helper x
  where
    import Data.Array (find)   -- ERROR: imports cannot appear here
    helper = find (_ == x)
```

**WRONG** -- fully-qualified call (PureScript has NO unimported qualified paths):
```purescript
-- THIS WILL NOT COMPILE
myFn n = Data.Int.toNumber n   -- ERROR: Data.Int.toNumber is not in scope
```

**WRONG** -- wrapping a qualified call in a local function does NOT help:
```purescript
-- THIS WILL NOT COMPILE EITHER
myFn n = toNumber n
  where
    toNumber x = Data.Int.toNumber x  -- ERROR: Data.Int.toNumber is STILL not in scope
```

**WRONG** -- operator/fixity declaration in `where` clause:
```purescript
-- THIS WILL NOT COMPILE
myFn nodes id = nodes !! id
  where
    infixl 8 Data.Array.index as !!  -- ERROR: fixity declarations are module-level only
```

The ONLY way to use a function from another module is to `import` it at the top of the file. There are no workarounds, no local wrappers, no fully-qualified paths. If you need `toNumber`, `sum`, `length`, `head`, `range`, or any other function -- add the import at the top.

**RIGHT** -- everything imported at the top of the module:
```purescript
import Data.Array (find, head, length, range, (!!))
import Data.Foldable (sum)
import Data.Int (toNumber)

myFn x = find (_ == x)
convert n = toNumber n
total arr = sum arr
lookup nodes id = nodes !! id
```

This is the single most common error when generating PureScript. There are NO exceptions: every `import` and every fixity declaration (`infixl`, `infixr`, `infix`) must be at the module top level. If you need a function from another module, add the import at the top. Do not try to scope it.

---

## Type System Essentials

### Records

PureScript records are structural (not nominal). Field access uses `.`:

```purescript
type Point = { x :: Number, y :: Number }

origin :: Point
origin = { x: 0.0, y: 0.0 }

getX :: Point -> Number
getX p = p.x

-- Record update (returns new record):
moved :: Point
moved = origin { x = 5.0 }  -- { x: 5.0, y: 0.0 }

-- Wildcard record update (useful in modify_):
shift :: Point -> Point
shift = _ { x = 99.0 }  -- works as a function
```

Note the **asymmetry**: construction uses `:`, update uses `=`:
```purescript
{ x: 1.0 }        -- construction (colon)
r { x = 2.0 }     -- update (equals)
```

### Row Polymorphism

Extensible records use row variables:

```purescript
getName :: forall r. { name :: String | r } -> String
getName rec = rec.name

-- This accepts ANY record with a name field:
getName { name: "Alice", age: 30 }  -- "Alice"
getName { name: "Bob" }             -- "Bob"
```

**Row wildcards**: Use a named variable, not `_`:
```purescript
-- WRONG
fn :: { x :: Number | _ } -> Number  -- ERROR

-- RIGHT
fn :: forall r. { x :: Number | r } -> Number
```

### Algebraic Data Types

```purescript
data Color = Red | Green | Blue                    -- enum
data Shape = Circle Number | Rect Number Number    -- with fields
data Tree a = Leaf a | Branch (Tree a) (Tree a)    -- parameterized

-- Pattern matching:
describe :: Shape -> String
describe = case _ of
  Circle r -> "circle of radius " <> show r
  Rect w h -> "rectangle " <> show w <> "x" <> show h
```

### Newtypes

Zero-cost wrapper. Single constructor with single argument:

```purescript
newtype Name = Name String

-- Unwrap:
getName :: Name -> String
getName (Name s) = s

-- Derive instances automatically:
derive newtype instance Eq Name
derive newtype instance Ord Name
derive newtype instance Show Name
```

### Type Classes

```purescript
class Describable a where
  describe :: a -> String

instance Describable Int where
  describe n = "the number " <> show n

-- Multiple constraints:
fn :: forall a. Show a => Eq a => a -> a -> String
fn x y = if x == y then show x else "different"
```

### Phantom Types

Types with parameters that don't appear on the right-hand side:

```purescript
data Validated
data Unvalidated
newtype Input (status :: Type) = Input String

-- Only validated inputs can be processed:
process :: Input Validated -> Effect Unit
```

---

## Common Conversions

These come up constantly. Know them by heart:

```purescript
import Data.Int (toNumber, floor, round, ceil)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), split, trim, toLower, toUpper)
import Data.String.CodeUnits (length) as String

-- Int -> Number
toNumber 42          -- 42.0

-- Number -> Int
floor 3.7            -- 3
round 3.5            -- 4

-- String -> Number
Number.fromString "3.14"  -- Just 3.14

-- String -> Int (not direct -- go through Number)
-- Number.fromString "42" <#> floor  -- Just 42

-- Anything -> String
show 42              -- "42"
show 3.14            -- "3.14"
show true            -- "true"
```

**WRONG** -- do not convert Int to Number via String:
```purescript
toNumber n = show n # Number.fromString # fromMaybe 0.0  -- WRONG: use Data.Int.toNumber
```

---

## Operators

### Key Operators

```purescript
-- Function application and composition
f $ x       -- f x (low precedence, avoids parens)
f <<< g     -- compose: \x -> f (g x)
f >>> g     -- compose forward: \x -> g (f x)
x # f       -- reverse apply: f x (pipe-style, used heavily for builder APIs)
x <#> f     -- map flipped: map f x (functor)
x >>= f     -- bind (monadic chaining)
x <> y      -- append (Semigroup)
x <$> f     -- map: map f x (same as <#> but args flipped)

-- Comparison
x == y      -- Eq
x /= y      -- not equal
x < y       -- Ord (also <=, >, >=)
compare x y -- returns Ordering (LT, EQ, GT)
```

### Reverse Application (`#`) Pattern

Heavily used for builder-style configuration:

```purescript
import Hylograph.Scale as Scale

myScale = Scale.linear
  # Scale.domain [ 0.0, 100.0 ]
  # Scale.range  [ 0.0, 800.0 ]
  # Scale.nice

-- Equivalent to:
myScale = Scale.nice (Scale.range [0.0, 800.0] (Scale.domain [0.0, 100.0] Scale.linear))
```

---

## Common Data Structures

### Maybe

```purescript
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust, isNothing)

fromMaybe 0 (Just 5)   -- 5
fromMaybe 0 Nothing     -- 0
maybe "none" show (Just 5)  -- "5"

-- Pattern match:
case mx of
  Just x  -> "got " <> show x
  Nothing -> "empty"
```

### Either

```purescript
import Data.Either (Either(..), either, note)

-- Left = error, Right = success (by convention)
parseAge :: String -> Either String Int
parseAge s = note ("invalid: " <> s) (Int.fromString s)
```

### Tuple

```purescript
import Data.Tuple (Tuple(..), fst, snd)

pair :: Tuple String Int
pair = Tuple "hello" 42
fst pair  -- "hello"
```

### Array

```purescript
import Data.Array (head, tail, find, filter, mapWithIndex, concatMap, null, length, elem, nub, sort, uncons, range, (..), (!!))
import Data.Foldable (foldl, foldr, minimum, maximum, sum, for_)
import Data.Traversable (for, traverse)

-- Array comprehension via monad (list monad):
pairs :: Array { row :: Int, col :: Int }
pairs = do
  row <- [0, 1, 2]
  col <- [0, 1, 2]
  pure { row, col }
-- Produces 9 elements: [{row:0,col:0}, {row:0,col:1}, ...]

-- Range operator (must be imported!):
1 .. 10       -- [1, 2, 3, ..., 10]
range 1 10    -- same thing

-- Safe indexing:
[1,2,3] !! 0  -- Just 1
[1,2,3] !! 5  -- Nothing

-- mapWithIndex (index is first arg):
mapWithIndex (\i x -> { index: i, value: x }) ["a","b","c"]

-- Effectful traversal (for = traverse with flipped args):
-- for :: forall t m a b. Traversable t => Applicative m => t a -> (a -> m b) -> m (t b)
results <- for [1, 2, 3] \n -> do
  liftEffect (log (show n))
  pure (n * 2)
-- results :: Array Int = [2, 4, 6]
```

**CRITICAL: Arrays do not support cons-pattern matching.** Unlike Haskell lists, PureScript Arrays are JavaScript arrays under the hood and cannot be destructured with `:`.
```purescript
-- WRONG: `:` is List.Cons, NOT an Array operation. This WILL NOT COMPILE.
case myArray of
  first : rest -> ...  -- ERROR: No Cons pattern for Array

-- WRONG: even in a helper function building a path string:
buildPath = case points of
  []           -> ""
  first : rest -> "M" <> show first.x ...  -- ERROR: same problem

-- RIGHT: use uncons for head/tail decomposition
import Data.Array (uncons)

case uncons myArray of
  Nothing                    -> "empty"
  Just { head: x, tail: xs } -> "first: " <> show x <> ", rest has " <> show (length xs)

-- RIGHT: or avoid head/tail entirely with mapWithIndex:
buildPath points =
  let cmds = points # mapWithIndex \i pt ->
        let prefix = if i == 0 then "M" else "L"
        in prefix <> show pt.x <> "," <> show pt.y
  in joinWith "" cmds
```

### Map

```purescript
import Data.Map as Map
import Data.Map (Map)
import Data.Tuple (Tuple(..))

-- Construction:
m :: Map String Int
m = Map.fromFoldable [ Tuple "a" 1, Tuple "b" 2 ]

-- Lookup:
Map.lookup "a" m  -- Just 1

-- Keys/values (IMPORTANT: these return List, NOT Array):
Map.keys m    -- :: List String (NOT Array!)
Map.values m  -- :: List Int

-- Convert to Array when needed:
import Data.Array as Array
Array.fromFoldable (Map.keys m)    -- :: Array String
Array.fromFoldable (Map.values m)  -- :: Array Int
```

---

## Effect and Aff

### Effect (synchronous side effects)

```purescript
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref

-- Effect.Ref for mutable state:
counter :: Effect Unit
counter = do
  ref <- Ref.new 0
  Ref.modify_ (_ + 1) ref
  val <- Ref.read ref
  log (show val)  -- "1"
```

### Aff (asynchronous effects)

```purescript
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)  -- lift Effect into Aff or MonadEffect

-- Aff actions:
myAsync :: Aff String
myAsync = do
  liftEffect (log "starting")  -- Effect inside Aff
  result <- someAsyncOp
  pure result

-- Launch from Effect:
main :: Effect Unit
main = launchAff_ myAsync
```

### Lifting between effect types

```purescript
liftEffect :: forall m. MonadEffect m => Effect a -> m a
-- Works in: Aff, HalogenM, any MonadEffect
-- Use when you have an Effect but need MonadAff/MonadEffect

liftAff :: forall m. MonadAff m => Aff a -> m a
-- Works in: HalogenM, any MonadAff
```

---

## Halogen Essentials

### Component Structure

```purescript
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

type State = { count :: Int }
data Action = Increment | Decrement

component :: forall q i o m. MonadEffect m => H.Component q i o m
component = H.mkComponent
  { initialState: \_ -> { count: 0 }    -- input -> State
  , render                                -- State -> HTML
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction       -- Action -> HalogenM ...
      , initialize = Just Initialize      -- Optional: runs after mount
      }
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
    , HH.text (show state.count)
    ]

handleAction :: forall o m. MonadEffect m =>
  Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment -> H.modify_ _ { count = _ + 1 }  -- WRONG: can't use _ inside record update value
```

**WRONG** `_ { count = _ + 1 }` -- the inner `_` is not valid. Use:
```purescript
  Increment -> H.modify_ \s -> s { count = s.count + 1 }
```

### Subscriptions (bridging external events to Halogen)

```purescript
import Halogen.Subscription as HS

-- In handleAction:
{ listener, emitter } <- liftEffect HS.create
void $ H.subscribe emitter

-- From outside (Effect land), push events:
HS.notify listener MyAction
```

---

## String Building

PureScript has no string interpolation. Use `<>`:

```purescript
greeting :: String -> Int -> String
greeting name age =
  "Hello " <> name <> ", you are " <> show age <> " years old."

-- Multi-part SVG path:
pathD :: Number -> Number -> Number -> Number -> String
pathD x1 y1 x2 y2 =
  "M" <> show x1 <> "," <> show y1 <> "L" <> show x2 <> "," <> show y2
```

---

## Pattern Matching

### case _ of (anonymous lambda)

```purescript
-- These are equivalent:
colorFor :: String -> String
colorFor = case _ of
  "A" -> "#ff0000"
  "B" -> "#00ff00"
  _   -> "#888888"

colorFor' :: String -> String
colorFor' s = case s of
  "A" -> "#ff0000"
  "B" -> "#00ff00"
  _   -> "#888888"
```

### Guards

```purescript
classify :: Number -> String
classify n
  | n < 0.0   = "negative"
  | n == 0.0  = "zero"
  | n < 100.0 = "small"
  | otherwise  = "large"
```

### Destructuring

```purescript
-- Records:
fn :: { x :: Number, y :: Number } -> Number
fn { x, y } = x + y

-- Nested:
fn2 :: { point :: { x :: Number } } -> Number
fn2 { point: { x } } = x

-- ADT + binding:
area :: Shape -> Number
area (Circle r) = pi * r * r
area (Rect w h) = w * h
```

---

## Common Pitfalls

### 1. Name shadowing with constructors

```purescript
import Hylograph.Internal.Element.Types (ElementType(..))
-- This imports: Circle, Rect, Path, Line, Text, Group, SVG, Div, ...

-- WRONG: type alias shadows imported constructor
type Circle = { x :: Number, y :: Number, r :: Number }  -- ERROR: Circle already in scope

-- RIGHT: use a different name
type CircleData = { x :: Number, y :: Number, r :: Number }
```

### 2. Let bindings need `in`

```purescript
-- In do-blocks, `let` has no `in`:
main = do
  let x = 5
  log (show x)

-- Outside do-blocks, `let` NEEDS `in`:
result = let x = 5 in x + 1

-- Or use `where`:
result = x + 1
  where x = 5
```

### 3. Typeclass instances are global

You cannot define orphan instances (instances where neither the class nor the type is defined in your module) without a newtype wrapper:

```purescript
-- WRONG: orphan instance
instance Show (Array Int) where ...  -- won't compile if Show and Array are both foreign

-- RIGHT: wrap in newtype
newtype IntArray = IntArray (Array Int)
derive newtype instance Show IntArray
```

### 4. No function overloading

PureScript does not allow two top-level declarations with the same name, even with different types:

```purescript
-- WRONG: duplicate declaration
render :: State -> HTML    -- Halogen render
render state = ...

render :: String -> Effect Unit   -- standalone render
render selector = ...             -- ERROR: duplicate value declaration

-- RIGHT: use different names
renderComponent :: State -> HTML
renderComponent state = ...

renderStandalone :: String -> Effect Unit
renderStandalone selector = ...
```

### 5. Partial functions

Avoid functions that can crash at runtime:

```purescript
-- DANGEROUS: these throw on empty
import Data.Array.Partial (head)
import Partial.Unsafe (unsafePartial)

-- SAFE: return Maybe
import Data.Array (head)  -- head :: forall a. Array a -> Maybe a
```

### 6. `void` discards return values

```purescript
import Data.Functor (void)

-- When you don't need the result:
void $ rerender "#app" myTree
-- or
_ <- rerender "#app" myTree
```

### 7. Deriving instances

```purescript
-- For data types:
derive instance Eq MyType
derive instance Ord MyType

-- For newtypes (delegates to wrapped type):
derive newtype instance Eq MyNewtype
derive newtype instance Show MyNewtype

-- Generic deriving (for Show on data types):
derive instance Generic MyType _
instance Show MyType where show = genericShow
```

---

## Import Patterns

### Qualified imports (for namespacing)

```purescript
import Data.Array as Array
import Data.Map as Map
import Data.Set as Set

Array.filter (_ > 0) [1, -2, 3]
Map.lookup "key" myMap
```

### Selective imports (for common functions)

```purescript
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), either, note)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Foldable (foldl, minimum, maximum, for_)
import Data.Array (find, filter, mapWithIndex, concatMap, null, (!!))
```

### Re-exports

Some modules re-export their children. `Prelude` re-exports most basics:
```purescript
import Prelude  -- gets: show, map, bind, (<>), (<$>), (>>=), (#), etc.
```

**CRITICAL: `Prelude` does NOT re-export `Maybe`, `Either`, `Tuple`, or `Array` functions.**
You must always import these explicitly:
```purescript
import Data.Maybe (Maybe(..), fromMaybe, maybe)   -- for Nothing, Just
import Data.Either (Either(..), either)            -- for Left, Right
import Data.Tuple (Tuple(..), fst, snd)            -- for Tuple
import Data.Array (find, filter, map, ...)         -- for array operations
```

If you use `Nothing` or `Just` without importing `Data.Maybe`, you will get a "not in scope" error.

**Self-check**: before finishing a module, scan your code for `Nothing`, `Just`, `Maybe`, `Ref.new Nothing`, `{ delay: Nothing, ... }`, or `Maybe` in any type signature. If ANY appear, you need `import Data.Maybe (Maybe(..))` (or the specific constructors) at the top. This applies even in callback-heavy code where `Nothing` appears inside record literals or `Ref` initialization.

---

## Numeric Gotchas

```purescript
-- PureScript has separate Int and Number types (no implicit conversion)
1 + 2       -- Int (inferred)
1.0 + 2.0   -- Number (inferred)
1 + 2.0     -- ERROR: type mismatch

-- Convert explicitly:
import Data.Int (toNumber)
toNumber 42 + 3.14  -- 45.14

-- Integer division:
7 / 2       -- ERROR for Int (use div)
div 7 2     -- 3
mod 7 2     -- 1

-- Number division:
7.0 / 2.0   -- 3.5
```

---

## Debugging Aids

```purescript
import Debug (spy, trace, traceM)

-- spy :: forall a. String -> a -> a  (logs and returns value)
let result = spy "myValue" (computeSomething x)

-- traceM :: forall m. Monad m => String -> m Unit
traceM ("current state: " <> show state)

-- trace :: forall a. String -> (Unit -> a) -> a  (lazy)
trace "entering function" \_ -> actualComputation
```

These are for development only. Remove before committing.
