module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Expr.ExprSpec as ExprSpec
import Test.Expr.PathSpec as PathSpec
import Test.Expr.PolymorphismSpec as PolymorphismSpec
import Test.Expr.UnitsSpec as UnitsSpec
import Test.Expr.ParabolaExample as ParabolaExample
import Test.Expr.SankeyExample as SankeyExample
import Test.Expr.TreeExample as TreeExample
import Test.Expr.UpdatePatternExample as UpdatePatternExample
import Test.Scale.ScaleSpec as ScaleSpec
import Test.Scale.LinearGolden as LinearGolden
import Test.Scale.PowGolden as PowGolden
import Test.Scale.LogGolden as LogGolden

main :: Effect Unit
main = do
  log "\n=== Expression DSL Tests ==="
  ExprSpec.runTests

  log "\n=== Path DSL Tests ==="
  PathSpec.runTests

  log "\n=== Interpreter Polymorphism Tests ==="
  PolymorphismSpec.runTests

  log "\n=== Unit Arithmetic Tests ==="
  UnitsSpec.runTests

  log "\n=== Scale Tests ==="
  ScaleSpec.runTests

  log "\n=== Linear Golden Tests (D3 compatibility) ==="
  LinearGolden.runTests

  log "\n=== Pow/Sqrt Golden Tests (D3 compatibility) ==="
  PowGolden.runTests

  log "\n=== Log Golden Tests (D3 compatibility) ==="
  LogGolden.runTests

  ParabolaExample.runExample

  SankeyExample.runExample

  TreeExample.runExample

  UpdatePatternExample.runExample

  log "\n=== All tests passed! ==="
