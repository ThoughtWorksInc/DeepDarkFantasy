{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  RankNTypes,
  FlexibleInstances
#-}

module Main where

import DDF.PE
import DDF.Double
import DDF.Size
import qualified Prelude as M
import Prelude ((>), (>=))
import System.Exit (exitFailure)
import Control.Monad
import DDF.Meta.Interpreter
import Test.QuickCheck
import DDF.Eval

class TestEqual x where
  testEqual :: x -> x -> M.IO ()

instance TestEqual M.Double where
  testEqual l r = quickCheck (l == r)

instance TestEqual (M.Double -> M.Double) where
  testEqual l r = quickCheck (\x -> l x == r x)

instance TestEqual (M.Double -> M.Double -> M.Double) where
  testEqual l r = quickCheck (\x y -> l x y == r x y)

test :: TestEqual x => (forall r. Double r => r () x) -> (M.Int -> M.Int -> M.Bool) -> M.IO ()
test x c = do
  quickCheck (c (runSize x) (runSize (pe x)))
  testEqual (runEval x ()) (runEval (pe x) ())

optimized x y = quickCheck (runSize x > runSize y)

main :: M.IO ()
main = do
  test (doublePlus2 (double 1.0) (double 1.0)) (>)
  test (doublePlus1 (double 1.0)) n
  test (abs (doublePlus2 z (double 1.0))) n
  test (doublePlus1 (double 1.0)) n
  test (abs (doublePlus2 z (double 0.0))) (>)
  test (doubleExp1 (double 1.0)) (>)
  test (abs (doubleExp1 z)) n
  test (abs $ abs $ (app2 (s doublePlus) z (s z))) n
  where
    n _ _ = M.True
