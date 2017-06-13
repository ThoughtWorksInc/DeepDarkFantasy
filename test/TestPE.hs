module Main where

import DDF.PE
import DDF.Double
import qualified Prelude as M
import System.Exit (exitFailure)
import Control.Monad
import DDF.Meta.Interpreter
import Test.QuickCheck
import DDF.Eval

newtype Comp h a = Comp { runComp :: M.Int }

instance DBI Comp where
  z = Comp 0
  s (Comp a) = Comp a
  abs (Comp f) = Comp f
  app (Comp f) (Comp x) = Comp (f + x + 1)

instance Double Comp where
  double = M.const (Comp 0)
  doublePlus = Comp 0
  doubleMinus = Comp 0
  doubleMult = Comp 0
  doubleDivide = Comp 0
  doubleExp = Comp 0

optimized :: Comp h a -> Comp h a -> M.IO ()
optimized e1 e2 = unless ((M.>) (runComp e1) (runComp e2)) exitFailure

main :: M.IO ()
main = do
  test1
  test2
  test3
  test4
  test5
  test6
  test7
  test8

-- Tests

equiv0 t1 t2 = quickCheck (runEval t1 () == runEval t2 ())
equiv1 t1 t2 = quickCheck (\n -> runEval t1 () n == runEval t2 () n)
equiv2 t1 t2 = quickCheck (\n m -> runEval t1 () n m == runEval t2 () n m)

t1, t1' :: Double r => r () M.Double
t1 = app2 doublePlus (double 1.0) (double 2.0)
-- plus 1.0 2.0
t1' = pe t1
-- 3.0

test1 :: M.IO ()
test1 = do
  equiv0 t1 t1'
  optimized t1 t1'

t2, t2' :: Double r => r () (M.Double -> M.Double)
t2 = app doublePlus (double 1.0)
-- plus 1.0
t2' = pe t2
-- \0 -> (plus 1.0 0)

test2 :: M.IO ()
test2 = equiv1 t2 t2'

t3, t3' :: Double r => r () (M.Double -> M.Double)
t3 = abs (app2 doublePlus z (double 1.0))
-- (\0 -> (plus 0 1.0))
t3' = pe t3
-- (\0 -> (plus 0 1.0))

test3 :: M.IO ()
test3 = equiv1 t3 t3'

t4, t4' :: Double r => r () (M.Double -> M.Double)
t4 = app doublePlus (double 0.0)
-- (plus 0.0)
t4' = pe t4
-- (\0 -> 0)

test4 :: M.IO ()
test4 = do
  equiv1 t4 t4'
  optimized t4 t4'

t5, t5' :: Double r => r () (M.Double -> M.Double)
t5 = abs (app2 doublePlus z (double 0.0))
-- (\0 -> (plus 0 0.0))
t5' = pe t5
-- (\0 -> 0)

test5 :: M.IO ()
test5 = do
  equiv1 t5 t5'
  optimized t5 t5'

t6, t6' :: Double r => r () M.Double
t6 = app doubleExp (double 1.0)
-- (exp 1.0)
t6' = pe t6
-- 2.718281828459045

test6 :: M.IO ()
test6 = do
  equiv0 t6 t6'
  optimized t6 t6'

t7, t7' :: Double r => r () (M.Double -> M.Double)
t7 = abs $ app doubleExp z
-- (\0 -> (exp 0))
t7' = pe t7
-- (\0 -> (exp 0))

test7 :: M.IO ()
test7 = equiv1 t7 t7'  

t8, t8' :: Double r => r () (M.Double -> M.Double -> M.Double)
t8 = abs $ abs $ (app2 (s doublePlus) z (s z))
-- (\0 1 -> (plus 1 0))
t8' = pe t8
-- (\0 1 -> (plus 1 0))

test8 :: M.IO ()
test8 = equiv2 t8 t8'
