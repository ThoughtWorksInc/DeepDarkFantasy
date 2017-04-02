{-# LANGUAGE NoImplicitPrelude,
  LambdaCase #-}

module DDF.Eval where

import DDF.ImportMeta
import DDF.Lang
import qualified Prelude as M
import qualified Control.Monad.Writer as M
import qualified Control.Monad.State as M
import qualified GHC.Float as M
import qualified Data.Functor.Identity as M
import qualified Data.Bool as M

newtype Eval h x = Eval {runEval :: h -> x}

comb = Eval . M.const

instance DBI Eval where
  z = Eval M.fst
  s (Eval a) = Eval $ a . M.snd
  abs (Eval f) = Eval $ \a h -> f (h, a)
  app (Eval f) (Eval x) = Eval $ \h -> f h $ x h

instance Bool Eval where
  bool = comb
  ite = comb M.bool

instance Char Eval where
  char = comb

instance Prod Eval where
  mkProd = comb (,)
  zro = comb M.fst
  fst = comb M.snd

instance Lang Eval where
  double = comb
  doublePlus = comb (+)
  doubleMinus = comb (-)
  doubleMult = comb (*)
  doubleDivide = comb (/)
  fix = comb loop
    where loop x = x $ loop x
  left = comb M.Left
  right = comb M.Right
  sumMatch = comb $ \l r -> \case
                             M.Left x -> l x
                             M.Right x -> r x
  unit = comb ()
  exfalso = comb absurd
  nothing = comb M.Nothing
  just = comb M.Just
  ioRet = comb M.return
  ioBind = comb (>>=)
  nil = comb []
  cons = comb (:)
  listMatch = comb $ \l r -> \case
                            [] -> l
                            x:xs -> r x xs
  optionMatch = comb $ \l r -> \case
                              M.Nothing -> l
                              M.Just x -> r x
  ioMap = comb M.fmap
  writer = comb (M.WriterT . M.Identity)
  runWriter = comb M.runWriter
  doubleExp = comb M.exp
  float = comb
  floatPlus = comb (+)
  floatMinus = comb (-)
  floatMult = comb (*)
  floatDivide = comb (/)
  floatExp = comb M.exp
  float2Double = comb M.float2Double
  double2Float = comb M.double2Float
  state = comb M.state
  runState = comb M.runState
  putStrLn = comb M.putStrLn