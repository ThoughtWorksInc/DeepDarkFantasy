{-# LANGUAGE NoImplicitPrelude #-}

module DDF.Show (module DDF.Show) where

import DDF.Lang
import qualified Prelude as M

data AST = Leaf M.String | App M.String AST [AST] | Lam M.String [M.String] AST

appAST (Leaf f) x = App f x []
appAST (App f x l) r = App f x (l ++ [r])
appAST l r = appAST (Leaf $ show l) r

lamAST str (Lam st l t) = Lam str (st:l) t
lamAST str r = Lam str [] r

instance M.Show AST where
  show (Leaf f) = f
  show (App f x l) = "(" ++ f ++ " " ++ show x ++ M.concatMap ((" " ++) . show) l ++ ")"
  show (Lam str l t) = "(\\" ++ str ++ M.concatMap (" " ++) l ++ " -> " ++ show t ++ ")"

newtype Show h a = Show {runShow :: [M.String] -> M.Int -> AST}
name = Show . M.const . M.const . Leaf

instance DBI Show where
  z = Show $ M.const $ Leaf . show . M.flip (-) 1
  s (Show v) = Show $ \vars -> v vars . M.flip (-) 1
  abs (Show f) = Show $ \vars x -> lamAST (show x) (f vars (x + 1))
  app (Show f) (Show x) = Show $ \vars h -> appAST (f vars h) (x vars h)
  hoas f = Show $ \(v:vars) h ->
    lamAST v (runShow (f $ Show $ M.const $ M.const $ Leaf v) vars (h + 1))

instance Bool Show where
  bool = name . show
  ite = name "ite"

instance Char Show where
  char = name . show

instance Prod Show where
  mkProd = name "mkProd"
  zro = name "zro"
  fst = name "fst"

instance Lang Show where
  double = name . show
  doublePlus = name "plus"
  doubleMinus = name "minus"
  doubleMult = name "mult"
  doubleDivide = name "divide"
  doubleExp = name "exp"
  fix = name "fix"
  left = name "left"
  right = name "right"
  sumMatch = name "sumMatch"
  unit = name "unit"
  exfalso = name "exfalso"
  nothing = name "nothing"
  just = name "just"
  ioRet = name "ioRet"
  ioBind = name "ioBind"
  nil = name "nil"
  cons = name "cons"
  listMatch = name "listMatch"
  optionMatch = name "optionMatch"
  ioMap = name "ioMap"
  writer = name "writer"
  runWriter = name "runWriter"
  float = name . show
  floatPlus = name "plus"
  floatMinus = name "minus"
  floatMult = name "mult"
  floatDivide = name "divide"
  floatExp = name "exp"
  float2Double = name "float2Double"
  double2Float = name "double2Float"
  state = name "state"
  runState = name "runState"
  putStrLn = name "putStrLn"