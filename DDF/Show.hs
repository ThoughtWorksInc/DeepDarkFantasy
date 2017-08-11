{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module DDF.Show where

import DDF.Lang
import qualified Prelude as M
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF

data AST = Leaf M.String | App M.String AST [AST] | Lam M.String [M.String] AST

appAST (Leaf f) x = App f x []
appAST (App f x l) r = App f x (l ++ [r])
appAST l r = appAST (Leaf $ show l) r

lamAST str (Lam st l t) = Lam str (st:l) t
lamAST str r = Lam str [] r

vars = [pre : suf | suf <- "":M.map show [0..], pre <- ['a'..'z']]

instance M.Show AST where
  show (Leaf f) = f
  show (App f x l) = "(" ++ f ++ " " ++ show x ++ M.concatMap ((" " ++) . show) l ++ ")"
  show (Lam str l t) = "(\\" ++ str ++ M.concatMap (" " ++) l ++ " -> " ++ show t ++ ")"

newtype Show h a = Show {runShow :: [M.String] -> M.Int -> AST}
name = Show . M.const . M.const . Leaf

showAST (Show sh) = sh vars 0

instance DBI Show where
  z = Show $ M.const $ Leaf . show . M.flip (-) 1
  s (Show v) = Show $ \va -> v va . M.flip (-) 1
  abs (Show f) = Show $ \va x -> lamAST (show x) (f va (x + 1))
  app (Show f) (Show x) = Show $ \va h -> appAST (f va h) (x va h)
  hoas f = Show $ \(v:va) h ->
    lamAST v (runShow (f $ Show $ M.const $ M.const $ Leaf v) va (h + 1))

instance Bool Show where
  bool = name . show
  ite = name "ite"

instance Char Show where
  char = name . show

instance Prod Show where
  mkProd = name "mkProd"
  zro = name "zro"
  fst = name "fst"

instance Double Show where
  double = name . show
  doublePlus = name "plus"
  doubleMinus = name "minus"
  doubleMult = name "mult"
  doubleDivide = name "divide"
  doubleExp = name "exp"
  doubleEq = name "eq"

instance Float Show where
  float = name . show
  floatPlus = name "plus"
  floatMinus = name "minus"
  floatMult = name "mult"
  floatDivide = name "divide"
  floatExp = name "exp"

instance Option Show where
  nothing = name "nothing"
  just = name "just"
  optionMatch = name "optionMatch"

instance Map.Map Show where
  empty = name "Map.empty"
  singleton = name "Map.singleton"
  lookup = name "Map.lookup"
  alter = name "Map.alter"
  mapMap = name "Map.mapMap"
  unionWith = name "Map.unionWith"

instance Bimap Show where
  size = name "size"
  lookupL = name "lookupL"
  lookupR = name "lookupR"
  toMapL = name "toMapL"
  toMapR = name "toMapR"
  updateL = name "updateL"
  updateR = name "updateR"
  empty = name "empty"
  singleton = name "singleton"
  insert = name "insert"

instance Dual Show where
  dual = name "dual"
  runDual = name "runDual"

instance Unit Show where
  unit = name "unit"

instance Sum Show where
  left = name "left"
  right = name "right"
  sumMatch = name "sumMatch"

instance Int Show where
  int = name . show
  pred = name "pred"
  isZero = name "isZero"

instance List Show where
  nil = name "nil"
  cons = name "cons"
  listMatch = name "listMatch"
  listAppend = name "++"

instance Y Show where
  y = name "Y"

instance IO Show where
  putStrLn = name "putStrLn"

instance Functor Show x where
  map = name "map"

instance Applicative Show x where
  pure = name "pure"
  ap = name "ap"

instance Monad Show x where
  join = name "join"
  bind = name "bind"

instance VTF.VectorTF Show where
  zero = name "VTF.zero"
  basis = name "VTF.basis"
  plus = name "VTF.plus"
  mult = name "VTF.mult"
  vtfMatch = name "VTF.vtfMatch"

instance DiffWrapper Show where
  diffWrapper = name "diffWrapper"
  runDiffWrapper = name "runDiffWrapper"

instance Fix Show where
  fix = name "fix"
  runFix = name "runFix"

instance FreeVector Show where
  freeVector = name "freeVector"
  runFreeVector = name "runFreeVector"

instance Lang Show where
  exfalso = name "exfalso"
  writer = name "writer"
  runWriter = name "runWriter"
  float2Double = name "float2Double"
  double2Float = name "double2Float"
  state = name "state"
  runState = name "runState"
