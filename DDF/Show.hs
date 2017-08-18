{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module DDF.Show where

import DDF.Lang
import qualified Prelude as M
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF

data Name = Prefix M.String | Infix M.String

data AST = Node [M.String] Name [AST]

appAST (Node [] n rest) x = Node [] n (rest ++ [x])
appAST f x = Node [] (Prefix (show f)) [x]

lamAST str (Node abst n rest) = Node (str:abst) n rest

vars = [pre : suf | suf <- "":M.map show [0..], pre <- ['a'..'z']]

leaf x = Node [] x []

paren :: M.String -> M.String
paren str = "(" ++ str ++ ")"

apps :: Name -> [AST] -> M.String
apps (Prefix n) rest = M.unwords (n:(M.map show rest))
apps (Infix n) [] = n
apps (Infix n) [l] = (n ++ " " ++ show l)
apps (Infix n) [l, r] = show l ++ " " ++ n ++ " " ++ show r
apps (Infix n) (l:r:rest) = apps (Prefix (paren (show l ++ " " ++ n ++ " " ++ show r))) rest

instance M.Show AST where
  show (Node [] n []) = apps n []
  show (Node [] n rest) = paren $ apps n rest
  show (Node abst n rest) = paren ("\\" ++ M.unwords abst ++ " -> " ++ apps n rest)

newtype Show h a = Show {runShow :: [M.String] -> M.Int -> AST}

cname = Show . M.const . M.const . leaf
name = cname . Prefix
iname = cname . Infix

showAST (Show sh) = sh vars 0

instance DBI Show where
  z = Show $ M.const $ leaf . Prefix . show . M.flip (-) 1
  s (Show v) = Show $ \va -> v va . M.flip (-) 1
  abs (Show f) = Show $ \va x -> lamAST (show x) (f va (x + 1))
  app (Show f) (Show x) = Show $ \va h -> appAST (f va h) (x va h)
  hoas f = Show $ \(v:va) h ->
    lamAST v (runShow (f $ Show $ M.const $ M.const $ leaf $ Prefix v) va (h + 1))

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
  nil = name "[]"
  cons = iname ":"
  listMatch = name "listMatch"
  listAppend = iname "++"

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

instance Ordering Show where
  ordering = name . show
  sel = name "sel"