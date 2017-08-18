{-# LANGUAGE
  NoImplicitPrelude,
  LambdaCase,
  TypeFamilies,
  FlexibleContexts,
  MultiParamTypeClasses,
  FlexibleInstances
#-}

module DDF.Eval where

import DDF.ImportMeta
import qualified Prelude as M
import qualified Control.Monad.Writer as M (WriterT(WriterT), runWriter)
import qualified Control.Monad.State as M
import qualified GHC.Float as M
import qualified Data.Functor.Identity as M
import qualified Data.Bool as M
import qualified Data.Map as M.Map
import qualified DDF.Meta.Dual as M
import qualified DDF.Map as Map
import qualified DDF.Meta.VectorTF as M.VTF
import qualified Data.Bimap as M.Bimap
import qualified DDF.VectorTF as VTF
import qualified DDF.Meta.DiffWrapper as M.DW
import qualified Data.Functor.Foldable as M
import qualified DDF.Meta.FreeVector as M
import DDF.Lang

newtype Eval h x = Eval {runEval :: h -> x}

comb = Eval . M.const

instance DBI Eval where
  z = Eval M.fst
  s (Eval a) = Eval $ a . M.snd
  abs (Eval f) = Eval $ \h a -> f (a, h)
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

instance Double Eval where
  double = comb
  doublePlus = comb (+)
  doubleMinus = comb (-)
  doubleMult = comb (*)
  doubleDivide = comb (/)
  doubleExp = comb M.exp
  doubleEq = comb (==)

instance Float Eval where
  float = comb
  floatPlus = comb (+)
  floatMinus = comb (-)
  floatMult = comb (*)
  floatDivide = comb (/)
  floatExp = comb M.exp

instance Option Eval where
  nothing = comb M.Nothing
  just = comb M.Just
  optionMatch = comb $ \l r -> \case
                              M.Nothing -> l
                              M.Just x -> r x

instance Map.Map Eval where
  empty = comb M.Map.empty
  singleton = comb M.Map.singleton
  lookup = flip1 $ comb M.Map.lookup
  alter = comb M.Map.alter
  mapMap = comb M.fmap
  unionWith = comb M.Map.unionWith

instance Bimap Eval where
  size = comb M.Bimap.size
  lookupL = flip1 $ comb M.Bimap.lookup
  lookupR = flip1 $ comb M.Bimap.lookupR
  toMapL = comb M.Bimap.toMap
  toMapR = comb M.Bimap.toMapR
  empty = comb M.Bimap.empty
  singleton = comb $ \(a, b) -> M.Bimap.singleton a b
  insert = comb $ \(a, b) -> M.Bimap.insert a b
  updateL = comb M.Bimap.update
  updateR = comb M.Bimap.updateR

instance Dual Eval where
  dual = comb M.Dual
  runDual = comb M.runDual

instance Unit Eval where
  unit = comb ()

instance Sum Eval where
  left = comb M.Left
  right = comb M.Right
  sumMatch = comb $ \l r -> \case
                             M.Left x -> l x
                             M.Right x -> r x

type instance ObjOrdC Eval = M.Ord
instance ObjOrd Eval M.Int where
  cmp = comb M.compare

instance Int Eval where
  int = comb
  pred = comb ((-) 1)

instance Y Eval where
  y = comb loop
    where loop x = x $ loop x

instance List Eval where
  nil = comb []
  cons = comb (:)
  listMatch = comb $ \l r -> \case
                            [] -> l
                            x:xs -> r x xs

instance Functor Eval M.IO where
  map = comb M.fmap

instance Applicative Eval M.IO where
  pure = comb M.pure
  ap = comb M.ap

instance Monad Eval M.IO where
  join = comb M.join
  bind = comb (>>=)

instance IO Eval where
  putStrLn = comb M.putStrLn

instance (ObjOrd Eval a, ObjOrd Eval b) => ObjOrd Eval (M.VTF.VectorTF a b) where
  cmp = comb M.compare

instance ObjOrd2 Eval M.VTF.VectorTF (ObjOrd Eval) (ObjOrd Eval) where
  objOrd2 _ _ _ _ = Dict

instance VTF.VectorTF Eval where
  zero = comb M.VTF.Zero
  basis = comb M.VTF.Basis
  plus = comb M.VTF.Plus
  mult = comb M.VTF.Mult
  vtfMatch = comb $ \zr b p m -> \case
                                 M.VTF.Zero -> zr
                                 M.VTF.Basis t -> b t
                                 M.VTF.Plus l r -> p l r
                                 M.VTF.Mult l r -> m l r

instance DiffWrapper Eval where
  diffWrapper = comb M.DW.DiffWrapper
  runDiffWrapper = comb M.DW.runDiffWrapper

instance Fix Eval where
  fix = comb M.Fix
  runFix = comb M.unfix

instance FreeVector Eval where
  freeVector = comb M.FreeVector
  runFreeVector = comb M.runFreeVector

instance Lang Eval where
  exfalso = comb absurd
  writer = comb (M.WriterT . M.Identity)
  runWriter = comb M.runWriter
  float2Double = comb M.float2Double
  double2Float = comb M.double2Float
  state = comb M.state
  runState = comb M.runState

instance Ordering Eval where
  ordering = comb
  sel = comb f where
    f x _ _ M.LT = x
    f _ x _ M.EQ = x
    f _ _ x M.GT = x