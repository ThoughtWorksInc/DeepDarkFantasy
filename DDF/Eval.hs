{-# LANGUAGE
  NoImplicitPrelude,
  LambdaCase,
  TypeFamilies,
  FlexibleContexts,
  MultiParamTypeClasses,
  FlexibleInstances
#-}

module DDF.Eval where

import DDF.Lang
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
import qualified DDF.Meta.Util as M

newtype Eval h x = Eval {runEval :: h -> x}

comb = Eval . M.const

type instance OrdC Eval = NoOrdC

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
  doubleCmp = comb M.compare

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
  lookup' = flip1 $ comb M.Map.lookup
  alter' = comb M.Map.alter
  mapMap = comb M.fmap
  unionWithKey' = comb M.Map.unionWithKey

instance Bimap Eval where
  size = comb M.Bimap.size
  lookupL' = flip1 $ comb M.Bimap.lookup
  lookupR' = flip1 $ comb M.Bimap.lookupR
  toMapL = comb M.Bimap.toMap
  toMapR = comb M.Bimap.toMapR
  empty = comb M.Bimap.empty
  singleton = comb $ \(a, b) -> M.Bimap.singleton a b
  insert' = comb $ \(a, b) -> M.Bimap.insert a b
  updateL' = comb M.Bimap.update
  updateR' = comb M.Bimap.updateR

instance Dual Eval where
  dual = comb M.Dual
  runDual = comb M.runDual
  dualGetOrdC = Sub Dict

instance Unit Eval where
  unit = comb ()

instance Sum Eval where
  left = comb M.Left
  right = comb M.Right
  sumMatch = comb $ \l r -> \case
                             M.Left x -> l x
                             M.Right x -> r x

instance Int Eval where
  int = comb
  pred = comb ((-) 1)
  intCmp = comb M.compare

instance Y Eval where
  y = comb loop
    where loop x = x $ loop x

instance List Eval where
  nil = comb []
  cons = comb (:)
  listMatch = comb $ \l r -> \case
                            [] -> l
                            x:xs -> r x xs

instance IO Eval where
  putStrLn = comb M.putStrLn
  ioMap = comb M.fmap
  ioPure = comb M.pure
  ioAP = comb M.ap
  ioBind = comb (>>=)
  ioJoin = comb M.join

instance VTF.VectorTF Eval where
  zero = comb M.VTF.Zero
  basis = comb M.VTF.Basis
  plus = comb M.VTF.Plus
  mult = comb M.VTF.Mult
  vtfMatch =
      comb $ \zr b p m -> \case
                          M.VTF.Zero -> zr
                          M.VTF.Basis t -> b t
                          M.VTF.Plus l r -> p l r
                          M.VTF.Mult l r -> m l r
  vtfCmp =
      comb $ x where
        x t f = c where
          c M.VTF.Zero M.VTF.Zero = M.EQ
          c M.VTF.Zero _ = M.LT
          c _ M.VTF.Zero = M.GT
          c (M.VTF.Basis l) (M.VTF.Basis r) = t l r
          c (M.VTF.Basis _) _ = M.LT
          c _ (M.VTF.Basis _) = M.GT
          c (M.VTF.Plus ll lr) (M.VTF.Plus rl rr) = M.chainOrd (f ll rl) (f lr rr)
          c (M.VTF.Plus _ _) _ = M.LT
          c _ (M.VTF.Plus _ _) = M.GT
          c (M.VTF.Mult ll lr) (M.VTF.Mult rl rr) = M.chainOrd (runEval cmp () ll rl) (f lr rr)
  vtfGetOrdC = Sub Dict

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
