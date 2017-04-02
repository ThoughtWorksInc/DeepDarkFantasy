{-# LANGUAGE
  NoImplicitPrelude,
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeFamilies,
  KindSignatures,
  MultiParamTypeClasses,
  FlexibleInstances,
  NoMonomorphismRestriction
#-}

module DDF.Diff where

import DDF.Lang
import qualified Prelude as M
import Data.Proxy
import Data.Constraint

type family Diff (v :: *) (x :: *)
type instance Diff v () = ()
type instance Diff v (l, r) = (Diff v l, Diff v r)
type instance Diff v (l -> r) = Diff v l -> Diff v r
type instance Diff v Void = Void
type instance Diff v M.Double = (M.Double, v)
type instance Diff v M.Float = (M.Float, v)
type instance Diff v (Writer l r) = Writer (Diff v l) (Diff v r)
type instance Diff v (IO l) = IO (Diff v l)
type instance Diff v (Maybe l) = Maybe (Diff v l)
type instance Diff v [l] = [Diff v l]
type instance Diff v (Either l r) = Either (Diff v l) (Diff v r)
type instance Diff v (State l r) = State (Diff v l) (Diff v r)
type instance Diff v M.Bool = M.Bool
type instance Diff v M.Char = M.Char

class Monoid repr w => WithDiff repr w where
  withDiff :: repr h ((w -> x) -> w -> Diff x w)

class DBI repr => ConvDiff repr w where
  toDiff :: forall h x. Monoid repr x => Proxy x -> repr h (w -> Diff x w)
  toDiff _ = toDiffBy1 @repr @w @x zero
  toDiffBy :: forall h x. repr h (x -> w -> Diff x w)
  fromDiff :: forall h x. Monoid repr x => Proxy x -> repr h (Diff x w -> w)
  fromDiff _ = fromDiffBy1 @repr @w @x zero
  fromDiffBy :: repr h (x -> Diff x w -> w)

withDiff1 = app withDiff
toDiffBy1 :: forall repr w x h. ConvDiff repr w => repr h x -> repr h (w -> Diff x w)
toDiffBy1 = app toDiffBy
fromDiffBy1 :: forall repr w x h. ConvDiff repr w => repr h x -> repr h (Diff x w -> w)
fromDiffBy1 = app fromDiffBy

selfWithDiff :: (DBI repr, WithDiff repr w) => repr h (w -> Diff w w)
selfWithDiff = withDiff1 id

instance Lang repr => ConvDiff repr () where
  toDiffBy = const1 id
  fromDiffBy = const1 id

instance Lang repr => ConvDiff repr Double where
  toDiffBy = flip1 mkProd
  fromDiffBy = const1 zro

instance Lang repr => ConvDiff repr Float where
  toDiffBy = flip1 mkProd
  fromDiffBy = const1 zro

instance (Lang repr, ConvDiff repr l, ConvDiff repr r) => ConvDiff repr (l, r) where
  toDiffBy = lam $ \x -> bimap2 (toDiffBy1 x) (toDiffBy1 x)
  fromDiffBy = lam $ \x -> bimap2 (fromDiffBy1 x) (fromDiffBy1 x)

instance (Lang repr, ConvDiff repr l, ConvDiff repr r) => ConvDiff repr (Either l r) where
  toDiffBy = lam $ \x -> bimap2 (toDiffBy1 x) (toDiffBy1 x)
  fromDiffBy = lam $ \x -> bimap2 (fromDiffBy1 x) (fromDiffBy1 x)

instance (Lang repr, ConvDiff repr l, ConvDiff repr r) => ConvDiff repr (l -> r) where
  toDiffBy = lam2 $ \x f -> (toDiffBy1 x) `com2` f `com2` (fromDiffBy1 x)
  fromDiffBy = lam2 $ \x f -> (fromDiffBy1 x) `com2` f `com2` (toDiffBy1 x)

instance (Lang repr, ConvDiff repr l) => ConvDiff repr [l] where
  toDiffBy = lam $ \x -> map1 (toDiffBy1 x)
  fromDiffBy = lam $ \x -> map1 (fromDiffBy1 x)

instance Lang repr => ProdCon (WithDiff repr) l r where prodCon = Sub Dict

instance Lang repr => WithDiff repr () where
  withDiff = const1 id

instance Lang repr => WithDiff repr Double where
  withDiff = lam2 $ \con d -> mkProd2 d (app con doubleOne)

instance Lang repr => WithDiff repr M.Float where
  withDiff = lam2 $ \con d -> mkProd2 d (app con floatOne)

instance (Lang repr, WithDiff repr l, WithDiff repr r) => WithDiff repr (l, r) where
  withDiff = lam $ \con -> bimap2 (withDiff1 (lam $ \l -> app con (mkProd2 l zero))) (withDiff1 (lam $ \r -> app con (mkProd2 zero r)))
