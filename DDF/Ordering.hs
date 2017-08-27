{-# Language
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  MultiParamTypeClasses,
  ConstraintKinds,
  FlexibleInstances,
  UndecidableInstances,
  KindSignatures,
  ConstraintKinds,
  TypeFamilies,
  UndecidableSuperClasses
#-}

module DDF.Ordering (module DDF.Ordering, module DDF.Bool) where

import DDF.Bool
import qualified Prelude as M

class Bool r => Ordering r where
  sel :: r h (a -> a -> a -> M.Ordering -> a)
  ordering :: M.Ordering -> r h M.Ordering
  ltOrd :: r h M.Ordering
  ltOrd = ordering M.LT
  eqOrd :: r h M.Ordering
  eqOrd = ordering M.EQ
  gtOrd :: r h M.Ordering
  gtOrd = ordering M.GT
  isLT :: r h (M.Ordering -> M.Bool)
  isLT = sel3 true  false false
  isEQ :: r h (M.Ordering -> M.Bool)
  isEQ = sel3 false true  false
  isGT :: r h (M.Ordering -> M.Bool)
  isGT = sel3 false false true
  chainOrd :: r h (M.Ordering -> M.Ordering -> M.Ordering)
  chainOrd = lam2 $ \l r -> sel4 ltOrd r gtOrd l
  cmpMap :: r h ((b -> a) -> Cmp a -> Cmp b)
  cmpMap = lam4 $ \f c l r -> app2 c (app f l) (app f r)

sel1 = app1 sel
sel2 = app2 sel
sel3 = app3 sel
sel4 = app4 sel

isLT1 = app1 isLT
isEQ1 = app1 isEQ
isGT1 = app1 isGT

chainOrd1 = app1 chainOrd
chainOrd2 = app2 chainOrd

type family OrdC (r :: * -> * -> *) :: * -> Constraint

class (Ordering r, M.Ord x) => Ord r x where
  cmp :: r h (x -> x -> M.Ordering)
  eq :: r h (x -> x -> M.Bool)
  eq = lam2 $ \l r -> isEQ1 $ cmp2 l r
  getOrdC :: Proxy r -> Dict (OrdC r x)

class (Ord r x, OrdC r x) => OrdWC r x
instance (Ord r x, OrdC r x) => OrdWC r x

eq1 = app1 eq
eq2 = app2 eq

cmp1 = app1 cmp
cmp2 = app2 cmp

cmpMap1 = app1 cmpMap
cmpMap2 = app2 cmpMap
cmpMap3 = app3 cmpMap
cmpMap4 = app4 cmpMap

type Cmp a = a -> a -> M.Ordering

class NoOrdC x
instance NoOrdC x
