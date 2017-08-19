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

sel1 = app1 sel
sel2 = app2 sel
sel3 = app3 sel
sel4 = app4 sel

isLT1 = app1 isLT
isEQ1 = app1 isEQ
isGT1 = app1 isGT

chainOrd1 = app1 chainOrd
chainOrd2 = app2 chainOrd

class Ordering r => ObjOrd r x where
  cmp :: r h (x -> x -> M.Ordering)
  eq :: r h (x -> x -> M.Bool)
  eq = lam2 $ \l r -> isEQ1 $ cmp2 l r

eq1 = app1 eq
eq2 = app2 eq

cmp1 = app1 cmp
cmp2 = app2 cmp

type Cmp a = a -> a -> M.Ordering
