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
import DDF.Meta.Diff
import qualified Prelude as M

class Bool repr => Ordering repr where
  sel :: repr h (a -> a -> a -> M.Ordering -> a)
  ordering :: M.Ordering -> repr h M.Ordering
  ltOrd :: repr h M.Ordering
  ltOrd = ordering M.LT
  eqOrd :: repr h M.Ordering
  eqOrd = ordering M.EQ
  gtOrd :: repr h M.Ordering
  gtOrd = ordering M.GT

sel1 = app1 sel
sel2 = app2 sel
sel3 = app3 sel
sel4 = app4 sel

isLT = sel3 true  false false
isLT1 = app1 isLT
isEQ = sel3 false true  false
isEQ1 = app1 isEQ
isGT = sel3 false false true
isGT1 = app1 isGT

type family ObjOrdC (r :: * -> * -> *) :: * -> Constraint

class (ObjOrdC r x, Ordering r) => ObjOrd r x where
  cmp :: r h (x -> x -> M.Ordering)
  eq :: r h (x -> x -> M.Bool)
  eq = lam2 $ \l r -> isEQ1 $ cmp2 l r

class M.Ord x => MetaOrd x where
  diffOrd :: Proxy (v, x) -> Dict (MetaOrd (DiffType v x))

class Ordering r => ObjOrd2 r f ac bc where
  objOrd2 :: (ac a, bc b) => Proxy ac -> Proxy bc -> Proxy a -> Proxy b -> Dict (ObjOrd r (f a b))

eq1 = app1 eq
eq2 = app2 eq

cmp1 = app1 cmp
cmp2 = app2 cmp