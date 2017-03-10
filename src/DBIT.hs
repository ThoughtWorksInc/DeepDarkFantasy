{-# LANGUAGE
    MultiParamTypeClasses,
    RankNTypes,
    ScopedTypeVariables,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    IncoherentInstances,
    PolyKinds,
    LambdaCase,
    NoMonomorphismRestriction,
    TypeFamilies,
    LiberalTypeSynonyms,
    EmptyCase #-}

module DBIT where

class DBI (repr :: * -> * -> *) where

class NT l r where

instance (DBI repr, NT (repr l) (repr r)) => NT (repr l) (repr (a, r)) where

instance NT x x where

hlam :: forall repr a b h. DBI repr =>
 ((forall k. NT (repr (a, h)) k => k a) -> (repr (a, h)) b) -> repr h (a -> b)
hlam f = undefined

polyDBI :: DBI repr => repr h (a -> a)
polyDBI = hlam $ \x -> x
