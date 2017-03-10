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

module PolyT where
import DBIT

poly :: DBI repr => repr h (a -> a)
poly = hlam $ \x -> x
