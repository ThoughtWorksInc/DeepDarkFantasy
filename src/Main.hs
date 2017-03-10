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
    NoMonomorphismRestriction #-}

module Main (main) where
import qualified HOAS
import qualified Comb
import qualified DBI
import qualified Poly

main = Poly.main
