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

main = DBI.main
