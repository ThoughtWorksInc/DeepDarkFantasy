{-# LANGUAGE
  NoImplicitPrelude,
  TemplateHaskell,
  MultiParamTypeClasses
#-}

module DDF.Term (module DDF.Term, module DDF.TermGen) where
import DDF.TermGen

import qualified Prelude as M

$genInstance

testing :: Term Lang () M.Double
testing = doubleZero
