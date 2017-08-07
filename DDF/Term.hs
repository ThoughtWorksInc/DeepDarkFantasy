{-# LANGUAGE
  NoImplicitPrelude,
  TemplateHaskell,
  MultiParamTypeClasses
#-}

module DDF.Term (module DDF.TermGen) where
import DDF.TermGen

$genInstance
