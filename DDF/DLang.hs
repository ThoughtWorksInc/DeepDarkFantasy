{-# LANGUAGE
  RankNTypes,
  NoImplicitPrelude,
  TypeOperators,
  NoMonomorphismRestriction
#-}

module DDF.DLang (module DDF.DLang, module DDF.Lang) where

import DDF.Lang

class Lang r => DLang r where
