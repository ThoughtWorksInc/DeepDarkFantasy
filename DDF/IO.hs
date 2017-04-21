{-# LANGUAGE NoImplicitPrelude, FlexibleContexts #-}

module DDF.IO (module DDF.IO, module DDF.List, module DDF.Char, module DDF.Unit) where

import DDF.List
import DDF.Char
import DDF.Unit
import qualified Prelude as M

class (List r, Unit r, Char r, Monad r M.IO) => IO r where
  putStrLn :: r h (String -> M.IO ())
