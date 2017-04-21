{-# LANGUAGE NoImplicitPrelude #-}

module DDF.IO (module DDF.IO, module DDF.List, module DDF.Char, module DDF.Unit) where

import DDF.List
import DDF.Char
import DDF.Unit
import qualified Prelude as M

class (List r, Unit r, Char r) => IO r where
  ioRet :: r h (a -> M.IO a)
  ioBind :: r h (M.IO a -> (a -> M.IO b) -> M.IO b)
  ioMap :: r h ((a -> b) -> M.IO a -> M.IO b)
  putStrLn :: r h (String -> M.IO ())
