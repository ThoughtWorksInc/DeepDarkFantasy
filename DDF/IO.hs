{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, NoMonomorphismRestriction #-}

module DDF.IO (module DDF.IO, module DDF.List, module DDF.Char, module DDF.Unit) where

import DDF.List
import DDF.Char
import DDF.Unit
import qualified Prelude as M

string [] = nil
string (c:str) = cons2 (char c) (string str)

class (List r, Unit r, Char r, Monad r M.IO) => IO r where
  putStrLn :: r h (String -> M.IO ())

putStrLn1 = app putStrLn
