{-# LANGUAGE 
  NoImplicitPrelude,
  FlexibleContexts,
  NoMonomorphismRestriction,
  FlexibleInstances,
  MultiParamTypeClasses
#-}

module DDF.IO (module DDF.IO, module DDF.List, module DDF.Char, module DDF.Unit) where

import DDF.List
import DDF.Char
import DDF.Unit
import qualified Prelude as M

string [] = nil
string (c:str) = cons2 (char c) (string str)

class (List r, Unit r, Char r) => IO r where
  putStrLn :: r h (String -> M.IO ())
  ioMap :: r h ((a -> b) -> M.IO a -> M.IO b)
  ioPure :: r h (a -> M.IO a)
  ioAP :: r h (M.IO (a -> b) -> M.IO a -> M.IO b)
  ioBind :: r h (M.IO a -> (a -> M.IO b) -> M.IO b)
  ioBind = lam2 $ \m f -> join1 (map2 f m)
  ioJoin :: r h (M.IO (M.IO a) -> M.IO a)
  ioJoin = lam $ \m -> bind2 m id
  {-# MINIMAL putStrLn, ioMap, ioPure, ioAP, (ioBind | ioJoin) #-}

instance IO r => Functor r M.IO where
  map = ioMap

instance IO r => Applicative r M.IO where
  pure = ioPure
  ap = ioAP

instance IO r => Monad r M.IO where
  join = ioJoin
  bind = ioBind

putStrLn1 = app putStrLn
