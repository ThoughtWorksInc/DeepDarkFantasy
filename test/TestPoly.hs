module Main where

import DDF.Poly hiding (main)
import Control.Monad
import System.Exit (exitFailure)

main :: IO ()
main = do
  x <- solve (const $ return ()) (const . const $ return ())
  unless (x - 4 < 0.1) exitFailure
  return ()