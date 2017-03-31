module Main where

import Poly hiding (main)
import Xor hiding (main)
import Control.Monad
import System.Exit (exitFailure)
import System.Random

main :: IO ()
main = do
  g <- getStdGen
  xor <- findXor g (const $ return ()) (const . const . const $ return ())
  let doXor :: Double -> Double -> Double -> IO ()
      doXor l r ret = unless (xor (l, r) - ret < 0.2) exitFailure
  doXor 0 0 0
  doXor 0 1 1
  doXor 1 0 1
  doXor 1 1 0
  return ()
