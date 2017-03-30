module Util where

vars = [pre : suf | suf <- "":map show [0..], pre <- ['a'..'z']]

isSquare n = sq * sq == n
  where sq = floor $ sqrt (fromIntegral n::Double)