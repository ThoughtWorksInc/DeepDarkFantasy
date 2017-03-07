module Util where

vars = [pre : suf | suf <- "":map show [0..], pre <- ['a'..'z']]
