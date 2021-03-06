{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest (docTestOpts ++ ["-isrc","src/CLaSH/Prelude.hs"]) >>
       doctest (docTestOpts ++ ["src/CLaSH/Tutorial.hs"]) >>
       doctest (docTestOpts ++ ["src/CLaSH/Examples.hs"])

docTestOpts :: [String]
docTestOpts =
#if __GLASGOW_HASKELL__ >= 802
  ["-fdiagnostics-color=never"]
#else
  []
#endif
