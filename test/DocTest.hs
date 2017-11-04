module DocTest where

import Test.DocTest (doctest)
-- import System.FilePath.Glob (glob)

main :: IO ()
-- main = glob "src/*.hs" >>= doctest
main = doctest ["-isrc", "src/Instrument.hs"]