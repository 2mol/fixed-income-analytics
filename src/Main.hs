module Main where

import qualified Instrument as I
-- import qualified YieldCurve as Y

main :: IO ()
main = do
  let c = I.Floating (Just 1) in
    putStrLn $ show c
