module Main where

import qualified Data.Time as T
import Text.Pretty.Simple (pPrint)

import Instrument
    ( BondDef(..)
    , Coupon(..)
    , Maturity(..)
    )
-- import qualified YieldCurve as Y

main :: IO ()
main = do
    let
        maturityDate = T.fromGregorian 2022 12 31
        bond =
            BondDef
                { bId = 666
                , coupon = Fixed 0.0125
                , frequency = 2.0
                , maturity = MaturityDate maturityDate
                , issueDate = Nothing
                , lastPaymentDate = Just maturityDate
                }
    pPrint bond
