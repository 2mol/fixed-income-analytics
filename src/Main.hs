module Main where

import Text.Pretty.Simple (pPrint)
import Data.Time (fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

import Instrument
    ( BondDef(..)
    , Coupon(..)
    , Maturity(..)
    )
-- import qualified YieldCurve as Y

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    putStrLn $ "today is: " ++ show today
    let maturityDate = fromGregorian 2022 12 31
        bond =
            BondDef
                { bId = 0
                , coupon = Fixed 0.0125
                , frequency = 2.0
                , maturity = MaturityDate maturityDate
                , issue = Nothing
                , lastPayment = Just maturityDate
                }
    pPrint bond
