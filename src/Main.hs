module Main where

import Text.Pretty.Simple (pPrint)
import Data.Time (Day, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

import Instrument
    ( BondDef(..)
    , CouponDef(..)
    , Maturity(..)
    , calcCashFlows
    )
-- import qualified YieldCurve as Y

maturityDate :: Day
maturityDate = fromGregorian 2022 12 31

analysisDate :: Day
analysisDate = fromGregorian 2017 11 4

vanillaBond :: BondDef
vanillaBond =
    BondDef
        { bId = 0
        , couponInfo = Fixed 0.0125
        , frequency = 2.0
        , maturity = MaturityDate maturityDate
        , issue = Nothing
        }

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    putStrLn $ "today is: " ++ show today
    pPrint vanillaBond
    let cashFlows = calcCashFlows vanillaBond 100 today
    pPrint cashFlows
