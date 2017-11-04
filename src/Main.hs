module Main where

import Text.Pretty.Simple (pPrint)
import Data.Time (Day, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

import Instrument
    ( BondDef(..)
    , Coupon(..)
    , Maturity(..)
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
        , coupon = Fixed 0.0125
        , frequency = 2.0
        , maturity = MaturityDate maturityDate
        , issue = Nothing
        , nextPayment = Nothing
        , lastPayment = Just maturityDate
        }

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    putStrLn $ "today is: " ++ show today
    -- let maturityDate = fromGregorian 2022 12 31
    --     bond =
    --         BondDef
    --             { bId = 0
    --             , coupon = Fixed 0.0125
    --             , frequency = 2.0
    --             , maturity = MaturityDate maturityDate
    --             , issue = Nothing
    --             , nextPayment = Nothing
    --             , lastPayment = Just maturityDate
    --             }
    pPrint vanillaBond
