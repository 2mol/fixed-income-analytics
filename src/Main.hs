module Main where

import Text.Pretty.Simple (pPrint)
import Data.Time (Day, fromGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)

-- import InstrumentOld
--     ( BondDef(..)
--     , CouponDef(..)
--     , calcCashFlows
--     )
-- import qualified CsvLoader as C
import Types
import Bond (BondDef(..), CouponDef(..))
import CashFlow (calcCashFlows)

mDate :: Day
mDate = fromGregorian 2022 12 31

aDate :: Day
aDate = fromGregorian 2017 11 4

vanillaBond :: BondDef
vanillaBond =
    BondDef
        { couponDef = Fixed (CashNominal 0.0125)
        , frequency = 2.0
        , maturityDate = mDate
        , mIssueDate = Nothing
        , lastPaymentDate = mDate
        , principal = 100
        }

fakeYieldCurve :: YearDelta -> InterestRate
fakeYieldCurve = const (InterestRate 0)

cashFlowTest :: IO ()
cashFlowTest = do
    now <- getCurrentTime
    let today = utctDay now
    putStrLn $ "today is: " ++ show today
    pPrint vanillaBond
    let cashFlows = calcCashFlows vanillaBond today fakeYieldCurve
    pPrint cashFlows

-- csvTest = C.main

main :: IO ()
main = cashFlowTest
