module YieldCurve where

import Types

data YieldData = YieldData
    { tenor :: YearDelta
    , yield :: InterestRate
    } deriving (Show)

type YieldCurve = YearDelta -> InterestRate

interpolate :: [YieldData] -> YieldCurve
interpolate = undefined

-- calcForwardRates :: [YearDelta] -> YieldCurve -> [Double]
-- calcForwardRates couponTerms yieldCurve =
--     undefined

discount :: CashNominal -> InterestRate -> CashPV
discount = undefined
