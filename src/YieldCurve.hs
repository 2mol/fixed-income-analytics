module YieldCurve where

import Types

data YieldInfo = YieldInfo
    { tenor :: YearDelta
    , yield :: InterestRate
    } deriving (Show)

type YieldCurve = Double -> Double

interpolateYieldCurve :: [YieldInfo] -> YieldCurve
interpolateYieldCurve = undefined

calcForwardRates :: [Double] -> YieldCurve -> [Double]
calcForwardRates couponTerms yieldCurve =
    undefined

discount :: CashNominal -> InterestRate -> CashPV
discount = undefined
