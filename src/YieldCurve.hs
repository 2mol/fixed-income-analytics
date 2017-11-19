module YieldCurve where

data YieldInfo = YieldInfo
    { tenor :: Double
    , yield :: Double
    } deriving (Show)

type YieldCurve = Double -> Double

interpolateYieldCurve :: [YieldInfo] -> YieldCurve
interpolateYieldCurve = undefined