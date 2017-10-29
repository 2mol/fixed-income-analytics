module YieldCurve where


newtype Tenor = Tenor Double deriving (Eq, Show)
newtype Yield = Yield Double deriving (Eq, Show)

data YieldDataPoint = YieldDataPoint
    { tenor :: Tenor
    , yield :: Yield
    } deriving (Eq, Show)

-- data Currency = CHF | USD | EUR | CAD | JPY
--     deriving (Eq, Enum, Show, Read)

type YieldCurveData = [YieldDataPoint]

--data YieldCurveData = YieldCurveData
    --{ currency :: Currency
    --, date :: Date
    --, dataPoints :: [YieldDataPoint]
    --}

type YieldCurve = Tenor -> Yield

data InterpolationType = Linear | Quadratic

interpolateYieldCurve :: InterpolationType -> YieldCurveData -> YieldCurve
interpolateYieldCurve = undefined
