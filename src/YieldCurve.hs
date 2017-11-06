module YieldCurve where


newtype Tenor = Tenor Double deriving (Show)
newtype Yield = Yield Double deriving (Show)

data YieldDataPoint = YieldDataPoint
    { tenor :: Tenor
    , yield :: Yield
    } deriving (Show)

type YieldCurve = Tenor -> Yield

interpolateYieldCurve :: [YieldDataPoint] -> YieldCurve
interpolateYieldCurve = undefined
