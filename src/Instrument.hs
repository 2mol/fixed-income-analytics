-- {-# LANGUAGE DuplicateRecordFields #-}

module Instrument where

import Data.Time (Day)
-- import qualified Data.Map as Map
-- import Data.Map (Map)

type CouponAmount = Double
type Frequency = Double
type Price = Double
type Spread = Double
type Yield = Double

data Coupon = Fixed CouponAmount | Floating (Maybe CouponAmount)
    deriving (Show)

-- data Frequency = Annual | SemiAnnual | Quarterly
--     deriving (Eq, Ord, Show, Read)

-- frequencyMap :: Map Frequency Double
-- frequencyMap = Map.fromList
--     [ (Annual, 1.0)
--     , (SemiAnnual, 2.0)
--     , (Quarterly, 4.0)
--     ]


data Maturity = MaturityDate Day | Perpetual
    deriving (Eq, Show)

data BondDef = BondDef
    { bId :: Int
    , coupon :: Coupon
    , frequency :: Frequency
    , maturity :: Maturity
    , issueDate :: Maybe Day
    , lastPaymentDate :: Maybe Day
    } deriving (Show)

data CashFlow = CashFlow
    { flowDate :: Day
    , nominalAmount :: Double
    }

data KeyRateDuration = KeyRateDuration
    { tenor :: Double
    , krd :: Double
    }

data AnalyzedBond = AnalyzedBond
    { aId :: Int
    -- bondDef :: BondDef
    -- , Principal
    -- , Exposure
    , cashFlows :: [CashFlow]
    , price :: Price
    , zSpread :: Spread
    , ytm :: Yield
    , accruedInterest :: Double
    , cleanPrice :: Double
    , fisherWeilDuration :: Double
    , creditDuration :: Double
    , durationToWorst :: Double
    , keyRateDurations :: [KeyRateDuration]
    }

newtype Exposure = Exposure Double
data PricingInfo = ZSpread Double | Price Double | YTM Double

analyzeBond ::
    BondDef
    -- -> Exposure
    -> Maybe PricingInfo
    -- -> YieldCurve
    -> AnalyzedBond
analyzeBond bondDef pricigInfo =
    let
        aCashFlows =
            calcCashFlows bondDef

        (aPrice, aZSpread, aytm) =
            calcPricing bondDef pricigInfo
    in
        AnalyzedBond
            { aId = bId bondDef
            , cashFlows = aCashFlows
            , price = aPrice
            , zSpread = aZSpread
            , ytm = aytm
            , accruedInterest = undefined
            , cleanPrice = undefined
            , fisherWeilDuration = undefined
            , creditDuration = undefined
            , durationToWorst = undefined
            , keyRateDurations = undefined
            }

calcCashFlows :: BondDef -> [CashFlow]
calcCashFlows = undefined

calcPricing :: BondDef -> Maybe PricingInfo -> (Price, Spread, Yield)
calcPricing = undefined

-- data Currency = CHF | USD | EUR | CAD | JPY
--     deriving (Eq, Show, Read)


--

-- price :: BondDef -> ZSpread -> Price
-- price = undefined

-- yield :: BondDef -> ZSpread -> YieldToMaturity
-- yield = undefined

-- spread :: BondDef -> Price -> ZSpread
-- spread = undefined