{-# LANGUAGE RecordWildCards #-}

module Instrument where

import Data.Time
    ( Day
    , diffDays
    , addDays
    , addGregorianYearsClip
    )

-- import qualified Data.Time as T
-- import qualified Data.Map as Map
-- import Data.Map (Map)
-- import Data.Maybe (fromMaybe)

-- $setup
-- >>> import Data.Time (fromGregorian)
-- :{
-- maturityDate = fromGregorian 2022 12 31
-- vanillaBond =
--     BondDef
--         { bId = 0
--         , coupon = Fixed 0.0125
--         , frequency = 2.0
--         , maturity = MaturityDate maturityDate
--         , issue = Nothing
--         , nextPayment = Nothing
--         , lastPayment = Just maturityDate
--         }
-- :}

type CouponAmount = Double
type Frequency = Double
type BondPrice = Double
type Spread = Double
type Yield = Double
type Exposure = Double
type YearDelta = Double

-- newtype TestYD = TestYD Double deriving Num

const_YEAR_DAYS :: Double
const_YEAR_DAYS = 365.2422

data Currency = CHF | USD | EUR | CAD | JPY
    deriving (Eq, Show, Read)

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

data Maturity = MaturityDate Day | Perpetual (Maybe Day)
    deriving (Eq, Show)

data BondDef = BondDef
    { bId :: Int
    , coupon :: Coupon
    , frequency :: Frequency
    , maturity :: Maturity
    , issue :: Maybe Day
    , nextPayment :: Maybe Day
    , lastPayment :: Maybe Day
    } deriving Show

data CashFlow = CashFlow
    { approxFlowDate :: Day
    , cTerm :: Integer
    , nominalAmount :: Double
    , presentValue :: Double
    } deriving Show

-- data CashFlows = CashFlows Day Exposure [CashFlow] deriving Show
type CashFlows = [CashFlow]

data KeyRateDuration = KeyRateDuration
    { kTerm :: Double
    , krd :: Double
    } deriving Show

data AnalyzedBond = AnalyzedBond
    { aId :: Int
    , cashFlows :: CashFlows
    , price :: BondPrice
    , zSpread :: Spread
    , ytm :: Yield
    , accruedInterest :: Double
    , cleanPrice :: Double
    , fisherWeilDuration :: Double
    , creditDuration :: Double
    , durationToWorst :: Double
    , keyRateDurations :: [KeyRateDuration]
    } deriving Show

data PricingInfo = ZSpread Spread | Price BondPrice | YTM Yield

-- analyzeBond ::
--     BondDef
--     -> Day
--     -> Maybe Exposure
--     -> Maybe PricingInfo
--     -- -> YieldCurve
--     -> AnalyzedBond
-- analyzeBond bondDef analysisDate mExposure mPricigInfo =
--     let
--         (aPrice, aZSpread, aytm) =
--             undefined bondDef mPricigInfo
--     in
--         AnalyzedBond
--             { aId = bId bondDef
--             , cashFlows = undefined
--             , price = aPrice
--             , zSpread = aZSpread
--             , ytm = aytm
--             , accruedInterest = undefined
--             , cleanPrice = undefined
--             , fisherWeilDuration = undefined
--             , creditDuration = undefined
--             , durationToWorst = undefined
--             , keyRateDurations = undefined
--             }

-- | Calculates the time from the analysisDate to each cashflow.
-- Term refers to the timespan, measured in years.
--
-- >>> calcFlowTerms vanillaBond (fromGregorian 2022 12 31)
-- [0.0]
--
-- >>> calcFlowTerms vanillaBond (fromGregorian 2023 1 1)
-- []
calcFlowTerms :: BondDef -> Day -> [Double]
calcFlowTerms BondDef{..} analysisDate =
    let
        yearsBetweenFlows =
            1 / frequency

        yearsToMaturity =
            case maturity of
                MaturityDate day ->
                    dateDelta analysisDate day
                Perpetual _ ->
                    666

        lowerBound =
            case issue of
                Nothing -> 0
                Just issueDate ->
                    max (dateDelta analysisDate issueDate) 0

        numberOfFlows =
            floor $ (yearsToMaturity - lowerBound) * frequency
    in
        if yearsToMaturity < 0 then
            []
        else
            scanl (-) yearsToMaturity (take numberOfFlows (repeat yearsBetweenFlows))

daysToYears :: Integer -> YearDelta
daysToYears = (/const_YEAR_DAYS) . fromIntegral

dateDelta :: Day -> Day -> YearDelta
dateDelta date1 date2 =
    daysToYears $ diffDays date2 date1

calcFlowDay :: Day -> YearDelta -> Day
calcFlowDay analysisDate term = addDays (round term) analysisDate

-- data BondDef = BondDef
-- { bId :: Int
-- , coupon :: Coupon
-- , frequency :: Frequency
-- , maturity :: Maturity
-- , issue :: Maybe Day
-- , nextPayment :: Maybe Day
-- , lastPayment :: Maybe Day
-- } deriving Show

calcCashFlows :: BondDef -> Day -> CashFlows
calcCashFlows BondDef{..} analysisDate =
    let
        maturityDate =
            case maturity of
                MaturityDate day -> day
                Perpetual (Just nextPaymentDay) -> addGregorianYearsClip 1000 analysisDate

        terms =
            calcFlowTerms BondDef{..} analysisDate

        dates =
            map (calcFlowDay analysisDate) terms

        tuples =
            zip3
    in
        undefined




-- calcPricing :: BondDef -> Maybe PricingInfo -> (BondPrice, Spread, Yield)
-- calcPricing = undefined
