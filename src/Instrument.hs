{-# LANGUAGE RecordWildCards #-}

module Instrument where

import Data.List (zipWith4)
import Data.Time
    ( Day
    , diffDays
    , addDays
    )

-- | Set up some data for DocTest
-- $setup
-- >>> import Data.Time (fromGregorian)
-- :{
-- maturityDate = fromGregorian 2022 12 31
-- vanillaBond =
--     BondDef
--         { bId = 0
--         , couponInfo = Fixed 0.0125
--         , frequency = 2.0
--         , maturity = MaturityDate maturityDate
--         , issue = Nothing
--         , nextPayment = Nothing
--         }
-- :}

type CouponAmount = Double
type Frequency = Double
type BondPrice = Double
type Spread = Double
type Yield = Double
type Exposure = Double
type YearDelta = Double

const_YEAR_DAYS :: Double
const_YEAR_DAYS = 365.2422

data Currency = CHF | USD | EUR | CAD | JPY
    deriving (Eq, Show, Read)

data CouponDef = Fixed CouponAmount | Floating (Maybe CouponAmount)
    deriving (Show)

data Maturity = MaturityDate Day | Perpetual (Maybe Day)
    deriving (Eq, Show)

data BondDef = BondDef
    { bId :: Int
    , couponInfo :: CouponDef
    , frequency :: Frequency
    , maturity :: Maturity
    , issue :: Maybe Day
    , nextPayment :: Maybe Day
    } deriving Show

data CashFlow = CashFlow
    { cTerm :: YearDelta
    , approxFlowDate :: Day
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

analyzeBond ::
    BondDef
    -> Day
    -> Maybe Exposure
    -> Maybe PricingInfo
    -- -> YieldCurve
    -> AnalyzedBond
analyzeBond bondDef analysisDate mExposure mPricigInfo =
    let
        (aPrice, aZSpread, aytm) =
            undefined bondDef mPricigInfo
    in
        AnalyzedBond
            { aId = bId bondDef
            , cashFlows = undefined
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

--

-- | Calculates the time from the analysisDate to each cashflow.
-- Term refers to the timespan, measured in years.
--
-- >>> calcFlowTerms vanillaBond (fromGregorian 2022 12 31)
-- [0.0]
--
-- >>> calcFlowTerms vanillaBond (fromGregorian 2023 1 1)
-- []
calcFlowTerms :: BondDef -> Day -> [YearDelta]
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
calcFlowDay analysisDate term =
    addDays days analysisDate
    where
        days =
            round $ term * const_YEAR_DAYS

-- data BondDef = BondDef
-- { bId :: Int
-- , coupon :: Coupon
-- , frequency :: Frequency
-- , maturity :: Maturity
-- , issue :: Maybe Day
-- , nextPayment :: Maybe Day
-- } deriving Show

calcCouponAmounts :: CouponDef -> [YearDelta] -> [CouponAmount]
calcCouponAmounts couponInfo terms =
    case couponInfo of
        Fixed coupon ->
            calcFixedCouponAmounts coupon (length terms)
        -- Floating mNextCouponAmount ->
        Floating _ ->
            undefined

calcFixedCouponAmounts :: CouponAmount -> Int -> [CouponAmount]
calcFixedCouponAmounts coupon n =
    if n <= 0 then
        []
    else
        take (n-1) (repeat coupon) ++ [1 + coupon]

calcCashFlows :: BondDef -> Day -> CashFlows
calcCashFlows BondDef{..} analysisDate =
    let
        terms =
            calcFlowTerms BondDef{..} analysisDate

        dates =
            map (calcFlowDay analysisDate) terms

        nominalValues =
            calcCouponAmounts couponInfo terms

        presentValues =
            take (length terms) (repeat 0.0)
    in
        -- CashFlow <$> terms <*> dates <*> nominalValues <*> presentValues
        zipWith4 CashFlow terms dates nominalValues presentValues




-- calcPricing :: BondDef -> Maybe PricingInfo -> (BondPrice, Spread, Yield)
-- calcPricing = undefined
