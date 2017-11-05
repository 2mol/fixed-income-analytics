{-# LANGUAGE RecordWildCards #-}

module Instrument where

import Data.List (zipWith4)
import Data.Time
    ( Day
    , diffDays
    , addDays
    )
import Numeric (showFFloat)

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
type Principal = Int
type YearDelta = Double

constYEARDAYS :: Double
constYEARDAYS = 365.2422

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
    }

instance Show CashFlow where
    show CashFlow{..} =
        show approxFlowDate ++ " ~ " ++ nom ++ ", PV: " ++ pv
        where
            nom = showFFloat (Just 2) nominalAmount ""
            pv = showFFloat (Just 2) presentValue ""

data CashFlows = CashFlows Day Principal [CashFlow]

instance Show CashFlows where
    show (CashFlows ad principal cfs) =
        "Analysis Date: " ++ show ad ++ "\n"
        ++ "Principal: " ++ show principal ++ "\n"
        ++ flows
        where
            flows =
                if null cfs then
                    "No flows from analysis date onwards\n"
                else
                    concatMap (\c -> show c ++ "\n") cfs


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
    -> Maybe Principal
    -> Maybe PricingInfo
    -- -> YieldCurve
    -> AnalyzedBond
analyzeBond bondDef analysisDate mPrincipal mPricigInfo =
    let
        (aPrice, aZSpread, aytm) =
            undefined bondDef mPricigInfo
    in
        AnalyzedBond
            { aId = bId bondDef
            , cashFlows = calcCashFlows bondDef analysisDate
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
            reverse $ scanl (-) yearsToMaturity (replicate numberOfFlows yearsBetweenFlows)

daysToYears :: Integer -> YearDelta
daysToYears = (/constYEARDAYS) . fromIntegral

dateDelta :: Day -> Day -> YearDelta
dateDelta date1 date2 =
    daysToYears $ diffDays date2 date1

calcFlowDay :: Day -> YearDelta -> Day
calcFlowDay analysisDate term =
    addDays days analysisDate
    where
        days =
            round $ term * constYEARDAYS

calcFixedCouponAmounts :: CouponAmount -> Int -> [CouponAmount]
calcFixedCouponAmounts coupon n =
    if n <= 0 then
        []
    else
        replicate (n - 1) coupon ++ [1 + coupon]

scaleCashFlow :: Principal -> CashFlow -> CashFlow
scaleCashFlow principal cf@CashFlow {nominalAmount = nom, presentValue = pv} =
    cf {nominalAmount = nom * principalF, presentValue = pv * principalF}
    where
        principalF = fromIntegral principal

calcCashFlows :: BondDef -> Day -> CashFlows
calcCashFlows BondDef{..} analysisDate =
    let
        terms =
            calcFlowTerms BondDef{..} analysisDate

        dates =
            map (calcFlowDay analysisDate) terms

        nominalValues =
            case couponInfo of
                Fixed coupon ->
                    calcFixedCouponAmounts coupon (length terms)
                Floating _ ->
                    undefined

        presentValues =
            repeat 0 -- TODO

        cashFlowList =
            zipWith4 CashFlow terms dates nominalValues presentValues

        principal = 100

        scaledCashFlowList =
            map (scaleCashFlow principal) cashFlowList
    in
        -- CashFlow <$> terms <*> dates <*> nominalValues <*> presentValues
        CashFlows analysisDate principal scaledCashFlowList




-- calcPricing :: BondDef -> Maybe PricingInfo -> (BondPrice, Spread, Yield)
-- calcPricing = undefined
