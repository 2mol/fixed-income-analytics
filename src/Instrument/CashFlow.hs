{-# LANGUAGE RecordWildCards #-}

module Instrument.CashFlow where

import Data.Time (Day, addDays, diffDays)
-- import Data.List (zipWith4)
import Instrument.Bond
    ( BondDef(..)
    , CouponDef(..)
    )

data CashFlow = CashFlow
    { term :: Double
    -- , approxFlowDate :: Day
    , nominalAmount :: Double
    , presentValue :: Double
    } deriving Show

data CashFlows = CashFlows Day Double [CashFlow] deriving Show

constYEARDAYS :: Double
constYEARDAYS = 365.24219

calcFlowStream :: BondDef -> Day -> CashFlows
calcFlowStream bond@BondDef{..} analysisDate =
    CashFlows analysisDate principal $ calcFlows bond analysisDate

calcFlows :: BondDef -> Day -> [CashFlow]
calcFlows
    BondDef
        { maturityDate = maturityDate
        , frequency = frequency
        , lastPaymentDate = lastPaymentDate
        , mIssueDate = mIssueDate
        , couponInfo = couponInfo
        }
    analysisDate =
    let
        yearsToMaturity =
            dateDelta analysisDate maturityDate

        yearsToLastPayment =
            dateDelta analysisDate lastPaymentDate

        firstPaymentLowerBound =
            case mIssueDate of
                Nothing -> 0
                Just issueDate ->
                    max (dateDelta analysisDate issueDate) 0

        numberOfFlows =
            floor $ (yearsToLastPayment - firstPaymentLowerBound) * frequency

        yearsBetweenFlows =
            1 / frequency

        terms =
            scanl (-) yearsToLastPayment (replicate numberOfFlows yearsBetweenFlows)

        coupons =
            case couponInfo of
                Fixed coupon ->
                    calcFixedCoupons coupon terms
                Floating _ ->
                    undefined

        presentValues =
            repeat 667 -- TODO

        couponPayments =
            zipWith3 CashFlow terms coupons presentValues

        principalRedemption =
            CashFlow yearsToMaturity 1 666
    in
        principalRedemption : couponPayments

-- calcFlowsOld :: BondDef -> Day -> [CashFlow]
-- calcFlowsOld bond@BondDef{..} analysisDate =
--     let
--         terms =
--             calcFlowTermsOld bond analysisDate

--         coupons =
--             case couponInfo of
--                 Fixed coupon ->
--                     calcFixedCoupons coupon terms
--                 Floating _ ->
--                     undefined

--         payments = map (*principal) coupons

--         presentValues =
--             repeat 0 -- TODO
--     in
--         zipWith3 CashFlow terms payments presentValues
        -- CashFlow <$> terms <$> dates <$> payments <$> presentValues

-- helper functions:

-- calcFlowTermsOld :: BondDef -> Day -> [Double]
-- calcFlowTermsOld BondDef{..} analysisDate =
--     let
--         yearsBetweenFlows =
--             1 / frequency

--         yearsToMaturity =
--             dateDelta analysisDate maturityDate

--         yearsToLastPayment =
--             dateDelta analysisDate lastPaymentDate

--         firstPaymentLowerBound =
--             case mIssueDate of
--                 Nothing -> 0
--                 Just issueDate ->
--                     max (dateDelta analysisDate issueDate) 0

--         numberOfFlows =
--             floor $ (yearsToLastPayment - firstPaymentLowerBound) * frequency

--         couponTerms =
--             scanl (-) yearsToLastPayment (replicate numberOfFlows yearsBetweenFlows)
--     in
--         yearsToMaturity : couponTerms

dateDelta :: Day -> Day -> Double
dateDelta date1 date2 =
    -- dayDeltaToYearDelta $ diffDays date2 date1
    fromIntegral (diffDays date2 date1) / constYEARDAYS

-- dayDeltaToYearDelta :: Integer -> Double
-- dayDeltaToYearDelta = (/constYEARDAYS) . fromIntegral

-- calcFlowDate :: Day -> Double -> Day
-- calcFlowDate analysisDate term =
--     let
--         daysDelta = round (term * constYEARDAYS)
--     in
--         addDays daysDelta analysisDate

calcFixedCoupons :: Double -> [a] -> [Double]
calcFixedCoupons coupon terms = map (const coupon) terms ++ [1]

-- scale :: Double -> CashFlow -> CashFlow
-- scale scaleFactor cashFlow@CashFlow {nominalAmount = nom, presentValue = pv} =
--     cashFlow {nominalAmount = nom * scaleFactor, presentValue = pv * scaleFactor}
