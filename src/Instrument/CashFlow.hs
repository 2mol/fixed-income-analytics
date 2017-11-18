{-# LANGUAGE RecordWildCards #-}

module Instrument.CashFlow where

import Data.Time (Day, addDays, diffDays)
import Data.List (zipWith4)
import Instrument.Bond
    ( BondDef(..)
    , CouponDef(..)
    )

data CashFlow = CashFlow
    { term :: Double
    , approxFlowDate :: Day
    , nominalAmount :: Double
    , presentValue :: Double
    }

constYEARDAYS :: Double
constYEARDAYS = 365.24219

calcFlows :: BondDef -> Day -> [CashFlow]
calcFlows bond@BondDef{..} analysisDate =
    let
        terms =
            calcFlowTerms bond analysisDate

        dates =
            map (calcFlowDate analysisDate) terms

        coupons =
            case couponInfo of
                Fixed coupon ->
                    calcFixedCoupons coupon terms
                Floating _ ->
                    undefined

        payments = map (*principal) coupons

        presentValues =
            repeat 0 -- TODO
    in
        zipWith4 CashFlow terms dates payments presentValues
        -- CashFlow <$> terms <$> dates <$> payments <$> presentValues

-- helper functions:

calcFlowTerms :: BondDef -> Day -> [Double]
calcFlowTerms BondDef{..} analysisDate =
    let
        yearsBetweenFlows =
            1 / frequency

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

        couponTerms =
            scanl (-) yearsToLastPayment (replicate numberOfFlows yearsBetweenFlows)
    in
        yearsToMaturity : couponTerms

dateDelta :: Day -> Day -> Double
dateDelta date1 date2 =
    daysToYears $ diffDays date2 date1

daysToYears :: Integer -> Double
daysToYears = (/constYEARDAYS) . fromIntegral

calcFlowDate :: Day -> Double -> Day
calcFlowDate analysisDate term =
    let
        daysDelta = round (term * constYEARDAYS)
    in
        addDays daysDelta analysisDate

calcFixedCoupons :: Double -> [a] -> [Double]
calcFixedCoupons coupon terms = map (const coupon) terms ++ [1]

-- scale :: Double -> CashFlow -> CashFlow
-- scale scaleFactor cashFlow@CashFlow {nominalAmount = nom, presentValue = pv} =
--     cashFlow {nominalAmount = nom * scaleFactor, presentValue = pv * scaleFactor}
