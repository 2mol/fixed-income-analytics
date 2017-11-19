{-# LANGUAGE RecordWildCards #-}

module Instrument.CashFlow where

import Data.Time (Day, diffDays)
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
    let
        couponTerms =
            calcTerms bond analysisDate

        flows =
            case couponInfo of
                Fixed coupon ->
                    calcFixedFlows coupon couponTerms maturityDate analysisDate
                Floating _ ->
                    undefined
    in
        CashFlows analysisDate principal flows

calcTerms :: BondDef -> Day -> [Double]
calcTerms
    BondDef
        { frequency = frequency
        , lastPaymentDate = lastPaymentDate
        , mIssueDate = mIssueDate
        }
    analysisDate =
    let
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
    in
        scanl (-) yearsToLastPayment (replicate numberOfFlows yearsBetweenFlows)

calcFixedFlows :: Double -> [Double] -> Day -> Day -> [CashFlow]
calcFixedFlows coupon couponTerms maturityDate analysisDate =
    let
        yearsToMaturity =
            dateDelta analysisDate maturityDate

        coupons =
            map (const coupon) couponTerms

        presentValues =
            repeat 667 -- TODO

        principalRedemption =
            CashFlow yearsToMaturity 1 666

        couponPayments =
            zipWith3 CashFlow couponTerms coupons presentValues
    in
        principalRedemption : couponPayments

dateDelta :: Day -> Day -> Double
dateDelta date1 date2 =
    fromIntegral (diffDays date2 date1) / constYEARDAYS
