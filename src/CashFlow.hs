{-# LANGUAGE RecordWildCards #-}

module CashFlow where

import Data.Time (Day, addDays, diffDays)
import Bond
    ( BondDef(..)
    , CouponDef(..)
    )
import YieldCurve (YieldCurve)

data CashFlow = CashFlow
    { term :: Double
    -- , approxFlowDate :: Day
    , nominalAmount :: Double
    , presentValue :: Double
    } deriving Show

data CashFlows = CashFlows
    { analysisDate :: Day
    , principal :: Double
    , flows :: [CashFlow]
    } deriving Show

calcFlowStream :: BondDef -> Day -> YieldCurve -> CashFlows
calcFlowStream bond@BondDef{..} analysisDate yieldCurve =
    let
        couponTerms =
            calcTerms bond analysisDate

        flows =
            calcFlows couponInfo couponTerms maturityDate analysisDate yieldCurve

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

calcFlows :: CouponDef -> [Double] -> Day -> Day -> YieldCurve -> [CashFlow]
calcFlows couponInfo couponTerms maturityDate analysisDate yieldCurve =
    let
        yearsToMaturity =
            dateDelta analysisDate maturityDate

        coupons =
            case couponInfo of
                Fixed coupon ->
                    map (const coupon) couponTerms
                Floating _ ->
                    calcForwardRates couponTerms yieldCurve

        principalRedemption =
            CashFlow yearsToMaturity 1 666

        presentValues =
            undefined

        couponPayments =
            zipWith3 CashFlow couponTerms coupons presentValues
    in
        principalRedemption : couponPayments

calcForwardRates :: [Double] -> YieldCurve -> [Double]
calcForwardRates couponTerms yieldCurve =
    undefined

constYEARDAYS :: Double
constYEARDAYS = 365.24219

dateDelta :: Day -> Day -> Double
dateDelta date1 date2 =
    fromIntegral (diffDays date2 date1) / constYEARDAYS

calcFlowDay :: Day -> Double -> Day
calcFlowDay analysisDate term =
    let
        days = round $ term * constYEARDAYS
    in
        addDays days analysisDate
