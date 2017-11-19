{-# LANGUAGE RecordWildCards #-}

module CashFlow where

import Data.Time (Day, diffDays)
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

        undiscountedFlows =
            case couponInfo of
                Fixed coupon ->
                    calcFixedFlows coupon couponTerms maturityDate analysisDate
                Floating _ ->
                    undefined

        flows = map ($ 666) undiscountedFlows
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

calcFixedFlows :: Double -> [Double] -> Day -> Day -> [Double -> CashFlow]
calcFixedFlows coupon couponTerms maturityDate analysisDate =
    let
        yearsToMaturity =
            dateDelta analysisDate maturityDate

        coupons =
            map (const coupon) couponTerms

        principalRedemption =
            CashFlow yearsToMaturity 1

        couponPayments =
            zipWith CashFlow couponTerms coupons
    in
        principalRedemption : couponPayments

constYEARDAYS :: Double
constYEARDAYS = 365.24219

dateDelta :: Day -> Day -> Double
dateDelta date1 date2 =
    fromIntegral (diffDays date2 date1) / constYEARDAYS
