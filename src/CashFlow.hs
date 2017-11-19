{-# LANGUAGE RecordWildCards #-}

module CashFlow where

import Data.Time (Day, addDays, diffDays)
import Types
import Bond
    ( BondDef(..)
    , CouponDef(..)
    )
import YieldCurve (YieldCurve)

data CashFlow = CashFlow
    { term :: YearDelta
    -- , approxFlowDate :: Day
    , nominalAmount :: CashNominal
    , presentValue :: CashPV
    } deriving Show

data CashFlows = CashFlows
    { analysisDate :: Day
    , principal :: CashNominal
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

calcTerms :: BondDef -> Day -> [YearDelta]
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
                Nothing -> YearDelta 0
                Just issueDate ->
                    max (dateDelta analysisDate issueDate) 0

        numberOfFlows =
            floor $ unYearDelta (yearsToLastPayment - firstPaymentLowerBound) * frequency

        yearsBetweenFlows =
            YearDelta $ 1 / frequency
    in
        scanl (-) yearsToLastPayment (replicate numberOfFlows yearsBetweenFlows)

calcFlows :: CouponDef -> [YearDelta] -> Day -> Day -> YieldCurve -> [CashFlow]
calcFlows couponInfo couponTerms maturityDate analysisDate yieldCurve =
    let
        yearsToMaturity =
            dateDelta analysisDate maturityDate

        coupons =
            case couponInfo of
                Fixed coupon ->
                    map (const coupon) couponTerms
                Floating _ ->
                    calcFloatingCoupons couponTerms yieldCurve

        principalRedemption =
            CashFlow yearsToMaturity 1 666

        presentValues =
            undefined

        couponPayments =
            zipWith3 CashFlow couponTerms coupons presentValues
    in
        principalRedemption : couponPayments

calcFloatingCoupons :: a
calcFloatingCoupons = undefined

constYEARDAYS :: Double
constYEARDAYS = 365.24219

dateDelta :: Day -> Day -> YearDelta
dateDelta date1 date2 =
    YearDelta $ fromIntegral (diffDays date2 date1) / constYEARDAYS

calcFlowDay :: Day -> YearDelta -> Day
calcFlowDay analysisDate term =
    let
        days = round $ (unYearDelta term) * constYEARDAYS
    in
        addDays days analysisDate
