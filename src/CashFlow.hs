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
    }

data CashFlows = CashFlows
    { analysisDate :: Day
    , principal :: CashNominal
    , flows :: [CashFlow]
    }

calcCashFlows :: BondDef -> Day -> YieldCurve -> CashFlows
calcCashFlows bond@BondDef{..} analysisDate yieldCurve =
    let
        couponTerms =
            calcTerms bond analysisDate

        flows =
            reverse $
            calcFlowList
                couponDef
                couponTerms
                maturityDate
                analysisDate
                yieldCurve
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

        timeSpan =
            yearsToLastPayment - firstPaymentLowerBound

        numberOfFlows =
            floor $ unYearDelta timeSpan * frequency

        yearsBetweenFlows =
            YearDelta $ 1 / frequency
    in
        scanl (-) yearsToLastPayment (replicate numberOfFlows yearsBetweenFlows)

calcFlowList :: CouponDef -> [YearDelta] -> Day -> Day -> YieldCurve -> [CashFlow]
calcFlowList couponDef couponTerms maturityDate analysisDate yieldCurve =
    let
        yearsToMaturity =
            dateDelta analysisDate maturityDate

        couponPayments =
            case couponDef of
                Fixed coupon ->
                    calcFixedCoupons coupon couponTerms
                    -- map (CashNominal . unInterestRate . const coupon) couponTerms
                Floating nextCoupon ->
                    calcFloatingCoupons nextCoupon couponTerms yieldCurve

        principalRedemption =
            CashFlow yearsToMaturity 1 666

        couponPresentValues =
            repeat 667

        coupons =
            zipWith3 CashFlow couponTerms couponPayments couponPresentValues
    in
        principalRedemption : coupons

calcFixedCoupons :: InterestRate -> [a] -> [CashNominal]
calcFixedCoupons coupon =
    let
        couponAmount =
            CashNominal $ unInterestRate coupon
    in
        map (const couponAmount)

calcFloatingCoupons :: InterestRate -> [YearDelta] -> YieldCurve -> [CashNominal]
calcFloatingCoupons nextCoupon couponTerms yieldCurve =
    let
        bla =
            undefined
    in
        undefined

-- small helper functions:

constYEARDAYS :: Double
constYEARDAYS = 365.24219

dateDelta :: Day -> Day -> YearDelta
dateDelta date1 date2 =
    YearDelta $ fromIntegral (diffDays date2 date1) / constYEARDAYS

calcFlowDay :: Day -> YearDelta -> Day
calcFlowDay analysisDate term =
    let
        days = round $ unYearDelta term * constYEARDAYS
    in
        addDays days analysisDate


-- some show instances

-- data CF =
--     CF
--     { d :: Day
--     , nom :: CashNominal
--     , pv :: CashPV
--     } deriving Show

-- showFromCashFlow :: Day -> CashNominal -> CashFlow -> CF
-- showFromCashFlow ad principal CashFlow{..} =
--     CF
--         { d = calcFlowDay ad term
--         , nom = nominalAmount * principal
--         , pv = presentValue * CashPV (unCashNominal principal)
--         }

instance Show CashFlow where
    show CashFlow{..} =
        let
            nom = show nominalAmount
            pv = show presentValue
            t = show term
        in
            unwords [t, "~", nom, "~ PV:", pv]

instance Show CashFlows where
    show CashFlows{..} =
        "\nAnalysis Date: " ++ show analysisDate
        ++ "\nPrincipal: " ++ show principal
        -- ++ "\nFlows:" ++ show (map (showFromCashFlow analysisDate principal) flows)
        ++ "\nFlows:" ++ show flows
