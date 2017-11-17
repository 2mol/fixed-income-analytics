{-# LANGUAGE RecordWildCards #-}

module Instrument.CashFlow where

import Data.Time (Day)
-- import Data.List (zipWith4)
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
            calcTerms bond analysisDate

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
        -- zipWith4 CashFlow terms dates payments presentValues
        CashFlow <$> terms <$> dates <$> payments <$> presentValues

-- helper functions:

calcTerms :: BondDef -> Day -> [Double]
calcTerms = undefined

calcFlowDate :: Day -> Double -> Day
calcFlowDate = undefined

calcFixedCoupons :: Double -> [a] -> [Double]
calcFixedCoupons coupon terms = map (const coupon) terms ++ [1]

scale :: Double -> CashFlow -> CashFlow
scale scaleFactor cashFlow@CashFlow {nominalAmount = nom, presentValue = pv} =
    cashFlow {nominalAmount = nom * scaleFactor, presentValue = pv * scaleFactor}
