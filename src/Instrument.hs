{-# LANGUAGE DuplicateRecordFields #-}

module Instrument where

import Data.Time (Day)
-- import YieldCurve

data Currency = CHF | USD | EUR | CAD | JPY
    deriving (Eq, Show, Read)

-- data BondType = Fixed | Floating

data FixedBond = FixedBond
    { coupon :: Double
    , frequency :: Double
    , maturity :: Day
    , issueDate :: Maybe Day
    }

data FloatingBond = FloatingBond
    { nextCoupon :: Maybe Double
    , frequency :: Double
    , maturity :: Day
    , issueDate :: Maybe Day
    }
    
-- data BondInput = ...

-- data Bond = FixedBond | FloatingBond

-- type Instrument = [Bond]
