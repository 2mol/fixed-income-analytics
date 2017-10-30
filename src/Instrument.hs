{-# LANGUAGE DuplicateRecordFields #-}

module Instrument where

import Data.Time (Day)
import qualified Data.Map as Map
import Data.Map (Map)

data Currency = CHF | USD | EUR | CAD | JPY
    deriving (Eq, Show, Read)

data CouponType = Fixed | Floating
    deriving (Show)

newtype Coupon = Coupon Double
    deriving (Show)
    
newtype ZSpread = ZSpread Double
    deriving (Show)

data Frequency = Annual | SemiAnnual | Quarterly
    deriving (Eq, Ord, Show, Read)

frequencyMap :: Map Frequency Double
frequencyMap = Map.fromList
    [ (Annual, 1.0)
    , (SemiAnnual, 2.0)
    , (Quarterly, 4.0)
    ]

data Maturity = Date Day | Perpetual
    deriving (Eq, Show)

data BondDef = BondDef
    { couponType :: CouponType
    , coupon :: Coupon
    , frequency :: Frequency
    , maturity :: Maturity
    , issueDate :: Maybe Day
    , zSpread :: Double
    } deriving (Show)

newtype Exposure = Exposure Double

data Instrument
    = Bond BondDef
    | BondFuture BondDef BondDef
    | InterestRateSwap BondDef BondDef
    | Composite [Instrument]

-- bondFromRawData :: Bla -> Bond

-- data FixedBond = FixedBond
--     { coupon :: Double
--     , frequency :: Double
--     , maturity :: Day
--     , issueDate :: Maybe Day
--     }

-- data FloatingBond = FloatingBond
--     { nextCoupon :: Maybe Double
--     , frequency :: Double
--     , maturity :: Day
--     , issueDate :: Maybe Day
--     }
    
-- data BondInput = ...

-- data Bond = FixedBond | FloatingBond
