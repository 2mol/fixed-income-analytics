{-# LANGUAGE DuplicateRecordFields #-}

module Instrument where

import Data.Time (Day)
import qualified Data.Map as Map
import Data.Map (Map)

data Currency = CHF | USD | EUR | CAD | JPY
    deriving (Eq, Show, Read)

-- data CouponType = Fixed | Floating
--     deriving (Show)

type CouponAmount = Double

data Coupon = Fixed CouponAmount | Floating (Maybe CouponAmount)
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
    { coupon :: Coupon
    , frequency :: Frequency
    , maturity :: Maturity
    , issueDate :: Maybe Day
    -- , zSpread :: Double
    } deriving (Show)

newtype Exposure = Exposure Double

data Instrument
    = Bond BondDef
    | Composite [(Exposure, Instrument)]
    -- | BondFuture BondDef BondDef
    -- | InterestRateSwap BondDef BondDef
    -- | Composite [Instrument]

--

type ZSpread = Double
type Price = Double
type YieldToMaturity = Double

price :: BondDef -> ZSpread -> Price
price = undefined

yield :: BondDef -> ZSpread -> YieldToMaturity
yield = undefined

spread :: BondDef -> Price -> ZSpread
spread = undefined