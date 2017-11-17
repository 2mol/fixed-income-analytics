-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Instrument.Bond where

-- import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Time (Day)

data CouponDef = Fixed Double | Floating (Maybe Double)
    deriving Show

-- separating bond input from bond definition

data BondDef = BondDef
    { couponInfo :: CouponDef
    , frequency :: Double
    , maturityDate :: Day
    , issueDate :: Maybe Day
    , lastPaymentDate :: Day -- removed maybe because we can choose a smart default on input
    , principal :: Double -- same: choose 1.0 as smart default (or 100)
    } deriving Show

fromList :: [(String, String)] -> BondDef
fromList = fromMap . M.fromList

fromMap :: Map String String -> BondDef
fromMap = undefined