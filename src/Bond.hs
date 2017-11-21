-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Bond where

-- import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Time (Day)
import Types

data CouponDef = Fixed InterestRate | Floating (Maybe InterestRate)
    deriving Show

data BondDef = BondDef
    { couponDef :: CouponDef
    , frequency :: Double
    , maturityDate :: Day
    , mIssueDate :: Maybe Day
    , lastPaymentDate :: Day -- removed maybe because we can choose a smart default on input
    , principal :: CashNominal -- same: choose 1.0 as smart default (or 100)
    } deriving Show

data MinimalBondDef = MinimalBondDef
    { xxcouponInfo :: CouponDef
    , xxfrequency :: Double
    , xxmaturityDate :: Day
    -- , xxissueDate :: Maybe Day
    } deriving Show

fromList :: [(String, String)] -> BondDef
fromList = undefined

fromMap :: Map String String -> BondDef
fromMap = fromList . M.toList

fromMinimal :: MinimalBondDef -> BondDef
fromMinimal = undefined