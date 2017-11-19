{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Numeric (showFFloat)

newtype YearDelta =
    YearDelta { unYearDelta :: Double }
    deriving (Eq, Ord, Num)

newtype CashNominal =
    CashNominal { unCashNominal :: Double }
    deriving Num

newtype CashPV =
    CashPV { unCashPV :: Double }
    deriving Num

newtype InterestRate =
    InterestRate { unInterestRate :: Double }
    deriving Num

showDouble :: RealFloat a => a -> String
showDouble a = showFFloat (Just 2) a ""

instance Show YearDelta where
    show (YearDelta a) = "yd>" ++ showDouble a

instance Show CashNominal where
    show (CashNominal a) = showDouble a

instance Show CashPV where
        show (CashPV a) = showDouble a

instance Show InterestRate where
    show (InterestRate a) = showDouble (a * 100) ++ "%"
