{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

newtype YearDelta = YearDelta { unYearDelta :: Double } deriving (Show, Eq, Ord, Num)
newtype CashNominal = CashNominal { unCashNominal :: Double } deriving (Show, Num)
newtype CashPV = CashPV { unCashPV :: Double } deriving (Show, Num)
newtype InterestRate = InterestRate { unInterestRate :: Double } deriving (Show, Num)
