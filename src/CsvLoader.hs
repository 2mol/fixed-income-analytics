{-# LANGUAGE OverloadedStrings #-}

module CsvLoader where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Time (Day, fromGregorian, defaultTimeLocale)
-- import Data.Text (Text)

import qualified Instrument as I
import Instrument (BondDef)

-- data Person = Person
--     { name   :: !String
--     , salary :: !Int
--     }

--         { bId = 0
--         , couponInfo = Fixed 0.0125
--         , frequency = 2.0
--         , maturity = MaturityDate maturityDate
--         , issue = Nothing
--         }

instance FromNamedRecord BondDef

instance FromField I.CouponDef --where
    -- parseField r = I.Fixed <$> r .: "Coupon"

instance FromField I.Maturity

instance FromField Day

-- instance FromField Day where
--     parseField = parseTimeM True defaultTimeLocale "%Y/%m/%d" . show

-- instance FromNamedRecord BondDef where
--     parseNamedRecord r =
--         BondDef <$> r .: "Id"
--         <*> (I.Fixed r .: "CouponType")
--         <*> r .: "Frequency"
--         -- <*> r .: "MaturityDate"
--         <*> repeat (fromGregorian 2022 12 31)
--         <*> repeat (Nothing)

main :: IO ()
main = do
    csvData <- BL.readFile "bonds2.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ b ->
            putStrLn $ show (b :: BondDef)--(I.bId b) ++ " earns " ++ show (I.frequency b) ++ " dollars"