module Instrument where

import Data.Time (Day, diffDays)--, NominalDiffTime)
-- import qualified Data.Time as T
-- import qualified Data.Map as Map
-- import Data.Map (Map)
import Data.Maybe (fromMaybe)

type CouponAmount = Double
type Frequency = Double
type BondPrice = Double
type Spread = Double
type Yield = Double
type Exposure = Double

data Currency = CHF | USD | EUR | CAD | JPY
    deriving (Eq, Show, Read)

data Coupon = Fixed CouponAmount | Floating (Maybe CouponAmount)
    deriving (Show)

-- data Frequency = Annual | SemiAnnual | Quarterly
--     deriving (Eq, Ord, Show, Read)

-- frequencyMap :: Map Frequency Double
-- frequencyMap = Map.fromList
--     [ (Annual, 1.0)
--     , (SemiAnnual, 2.0)
--     , (Quarterly, 4.0)
--     ]

data Maturity = MaturityDate Day | Perpetual
    deriving (Eq, Show)

data BondDef = BondDef
    { bId :: Int
    , coupon :: Coupon
    , frequency :: Frequency
    , maturity :: Maturity
    , issue :: Maybe Day
    , lastPayment :: Maybe Day
    } deriving Show

data CashFlow = CashFlow
    { approxFlowDate :: Day
    , cTerm :: Integer -- or Double?
    , nominalAmount :: Double
    , presentValue :: Double
    } deriving Show

data CashFlows = CashFlows Day Exposure [CashFlow] deriving Show

data KeyRateDuration = KeyRateDuration
    { kTerm :: Double
    , krd :: Double
    } deriving Show

data AnalyzedBond = AnalyzedBond
    { aId :: Int
    , cashFlows :: CashFlows
    , price :: BondPrice
    , zSpread :: Spread
    , ytm :: Yield
    , accruedInterest :: Double
    , cleanPrice :: Double
    , fisherWeilDuration :: Double
    , creditDuration :: Double
    , durationToWorst :: Double
    , keyRateDurations :: [KeyRateDuration]
    } deriving Show

data PricingInfo = ZSpread Spread | Price BondPrice | YTM Yield

analyzeBond ::
    BondDef
    -> Day
    -> Maybe Exposure
    -> Maybe PricingInfo
    -- -> YieldCurve
    -> AnalyzedBond
analyzeBond bondDef analysisDate mExposure mPricigInfo =
    let
        aCashFlows =
            calcCashFlows bondDef analysisDate mExposure

        (aPrice, aZSpread, aytm) =
            calcPricing bondDef mPricigInfo
    in
        AnalyzedBond
            { aId = bId bondDef
            , cashFlows = aCashFlows
            , price = aPrice
            , zSpread = aZSpread
            , ytm = aytm
            , accruedInterest = undefined
            , cleanPrice = undefined
            , fisherWeilDuration = undefined
            , creditDuration = undefined
            , durationToWorst = undefined
            , keyRateDurations = undefined
            }

calcFlowTerms :: Day -> Maybe Day -> Day -> Frequency -> [Double]
calcFlowTerms analysisDate mIssueDate maturityDate freq =
    if analysisDate > maturityDate  then
        []
    else
        let
            step =
                1 / freq

            daysToMaturity =
                diffDays maturityDate analysisDate

            yearsToMaturity =
                (fromIntegral daysToMaturity) / 365.12 :: Double

            yearsToFirstFlow =
                case mIssueDate of
                    Nothing -> 0
                    Just issueDate -> calcYearsToFirstFlow analysisDate issueDate

        in
            reverse [yearsToMaturity, yearsToMaturity - step .. yearsToFirstFlow]

calcYearsToFirstFlow :: Day -> Day -> Double
calcYearsToFirstFlow analysisDate issueDate =
    let
        daysToIssue =
            diffDays issueDate analysisDate

        yearsToIssue =
            (fromIntegral daysToIssue) / 365.12 :: Double
    in
        max yearsToIssue 0

calcCashFlows :: BondDef -> Day -> Maybe Exposure -> CashFlows
calcCashFlows _ analysisDate mExposure =
    let
        exposure =
            fromMaybe 1 mExposure
    in
        CashFlows analysisDate exposure []

calcPricing :: BondDef -> Maybe PricingInfo -> (BondPrice, Spread, Yield)
calcPricing = undefined




--

-- price :: BondDef -> ZSpread -> Price
-- price = undefined

-- yield :: BondDef -> ZSpread -> YieldToMaturity
-- yield = undefined

-- spread :: BondDef -> Price -> ZSpread
-- spread = undefined