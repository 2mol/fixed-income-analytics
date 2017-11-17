# fixed-income-analytics

## Cash Flow calculations

- bond definition -> timespan, coupon, and payment frequency -> can list all coupons on each date, plus principal redemption

### Bond Definition is:

- couponInfo :: (Fixed CouponAmount | Floating (Maybe CouponAmount))
- frequency :: Double
- maturityDate :: Day
- issueDate :: Maybe Day
- lastPaymentDate :: Maybe Day

Create bond by assuming some defaults. Do this with Dictionary/Map?