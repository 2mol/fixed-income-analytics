TODO:
=====

- [ ] newtype, with derived Num instances instead of current 'type Foo = Double'.
- [ ] function mkBond, so that Instrument module doesn not expose type constructors.
- [ ] split last coupon and redemption (simplifies & is more flexible later).
- [ ] think about going Date first, Term second. Might enable smart use of Fractional instead of Double (since daysDelta is always a multiplier for 1 / constYEARDAYS).
- [ ] ieee754 float comparisons with Data.AEq (~==)
- [x] doctest (http://yannesposito.com/Scratch/en/blog/Haskell-Tutorials--a-tutorial/)
- [x] better show instances for bond and cashflows

Wishlist:
=========

- [ ] command line tool
(- [ ] parse .csv) -- UGH
- [ ] parse yaml
- [ ] pull (& parse) yield curve data from various central bank sources or quantopian.
- [ ] web tool
- [ ] cashing

