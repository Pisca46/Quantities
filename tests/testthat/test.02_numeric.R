#                               test numeric functions
#
# Purpose   :   Test the functions in numeric.R
#
#
# Copyright :   (C) 2016, Vis Consultancy, the Netherlands
#               This program is free software: you can redistribute it and/or modify
#               it under the terms of the GNU General Public License as published by
#               the Free Software Foundation, either version 3 of the License, or
#               (at your option) any later version.
#
#               This program is distributed in the hope that it will be useful,
#               but WITHOUT ANY WARRANTY; without even the implied warranty of
#               MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#               GNU General Public License for more details.
#
#               You should have received a copy of the GNU General Public License
#               along with edf package for R.  If not, see <http://www.gnu.org/licenses/>.
#
# History   :
#     Oct17 - Created
#     Dec17 - Version 0.1 (Dec 20)
# ------------------------------------------------------------------------------

require (testthat)
context ("Test the numeric functions.")

getNumericUpperSpacing  <- quantities:::getNumericUpperSpacing
getNumericLowerSpacing  <- quantities:::getNumericLowerSpacing
getEpsDistance          <- quantities:::getEpsDistance
coerceNearToInt         <- quantities:::coerceNearToInt
# asNumeric               <- quantities:::asNumeric
# asInteger               <- quantities:::asInteger
roundDecimals           <- quantities:::roundDecimals
na                      <- as.numeric (NA)

# ------------------------------------------------------------------------------
#                               numeric precision
# ------------------------------------------------------------------------------

eps <- 2^-52

test_that("numeric upper spacing", {
    expect_equal (getNumericUpperSpacing(1), .Machine$double.eps)
    expect_equal (getNumericUpperSpacing(1), eps)
    expect_equal (getNumericUpperSpacing (2^52), 1)
    expect_equal (getNumericUpperSpacing (c(1.5, 8, 2^52)), c(eps, 3*eps, 1))
})

test_that("numeric lower spacing", {
    expect_equal (getNumericLowerSpacing(1), .Machine$double.eps)
    expect_equal (getNumericLowerSpacing(1), eps)
    expect_equal (getNumericLowerSpacing (2^52), .5)
    expect_equal (getNumericLowerSpacing (6), getNumericUpperSpacing (6))
    expect_equal (getNumericLowerSpacing (c(1.5, 8, 2^52)), c(eps, 2*eps, .5))
})

test_that ("getEpsDistance", {
    expect_equal (getEpsDistance (1+4*eps, 1+24*eps), 20)
    eps6 <- getNumericUpperSpacing (6)
    expect_equal (getEpsDistance (6, 7)  , 1/eps6)
    expect_equal (getEpsDistance (-5, -7), 2/eps6)
    expect_equal (getEpsDistance (7, 8)  , 1/eps6)
    expect_equal (getEpsDistance (8, 9)  , 1/(eps6*2))
    expect_equal (getEpsDistance (7, 9)  , 3/(eps6*2))
    expect_equal (getEpsDistance (c(7,8), c(8,9))  , c(1/eps6, 1/(eps6*2)))
})

test_that("maxEpsDistance", {
    x <- c (1-3*eps/2, 1-2*eps/2, 1-eps/2, 1, 1+eps, 1+2*eps, 1+3*eps)
    expect_equal (coerceNearToInt (x, 0), c(NA, NA, NA, 1, NA, NA, NA))
    expect_equal (coerceNearToInt (x, 1), c(NA, NA,  1, 1,  1, NA, NA))
    expect_equal (coerceNearToInt (x, 2), c(NA,  1,  1, 1,  1,  1, NA))
    expect_equal (coerceNearToInt (x, 4), c(  1, 1,  1, 1,  1,  1,  1))
})

# ------------------------------------------------------------------------------
#                             is.wholeNUmber
# ------------------------------------------------------------------------------

test_that('is.wholenUmber', {
    reWN <- is.wholeNumber
    # compare with  (from example in help integee)
    rhWN <-function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
    expect_true  (reWN (4.0000000000000001))
    expect_false (reWN (4.000000000000001))
    expect_true  (rhWN (4.000000000000001))         # rhWN is less acurate
    expect_true  (reWN (3.9999999999999999))
    expect_false (reWN (3.999999999999999))
    expect_true  (rhWN (3.999999999999999))         # rhWN is less acurate
    expect_true  (reWN (33333.999999999999))
    expect_false (reWN (33333.99999999999))
    expect_true  (rhWN (33333.99999999999))         # rhWN is less acurate
    expect_true  (reWN (33333333333333333.5))
    expect_true  (rhWN (33333333333333333.5))       # rhWN is as acurate
    expect_false (reWN (3333333333333333.5))
    expect_false (rhWN (3333333333333333.5))
})

# ------------------------------------------------------------------------------
#                             asNumeric & asInteger
# ------------------------------------------------------------------------------

test_that ('asNumeric, single value', {
    na <- as.numeric (NA)
    expect_equal (asNumeric (12)         , 12)
    expect_equal (asNumeric ('60')       , 60)
    expect_equal (asNumeric (TRUE)       , 1 )
    expect_equal (asNumeric (12.)        , 12)
    expect_equal (asNumeric ('60.')      , 60)
    expect_equal (asNumeric (12.4)       , 12.4)
    expect_equal (asNumeric ('60.5')     , 60.5)
    expect_equal (asNumeric ('spam')     , na)
    expect_equal (asNumeric (list())     , na)
    expect_equal (asNumeric (NULL)       , na)
    expect_equal (asNumeric (NA)         , na)
})

test_that ('asNumeric, vectors', {
    n   <- c (1, 3, 5)
    expect_equal (asNumeric (n)         , c (1, 3, 5))
    nc  <- c ('3', NA, '7', '', '-55', 'spam', '99')
    expect_equal (asNumeric (nc)        , c(3, na, 7, na, -55, na, 99))
})

test_that ('asInteger', {
    expect_equal (asInteger (12)         , 12)
    expect_equal (asInteger ('60')       , 60)
    expect_equal (asInteger (FALSE)      , 0 )
    expect_equal (asInteger (12.)        , 12)
    expect_equal (asInteger ('60.')      , 60)
    expect_equal (asInteger (12.4)       , na)
    expect_equal (asInteger ('60.5')     , na)
    expect_equal (asInteger ('spam')     , na)
    expect_equal (asInteger (list())     , na)
    expect_equal (asInteger (NULL)       , na)
    expect_equal (asInteger (NA)         , na)
    expect_equal (asInteger (c(-2^31, -2^31+1, 2^31-1, 2^31)), c(NA, -2^31+1, 2^31-1, NA))
    expect_equal (asInteger (c(1-.26*eps, 1-.25*eps, 1, 1+.50*eps, 1+.51*eps)), c( NA, 1, 1, 1, NA))
    expect_equal (asInteger(c(-2^31 ,-2^31 + 1, 2^31-1, 2^31)), c(NA, -2147483647, 2147483647, NA))
})

# ------------------------------------------------------------------------------
#                               round on decimals
# ------------------------------------------------------------------------------

test_that ('roundDecimals with scalar', {
    expect_equal (roundDecimals ( 123456789 )          ,  123456789)
    expect_equal (roundDecimals (-123.456789)          , -123.457)
    expect_equal (roundDecimals ( 1234567.89)          ,  1234568)
    expect_equal (roundDecimals (-1234567.89, signif=6), -1234568)
    expect_equal (roundDecimals ( 1234567.89, signif=4),  1234568)
    expect_equal (roundDecimals (-12.3456789, signif=4), -12.35)
    expect_equal (roundDecimals ( 0         , signif=4),  0)
    expect_equal (roundDecimals (-1         , signif=4), -1)
})

test_that ('roundDecimals with vector', {
    vect <- roundDecimals (c(123456.789, 1122.334455))
    expect_equal (vect            , c(123457, 1122.33))
    expect_equal (format(vect)    , c('123457.00', '  1122.33')) # note the format properties
    expect_equal (format(vect[1]) , '123457')
})
