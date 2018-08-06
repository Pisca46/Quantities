#                               quantity defintion test
#
# Purpose   :   Test quantity defintion functions
#
#
# Copyright :   (C) 2017-2018, Vis Consultancy, the Netherlands
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
#     Mar-Jul18 - Revised
# ------------------------------------------------------------------------------

require (quantities)
qEnvir <- quantities:::qEnvir
require (testthat)
context ("Testing quantity definition functions.")

# remove test defs if present
if (!is.null(qEnvir$qDefs$abc)) qEnvir$qDefs$abc <- NULL
if (!is.null(qEnvir$qDefs$wtl)) qEnvir$qDefs$wtl <- NULL

addQwtl <- function () {
    # wrong test length
    metre       <- list (name='metre'     , plural='metres'     , symbol='m'  , factor=1)
    foot        <- list (name='foot'      , plural='feet'       , symbol="'"  , factor=1/3)
    inch        <- list (name='inch'      , plural='inches'     , symbol='"'  , factor=1/40)
    addQdefinition (
        qName='wtl', list(metre=metre, inch=inch, foot=foot), default='metre'
    )
}
addQwtl ()
# ------------------------------------------------------------------------------
#                              quantity definitions
# ------------------------------------------------------------------------------
test_that('prefix definitions', {
    p   <- qEnvir$qDefs$prefix
    s   <- paste (as.character(sapply(p$unit, function(X) X$symbol)), collapse='')
    mu <- '\u00B5'
    symbols <- paste ("YZEPTGMkhdadcm", mu, "npfazy", sep='')
    expect_equal(s, symbols)
    t   <- paste (as.character(sapply(p$unit, function(X) X$text)), collapse=',')
    te  <- "yotta,zetta,exa,peta,tera,giga,mega,kilo,hecto,deca,,deci,centi,milli,micro,nano,pico,femto,atto,zepto,yocto"
    expect_equal(t, te)
    n   <- paste (names (p$units), collapse=',')
    ne  <- "yotta,zetta,exa,peta,tera,giga,mega,kilo,hecto,deca,none,deci,centi,milli,micro,nano,pico,femto,atto,zepto,yocto"
    expect_equal(n, ne)
    n   <- paste (as.character(sapply(p$unit, function(X) X$name)), collapse=',')
    expect_equal(n, ne)
    n   <- as.character(sapply(p$unit, function(X) X$name))
    n3  <- n[c(1,2,3,4,5,6,7,8,11,14,15,16,17,18,19,20,21)]
    n1  <- n[c(8,9,10,11,12,13,14)]
    expect_equal(length(n)+3, length(n3)+length(n1))
    u3  <- p$units[n3]
    u1  <- p$units[n1]
    e3  <- as.numeric (sapply (u3, function(X) X$factor))
    e1  <- as.numeric (sapply (u1, function(X) X$factor))
    for (i in 2:length(e3)) expect_equal (e3[i-1], 1000*e3[i])
    # for (i in 2:length(e3)) expect_true (e3[i-1] == 1000*e3[i])  this fails !!
    for (i in 2:length(e1)) expect_equal (e1[i-1], 10  *e1[i])
    })

test_that ('predefined quantity definitions', {
  expect_equal (names (qEnvir$qDefs$wtl$units), c("metre", "foot", "inch"))  # in decreasing order
})

test_that('New quantity definitions', {
    nDefs <- length(qEnvir$qDefs)
    incr  <- ifelse (is.null(qEnvir$qDefs$abc), 1, 0)
    addQdefinition ("abc", list (name='def'), default = 'def')
    qDefs <- qEnvir$qDefs
    expect_equal(length(qDefs), nDefs + incr)

    expect_equal(length(qDefs$abc$units), 1)
    abc <- qDefs$abc
    expect_equal(abc$default, 'def')
    u   <- abc$units[[1]]
    expect_equal(u$name  , 'def')
    expect_equal(u$plural, 'defs')
    expect_equal(u$symbol, 'def')
    expect_equal(u$text  , 'def')
    expect_equal(u$factor,   1  )
    # redefine
    w <- "An old defintion for quantity abc has been replaced"
    expect_warning (addQdefinition ("abc", list (name='def'), default='def'), w)
    # redefine more alaborate
    u1 <- list (name='u1', plural='u1s', text='u1text', symbol='i', factor=2.54)
    u2 <- list (name='u2', plural='u2s'               , symbol='m', factor=1)
    addQdefinition ("abc", list (u1=u1, u2=u2), default='u2')
    abc <- qEnvir$qDefs$abc
    expect_equal (length(abc$units), 2)
    u1  <- abc$units$u1
    expect_equal (u1$name  , 'u1'    )
    expect_equal (u1$plural, 'u1s'   )
    expect_equal (u1$text  , 'u1text')
    expect_equal (u1$symbol, 'i'     )
    expect_equal (u1$factor, 2.54)
    u2 <-abc$units$u2
    expect_equal (u2$text    , 'u2')
    expect_equal (abc$default, 'u2')
})

# ------------------------------------------------------------------------------
#                        remove temp quantity definitions
# ------------------------------------------------------------------------------
if (!is.null(qEnvir$qDefs$abc)) qEnvir$qDefs$abc <- NULL
if (!is.null(qEnvir$qDefs$wtl)) qEnvir$qDefs$wtl <- NULL
