#                                   duration
#
# Purpose   :   defining metric prefixes
#
# Copyright :   (C) 2015-2018, Vis Consultancy, the Netherlands
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
#               along with this package for R.  If not, see <http://www.gnu.org/licenses/>.
#
#
# History   :
#     Oct17 - Created
#     Jun18  - Revised
# ------------------------------------------------------------------------------
#' @include quantityDefinitions.R
#' @include quantities.R
NULL
# ------------------------------------------------------------------------------
#                              add prefix definition
# ------------------------------------------------------------------------------
addQprefix <- function () {
    # add defintion
    mu <- '\u00B5'  # = micro sign (or use mu = '\u03BC'), see http://graphemica.com
    # see https://en.wikipedia.org/wiki/Metric_prefix
    yotta   <- list (name='yotta', plural='yotta', symbol='Y' , pe = 24, factor=1000000000000000000000000)
    zetta   <- list (name='zetta', plural='zetta', symbol='Z' , pe = 21, factor=1000000000000000000000)
    exa     <- list (name='exa',   plural='exa'  , symbol='E' , pe = 18, factor=1000000000000000000)
    peta    <- list (name='peta' , plural='peta' , symbol='P' , pe = 15, factor=1000000000000000)
    tera    <- list (name='tera' , plural='tera' , symbol='T' , pe = 12, factor=1000000000000)
    giga    <- list (name='giga' , plural='giga' , symbol='G' , pe =  9, factor=1000000000)
    mega    <- list (name='mega' , plural='mega' , symbol='M' , pe =  6, factor=1000000)
    kilo    <- list (name='kilo' , plural='kilo' , symbol='k' , pe =  3, factor=1000)
    hecto   <- list (name='hecto', plural='hecto', symbol='h' , pe =  2, factor=100)
    deca    <- list (name='deca' , plural='deca' , symbol='da', pe =  1, factor=10)
    none    <- list (name='none' , plural=''     , symbol=''  , pe =  0, factor=1,  text='')
    deci    <- list (name='deci' , plural='deci' , symbol='d' , pe = -1, factor=0.1)
    centi   <- list (name='centi', plural='centi', symbol='c' , pe = -2, factor=0.01)
    milli   <- list (name='milli', plural='milli', symbol='m' , pe = -3, factor=0.001)
    micro   <- list (name='micro', plural='micro', symbol= mu , pe = -6, factor=0.000001)
    nano    <- list (name='nano' , plural='nano' , symbol='n' , pe = -9, factor=0.000000001)
    pico    <- list (name='pico' , plural='pico' , symbol='p' , pe =-12, factor=0.000000000001)
    femto   <- list (name='femto', plural='femto', symbol='f' , pe =-15, factor=0.000000000000001)
    atto    <- list (name='atto' , plural='atto' , symbol='a' , pe =-18, factor=0.000000000000000001)
    zepto   <- list (name='zepto', plural='zepto', symbol='z' , pe =-21, factor=0.000000000000000000001)
    yocto   <- list (name='yocto', plural='yocto', symbol='y' , pe =-24, factor=0.000000000000000000000001)

    units   <- list (yotta=yotta, zetta=zetta, exa=exa, peta=peta, tera=tera, giga=giga, mega=mega, kilo=kilo,
                     hecto=hecto, deca=deca, none=none, deci=deci, centi=centi,
                     milli=milli, micro=micro, nano=nano, pico=pico, femto=femto, atto=atto, zepto=zepto, yocto=yocto)
    addQdefinition ( qName='prefix', units=units, default='none')
    # exponent added also to avoid rounding errors; expect_true (e3[i-1] == 1000*e3[i]) fails in test_that !!
}
# packageQdefs$prefix <- addQprefix     addPackageDefs() treats addPackageQdefs seperately
