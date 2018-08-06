#                                   Length and related quantity definitions
#
# Purpose   :   define length quantity units
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
#     Jun18 - Created
# ------------------------------------------------------------------------------
#' @include quantityDefinitions.R
NULL
# ------------------------------------------------------------------------------
#                        add length quantity definition
# ------------------------------------------------------------------------------
addQlength <- function () {
    # by default: text = name and plural = text+s
    # for details on imperial units see https://en.wikipedia.org/wiki/Imperial_units#Length
    iso     <- 'ISO'
    imp     <- 'imperial'
    nau     <- 'nautic'             # see https://en.wikipedia.org/wiki/Nautical_mile
    sur     <- 'survey'
    nm      <-  'nautical mile'
    metre   <- list (name='metre'   , symbol='m'    , system=iso, factor=   1                          )
    thou    <- list (name='thou'    , symbol='th'   , system=imp, factor=   0.0000254                  )
    inch    <- list (name='inch'    , symbol='in'   , system=imp, factor=   0.0254   , plural='inches' )
    foot    <- list (name='foot'    , symbol='ft'   , system=imp, factor=   0.3048   , plural='feet'   )
    yard    <- list (name='yard'    , symbol='yd'   , system=imp, factor=   0.9144                     )
    chain   <- list (name='chain'   , symbol='ch'   , system=imp, factor=  20.1168                     )
    furlong <- list (name='furlong' , symbol='fur'  , system=imp, factor= 201.168                      )
    mile    <- list (name='mile'    , symbol='ml'   , system=imp, factor=1609.344                      )
    link    <- list (name='link'    , symbol='link' , system=sur, factor=   0.201168                   )  # survey unit
    rod     <- list (name='rod'     , symbol='rod'  , system=sur, factor=   5.0292                     )  # surveyunit, also called perch or pole
    fathom  <- list (name='fathom'  , symbol='fmt'  , system=nau, factor=    1.852                     )
    cable   <- list (name='cable'   , symbol='cbl'  , system=nau, factor=  185.2                       )
    nMile   <- list (name= nm       , symbol= 'M'   , system=nau, factor= 1852                         )

    units   <- list(metre=metre, thou=thou, inch=inch, foot=foot, yard=yard, chain=chain, furlong=furlong, mile=mile,
                    link=link, rord=rod, fathom=fathom, cable=cable, nMile=nMile)
    addQdefinition (qName='length', units=units, default='metre')
}
packageQdefs$length <- addQlength

# ------------------------------------------------------------------------------
#                        add arae quantity definition
# ------------------------------------------------------------------------------
# for details see: https://en.wikipedia.org/wiki/Area and for the acre: https://en.wikipedia.org/wiki/Acre
# for barn see https://www.bipm.org/utils/common/pdf/si_brochure_8_en.pdf
addQarea <- function () {
    # by default: text = name and plural = text+s
    iso     <- 'ISO'
    imp     <- 'imperial'
    sur     <- 'survey'
    def     <- 'length$metre^2'
    smS     <- 'm\u00B2'
    smT     <- 'metre\u00B2'
    smP     <- 'metres\u00B2'
    sqMetre <- list (name='sqMetre' , symbol=smS    , system=iso, factor=    1, def=def, text=smT, plural=smP )
    are     <- list (name='are'     , symbol='a'    ,             factor=  100                                )
    acre    <- list (name='acre'    , symbol='ac'   , system=imp, factor= 1609.344 * 1609.344 / 640           )  # square mile / 640
    perch   <- list (name='perch'   , symbol='perch', system=sur, factor=   25.29285264                       )
    rood    <- list (name='rood '   , symbol='rood' , system=imp, factor= 1011.7141056                        )
    barn    <- list (name='barn'    , symbol='b'    , factor= 10^-28                                          )

    units   <- list(sqMetre=sqMetre, are=are, acre=acre, perch=perch, rood=rood, barn=barn)
    addQdefinition (qName='area', units=units, default='sqMetre')
}
packageQdefs$area <- addQarea

# ------------------------------------------------------------------------------
#                        add volume quantity definition
# ------------------------------------------------------------------------------
# for details see: https://en.wikipedia.org/wiki/Imperial_units; UScustomary volume units are different
addQvolume <- function () {
    # by default: text = name and plural = text+s
    iso     <- 'ISO'
    imp     <- 'imperial'
    def     <- 'length$metre^-3'
    cmS     <- 'm\u00B3'
    cmT     <- 'metre\u00B3'
    cmP     <- 'metres\u00B3'
    oS      <- 'fl oz'
    oT      <- 'fluid ounce'
    oP      <- 'fluid ounces'
    cbMetre <- list (name='cbMetre' , symbol= cmS , system=iso, factor= 1000, def=def, text=cmT, plural=cmP)
    litre   <- list (name='litre'   , symbol='l'  ,             factor=     1                  )
    gallon  <- list (name='gallon'  , symbol='gal', system=imp, factor=    14.54609)  # by definition (1985)
    quart   <- list (name='quart'   , symbol='gal', system=imp, factor=    14.54609 / 4 )
    pint    <- list (name='pint'    , symbol='pt' , system=imp, factor=    14.54609 / 8 )
    gill    <- list (name='gill'    , symbol='gi' , system=imp, factor=    14.54609 / 32)
    ounce   <- list (name='ounce'   , symbol= oS  , system=imp, factor=    14.54609 / 160, text=oT, plural=oP )

    units   <- list(cbMetre=cbMetre, litre=litre, gallon=gallon, quart=quart, pint=pint, gill=gill, ounce=ounce)
    addQdefinition (qName='volume', units=units, default='cbMetre')
}
packageQdefs$volume <- addQvolume

# ------------------------------------------------------------------------------
#                         is/as length, area, and volume
# ------------------------------------------------------------------------------

#' @rdname isAsQuantity
#' @export
is.length <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        d   <- qDim (x)
        ok  <- length (d) ==  1 && d == c(length=1)
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.length <- function (x, unit='metre') {
    unit <- paste ('length$', unit, sep='')
    as.quantity (x, unit)
}

#' @rdname isAsQuantity
#' @export
is.area <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        d   <- qDim (x)
        ok  <- length (d) ==  1 && d == c(length=2)
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.area <- function (x, unit='metre^2') {
    unit <- paste ('length$', unit, sep='')
    as.quantity (x, unit)
}

#' @rdname isAsQuantity
#' @export
is.volume <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        d   <- qDim (x)
        ok  <- length (d) ==  1 && d == c(length=3)
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.length <- function (x, unit='metre^3') {
    unit <- paste ('length$', unit, sep='')
    as.quantity (x, unit)
}
