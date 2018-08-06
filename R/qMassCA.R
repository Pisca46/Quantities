#                       quantity definitions for mass, force, energy and power
#
# Purpose   :   define quantity units
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
#     Jun18 - Created for durations
# ------------------------------------------------------------------------------
#' @include quantityDefinitions.R
# NULL

# ------------------------------------------------------------------------------
#                          add mass quantity definition
# ------------------------------------------------------------------------------
addQmass <- function () {
    # by default: text = name and plural = text+s
    gram         <- list (name='gram'         , plural='grams'   , symbol='g'  , system='ISO', factor=1)
    # imperial, for details see https://en.wikipedia.org/wiki/Imperial_units
    grain        <- list (name='grain'        , plural='grains'        , symbol='gr' , system='imperial', factor= 453.59237/7000)
    drachm       <- list (name='drachm'       , plural='drachms'       , symbol='dr' , system='imperial', factor= 453.59237/256)
    ounce        <- list (name='ounce'        , plural='ounces'        , symbol='oz' , system='imperial', factor= 453.59237/16)
    pound        <- list (name='pound'        , plural='pounds'        , symbol='lb' , system='imperial', factor= 453.59237)
    stone        <- list (name='stone'        , plural='stones'        , symbol='st' , system='imperial', factor= 453.59237*14)
    quarter      <- list (name='quarter'      , plural='quarters'      , symbol='qr' , system='imperial', factor= 453.59237*28)
    hundredweight<- list (name='hundredweight', plural='hundredweights', symbol='cwt', system='imperial', factor= 453.59237*112)
    ton          <- list (name='ton'          , plural='tons'          , symbol='t'  , system='imperial', factor= 453.59237*2240)
    units        <- list(gram=gram, grain=grain, drachm=drachm, ounce=ounce, pound=pound,
                    stone=stone, quarter=quarter, hundredweight=hundredweight, ton=ton)

    addQdefinition (qName='mass', units=units, default='kilo.gram')
}
packageQdefs$mass <- addQmass

addQforce <- function () {
    # for details seee: https://en.wikipedia.org/wiki/Force
    def          <- 'mass$kilo.gram length$metre time$second^-2'
    newton       <- list (name='newton'       , plural='newtons'       , symbol='N'  , system='ISO'     , factor=1       , def=def)
    poundForce   <- list (name='pound-force'  , plural='pounds-force'  , symbol='lbf', system='imperial', factor=4.448222               )
    units        <- list (newton=newton, `pound-force`=poundForce)

    addQdefinition (qName='force', units=units, default='newton')
}
packageQdefs$force <- addQforce

addQenergy <- function () {
    # for calorie the 'thermochemical calorie' (= United States customary unit) is used, for details see e.g.: https://en.wikipedia.org/wiki/Calorie
    # for foot pound see: https://en.wikipedia.org/wiki/Foot-pound_(energy)
    def          <- 'mass$kilo.gram length$metre^2 time$second^-2'

    joule        <- list (name='joule'        , plural='joules'        , symbol='J'    , system='ISO'     , factor=1     , def=def)
    footPound    <- list (name='foot pound'   , plural='feet pound'    , symbol='ft lb', system='imperial', factor=1.3558179483314004)
    calorie      <- list (name='calorie'      , plural='calories'      , symbol='cal'  , system='USC'     , factor=4.184)
    units        <- list (joule=joule, footPound=footPound, calorie=calorie)

    addQdefinition (qName='energy', units=units, default='joule')
}
packageQdefs$energy <- addQenergy

addQpower <- function () {
    # for horsepower, the imperial horsepower is used, see https://en.wikipedia.org/wiki/Horsepower
    def          <- 'energy$joule time$second^-1'    # = 'mass$kilo.gram length$metre^2 time$second^-3'

    watt    <- list (name='watt'         , plural='watts'         , symbol='W'    , system='ISO'     , factor=1     , def=def)
    hpI     <- list (name='horsepower(I)', plural='horsepower(I)' , symbol='hp(I)', system='imperial', factor=745.69987158227022, note='imperial / mechanical')
    hpE     <- list (name='horsepower(E)', plural='horsepower(E)' , symbol='hp(E)'                   , factor=746               , note='electrical')
    hpM     <- list (name='horsepower(M)', plural='horsepower(M)' , symbol='hp(M)'                   , factor=735.49875         , note='metric')

    units        <- list (watt=watt, hpI=hpI, hpE=hpE, hpM=hpM)
    addQdefinition (qName='power', units=units, default='watt')
}
packageQdefs$power <- addQpower

# ------------------------------------------------------------------------------
#                      is/as mass, force, energy, and power
# ------------------------------------------------------------------------------

#' @rdname isAsQuantity
#' @export
is.mass <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        d   <- qDim (x)
        ok  <- length (d) ==  1 && d == c(mass=1)
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.mass <- function (x, unit='kilo.gram') {
    unit <- paste ('length$', unit, sep='')
    as.quantity (x, unit)
}

#' @rdname isAsQuantity
#' @export
is.force <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        ok <- sameDimensions (qDim (x), c(mass=1, length=1, time=-2))
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.force <- function (x, unit='newton') {
    unit <- paste ('force$', unit, sep='')
    as.quantity (x, unit)
}

#' @rdname isAsQuantity
#' @export
is.energy <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        ok <- sameDimensions (qDim (x), c(mass=1, length=2, time=-2))
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.energy <- function (x, unit='joule') {
    unit <- paste ('energy$', unit, sep='')
    as.quantity (x, unit)
}
#' @rdname isAsQuantity
#' @export
is.power <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        ok <- sameDimensions (qDim (x), c(mass=1, length=2, time=-3))
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.power <- function (x, unit='watt') {
    unit <- paste ('power$', unit, sep='')
    as.quantity (x, unit)
}
