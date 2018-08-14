#                              numeric functions
#
# Purpose   :   Internal nemeric utility functions
#
# Copyright :   (C) 2015-2016, Vis Consultancy, the Netherlands
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
#     Dec17 - Version 0.1 (Dec 20)
#     Aug18 - getNumericUpperSpacing, getNumericLowerSpacing, getEpsDistance, and coerceNearToInt exported
# ------------------------------------------------------------------------------
#
# -----------------------------------------------------------------------------
#         numeric details: spacing and distance beween successive values
#                           getSpacing and epsDistance
# -----------------------------------------------------------------------------
#' get the spacing, distance and near values for successive numeric values
#' @param x numeric vector
#' @param y numeric vector
#' @param maxEpsDistance an integer specifying the number of successive numerical values considerd to be nearby.
#' @return the space between two successive numeric values
#' @details getNumericUpperSpacing and getNumericLowerSpacing return the distance to
#' the next higher / lower numeric value \cr
#' getEpsDistance return the number of intermediate numeric values between x and Y plus 1 \cr
#' coerceNearToInt coerce values near to an integer valeus to that integer \cr
#' More precise, the spacing between x and the successive numeric value is returned. \cr
#' @references https://en.wikipedia.org/wiki/Machine_epsilon
#' @name numericEps
# NULL
#> NULL


#' @rdname numericEps
# #' @examples
# #' getNumericUpperSpacing (1) == .Machine$double.eps        # returns TRUE
# #' getNumericUpperSpacing (2^52)                            # retruns 1
#' @export
getNumericUpperSpacing <- function (x) {
    if (!is.numeric(x)) stop ("x must be numeric")
    exp <- floor (log2 (abs (x)))
    signifBits  <- .Machine$double.digits - 1
    2^(exp-signifBits)
}

#' @rdname numericEps
# #' @examples
# #' getNumericLowerSpacing(1) == .Machine$double.eps / 2     # returns TRUE
# #' getNumericLowerSpacing (2^53)                            # retruns 1
#' @export
getNumericLowerSpacing <- function (x) {
    if (!is.numeric(x)) stop ("x must be numeric")
    lg2 <- log2 (abs(x))
    exp <- floor (lg2)
    isPow2  <- lg2 == exp
    exp[isPow2] <- exp[isPow2] -1
    signifBits  <- .Machine$double.digits - 1
    2^(exp-signifBits)
}

#' @rdname numericEps
# #' @examples
# #' getEpsDistance (2,4) == 2^52
# #' getEpsDistance (4,8) == 2^52
# #' getEpsDistance (2.5, 5) == 2^52
#' @export
getEpsDistance <- function (x, y) {
    if (!is.numeric(x)) stop ("x must be numeric")
    if (!is.numeric(y)) stop ("y must be numeric")
    if (length(x) != length(y)) stop ("x and y must have the same length")
    x2      <- abs(x)
    y2      <- abs(y)
    x       <- pmin(x2,y2)
    y       <- pmax(x2,y2)
    expX    <- floor (log2(x))
    expY    <- floor (log2(y))
    diff    <- expY - expX
    if (max(diff) > 1)   stop ("x and Y are too far apart")     # at max one power of 2 in between
    dist    <- numeric (length(x))
    diff0s  <- diff==0
    xD0     <- x[diff0s]
    yD0     <- y[diff0s]
    expXD0  <- expX[diff0s]
    epsXD0  <- 2^(expXD0-52)
    dist[diff0s] <- round ((yD0 - xD0) / epsXD0)

    if (sum(diff0s < length(x))) {       # i.e. some with diff = 1
        xD1     <- x[!diff0s]
        yD1     <- y[!diff0s]
        expXD1  <- expX[!diff0s]
        expYD1  <- expY[!diff0s]
        epsXD1  <- 2^(expXD1-52)
        epsYD1  <- 2^(expYD1-52)
        pow2    <- 2^expYD1
        dist[!diff0s] <- round ((pow2-xD1)/epsXD1 + (yD1-pow2)/epsYD1)
    }
    dist
}

#' @rdname numericEps
# #' @examples
# #' eps <- 2^-52
# #' x <- c (1-3*eps/2, 1-2*eps/2, 1-eps/2, 1, 1+eps, 1+2*eps, 1+3*eps)
# #' coerceNearToInt (x, 1)                   # returns  c(NA, NA,  1, 1,  1, NA, NA)
#' @export
coerceNearToInt <- function (x, maxEpsDistance) {
    n           <- round (x)
    delta       <- getEpsDistance (x, n)
    near        <- delta <= maxEpsDistance
    n[!near]    <- NA
    n
}

# -----------------------------------------------------------------------------
#                conversion to numeeric/integer without warnings
# -----------------------------------------------------------------------------
#' coerce a value to a numeric or an integer
#' @param x an R vector value to be coerced
#' @return the numeric / integer value or NA if x could not be converted
#' @details asNumeric is the same as as.Numeric with suppressed warnings. \cr
#' asInteger does not perform any rounding. A NA is returned in stead. \cr
#' is.wholeNUmber returns TRUE is its argument can be converted to an integer without rounding
#' @name numericAndInteger
NULL
#> NULL

#' @rdname numericAndInteger
#' @export
is.wholeNumber <- function (x) {
    ok <- is.integer(x)
    if (ok)                     return (TRUE)
    if (!is.numeric(x)) {
        x <- asNumeric (x)
        if (is.na(x))           return (FALSE)
    }
    return (abs(x - round(x)) < getNumericUpperSpacing (x))
}

#' @rdname numericAndInteger
#' @export
asNumeric <- function (x) {
    n   <- as.numeric (NA)
    if (is.numeric(x)) n <- as.numeric (x)
    else if (is.character(x) | is.logical(x)) {
        WarnOptionSave <- getOption("warn")
        options (warn = -1)
        n <- as.numeric (x)
        options (warn = WarnOptionSave)
    }
    n
}

#' @rdname numericAndInteger
#' @export
asInteger <- function (x) {
    n           <- asNumeric(x)
    i           <- round (n)
    i[i !=  n]  <- NA              # NA for non-integer values
    maxInt      <- .Machine$integer.max
    huge        <- n < -maxInt | n > maxInt
    i[huge]     <- NA
    as.integer(i)
}

# -----------------------------------------------------------------------------
#                               rounding decimals
# -----------------------------------------------------------------------------
#' rounding the decimals to a given number of significant numeric digits
#' @param x a numeric vector to be rounded
#' @param signif the number of significant numeric digits. default = 6
#' @return the rounded number
# #' @examples
# #' quantities:::roundDecimals (123456789.123)                # returns  123456789
# #' quantities:::roundDecimals (123.456789)                   # returns  123.457
# #' quantities:::roundDecimals (12.3456789, signif=4)         # returns  12.35
# #' quantities:::roundDecimals (c(123456.789, 1122.334455)    # returns  c(123457, 1122.33)
#' @keywords internal
roundDecimals <- function (x, signif=6) {
    n <- 10^signif - 1
    ifelse (abs(x) > n, round(x), signif (x, digits=signif))
}
