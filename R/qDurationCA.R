#                                   duration
#
# Purpose   :   duration quantities support
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
#     Dec17 - Version 0.1 (Dec 20)
#     Mar18  - Revised
# ------------------------------------------------------------------------------
#' @include quantityDefinitions.R
NULL

# ------------------------------------------------------------------------------
#                             add angle definition
# ------------------------------------------------------------------------------

addQangle <- function () {
    # by default: text = name and plural = text+s
    # see e.g.: https://en.wikipedia.org/wiki/ISO_80000-3  and https://en.wikipedia.org/wiki/Angle
    turn        <- list (name='turn'        , symbol=''       , factor=1, text='', plural='')   # no symbol, no text
    revolution  <- list (name='revolution'  , symbol='rev'    , factor=1)
    radian      <- list (name='radian'      , symbol='rad'    , factor=1/(2*pi))
    gon         <- list (name='gon'         , symbol='g'      , factor=1/400)
    degree      <- list (name='degree'      , symbol='\u00B0' , factor=1/360)  # '\u00B0'= degree; see http://graphemica.com
    minute      <- list (name='minute'      , symbol="'"      , factor=1/21600)
    second      <- list (name='second'      , symbol='"'      , factor=1/1296000)

    units       <- list (turn=turn, revolution=revolution, radian=radian, degree=degree, minute=minute, second=second)
    addQdefinition (qName='angle', units=units, default='revolution')
}
packageQdefs$angle <- addQangle

# ------------------------------------------------------------------------------
#                        add duration quantity definition
# ------------------------------------------------------------------------------

addQtime <- function () {
    # by default: text = name and plural = text+s
    # for a year see: https://en.wikipedia.org/wiki/Year
    iso     <- 'ISO'
    JDays  <- 365.25      # = days per Julian year (i.e. per annus)
    GDays  <- 365.2425    # = days per Gregorian year (i.e. per year)
    annus  <- list (name='annus'  , symbol='a'  , factor=JDays, plural='anni', note='Julian year', null='')
    year   <- list (name='year'   , symbol='y'  , factor=GDays               , note='Gregorian year', null='')
    quater <- list (name='quater' , symbol='q'  , factor=GDays/4             , note='Avarage Gregorian quater', null='')
    month  <- list (name='month'  , symbol='mo' , factor=GDays/12            , note='Avarage Gregorian month', null='')
    week   <- list (name='week'   , symbol='w'  , factor=7                   )
    day    <- list (name='day'    , symbol='d'  , factor=1                   )
    hour   <- list (name='hour'   , symbol='h'  , factor=1/24                )
    minute <- list (name='minute' , symbol='min', factor=1/1440              )
    second <- list (name='second' , symbol='s'  , factor=1/86400, system=iso )
    units  <- list(annus=annus, year=year, quater=quater, month=month, week=week, day=day,
                    hour=hour, minute=minute, second=second)
    addQdefinition (qName='time', units=units, default='second')
}
packageQdefs$time <- addQtime

addQfrequency <- function () {
    # see: https://en.wikipedia.org/wiki/Hertz
    dim     <- 'time^-1'
    hertz   <- list (name='hertz', symbol='Hz', factor=1, def="time$second^-1")
    units   <- list (hertz=hertz)
    addQdefinition (qName='frequency', units=units, default='hertz')
}
packageQdefs$frequency <- addQfrequency

# ------------------------------------------------------------------------------
#                        is/as time, frequency, and angle
# ------------------------------------------------------------------------------

#' @rdname isAsQuantity
#' @export
is.time <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        d   <- qDim (x)
        ok  <- length (d) ==  1 && d == c(time=1)
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.time <- function (x, unit='second') {
    unit <- paste ('time$', unit, sep='')
    as.quantity (x, unit)
}

#' @rdname isAsQuantity
#' @export
is.frequency <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        d   <- qDim (x)
        ok  <- length (d) ==  1 && d == c(time=-1)
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.frequency <- function (x, unit='second^-1') {
    unit <- paste ('time$', unit, sep='')
    as.quantity (x, unit)
}

#' @rdname isAsQuantity
#' @export
is.angle <- function (x) {
    ok <- is.quantity(x)
    if (ok) {
        d   <- qDim (x)
        ok  <- length (d) ==  1 && d == c(angle=1)
    }
    ok
}

#' @rdname isAsQuantity
#' @export
as.angle <- function (x, unit='angle') {
    unit <- paste ('angle$', unit, sep='')
    as.quantity (x, unit)
}

# ------------------------------------------------------------------------------
#                               format conversions
#                           timeToHms  &  hmsToSec
# ------------------------------------------------------------------------------
#' conversion of a duration to yy:mm:dd or hh:mm:ss
#' @param t a time quantity vector
#' @param sec a numeric vector with the time in seconds
#' @param day a numeric vector with the time in days
#' @param formal logical, if TRUE, the default, the yy:mm:dd or hh:mm:ss is used.
#' If FALSE redundant '0's and ':' are ommitted.
#' @param roundDecimals an integer to specify the number of non-zero figures for truncating the fraction of x.
#' Integers are nor rounded
#' @param year a character string which is either 'julian' or 'gregorian' (the default)
#' @param hhmmss a character string with the time denoted as hh:mm:ss and an optional fraction for the seconds
#' @param yymmdd a character string with the time denoted as yy:mm:dd and an optional fraction for the days
#' @return  secToHms and timeToHms return the duration as hh:mm:ss \cr
#' dayToYmd and timeToYmd return the duration as yy:mm:dd \cr
#' hmsToSec returns the duration as a time quantity with unbit 'second' \cr
#' ymdToDay returns the duration as a time quantity with unit 'day' \cr
#' @details Time is regarded as duration. Also hhh:mm:ss and yy.mm:dd are duration and not relative
#' to e.g. the beginning of a day or year. Condequenty, conversion are besed on the avarage month length.
#' Not on the calendre month length  \cr
#' If formal is FALSE, redundant '0's and ':'s wil be removed. \cr
#' secToHms, timeToHms, dayToYmd, and timeToYms coerce non-numeric input values to ''
#' hmsToSec and ymdToDay coerce illegal input values to NA
#' @name durationFormatConversion
NULL
#> NULL

# convert sec to hh:mm:ss
#' @rdname durationFormatConversion
#' @examples
#' secToHms (204.5)                     # returns  00:03:24.5
#' secToHms (-204, formal=FALSE)        # returns  -3:24
#' @export
secToHms <- function (sec, formal=TRUE, roundDecimals=6) {
    if (!is.numeric(sec))               stop ("sec must be numerical")
    if (!is.logical(formal))            stop ("formal must be logical")
    roundDecimals <- asInteger (roundDecimals)
    if (is.na(roundDecimals))     stop ("roundDecimals must be an integer")

    secToHms2 (x=sec, formal=formal, roundDecimals=roundDecimals)
}

# convert time to hh:mm:ss
#' @rdname durationFormatConversion
#' @examples
#' timeToHms (as.time (204.5))              # returns  00:03:24.5
#' timeToHms (as.time (75.5, 'minute'))     # returns  01:15:30
#' @export
timeToHms <- function (t, formal=TRUE, roundDecimals=6) {
    if (!is.time (t))               stop ("t must be a time quantity.")
    if (!is.logical(formal))        stop ("formal must be logical")
    roundDecimals <- asInteger (roundDecimals)
    if (is.na(roundDecimals))       stop ("roundDecimals must be an integer")

    sec <- as.numeric (as.quantity (t, 'time$second'))    # convert  to second
    secToHms2 (x=sec, formal=formal, roundDecimals=roundDecimals)
}

secToHms2 <- function (x, formal=TRUE, roundDecimals=6) {
    # prerequisite: no param checking required
    if (!is.logical(formal)) stop ("formal must be logical")
    n       <- length (x)
    if (n == 0 ) return ('')
    if (is.quantity(x)) {
        if (is.time (x)) {
            sec <- as.numeric (as.quantity (x, 'time$second'))    # convert  to second
        } else {
            stop (paste ("Cann't convert x to seconds"))
        }
    } else {
        sec <- asNumeric (x)
    }

    hhmmss  <- character (length=n)
    # isolate NA's and sign
    isNa        <- is.na (sec)
    if (length(sec) == sum (isNa))      return (hhmmss)
    aSec        <- sec [!isNa]                              # assigned, ie nonNa secs

    isNeg       <- aSec < 0
    aSec[isNeg] <- -aSec[isNeg]
    if (!is.null(roundDecimals))    aSec    <- roundDecimals (aSec, signif=roundDecimals)

    ss  <- aSec %%  60
    min <- aSec %/% 60
    mm  <- min  %%  60
    hh  <- min  %/% 60

    hms <- sprintf ('%02d:%02d:%09.6f', hh, mm, ss)
    hms <- gsub ('[.]?0*$', '', hms)                        # remove trailing '0's and a '.', if any
    if (!formal) {
        hms <- gsub ('^0*[:]?0*[:]?0*', '', hms)            # remove leading '0's and ':', if any
        hms <- gsub ('[:]0', ':', hms)                      # replacw ':0' with ':'
    }
    sgn             <- ifelse (isNeg, '-', '')
    hhmmss[!isNa]   <- paste (sgn, hms, sep='')
    hhmmss
}

#  convert hh:mm:ss to sec
#' @rdname durationFormatConversion
#' @examples
#' hmsToSec ("3:24")             # returns  3 * 60 + 24
#' hmsToSec ("-10:03:24.5")      # returns -10 * 3600 + 3 * 60 + 24.5
#' hmsToSec ("10.5::")           # returns 10.5 * 3600
#' @export
hmsToSec <- function (hhmmss) {
    n           <- length (hhmmss)
    sec         <- as.quantity (rep (NA, max(1,n)), 'time$second')              # reiurn 1 NA if n==0
    # check hhmmss
    if (n==0 || !is.character (hhmmss)) {
        warning ("hhmmss must be character, NAs introduced by coercion")
        return (sec)
    }

    # isolate NA's and signs
    isNa        <- is.na (hhmmss)
    aHms        <- trimws (hhmmss [!isNa])                  # assigned (ie nonNA) hhmmss's
    aN          <- length (aHms)
    isNeg       <- substr(hhmmss, 1, 1)== '-'
    aHms[isNeg] <- substring (aHms[isNeg], 2)

    # convert
    aHmsParts    <- strsplit(aHms, "[:]")
    aSec         <- numeric (aN)
    for (i in 1:aN) {
        hms     <- aHms[i]
        parts   <- aHmsParts[[i]]
        if ( grepl(':$', hms)) parts <- c(parts, '0')
        s       <- 0
        factor  <- 1
        for (j in length(parts):1) {
            part <- parts[j]
            if (part=='') part <- 0
            part <- asNumeric (part)
            if (is.na(part)) {
                s <- NA
                warning ("NAs introduced by coercion")
            }
            else {
                s <- s + factor * part
                factor <- factor * 60
            }
        }
        aSec[i] <- s
    }
    aSec[isNeg] <- -aSec[isNeg]
    sec[!isNa]  <- aSec
    sec
}

# ------------------------------------------------------------------------------
#                               format conversions
#                                 to/from YYMMDD
# ------------------------------------------------------------------------------

#  convert day to yy:mm:dd
#' @rdname durationFormatConversion
#' @examples
#' dayToYmd (204)                   # returns  00:06:21.38
#' dayToYmd (-204,   formal=FALSE)  # returns  -6:21.38
#' @export
dayToYmd <- function (day, formal=TRUE, year='gregorian', roundDecimals=4) {
    if (!is.numeric(day))           stop ("day must be numerical")
    if (!is.logical(formal))        stop ("formal must be logical")
    roundDecimals <- asInteger (roundDecimals)
    if (is.na(roundDecimals))       stop ("roundDecimals must be an integer")

    year    <- procesEnumertedParameter (tolower (year), c('julian', 'gregorian'))
    if (is.na (year))               stop ("Year must be (an abbreviation of) 'Julian' or 'Gregorian' (default).")

    dayToYmd2 (x=day, formal=formal, year=year, roundDecimals=roundDecimals)
}

#  convert time to yy:mm:dd
#' @rdname durationFormatConversion
#' @examples
#' timeToYmd (as.time (204.5, 'day'))   # returns  00:06:21.88
#' timeToYmd (as.time (75.5, 'year'))   # returns  75:06:00
#' @export
timeToYmd <- function (t, formal=TRUE, year='gregorian', roundDecimals=4) {
    if (!is.time (t))               stop ("t must be a time quantity.")
    if (!is.logical(formal))        stop ("formal must be logical")
    roundDecimals <- asInteger (roundDecimals)
    if (is.na(roundDecimals))       stop ("roundDecimals must be an integer")

    year    <- procesEnumertedParameter (tolower (year), c('julian', 'gregorian'))
    if (is.na (year))               stop ("Year must be (an abbreviation of) 'Julian' or 'Gregorian' (default).")

    day <- as.numeric (as.quantity (t, 'time$day'))    # convert  to day
    dayToYmd2 (x=day, formal=formal, year=year, roundDecimals=roundDecimals)
}

dayToYmd2 <- function (x, formal=TRUE, year='gregorian', roundDecimals=4) {
    n       <- length (x)
    if (n == 0 ) return ('')

    qsYr        <- ifelse (year=='julian', 'time$annus', 'time$year')          # ifelse strips attributes
    yr          <- as.quantity (1, qsYr)
    dPerY       <- as.numeric (as.quantity (yr, 'time$day'))
    # cat ("dayToYymmdd: dPerY=", dPerY, "yr=", year ,'\n')

    yymmdd  <- character (length = n)
    # isolate NA's and sign
    isNa        <- is.na (x)
    if (length(x) == sum (isNa))          return (yymmdd)
    aDay        <- x [!isNa]                              # assigned, ie nonNA, days
    isNeg       <- aDay < 0
    aDay[isNeg] <- -aDay[isNeg]                             # make all values positive

    year    <- aDay / dPerY
    yy      <- floor (year)
    month   <- (year-yy) * 12
    mm      <- floor (month)
    dd      <- (month - mm) * dPerY / 12
    if (!is.null(roundDecimals))    dd  <- roundDecimals (dd, signif=roundDecimals)

    ymd <- sprintf ('%02d:%02d:%07.4f', yy, mm, dd)
    ymd <- gsub ('[.]?0*$', '', ymd)                        # remove trailing '0's and a '.', if any
    if (!formal) {
        ymd <- gsub ('^0*[:]?0*[:]?0*', '', ymd)            # remove leading '0's and ':', if any
        ymd <- gsub ('[:]0', ':', ymd)                      # replacw ':0' with ':'
    }
    sgn             <- ifelse (isNeg, '-', '')
    yymmdd[!isNa]   <- paste (sgn, ymd, sep='')
    yymmdd
}



#  convert ymd to day
#' @rdname durationFormatConversion
#' @examples
#' ymdToDay ("00:06:21.38")         # returns 204.0012
#' ymdToDay ("-6:7.15")             # returns -2409.079
#' @export
ymdToDay <- function (yymmdd, year='gregorian') {
    if (!is.character (yymmdd))     return (as.numeric (NA))
    year    <- procesEnumertedParameter (tolower (year), c('julian', 'gregorian'))
    if (is.na (year))               stop ("Year must be (an abbreviation of) 'julian' or 'gregorian' (default).")
    yymmdd  <- trimws (yymmdd)
    sgn <- 1
    if (substr(yymmdd, 1, 1)=='-') {
        yymmdd <- substring (yymmdd, 2)
        sgn     <- -1
    }
    parts       <- unlist(strsplit(yymmdd, "[:]"))
    if ( grepl(':$', yymmdd)) parts <- c(parts, '0')

    qs          <- ifelse (year=='julian', 'time$annus', 'time$year')
    dPy         <- as.numeric (as.quantity ( as.quantity (1, qs ), 'time$day'))
    factors     <- c (dPy, dPy / 12, 1)
    days        <- 0
    for (i in 1:length(parts)) {
        part    <- parts[i]
        if (part == '') part <- 0
        part  <- asNumeric (part)
        if (is.na(part)) return (as.numeric (NA))
        days <- days + part * factors[i]
    }
    sgn * days
}
