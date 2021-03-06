% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qDurationCA.R
\name{durationFormatConversion}
\alias{durationFormatConversion}
\alias{secToHms}
\alias{timeToHms}
\alias{hmsToSec}
\alias{dayToYmd}
\alias{timeToYmd}
\alias{ymdToDay}
\title{conversion of a duration to yy:mm:dd or hh:mm:ss}
\usage{
secToHms(sec, formal = TRUE, roundDecimals = 6)

timeToHms(t, formal = TRUE, roundDecimals = 6)

hmsToSec(hhmmss)

dayToYmd(day, formal = TRUE, year = "gregorian", roundDecimals = 4)

timeToYmd(t, formal = TRUE, year = "gregorian", roundDecimals = 4)

ymdToDay(yymmdd, year = "gregorian")
}
\arguments{
\item{sec}{a numeric vector with the time in seconds}

\item{formal}{logical, if TRUE, the default, the yy:mm:dd or hh:mm:ss is used.
If FALSE redundant '0's and ':' are ommitted.}

\item{roundDecimals}{an integer to specify the number of non-zero figures for truncating the fraction of x.
Integers are nor rounded}

\item{t}{a time quantity vector}

\item{hhmmss}{a character string with the time denoted as hh:mm:ss and an optional fraction for the seconds}

\item{day}{a numeric vector with the time in days}

\item{year}{a character string which is either 'julian' or 'gregorian' (the default)}

\item{yymmdd}{a character string with the time denoted as yy:mm:dd and an optional fraction for the days}
}
\value{
secToHms and timeToHms return the duration as hh:mm:ss \cr
dayToYmd and timeToYmd return the duration as yy:mm:dd \cr
hmsToSec returns the duration as a time quantity with unbit 'second' \cr
ymdToDay returns the duration as a time quantity with unit 'day' \cr
}
\description{
conversion of a duration to yy:mm:dd or hh:mm:ss
}
\details{
Time is regarded as duration. Also hhh:mm:ss and yy.mm:dd are duration and not relative
to e.g. the beginning of a day or year. Condequenty, conversion are besed on the avarage month length.
Not on the calendre month length  \cr
If formal is FALSE, redundant '0's and ':'s wil be removed. \cr
secToHms, timeToHms, dayToYmd, and timeToYms coerce non-numeric input values to ''
hmsToSec and ymdToDay coerce illegal input values to NA
}
\examples{
secToHms (204.5)                     # returns  00:03:24.5
secToHms (-204, formal=FALSE)        # returns  -3:24
timeToHms (as.time (204.5))              # returns  00:03:24.5
timeToHms (as.time (75.5, 'minute'))     # returns  01:15:30
hmsToSec ("3:24")             # returns  3 * 60 + 24
hmsToSec ("-10:03:24.5")      # returns -10 * 3600 + 3 * 60 + 24.5
hmsToSec ("10.5::")           # returns 10.5 * 3600
dayToYmd (204)                   # returns  00:06:21.38
dayToYmd (-204,   formal=FALSE)  # returns  -6:21.38
timeToYmd (as.time (204.5, 'day'))   # returns  00:06:21.88
timeToYmd (as.time (75.5, 'year'))   # returns  75:06:00
ymdToDay ("00:06:21.38")         # returns 204.0012
ymdToDay ("-6:7.15")             # returns -2409.079
}
