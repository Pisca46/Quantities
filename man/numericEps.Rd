% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/numeric.R
\name{numericEps}
\alias{numericEps}
\alias{getNumericUpperSpacing}
\alias{getNumericLowerSpacing}
\alias{getEpsDistance}
\alias{coerceNearToInt}
\title{get the spacing, distance and near values for successive numeric values}
\usage{
getNumericUpperSpacing(x)

getNumericLowerSpacing(x)

getEpsDistance(x, y)

coerceNearToInt(x, maxEpsDistance)
}
\arguments{
\item{x}{numeric vector}

\item{y}{numeric vector}

\item{maxEpsDistance}{an integer specifying the number of successive numerical values considerd to be nearby.}
}
\value{
the space between two successive numeric values
}
\description{
get the spacing, distance and near values for successive numeric values
}
\details{
getNumericUpperSpacing and getNumericLowerSpacing return the distance to
the next higher / lower numeric value \cr
getEpsDistance return the number of intermediate numeric values between x and Y plus 1 \cr
coerceNearToInt coerce values near to an integer valeus to that integer \cr
More precise, the spacing between x and the successive numeric value is returned. \cr
}
\references{
https://en.wikipedia.org/wiki/Machine_epsilon
}
