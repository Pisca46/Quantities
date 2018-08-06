#                                   quantities
#
# Purpose   :   support of defined quantities
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
# History   :
#     Oct17 - Created
#     Mar18 - Revised
#     Jun18 - Revised
#     Jul18 - quantity defintions moved to 'quantityDefinitions.R'
# TO DO:
#     support quantity and unit name abbreviations
# ------------------------------------------------------------------------------
# an R object x of class quantity is a numeric with the following properties:
#   'quantity' is one of the class vectors
#   it may have the attributes: prefix, unit, and symbol with default values 'none', '', and ''
#   the prefix attributes specifies the name of the metric prefix (with 'none' for '')
#   the unit attribute specifies the unit(s) of power 1 and -1, e.g. c('metre', '/second', '/second')  for acceleration
#   the symbol attribute specifies the symbol(s) of power 1 ans -1, e.g. c('m', '/s', '/s) for acceleration
#   the units and symbols shall be at corresponding positions ('' values are allowed)
#' @include misc.R
#' @include numeric.R
NULL

# ------------------------------------------------------------------------------
#                      check, create, and convert quantities
# ------------------------------------------------------------------------------

#' Create, convert or test objects with class 'quantity'
#' @param x an quantity or a numeric object
#' @param unit a character string or character vector specifying the units of measurement
#' @param qName a charater string with the name of a quantity, e.g. 'time'
#' @details the unit is specified as a character string with single unit specifications seperated by spaces. The first
#' single unit specification may specify a metric quantity prefix, the other specify a single unit by
#' the quantity name, a '$', an optional metric unit prefix followed by a'.', an optional unit name, and
#' an optional exponent preceeded by a '^'. If the prefix is ommitted the default will be 'none',
#' if the unit name is ommited the default will be the one as specified in the quantity definition, and
#' if the exponent is ommitted the default will be 1. \cr
#' as.qqq (x, 'uuu') is just a short-hand for as.quantity (x, 'qqq$uuu)  \cr
#' If x is a numeric it is coverted to quntity wiht the same value and unit as uuu.
#' If x is a qunatity its value is converted to uuu.
#' @return  is.quantity returns TRUE or FALSE depending on wether or not x is of a class 'quantity' \cr
#' as.quantity converts a numeric to a 'quantity' or converts the scale of a quantity \cr
#' is.qqq returns TRUE or FALSE depending on wether or not x is an qqq quantity \cr
#' as.qqq returns a qqq quantity \cr
#' qUnit returns a character string with the unit specification \cr
#' qSymbol returns a character string with the quantity symbol(s) \cr
#' qText returns a character string with the quantity as textdev
#' @name isAsQuantity
NULL
#> NULL

#' @rdname isAsQuantity
#' @examples
#' is.quantity (1)                              # returns  FALSE
#' is.quantity (as.quantity (1, unit='mass'))   # returns TRUE
#' @export
is.quantity <- function (x) {
    c   <- class (x)
    l   <- length (c)
    isQ <- l >= 2
    if (isQ) isQ <- c[l-1] == 'quantity'  && c[l]=='numeric'
    if (isQ) {
        pqs <- parseQunit (x)
        if (!is.null (pqs$err)) isQ <- FALSE
    }
    isQ
}

#' @rdname isAsQuantity
#' @examples
#' as.quantity (5, 'time$milli.second^-2')  # returns 5 millisecond⁻²
#' t <- as.quantity (60, 'time')
#' t                                    # returns 60 seconds
#' as.quantity (t, 'time$minute')       # returns 1 minute
#' @export
as.quantity <- function (x, unit) {
    if (is.list(unit))  pqs <- unit
    else                pqs <- parseQunit (unit)
    if (!is.null (pqs$err)) stop (pqs$err)

    if (is.quantity (x)) {
        # scale converstion
        x <- convertQuantityScale (x, newPqs=pqs)
    } else {
        # x as new quantity
        x                   <- asNumeric (x)
        class (x)           <- c ('quantity', 'numeric')
    }
    attr (x, 'unit')    <- assemblePunit (pqs)          # without abbreviations
    x
}

#' @rdname isAsQuantity
#' @examples
#' qUnits ('time') [6:9]                # returns "day" "hour" "minute" "second"
#' @export
qUnits <- function (qName) {
    if (is.null(qEnvir$qDefs[[qName]])) {
        warning (paste ("Undefined quantity '", qName, "'", sep=''))
        units   <- ''
    } else {
        units   <- names (qEnvir$qDefs[[qName]]$units)
    }
    units
}

#' @rdname isAsQuantity
#' @examples
#' qUnit (1)                            # raises an error
#' t <- as.quantity (1, 'time')
#' qUnit (t)                            # returns "time$second"
#' @export
qUnit <- function (x) {
    if (!is.quantity(x)) {
        warning ("not a quantity")
        unit    <- ''
    } else {
        unit    <- attr(x, 'unit')
    }
    unit
}

#' @rdname isAsQuantity
#' @examples
#' qUnit (1)                            # raises an error
#' t <- as.quantity (1, 'time')
#' qUnit (t)
#' @export
qDim <- function (x) {
    if (!is.quantity(x)) {
        warning (paste ("not a quantity"))
        dim <- ''
    } else {
        dim <- qs2dim (qs=parseQunit(x))
    }
   dim
}

#' @rdname isAsQuantity
#' @examples
#' t <- as.quantity (2, unit='time$second')
#' qSymbol (t)                          # returns 's'
#' @export
qSymbol <- function (x) formatQuantityUnit (x, unitAs = 'symbol')$qf

#' @rdname isAsQuantity
#' @examples
#' t <- as.quantity (2, unit='time$second')
#' qText (t)                            # returns 'seconds'
#' @export
qText   <- function (x) formatQuantityUnit (x, unitAs = 'text')$qf

# ------------------------------------------------------------------------------
#                               basic internal functions
# ------------------------------------------------------------------------------
qs2dim <- function (qs) {
    # quantity specification (pqs or uqs) to an integer dim vector
    # prerequisite: no param check needed
    if (is.character (qs))  qs <- parseQunit (qs)
    qs      <- expandPqs(qs)$pqs
    qns     <- setdiff (unique (sapply(qs, function(X) X$q)), 'prefix')
    l       <- length (qns)
    dNames  <- character(l)
    dims    <- integer (l)
    for (i in 1:l) {
        qn          <- qns[i]
        dNames[i]   <- qn
        dims[i]     <- sum (sapply (qs, function (X) ifelse (X$q==qn, X$e, 0)))
    }
    names(dims) <- dNames
    dims
}

convertQuantityScale <- function (x, newPqs) {
    oldPqs  <- parseQunit (x)
    if (!is.null(oldPqs$err)) stop ("not a valid quantity")
    ok      <- setequal (qs2dim (oldPqs), qs2dim (newPqs))
    if (!ok) {
        warning ("Incompatible quantity dimension, NAs introduced by coercion")
        x <- rep (as.numeric (NA), times=length (x))
    } else {
        z   <- pqsConversionFactor (oldPqs, newPqs)
        x   <- as.numeric (x) * 10^-z$pe / (z$f)
    }
    x <- as.quantity (x, newPqs)
}

# user specificatiion : character vector with strings: quantity[unit]^exp
#' parses a user quantity specification
#' @keywords internal
parseQunit <- function (x) {          # x a quantity or a character (string or vector) with a quantity specification
    uqs     <- extractUqs (x)
    if (!is.null(uqs$err)) return (list(err=uqs$err))
    uqs     <- uqs$uqs
    l       <- length (uqs)
    pUnit   <- vector (mode='list', length=l)
    if (l > 0) for (i in 1:l) {
        qsi         <- uqs[i]
        # extract the exponent first
        qpue    <- extractQExponent (qsi)
        if (!is.null(qpue$err)) return (list(err=qpue$err))
        qpu         <- qpue$qpu
        e           <- qpue$e
        #extract the quantity
        qpu         <- extractQquantity (qpu)
        if (!is.null(qpu$err)) return (list(err=qpu$err))
        q           <- qpu$q
        if (q=='prefix' & e!=1) {
            return (list (err=paste ("The exponent for the 'prefix' quantity must be ommited or 1")))
        }
        pu          <- qpu$pu
        # extract the unit and its prefix
        pu          <- extractQunitAndPrefix (q=qEnvir$qDefs[[q]], pu=pu)
        if (!is.null(pu$err)) return (list(err=pu$err))
        pUi         <- list (q=q, p=pu$p, pe=pu$pe, u=pu$u, s=pu$s, e=e)        # p = prefix name & pe = prefix exponent; s= system
        pUi$unit    <- assemblePunit (list (pUi), formal=FALSE)
        pUnit[[i]]  <- pUi
    }
    if (l == 1 & pUnit[[1]]$q == 'prefix') {
        return (list (err="only a 'prefix', no quantity specified"))
    }
    if (l > 1) {
        prefixL <- as.logical (sapply (pUnit, function(X) X$q=='prefix'))
        if (sum (prefixL[2:length(prefixL)] > 0 )) {
            return (list(err="'prefix' may be used as quantity at the first postion only"))
        }
    }
    pUnit
}

pqsIsQSpec <- function (pqs) {                 # an input check
    if (!is.list(pqs)) return (FALSE)
    l   <- length (pqs)
    if (l == 0) return (FALSE)
    for (i in 1:l) {
        pqsi    <- pqs[[i]]
        q       <- pqsi$q
        qDef    <- qEnvir$qDefs[[q]]
        if (q=='prefix' & i != 1)       return (FALSE) # illegal prefix position
        if (q=='prefix' & l == 1)       return (FALSE) # a prefix def only
        if (is.null(qDef))              return (FALSE) # illegal quantity
        pDef    <- pqsi$p %in% qEnvir$qDefs$prefix$units
        if (is.null (pDef))             return (FALSE) # illegal prefix unit
        uDef    <- qDef$units[[pqsi$u]]
        if (is.null(uDef))              return (FALSE) # illegal unit
        if (is.na (asInteger(pqsi$e)))  return (FALSE) # illegal exponent
    }
    TRUE
}

extractUqs <- function (x) {
    c   <- class (x)
    l   <- length (c)
    isQ <- l >= 2
    if (isQ) isQ <- 'quantity' == c[l-1]  && 'numeric' ==  c[l]
    if (isQ)    uqs <- attr(x, 'unit')
    else        uqs <- x
    err <- list (err=paste ("Illegal quantity specification '", uqs, "'", sep=''))
    l   <- length(uqs)
    if (!is.character (uqs) | l==0)   return (err)
    if (l == 1) {  # split, if needed on ' '
        uqs    <- strsplit (uqs, ' ')[[1]]
        if (length(uqs)==0)   return (err)
        uqs <- uqs[uqs!='']
    }
    u <-list (uqs=uqs)
}

extractQExponent <- function (qsi) {
    split <- strsplit (qsi, '[/^]')[[1]]
    l <- length (split)
    if (l > 2) {
        return (list (err=paste ("Illegal", qsi, "only one '^' allowed")))
    }
    qpu <- split[1]
    if (l==2) {
        e   <- asInteger (split[2])
        if (is.na (e)) {
            return (list (err=paste ("Illegal non-integer exponent in '", qsi, "'", sep='')))
        }
    } else {
        e   <- 1
    }
    list (qpu=qpu, e=e)
}

extractQquantity <- function (qpu) {
    split  <- strsplit (qpu, '[/$]')[[1]]
    l <- length (split)
    if (l > 2) {
        return (list (err=paste ("Illegal", qpu, "only one '$' allowed")))
    }
    q0  <- split[1]
    q   <- procesEnumertedParameter (q0, names(qEnvir$qDefs))
    if (is.na (q))  return (list (err=paste ("Illegal quantity '", q0, "'", sep='')))
    if (l==2) {
        pu  <- split[2]
    } else {
        pu <- qEnvir$qDefs[[q]]$default
    }
    list (q=q, pu=pu)
}

extractQunitAndPrefix <- function (q, pu) {
    # NOTE: also invoked when adding q defintions, i.e. with q not yet in qEnvir! (and q$name may be 'prefix')
    pu  <- splitUnitAndPrefix (pu)
    if (!is.null(pu$er)) return (pu)
    p0  <- pu$p
    u0  <- pu$u

    # check unit (incl a prefix unit if q='prefix')
    if (is.na (u0)) {
        u <-  q$default
    } else {
        u  <- procesEnumertedParameter (u0, names(q$units))
        if (is.na (u))      return (list (err=paste ("Illegal '", q$name, "' unit '", u0, "'", sep='')))
    }
    s <- q$units[[u]]$system

    # check the unit prefix
    pus <- qEnvir$qDefs$prefix$units
    p   <- procesEnumertedParameter (p0, names(pus))
    if (is.na (p))            return (list (err=paste ("Illegal unit prefix '", p0, "'", sep='')))
    if (q$name=='prefix') {
        if (p!='none') {
            return (list (err=paste ("The unit prefix for the 'prefix' quantity must be ommited or 'none'")))
        }
        pe  <- pus[[u]]$pe
    } else {
        pe  <- pus[[p]]$pe
    }
    list (p=p, pe=pe, u=u, s=s)
}

splitUnitAndPrefix <- function (pu) {
    if (pu == '') return (list (p='none', u = NA))
    split  <- strsplit (pu, '[.]')[[1]]
    l <- length (split)
    if (l > 2) {
        return (list (err=paste ("Illegal", pu, "only one '.' allowed")))
    }
    if (l==2) {
        p   <- split[1]
        u   <- split[2]
    } else {
        if (nchar(pu) == nchar(split[1])) {      # i.e. no dot present => no prefix
            p  <-   'none'
            u  <-   split[1]
        } else {                                 # a dot present => prefix. = default unit (looks rare but allowd)
            p   <-  split[1]
            u   <-  NA
        }
    }
    list (p=p, u=u)
}

expandPqs <- function (pqs) {                                                   # XYZ
    # prerequisite: no param checking needed
    eqs     <- list()
    j       <- 1
    f       <- 1
    pe       <- 0
    if (pqs[[1]]$q == 'prefix') {
        pe          <- pe + qEnvir$qDefs$prefix$units[[pqs[[1]]$u]]$pe
        pqs[[1]]    <- NULL
    }
    for (i in 1:length(pqs)) {
        pqs0    <- pqs[[i]]
        q0      <- pqs0$q
        nQ0Dim  <- length (qEnvir$qDefs[[q0]]$dim)
        if (nQ0Dim == 1) {                              # not expandable
            eqs[[j]]    <- pqs0
            j           <- j + 1
        } else {
            us      <- qEnvir$qDefs[[pqs0$q]]$units
            u0      <- us[[pqs0$u]]
            e0      <- pqs0$e
            pqsD    <- u0$pqs                           # if not NULL: def substitution; no conversion factor fD/f0
            if (is.null (pqsD)) {
                uD      <- getDefPqs (us, pqs0$u)       # get/select a defined unit
                pqsD    <- uD$pqs
                # so by definition  q0$(none.(uD$name)^1 = pqsD    (e.g: power$watt = energy$joule time$second^-1)
                # Substituting gives:
                # q0 $ (pqs0$pe * pqs0$u) ^ pqs0$e  =
                # q0 $ (pq0s$pe * f0/fD pqsD) ^ pqs0$e =             with f0 = u0$factor and fD = ud$factor
                # q0 $ pq0s$pe^pqs0$e * fD/f0^pqs0$e  * pqsD^pqs0^e
                f       <- f * (u0$factor / uD$factor)^e0
            }
            pe      <- pe + pqs0$pe * e0
            pqs1    <- pqsD
            for (k in 1:length(pqs))  pqs1[[k]]$e <- pqs1[[k]]$e  * e0

            eqs1    <- expandPqs (pqs1)
            f       <- f * eqs1$f
            pe      <- pe + eqs1$pe
            pqs1    <- eqs1$pqs
            for (k in 1:length (pqs1)) {
                eqs[[j]]    <- pqs1[[k]]
                j           <- j + 1
            }
        }
    }
    list (pqs=eqs, f=f, pe=pe)
}

getDefPqs <- function (us, u0) {
    usDef   <- us [sapply (us, function (X) !is.null(X$pqs)) ]
    if (length(usDef) == 1) {
        u1      <- usDef[[1]]
    }  else {  # decide common sytem or first
        sys0    <- u0$system
        if (is.null(sys0)) sys0 <- 'ISO'                                        # default system
        isSys0  <- sapply (usDef, function(X) !is.null(X$system) && X$system==sys0)
        if (sum(isSys0))    i <- which.max(isSys0)
        else                i <- 1
        u1   <- usDef[[i]]
    }
    u1
}

assemblePunit <- function (pqs, formal=FALSE) {
    # returns a character vector
    l <- length(pqs)
    unit <- character (length=l)
    for (i in 1:l) {
        pUi    <- pqs[[i]]
        if (formal) {
            if (pUi$q == 'prefix')  unit[i]   <- paste ('prefix$', pUi$u, sep='')
            else                    unit[i]   <- paste (pUi$q,'$', pUi$p, '.', pUi$u, '^', pUi$e, sep='')
        } else {
            p       <- ifelse ( pUi$q == 'prefix' ||  (!is.null(pUi[['p']]) && pUi$p == 'none'), '', paste (pUi$p, '.', sep=''))     # pIu$p is read as pUi$pe if is.null(pIu$p)
            e       <- ifelse (pUi$e ==   1   , '', paste ('^', pUi$e, sep=''))
            unit[i] <- paste (pUi$q,'$', p, pUi$u, e, sep='')
        }
    }
    unit
}

# ------------------------------------------------------------------------------
#                               quantity operations
# ------------------------------------------------------------------------------
#' Quantity operations
#' @param x,y a quantity or a numeric
#' @param ... a number of quantities (or numerics depending on the operation)
#' @param ql a list of quantities and, eventually, NULLs
#' @param na.rm logical. Should missing values be removed?
#' @param unit a character string or vector with a unit specification
#' @param skipNull logical Skip NULL elements in unlist
#' @name qOperations
NULL
#> NULL

#' @rdname qOperations
#' @examples
#' x <- as.quantity (2, 'time$day')
#' y <- as.quantity (12, 'time$hour')
#' qSum (x,y)       # returns 2,5 days
#' qSum (y,x)       # returns 60 hours
#' qSum (x,y, unit='time$week')
#' @export
qSum <- function (..., na.rm=FALSE, unit=NULL) {
    qMinMaxSum (..., func='sum', na.rm=na.rm, unit=unit)
}

#' @rdname qOperations
#' @examples
#' x <- as.quantity (c(1,2,3), 'time$day')
#' qMax (x,y)       # returns 3 days
#' @export
qMax <- function (..., na.rm=FALSE, unit=NULL) {
    qMinMaxSum (..., func='max', na.rm=na.rm, unit=unit)
}

#' @rdname qOperations
#' @examples
#' x <- as.quantity (c(1,2,3), 'time$day')
#' qMin (x,y)       # returns 1 day
#' @export
qMin <- function (..., na.rm=FALSE, unit=NULL) {
    qMinMaxSum (..., func='min', na.rm=na.rm, unit=unit)
}

#' @rdname qOperations
#' @examples
#' x <- as.quantity (2, 'time$second')
#' qInv (x)      # returns 0.5 second⁻¹
#' @export
qInv <- function (x) {
    if (!is.quantity(x))  return (1/x)
    xQs <- parseQunit (x)
    x   <- as.numeric (x)
    as.quantity (1/x, InvertQpqs(xQs))
}

#' @rdname qOperations
#' @examples
#' x <- as.quantity (2, 'time$day')
#' y <- as.quantity (12, 'time$hour')
#' qProd (x, y)      # returns 1 day²
#' qProd (y, x)      # returns 576 hours²
#' @export
qProd <- function (..., na.rm=FALSE, unit=NULL) {
    factors <- list (...)
    noNums  <- sum (!sapply (factors, is.numeric))
    if (noNums) prod (...) # will result in a 'prod' error code
    isQ     <- sapply (factors, is.quantity)
    nFacs   <- factors [!isQ]
    nProd   <- prod (unlist(nFacs), na.rm=na.rm)
    if (sum (isQ)) {
        qFacs   <- factors [isQ]
        pDim    <- addDims (qFacs)
        if (!is.null(unit)) {
            uDim    <- qs2dim (unit)
            ok      <- sameDimensions (pDim, uDim)
            if (!ok) stop (paste ("dimension '", unit, "' differs from product dimension"))
        }
        p       <- multiplyQuantities (a=nProd, qFacs, na.rm=na.rm, pDim=pDim)  # product with default units for all dimensions
        if (!is.null (unit)) {
            p <- as.quantity (p, unit)
        } else {
            # cat ("qProd: uncondensed", "format(p)=", format(p), '\n')
            d <- condenseDim (qDim (p))     # optimise / condense dim
            p <- as.quantity (p, dim2unit(d))
            # cat ("qProd: condensed", "format(p)=", format(p), '\n')
            p <- normaliseQValue (p)
            # cat ("qProd:normalised", "format(p)=", format(p), '\n')
        }
    } else {
        p <- nProd
    }
    p
}

#' @rdname qOperations
#' @examples
#' x <- as.quantity (2, 'time$day')
#' y <- as.quantity (12, 'time$hour')
#' qDiv (x,y)       # returns 4
#' qDiv (y,x)       # returns .25
#' @export
qDiv <- function (x, y, na.rm=FALSE, unit=NULL) {
    qProd (x, qInv(y), na.rm=na.rm, unit=unit)
}

#' @rdname qOperations
#' @details sameQdim test whether or not two quantities have the same dimension. There scale may differ.
#' @examples
#' x <- as.quantity (2, 'time$day')
#' y <- as.quantity (12, 'time$hour')
#' sameQdim (x,y)       # returns TRUE
#' @export
sameQdim <- function (...) {          # one or more quantity objects
    qs  <- list (...)
    l   <- length(qs)
    ok  <- TRUE
    if (l) for (q in qs) {
        if (!is.quantity (q)) {
            warning ("Not all elements are quantities")
            ok <- FALSE
        }
    }
    if (l > 1) {
        dim     <- qs2dim (parseQunit (qs[[1]]))
        dNames  <- names (dim)
        for (i in 2:l) if (ok) {
            di  <- qs2dim (parseQunit (qs[[i]]))
            ok <- setequal (dNames, names(di))
            if (ok) ok <- sum (sapply (dNames, function(X) dim[X] != di[X]))==0
        }
    }
    ok
}

#' @rdname qOperations
#' @details the Q equivalent for unlist. All non-NULL elements must have the same dimension and
#' all scales are converted to the scale of the first element.
#' @examples
#' x <- as.quantity (2, 'time$day')
#' y <- as.quantity (12, 'time$hour')
#' qUnlist (list (x, NULL, y)) # returns as.quantity (c(2, .5), time$day)
#' @export
qUnlist <- function (ql, skipNull=TRUE, unit=NULL) {
    if (!is.null(unit)) {
        pu <- parseQunit (unit)
        if (!is.null (pu$err)) stop (pu$err)
    }
    isNull  <- as.logical (sapply(ql, is.null ))
    if (skipNull & sum(isNull)) {
        ql <- ql [!isNull]
        isNull <- logical (length (ql))
    }

    if (length (ql) == 0 | sum(isNull) == length(ql)) {
        if (is.null(unit)) return (NULL)
        return (as.quantity (numeric(), unit))
    }
    # at least one non-NULL
    isQ     <- as.logical (sapply (ql, is.quantity))
    # all non-NULL must be quantities (incl NAs)
    if (sum(!isNull) != sum (isQ))  stop ("all non-NULL elements must be quantities")
    ql1             <- ql[[which.max(isQ)]]
    dimQl1          <- qs2dim (parseQunit(ql1))
    if (sum(isNull))  ql[isNull] <- as.numeric (NA)
    # check compatibility of at least two quantities
    ok      <- TRUE
    for (q in ql[!isNull]) ok <- ok & sameDimensions (qs2dim (parseQunit(q)), dimQl1)      # qs2dim (parseQunit(q))
    if (!ok) stop ("Cann't unlist, ql has non-NULL elements with different dimensions")
    if (is.null(unit)) {
        unit <- qUnit(ql1)
    } else {
        qu <- as.quantity(q, unit)
        ok <- sameDimensions (qs2dim (parseQunit(qu)), dimQl1)
        if (!ok) stop ("The list elements and unit have different dimensions")
     }
    for (i in 1:length(ql)) {
        ql[[i]] <- as.quantity (ql[[i]], unit)       # convert to ql1 to the same scale
    }
    as.quantity (unlist(ql), unit)                    # unlist will strip all ettributes
}

#' Adjust a quantity scale to a value and/or prefix and unit constraints
#' @param x a quantity
#' @param to a numerical 'target' value
#' @param mode character string which may have the following values 'max', if 'to' is the maximum value,
#' 'min' if 'to' is the minimum value, 'round, if 'x' should be 'rounded' to 'to', and 'fraction'
#' if the fraction 'x'/'to' should be minimized.
#' @param prefix NULL or a characxter vector with the prefixes allowed. The vector may coantain
#' two special values '=' and'1000s'. '=' denotes the prefix of x, '1000s' all prefixes except
#' 'hecta', 'deca', 'deci', and 'centi'. If NULL all prefixes are allowed.
#' @param unit NULL or a character vector with the units allowed. The vector may contain also a '=',
#' denoting the unit of x. If NULL all units are allowed.
#' @details The mode, prefix and unit values may be abbreviated. \cr
#' The only quantity affected is the first (non-prefix) quantity in the quantity defintion.
#' @export
adjustQscale <- function (x, to=NULL,  mode='fraction', prefix=NULL, unit=NULL) {
    # check on legal input
    if (!is.quantity (x))   stop ("x must be a quantity")
    if (!is.null(to) && !is.numeric (to)) stop (("'to' must be NULL or numeric"))
    mode    <- procesEnumertedParameter (mode, type= c ('min', 'max', 'round', 'fraction'))
    if (is.na (mode))       stop ("mode must be eihter 'min', 'max', 'round', or 'fraction'")
    pqs <- parseQunit (x)
    qi  <- ifelse (pqs[[1]]$q != 'prefix', 1, 2)
    q   <- pqs[[qi]]$q
    # check prefix
    ps <- names (qEnvir$qDefs$prefix$units)
    if (!is.null(prefix)) {
        psp <- c(ps, '1000s', '=')
        for (i in 1:length(prefix)) {
            p   <- procesEnumertedParameter (prefix[i], psp)
            if (is.na (p)) stop (paste ("illegal prefix '", prefix[i], "'", sep=''))
            else prefix[i] <- p
        }
        if ('=' %in% prefix) {
            px      <- ifelse (pqs[[1]]$q == 'prefix', pqs[[1]]$u, pqs[[1]]$p)
            prefix  <- union (setdiff (prefix, '='), px)
        }
        if ('1000s' %in% prefix) {
            prefix  <- union (setdiff(prefix, '1000s'), setdiff(ps, c('hecto', 'deca', 'deci', 'centi'))) # add all 'thousants'
        }
        ps <- ps[sapply (ps, function (X) X %in% prefix)]   # order prefix according to ps
    }
    # check unit
    us  <- names (qEnvir$qDefs[[q]]$units)
    if (!is.null(unit)) {
        usp <- c (us, '=')
        for (i in 1:length(unit)) {
            u   <- procesEnumertedParameter(unit[i], usp)
            if (is.na (u)) stop (paste ("illegal unit '", unit[i], "'", sep=''))
            else unit[i] <- u
        }
        if ('=' %in% unit) {
            ux      <- pqs[[qi]]$u
            unit    <- union (setdiff(unit, '='), ux)
        }
        us <- us[sapply (us, function (X) X %in% unit)]   # order prefix according to us
    }
    # to == NULL
    if (is.null(to)) {
        if (!is.null(prefix) && prefix!='=') pqs[[qi]]$p <- prefix[1]
        if (!is.null(unit)   && unit  !='=') pqs[[qi]]$u <- unit[1]
        return (as.quantity (x, pqs))
    }
    return (adjustQprefixAndUnit (x, to, mode=mode, ps=ps, us=us))
}
# ------------------------------------------------------------------------------
#                          internal qOperation unctions
# ------------------------------------------------------------------------------
#                              qSum & qProd support
# ------------------------------------------------------------------------------
InvertQpqs <- function (pQs) {
    l <- length(pQs)
    if (l) for (i in 1:l) {
        pQs[[i]]$e <- - pQs[[i]]$e
    }
    pQs
}

qMinMaxSum <- function (..., func, na.rm, unit=NULL) {
    if      (func == 'min') f <- min
    else if (func == 'max') f <- max
    else                    f <- sum
    terms   <- list (...)
    l       <- length (terms)
    if (l) qTerms   <- sapply (terms, is.quantity)
    if (l==0 || sum(qTerms) == 0) return (f (..., na.rm=na.rm))
    if (sum(qTerms) != l) stop ("Either none or all .. parameters must quantities")
    # at least one param and all are quantities
    pqs0    <- processUnitParam (unit, defaultUnit=parseQunit(terms[[1]]))
    if (is.null (pqs0)) { # or use err to report errors
        if (is.null(unit)) stop ("Illegal unit")
        else               stop ("First parameter is not a quantity")
    }
    applyQMinMaxSum (terms=terms, func=func, f=f, na.rm=na.rm, pqs0=pqs0, unit=unit)
}

processUnitParam <- function (unit, defaultUnit=NULL) {
    if (is.null (unit)) unit <- defaultUnit
    if (is.list(unit)) {
        if (pqsIsQSpec (unit))  return (unit)
        else                    return (NULL)
    } else if (is.character (unit)) {
        pqs <- parseQunit (unit)
        if (is.null (pqs$err))  return (pqs)
        else                    return (NULL)
    } else                      return (NULL)
}

applyQMinMaxSum <- function (terms, func, f, na.rm, pqs0, unit=NULL) {
    # at least one param and all are quantities
    l       <- length (terms)
    dim0    <- qs2dim (pqs0)
    mmi     <- 1
    for (i in 1:l) {
        t       <- terms[[i]]
        sameDim <-  sameDimensions (dim0, qs2dim (parseQunit(t)))
        if (!sameDim)           stop ("All quantities must have the same dimension")
        t   <- as.quantity (t, pqs0)
        if (i == 1) mms <- t
        else        {
            if ((func=='max' & t > mms) | (func=='min' & t < mms)) mmi <- i
            mms <- f (mms, t, na.rm=na.rm)
        }
    }
    if ((func=='min' | func=='max') & is.null(unit))    mms <- terms[[mmi]]
    else                                                mms <- as.quantity (mms, pqs0)
    mms
}

sameDimensions <- function (...) {
    # prerequisite: no param check needed; at least2 dims
    dims    <- list (...)
    dim1    <- dims[[1]]
    names1  <- names(dim1)
    ok      <- TRUE
    for (i in 2:length(dims)) if (ok) {
        di  <- dims[[i]]
        ok <- setequal (names1, names(di))
        if (ok) ok <- sum (sapply (names1, function(X) dim1[X] != di[X]))==0
    }
    ok
}

dim2unit <- function (dim) {
    # prerequisite: no param check needed
    l       <- length (dim)
    u       <- character(length=l)
    dNames  <- names (dim)
    for (i in 1:l) {
        u[i]    <- defaultUnitDefinedQuanity (dNames[i], dim[i])
    }
    u
}

# dim2unit <- function (dim) {
#     # prerequisite: no param check needed
#     l       <- length (dim)
#     u       <- character(length=l)
#     dNames  <- names (dim)
#     for (i in 1:l) {
#         q       <- dNames[i]
#         e       <- as.integer (dim[i])
#         defU    <- qEnvir$qDefs[[q]]$default
#         u[i]    <- paste (q, '$', defU, '^', e, sep='')
#     }
#     u
# }

addDims <- function (factors) {
    lFac    <- length (factors)
    dims0   <- vector (mode='list', length=lFac)
    for (i in 1:lFac) dims0[[i]] <- qDim (factors[[i]])
    uNames  <- unique (names (unlist (dims0)))
    lUn         <- length (uNames)
    dim         <- integer (lUn)
    for (i in 1:lUn) {
        qn  <- uNames[i]
        qe  <- 0
        for (dim0 in dims0) {
            if (qn %in% names(dim0)) qe <- qe + dim0[qn]
        }
        dim[i] <- qe
    }
    names(dim)  <- uNames
    dim
}

multiplyQuantities <- function (a, qFactors, na.rm, pDim) {
    # prerequisite: no param check needed
    for (i in 1:length(qFactors)) {
        q       <- qFactors[[i]]
        u       <- defaultUnitsPqs(q)
        a       <- prod (a, as.numeric (as.quantity (q, u)), na.rm=na.rm)

    }
    as.quantity (a, dim2unit (pDim))

}

defaultUnitsPqs <- function (q) {
    # prerequisite: no param check needed
    pqsQ    <- parseQunit (q)
    l       <- length (pqsQ)
    u       <- character (length=l)
    for (i in 1:l) {
        pqsI    <- pqsQ[[i]]
        u[i]    <- defaultUnitDefinedQuanity (qName=pqsI$q, e=pqsI$e)
    }
    u
}

defaultUnitDefinedQuanity <- function (qName, e) {
    defU    <- qEnvir$qDefs[[qName]]$default
    paste (qName, '$', defU, '^', e, sep='')
}

condenseDim <- function (dim) {
    dDefs <- list()  # list if dimension defintions derived quantities
    for (qDef in qEnvir$qDefs) {
        if (!is.null(qDef$dim) && length(qDef$dim) > 1) {
            dDefs[[qDef$name]] <- qDef$dim
        }
    }
    if (length(dDefs) == 0) return (dim)
    dr          <- splitDim (deriveds=numeric(), rest=dim, dDefs=dDefs)
    c(dr$deriveds, dr$rest)
}

splitDim <- function (deriveds, rest, dDefs) {
    restQs          <- names(rest)
    di              <- length (deriveds) + 1
    options         <- list()
    options[[1]]    <- list (deriveds=deriveds, rest=rest)
    oi              <- 2
    for (i in 1:length(dDefs)) {
        dDef    <- dDefs[[i]]
        defQs   <- names(dDef)
        namesOk <- length (setdiff (defQs, restQs)) == 0   # i.e. all dimQs in defQs
        if (namesOk) {
            # 'substract' dDef
            der2            <- deriveds
            rest2           <- rest
            for (defQ in defQs) rest2[defQ] <- rest2[defQ] - dDef[defQ]
            rest2           <- rest2 [rest2 != 0]
            qName           <- names(dDefs)[i]
            if (qName %in% names(der2)) der2[qName] <- der2[qName] + 1
            else                        der2[qName] <- 1
            options[[oi]]   <- splitDim (deriveds=der2, rest=rest2, dDefs=dDefs)
            oi              <- oi + 1
        }
    }
    if (oi == 2) {
        dr <- options[[1]]
    } else {
        # minimise the total number of derived and base quantities
        nDims   <- sapply (options, function(X) length(X$deriveds) + length(X$rest))
        minNdim <- min (nDims)
        options <- options [nDims == minNdim]
        if (length(options)==1) dr <- options[[1]]
        else {
           nExp <- sapply (options, function(X) sum (abs (X$deriveds)) + sum (abs (X$rest)) )
           dr <- options[[which.min (nExp)]]
        }
    }
    dr
}

normaliseQValue <- function (x) {
    pqs0    <- parseQunit (x)
    pe0     <- sum (sapply (pqs0, function(X) X$pe*X$e))
    peV     <- floor (log10 (as.numeric(x)))
    pe      <- min (24, max (-24, pe0+peV))             # min = -24 (yocto) max = 24 (yotta)
    pe      <- pe - pe%%3                               # use 1000-folds only

    pqs1    <-pqs0
    if (pqs1[[1]]$q == 'prefix') {
        if (abs (pqs1[[2]]$e) == 1) pqs1[[1]] <- NULL   # remove prefic
    } else if (abs (pqs1[[1]]$e) != 1) {                # add prefix
        pqs1 <- unlist (list (list (list (q='prefix', e=1)), pqs0), recursive = FALSE)  # other prefix params added later
    }
    # remove all prefises from 2 on
    if (length(pqs1) > 1) for (i in 2:length(pqs1)) {
        pqs1[[i]]$p     <- 'none'
        pqs1[[i]]$pe    <- 0
        pqs1[[i]]$unit  <- assemblePunit(pqs1[i])       # assemblePunit expects a list
    }
    # add new prefix to the first element
    pe              <- pe * pqs1[[1]]$e
    pqs1[[1]]$pe    <- pe
    pus             <- qEnvir$qDefs$prefix$units
    pName           <- names (pus) [sapply(pus, function(X) X$pe==pe)]
    if (pqs1[[1]]$q == 'prefix')   { pqs1[[1]]$u     <- pName
    } else                         { pqs1[[1]]$p     <- pName }
    pqs1[[1]]$unit  <- assemblePunit(pqs1[1])
    as.quantity (x, pqs1)
}

#                              adjustQscale support
# ------------------------------------------------------------------------------
adjustQprefixAndUnit <- function (x, to, mode, ps, us) {
    # convert till cyclic, and select nearest
    results <- list()
    y  <- adjustQunit   (x, to, mode, us)
    u  <- qUnit (y)
    goon    <- TRUE
    while (goon) {
        results[[u]]    <- as.numeric (abs (x-y))
        y               <- adjustQprefix (y, to, mode, ps)
        u               <- qUnit (y)
        if (u %in% names(results)) goon=FALSE
        if (goon) {
            results[[u]]    <- as.numeric (abs (x-y))
            y               <- adjustQunit   (y, to, mode, us)
            u               <- qUnit (y)
            if (u %in% names(results)) goon=FALSE
        }
    }
    # get 'local' minimum
    delta   <- sapply (results, function(X) X)
    i       <- which.min (delta==min(delta))
    uSpec   <- names (results)[i]
    as.quantity (x, uSpec)
}

#' @keywords internal
adjustQprefix <- function (x, to, mode, ps) {
    # invoked by adjustQscale only, parameters checked.
    pqs     <- parseQunit (x)
    if (pqs[[1]]$q == 'prefix') {
        p0  <- pqs[[1]]$u
        e0  <- 1
    } else {
        p0  <- pqs[[1]]$p
        e0  <- pqs[[1]]$e
    }
    pes0    <- sapply (qEnvir$qDefs$prefix$units, function(X) X$pe)
    pe0     <- pes0[p0]
    # allowed prefixes
    pes     <- sapply (qEnvir$qDefs$prefix$units[ps], function(X) X$pe)
    pe      <- log10 (as.numeric(abs(x))/abs(to)) / e0 + pe0        # new optimaL prefix exponent
    inf     <- pes <= pe
    iInf    <- ifelse (sum(inf)==0, length(inf) , which.max (inf))
    peInf   <- pes[iInf]                                            # max lower or equal e value in pes
    if (pe == peInf | mode == 'min') {
        pe <- peInf
    } else {
        iSup    <- max (1, iInf - 1)
        peSup   <- pes[iSup]
        if (mode == 'max') {
            pe <- peSup
        } else if (mode == 'fraction') {
            peAv    <- (peInf + peSup)/2
            pe      <- ifelse (pe < peAv, peInf, peSup)
        } else {   # mode == round
            peAv    <- (10^peInf + 10^peSup) / 2
            pe      <- ifelse (10^pe < peAv, peInf, peSup)
        }
    }
    p   <- ps[which(pes==pe)]
    if (pqs[[1]]$q == 'prefix') {
        pqs[[1]]$u  <- p
    } else {
        pqs[[1]]$p  <- p
    }
    as.quantity (x, pqs)
}

#' @keywords internal
adjustQunit <- function (x, to, mode, us) {
    # invoked by adjustQscale only, parameters checked.
    pqs <- parseQunit (x)
    qi  <- ifelse (pqs[[1]]$q != 'prefix', 1, 2)
    q   <- pqs[[qi]]$q
    u0  <- pqs[[qi]]$u
    e0  <- pqs[[qi]]$e
    ufs0    <- sapply (qEnvir$qDefs[[q]]$units, function(X) X$factor)      # pe => uf = unit factor
    uf0     <- ufs0[u0]

    ufs     <- sapply (qEnvir$qDefs[[q]]$units[us], function(X) X$factor)
    uf      <- uf0 * abs(x) / abs(to)                   # new optimal factor
    inf     <- ufs <= uf
    iInf    <- ifelse (sum(inf)==0, length(inf) , which.max (inf))
    ufInf   <- ufs[iInf]                               # max lower or equal e value in pes
    if (uf == ufInf | mode == 'min') {
        uf <- ufInf
    } else {
        iSup    <- max (1, iInf - 1)
        ufSup   <- ufs[iSup]
        if (mode == 'max') {
            uf      <- ufSup
        } else if (mode == 'fraction') {
            ifAv   <- sqrt (ufInf * ufSup)
            uf <- ifelse (uf < ifAv, ufInf, ufSup)
        } else {   # mode == round
            ifAv <- (ufInf + ufSup) / 2
            uf <- ifelse (uf < ifAv, ufInf, ufSup)
        }
    }
    # get u and adjust to
    pqs[[qi]]$u <- us[which(ufs==uf)]
    as.quantity (x, pqs)
}

# ------------------------------------------------------------------------------
#                       common internal utility function
# ------------------------------------------------------------------------------

pqsConversionFactor   <- function (oldPqs, newPqs) {                        # XYZ
    z       <- expandPqs (oldPqs)
    ef      <- collectQfactors (z$pqs)
    peOld   <- z$pe + ef$pe
    fOld    <- z$f * ef$f

    z       <- expandPqs (newPqs)
    ef      <- collectQfactors (z$pqs)
    peNew   <- z$pe + ef$pe
    fNew    <- z$f * ef$f

    pe      <- peNew - peOld
    f       <- fNew / fOld
    list (f=f, pe=pe)
}

collectQfactors <- function (pqs) {
    pe  <- 0
    f   <- 1
    for (i in 1:length(pqs)) {
        qs      <- pqs[[i]]
        q        <- qs$q
        if (q == 'prefix') {
           pe    - pe + qEnvir$qDefs$prefix$units[[qs$u]]$pe
        } else {
            pe  <- pe + qEnvir$qDefs$prefix$units[[qs$p]]$pe * qs$e
            f   <- f * qEnvir$qDefs[[qs$q]]$units[[qs$u]]$factor^qs$e
        }
    }
    # cat ("collectQfactors:", "pe=", pe, "f=", f, '\n')
    list (pe=pe, f=f)
}

# ------------------------------------------------------------------------------
#                           format, print, and sumamry
# ------------------------------------------------------------------------------

#' @export
print.quantity <- function (x, ...) {
    cat (format.quantity (x, unitAs='text'), '\n')
}

#' @export
summary.quantity <- function (object, ...) {
    cat (format.quantity (x=object, unitAs='text'), '\n')
}

#' @export
format.quantity <- function (x, unitAs='symbol', roundDecimals=NULL, ...) {
    unitAs <- procesEnumertedParameter (unitAs, type= c ('symbol', 'text'))
    if (is.na (unitAs))                 stop ("unitAs must be eihter 'symbol' or 'text'")

    z   <- formatQuantityUnit (x, unitAs = unitAs)
    s   <- z$qf
    xV  <- as.numeric (z$x)
    if (!is.null(roundDecimals)) xC <- roundDecimals (xV)
    else                         xC <- format (xV, ...)
    xC <-paste (xC, collapse = ' ')
    paste (xC, s)
}

formatQuantityUnit <- function (x, unitAs='symbol')  {
    qs      <- attr (x, 'unit')
    pqs     <- parseQunit (qs)  # err or list of q, p. u, e values
    if (!is.null (pqs$err)) stop (pqs$err)

    pueF    <- character (length=length(pqs))
    for (i in 1:length(pqs)) {
        qpue    <- pqs[[i]]
        q       <- qpue$q
        p       <- qpue$p
        u       <- qpue$u
        if (unitAs=='symbol') {
            pf  <- qEnvir$qDefs$prefix$units[[p]]$symbol
            uf  <- qEnvir$qDefs[[q]]$units[[u]]$symbol
        } else {
            pf  <- qEnvir$qDefs$prefix$units[[p]]$text
            if (uAsPlural (x, pqs)) {
                uf  <- qEnvir$qDefs[[q]]$units[[u]]$plural
            } else {
                uf  <- qEnvir$qDefs[[q]]$units[[u]]$text
            }
        }
        if (i==2) pf2 <- pf                      # save because of possible merging
        ef      <- formatExponent(qpue$e)
        pueF[i] <- paste (pf, uf, ef, sep='')   # merge unit prefix, unit and exponent
    }
    # merge prefix and unit if needed
    if (pqs[[1]]$q == 'prefix' && pqs[[2]]$e == 1)  {
        if (pf2 =='') pueF[2] <- paste (pueF[1], pueF[2], sep='')
        else {
            z       <- mergePrefixAndUnit (x, pqs, unitAs = unitAs)
            x       <- z$x
            from    <- nchar (pf2) + 1
            pueF[2] <- paste (z$pf,substring (pueF[2], from), sep='')
        }
        pueF    <- pueF[2:length(pueF)]

    }
    pNotMerged <- pqs[[1]]$q == 'prefix' && pqs[[2]]$e != 1
    if (pNotMerged & unitAs=='symbol') {
        # not mergerd => keep space
        qf  <- paste (pueF[2:length(pueF)], collapse = '')
        qf  <- paste (pueF[1], qf, sep=' ')
    } else {
        sep <- ifelse (unitAs=='text', ' ', '')
        qf  <- paste (pueF, collapse = sep)
    }
    list (x=x, qf=qf)
}

formatExponent <- function (exp) {
    # overkill, usually exp<= 3
    figures <- c ("-", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
    supers  <- c ("\u207B","\u2070","\u00B9","\u00B2","\u00B3","\u2074", "\u2075",
                  "\u2076","\u2077","\u2078","\u2079") # see http://graphemica.com
    names (supers)  <- figures
    if (exp==1) return ('')
    e <- as.character (exp)
    c <- character (nchar(e))
    for (i in 1:nchar(e)) {
        c[i] <- supers[substr(e, i, i)]
    }
    paste (c, collapse = '')
}

uAsPlural <- function (x, pUnit) {
    if (is.null(x) || length (x)==0 || is.na(x)) return (FALSE)
    x <- asNumeric (x)
    if (length (x) == 0) return (FALSE)
    asPlural <- FALSE
    # only if one dimension with positive exponent and if this one is the first one
    eL <- as.integer (sapply(pUnit, function(X) X$e > 0))
    i <- ifelse (pUnit[[1]]$q == 'prefix', 2, 1)
    mayBe <- (sum(eL) == i) & (eL[i] > 0)
    if (mayBe) {
        maxX <- max (abs (x))
        if (maxX==0 | maxX > 1)  asPlural <- TRUE
    }
    asPlural
}

mergePrefixAndUnit <- function (x, pqs, unitAs) {
    pe  <- pqs[[1]]$pe + pqs[[2]]$pe
    per <- pe %% 3
    pe  <- pe - per
    x   <- x * 10^per
    if (x >= 1000) {
        x   <- x / 1000
        pe  <- pe + 3
    } else if (x < 1) {
        x   <- x * 1000
        pe  <- pe - 3
    }
    pus <- qEnvir$qDefs$prefix$units
    pu  <- pus [sapply (pus, function(X) X$pe==pe)][[1]]
    if (unitAs == 'symbol') pf  <- pu$symbol
    else                    pf  <- pu$text
    list (x=x, pf=pf)
}
