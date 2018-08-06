#                               define quantities
#
# Purpose   :   quantities defintion management
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
#     Jul18 - created, i.e. splitted from 'quantities.R'
# ------------------------------------------------------------------------------

#' @include misc.R
#' @include numeric.R
#' @include quantities.R
NULL

qEnvir          <- new.env ()
qEnvir$qDefs    <- list()
packageQdefs    <- list()       # the packaged to be added with addPackageDefs()
# ------------------------------------------------------------------------------
#                         set / get  quantity definition
# ------------------------------------------------------------------------------

#' Qunatities definitions
#' @param qName cheracter string, the name of quantity
#' @param units a list with unit definitions
#' @param default the name of the defeault unit.
#' @details addQdefinition adds /replaces a guantity efintion  \cr
#' printQnames prints the namesof quantities defined \cr
#' printQunits prints th equantity units and other quantity details
#' @name quanityDefintions
NULL
#> NULL

#' @rdname quanityDefintions
#' @export
addQdefinition <- function (qName, units, default) {
    q <- list ()
    q$name <- addQname (qName)
    # check units
    if (!is.list(units))            stop ("units must be a list")
    if (length(units)==0)           stop ('a quantity must have at least one unit')
    # add units
    if (!is.list(units[[1]]))  units <- list (units)
    us              <- list()
    for (u in units) us <- addQunitDef (us=us, u=u)
    # order units (assumed in some functions)
    fOrder          <- order (sapply (us, function(X) X$factor), decreasing=TRUE)
    q$units         <- us[fOrder]
    # add symbal table
    q$symbolNames   <- addSymbolNames (q)
    # add default unit
    q$default       <- addQdefault (q=q, default=default)
    # add q dim
    q$dim           <- addQdim (q)
    # save
    new             <- is.null (qEnvir$qDefs[[qName]])  || !is.null(qEnvir$qDefs[[qName]]$dummy)
    qEnvir$qDefs[[qName]]   <- q
    if (!new) warning (paste ("An old defintion for quantity", qName, "has been replaced"))
    # to save use saveQdefs in private utils
}

addQname <- function (qName) {
    # check quantity qName
    if (!is.character (qName   ))    stop ("Name must be a character string")
    if (length (qName) > 1) {
        qName <- qName[1]
        warning ("qName must of length 1, only the first element is used")
    }
    qName
}

# external
# addQunitDefintion <- function (q, unit, replace=FALSE) {                         # , defoult=NULL ???
#     # check params
#     q       <- qEnvir$qDefs[[q]]
#     if (is.null(q)) stop (paste ('Unknown quantity', q))
#     us      <- q$units
#     if (!replace & unit %in% names(us)) stop (paste ("Unit '", unit, "' already exists") )
#     # add unit
#     us      <- addQunitDef (us=us, u=unit)
#     # order units (assumed in some functions)
#     fOrder  <- order (sapply (us, function(X) X$factor), decreasing=TRUE)
#     q$units <- us[fOrder]
#     if (!is.null(default)) q$default  <- default        # e.g. kilo.aUnit
#     # update symbol name table
#     q$symbolNames <- addSymbolNames (q)
#     # update q dimension
#     q$dim   <- addQdim(q)
# }

# internal
addQunitDef <- function (us, u) {           # us list of units; u unit to be added
    # check unit name / text / plural / symbol / factor / defintion (if derived)
    if (is.null(u$name))                    stop ("A unit must have a name")
    if (!is.character(u$name))              stop ("A unit name must be a character string")
    if (length(u$name) != 1)                stop ("A unit may have one name only")
    if (u$name %in% names(us))              stop ('Duplicate unit name')

    if (is.null (u$text))  u$text <- u$name
    else if (!is.character(u$text))         stop ("A unit text, if present, must be a character string")

    if (is.null (u$plural))  u$plural <- paste (u$text, 's', sep='')
    else if (!is.character(u$plural))       stop ("A plural unit name, if present, must be a character string")

    if (is.null (u$symbol))  u$symbol <- u$name
    else if (!is.character(u$symbol))       stop ("A unit symbol name, if present, must be a character string")

    if (!is.null(u$factor)) {
        factor <- asNumeric (u$factor)
        if (is.na (factor))                 stop ("A unit factor, if present, must be a numeric value")
    } else {
        u$factor <- 1
    }
    if (!is.null(u$note)) {
        if (!is.character(u$note))          stop ("A unit note, if present, must be a character string")
    }
    # check a definition for a derived unit
    if (!is.null (u$def)) {
        pqs     <- parseQunit (u$def)
        if (!is.null(pqs$err))              stop (paste ("Illegal unit definition '", u$def, "'", sep=''))
        u$pqs   <- pqs
    }
    us[[u$name]] <- u
    us
}

addSymbolNames <- function (q) {
    us      <- q$units
    sNames  <- names(us)
    symbols <- as.character (sapply (us, function(X) X$symbol))
    if (length(sNames) != length(unique(sNames)))   stop ("Unit symbols must be unique")
    names(sNames) <- symbols
    sNames
}

addQdefault <- function (q, default) {
    pu  <- extractQunitAndPrefix (q, default)
    if (!is.null (pu$err)) {
        cat ("addQdefault:", "pu$err=", pu$err, '\n')
        stop ("The default must be a valid quantity unit with an optional prefix")
    }
    default
}

addQdim <- function (q) {
    us <- q$units
    uDefs <- sapply (us, function (X) !is.null(X$def))
    if (sum(uDefs) == 0) {
        dim         <- 1
        names(dim)  <- q$name
    }
    else { # a derived units
        pqss    <- lapply (us[uDefs], function(X) X$pqs)
        dim     <- qs2dim (pqss[[1]])
        qNames  <- names(dim)
        if (length (pqss) > 1) for (i in 2:length (pqss)) {
           dimI <- qs2dim (pqss[[i]])
           ok <- setequal (names(dimI), qNames)
           if (ok) ok <- sum (sapply (qNames, function(X) dimI[X] != dim[X]))==0
           if (!ok)     stop ("Derived units with different dimensions")
        }
    }
    dim
}

# ------------------------------------------------------------------------------
#                         print quantities
# ------------------------------------------------------------------------------
#' @rdname quanityDefintions
#' @export
printQnames <- function () {
    n   <- names (qEnvir$qDefs)
    paste (n)
}

#' @rdname quanityDefintions
#' @export
printQunits <- function (qName) {
    q       <- qEnvir$qDefs[[qName]]
    us      <- q$units
    us      <- sortUnitsforPrint (us)
    qL      <- newQLines ()
    qL$add ('name', q$name)
    qL$add ('dimension', q$dim)
    qL$add ('default unit', q$default)

    uDefL <- sapply (us, function(X) !is.null (X$def))
    if (sum (uDefL) == 0) {
        qL$add ('unit definitions', 'none')
    } else {
        qL$add ('unit definitions', itemOnly = TRUE)
        ud <- us[uDefL]
        for ( u in ud) {
            qL$add (paste ('  ', u$name, sep=''), u$def)
        }
    }
    qL$add ('units', itemOnly = TRUE)
    printQLines (qL)

    qT  <- newQTable ()
    qT$add ('name'              , sapply (us, function(X) X$name  )         )
    qT$add ('system'            , sapply (us, function(X) X$system), null='')
    qT$add (c('text','singular'), sapply (us, function(X) X$text  )         )
    qT$add (c('text','plural')  , sapply (us, function(X) X$plural)         )
    qT$add ('symbol'            , sapply (us, function(X) X$symbol)         )
    qT$add ('factor'            , sapply (us, function(X) X$factor)         )
    qT$add ('note'              , sapply (us, function(X) X$note  ), null='')
    printQTable (qT)
}

sortUnitsforPrint <- function (us) {
    noSysL      <- sapply (us, function (X) is.null (X$system))
    usNoSys     <- us[ noSysL]
    usRest      <- us[!noSysL]
    allSys      <- unique (sapply (usRest, function(X) X$system))
    usIso       <- list()
    if ('ISO' %in% allSys) {                                         # row 1: the international standart
        usIsoL  <- sapply (usRest, function (X) X$system == 'ISO')
        usIso   <- usRest[ usIsoL]
        usRest  <- usRest[!usIsoL]
    }
    usImp       <- list()                                           # row 2: second most important
    if ('imperial' %in% allSys) {
        usImpL  <- sapply (usRest, function (X) X$system == 'imperial')
        usImp   <- usRest[ usImpL]
        usRest  <- usRest[!usImpL]
    }
    if (length(usRest) > 0) {
        rSysO   <- names (sort (sapply (usRest, function (X) X$system)))
        usRest  <- usRest [rSysO]
    }
    c (usIso, usImp, usRest, usNoSys)
}



