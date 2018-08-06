#                        S3 print / summary functions support
#
# Purpose   :   rendering for print and summarize functions
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
#     Nov17 - created
#     Dec17 - time zone not printed if tzone[1] == ''; multi-line heders
#     Jan18 - printing list with all 'NULL' values corrected
#     Feb18 - skipping a value list with all NULLs improved
#     Mar18 - Revised
#     Jul18 - sprint replaced with paste in non dot colums (beause of UTF-8 chars)
#
# Legend:
# qL    - a number of quntity lines
# qT    - a quantity table
# q     - a list with a single line / column
# v     - q$v; a vector with quantities / values
# txt   - a character vector with the text for v


# #' @param h a character vector with a column header (one line per string)
# #' @param i a character string with the item designation
# #' @param v a list or vector with values to be printed
# #' @param u a character string specifying the units of measurement with two special
# #' values 'symbol' and 'text' signifying the rendering of quantity units (default
# #' 'text' for lines and 'symbol' for tables)
# #' @param etc a character string with additional text for a line
# #' @param itemOnly a logical indicating a caption, i.e. an item without a value
# #' @param null a character string for NULL value replacements, default 'NULL'
# #' @param skipAllNull a logical indiating whether or not a line or column with all
# #' null values should be skiped, default TRUE
# #' @param na a character string for NA value replacements, default 'NA'
# #' @param nan a character string for NaN value replacements, default 'NaN'
# #' @param zero 0 or a character string for 0 value replacements, default 0
# #' @param pInf a character string for Inf value replacements, default 'Inf'
# #' @param mInf a character string for -Inf value replacements, default '-Inf'
# #' @param true a character string for TRUE value replacements, default a check mark
# #' @param false a character string for FALSE value replacements, default ''
# #' @param allign with a column allignment. The possible values 'default', 'left'
# #' 'centre', 'right' and 'dot' may be abbreviated.

#' rendering lines and tables with quantities and other values
#' @param envir the envirionment created with newQlines or newQtable
#' @param file a character string with a file path and name
#' @param maxRows the maximum number of table rows to printed, default = 24
#' @param ... are used only as a sink for superfluous parameters
#' @return Both newQlines and newQtable return a new environment for the
#' lines / table  to be printed. Both environments inclued an add() function for
#' adding lines / columns.
#' @section Add functions:
#' newQlines and newQtable create an envivonment with an add() function for for adding text lines and columns respectively.
#' The add() function have the following parameters:  \cr
#' i (for lines only) a character string with the item designation  \cr
#' itemOnly (for lines only) a logical indicating a caption, i.e. an item without a value \cr
#' h (columns only) a character string with a column header. For multi line headers use a character vector \cr
#' v a list or vector with values to be printed \cr
#' u a character string specifying the units of measurement with two special
#' values 'symbol' and 'text' signifying the rendering of quantity units (default
#' 'text' for lines and 'symbol' for tables) \cr
#' etc (lines only) a character string with additional text added to the end of the line \cr
#' null a character string for NULL value replacements, default 'NULL' \cr
#' skipAllNull a logical indiating whether or not a line or column with all
#' null values should be skiped or not, default TRUE  \cr
#' na a character string for NA value replacements, default 'NA' \cr
#' nan a character string for NaN value replacements, default 'NaN' \cr
#' zero a character string for 0 value replacements, default 0 \cr
#' pInf a character string for Inf value replacements, default 'Inf' \cr
#' mInf a character string for -Inf value replacements, default '-Inf' \cr
#' true a character string for TRUE value replacements, default a check mark \cr
#' false a character string for FALSE value replacements, default '' \cr
#'allign with a column allignment. The possible values 'default', 'left'
#' 'centre', 'right' and 'dot' may be abbreviated. \cr
#' @details The printQlines will print the lines added to a lines environment. printQtable
#' will print all columns added to a table environment.
#' @name qPrint
NULL
#> NULL

# ------------------------------------------------------------------------------
#                 create a new lines / table printing environment
# ------------------------------------------------------------------------------
#  Creates a new environment with a (line) add funtion
#' @rdname qPrint
#' @examples
#' qL<- newQLines()
#' qL$add ('Recoring', 'new recording')
#' qL$add ('Date', Sys.time())
#' duration <- as.quantity (27000, 'time$second')
#' qL#add ('duration', duration)
#' qL$add ('sample rate', 44100, etc='/sec')
#' printQLines (qL)
#' @export
newQLines <- function () {
    q           <- new.env()
    q$type      <- 'lines'
    q$lines     <- list()
    add         <- function (i, v='', u=NULL, etc='', itemOnly=FALSE,skipAllNull=TRUE,
                             null='NULL', na='NA', nan='NaN', zero=0, pInf='Inf', mInf='-Inf')
    {
        line <- list (i=i, v=v, u=u, etc=etc, itemOnly=itemOnly, skipAllNull=skipAllNull,
                      null=null, na=na, nan=nan, zero=zero, pInf=pInf, mInf=mInf, type='line')
        q$lines[[length(q$lines)+1]] <<- line
    }
    q$add       <- add
    q
}

#  Creates a new environment with a (column) add funtion
#' @rdname qPrint
#' @examples
#' qT <- newQTable ()
#' qT$add ('channel', c(1,2,3,4))
#' qT$add ('filtered', c(TRUE, FALSE, TRUE, FALSE), allign = 'c')
#' qT$add ('sampleRate', c(512, 256, 512, 256), u='/sec')
#' qT$add (c('signal', 'max'), c(.123, 21.12, 21, 5421))
#' printQTable (qT)
#' @export
newQTable <- function () {
    q           <- new.env ()
    q$type      <- 'table'
    q$table     <- list ()
    q$add       <- function (h, v, u=NULL,  skipAllNull=TRUE, allign='default',
                             null='NULL', na='NA', nan='NaN', zero='0', pInf='Inf', mInf='-Inf', true='\u2713', false='') {
                             # '\u2713' = check mark; see http://graphemica.com
        col     <- list (h=h, v=v, u=u,skipAllNull=skipAllNull, allign=allign,
                         null=null, na=na, nan=nan, zero=zero, pInf=pInf, mInf=mInf, true=true, false=false, type='column')
        q$table[[length(q$table)+1]] <<- col
    }
    q
}

# ------------------------------------------------------------------------------
#                                 print QLines
# ------------------------------------------------------------------------------
#' @rdname qPrint
#' @export
printQLines <- function (envir, file='', ...) {
    lines <- envir$lines
    if (length (lines) > 0)  {
        toBeSkipped <- allNullsToBeSkipped (lines)
        if (sum(toBeSkipped)) lines <- lines [!toBeSkipped]
    }
    if (length (lines) == 0) return (invisible(NULL))
    # determine items width
    maxChars    <- envir$itemWidth                              # a given value
    if (is.null (maxChars)) maxChars <- itemWidth (lines)
    ifs         <- paste ("%-", maxChars, "s :", sep='')        # item format string
    # process lines
    for (line in lines) {
        if (line$itemOnly) {
            cat (line$i, '\n')
            next
        }
        # format lines
        if (length (line$v) == 0) {
            line$txt <- line$null
        } else {
            line <- processPvalues (line)
            line <- processPunit   (line)
        }
        cat (sprintf (ifs, line$i), line$txt, line$u, line$etc, '\n', file=file, append=TRUE)
    }
    invisible (NULL)
}

itemWidth <- function (lines) {
    items       <- as.character (sapply (lines, function(X) X$i))
    iOnlyLines  <- as.logical (sapply (lines, function (X) X$itemOnly==TRUE))
    max (nchar (items [!iOnlyLines]))
}

# ------------------------------------------------------------------------------
#                                  print QTable
# ------------------------------------------------------------------------------

#' @rdname qPrint
#' @export
printQTable <- function (envir, maxRows=24, file='', ...) {
    qT      <- envir
    table   <- qT$table
    nCols   <- length (table)
    if (nCols) {
        toBeSkipped <- allNullsToBeSkipped (table)
        if (sum(toBeSkipped)) table <- table [!toBeSkipped]
        nCols   <- length (table)
    }
    if (length(table) == 0) return (NULL)
    # check number of value rows
    x       <- QTableLength (table, maxRows)             # all (incl NULL) columns same length with max = maxRows
    if (x$err) stop ("All table columns (value vectors) must have the same length.")
    table   <- x$table
    sRows   <- x$sRows

    #  process values and units
    for (i in 1:nCols) {
        col         <- table[[i]]
        col         <- processPvalues (col)
        col         <- processPunit   (col)             # and add unit to header (depends on vClass)
        table[[i]]  <- col
    }
    # format table
    table           <- processQTHeader (table)
    hvCols          <- list()
    for (col in table) {
        hvCol       <- formatTableColumn (col, ...)
        if (!is.null (hvCol)) hvCols[[length(hvCols)+1]] <- hvCol
    }
    # print table
    if (length(hvCols)) for (r in 1:length (hvCols[[1]])) {      # plus header lines
        rowParts <- sapply (hvCols,function (X) X[r])
        cat (' ', rowParts, '\n', sep='', file=file, append=TRUE)
    }
    if (sRows) cat ("... and", sRows, 'more\n', file=file, append=TRUE)
}


QTableLength <- function (table, maxRows) {
    nL      <- as.logical (sapply (table, function(X) is.null (X$v)))
    vRows   <- sapply (table[!nL], function(X) length (X$v))
    err     <- sum(vRows != vRows[1])
    if (err) return (list (err=TRUE))
    # truncate,if needed
    vRows   <- vRows[1]
    sRows   <- 0                                        # rows to be skipped
    if (vRows > maxRows) {
        sRows   <- vRows - maxRows
        for (i in 1:length(table)) if (!nL[i]) {
            vRows   <- maxRows
            table[[i]]$v  <- table[[i]]$v[1:vRows]    # truncate value vector
            # remove now: values not printed should not have any impact on colomn width.
        }
    }
    # expand NUL columns, if needed and if any
    if (vRows & sum(nL)) for (i in 1:length(table)) if (nL[i]) {
        table[[i]]$v <- rep (table[[i]]$null, vRows)    # replace with all with NULL substitute
    }
    list (err=FALSE, table=table, sRows = sRows)
}

formatTableColumn <- function (col, ...) {       # col, a table column
    vClass  <- col$vClass
    allign  <- col$allign
    if (allign=='cente' | allign=='center') allign <- 'centre'
    allign  <- procesEnumertedParameter (allign, c('default', 'left', 'right', 'centre', 'dot'))
    if (is.na (allign)) stop ("Illegal allign parameter value")
    # set allignment
    someStd <- as.logical (sum (col$isStd))
    if (someStd) {
        if (allign == 'default') {
            allign <- 'left'
            if (vClass=='numeric' | vClass=='quantity') allign <- 'dot'
            else if (vClass[1] == 'logical')            allign <- 'centre'
            else if (vClass[1] == 'integer')            allign <- 'right'
        }
    } else {                            # all non std values
        if (allign == 'default') allign <- ifelse (vClass=='logical', 'centre', 'left')
    }
    # format the column
    if (allign=='dot') {
        hvCol   <- formatDotColumn (h=col$h, txt=col$txt, nonNumL=!col$isStd)
    } else {    # nonDot: 'POSIXt', 'Date', 'character', 'integer', 'logical'
        ht      <- c (col$h, col$txt)
        hvCol   <- formatNonDotColumn (ht, allign=allign)
    }
    hvCol
}

formatDotColumn <- function (h, txt, nonNumL) {
    # a dot column may cantain non-numeric character strings as well
    vWidth  <- getDotColumnWidthParams (txt, nonNumL)
    colW    <- max (nchar (h), vWidth$valueW, vWidth$nonNumW)   # net column width, ex ' ' as column separator

    # create column and add header
    tRows           <- length (txt)
    hRows           <- length ( h )
    hvCol           <- character (hRows + tRows)
    hvCol[1:hRows]  <- centreText (h, colW)
    t               <- character (tRows)
    t[!nonNumL]     <- formatDotColumnNums    (nVals=txt[!nonNumL], vWidth=vWidth, colW)
    t[ nonNumL]     <- formatDotColumnNonNums (cVals=txt[ nonNumL], vWidth=vWidth, colW)
    hvCol[(hRows+1):(hRows+tRows)] <- t
    hvCol
}

formatNonDotColumn <- function (ht, allign) {
    txtW    <- nchar (ht)
    colW    <- max (txtW)
    mW      <- colW - txtW
    if (allign=='left') {
        hvCol   <- paste (ht, strrep (' ', mW))                     # sep = ' ' for column separation
    }  else if (allign=='right') {
        hvCol   <- paste (strrep (' ', mW), ht, ' ', sep='')
    } else {
        lmW     <- mW %/% 2
        rmW     <- mW - lmW + 1
        hvCol   <- paste (strrep (' ', lmW), ht, strrep (' ', rmW), sep='')
    }
    hvCol
}

centreText <- function (txt, colW) {
    txtW    <- nchar (txt)
    mW      <- colW - txtW
    lmW     <- mW %/% 2
    rmW     <- mW - lmW + 1
    paste (strrep (' ', lmW), txt, strrep (' ', rmW), sep='')
}

getDotColumnWidthParams <- function (values, nonNumL) {
    nonNumW <- valueW <- preW <- 0
    postW   <- 1
    if (sum(nonNumL)) nonNumW <- max (nchar (values[nonNumL]))
    if (sum(!nonNumL)) {
        pp      <- strsplit (values[!nonNumL], '[.]')
        pre     <- sapply (pp, function (X) X[1])
        post    <- sapply (pp, function(X) ifelse (length(X)>1, paste('.', X[2], sep=''), ''))
        preW    <- max (nchar (pre))
        postW   <- max (nchar (post))                           # incl dot, if present
        valueW  <- preW + postW
    }
    list (valueW=valueW, preW=preW, postW=postW, nonNumW=nonNumW, pre=pre, post=post)
}

formatDotColumnNums <- function (nVals, vWidth, colW) {
    # determine left marging
    lmW     <- colW - vWidth$valueW
    if (lmW) {
        lmFs    <- paste ("%", lmW, 's', sep='')
        lm      <- sprintf(lmFs, '')
    } else lm <- ''
    fs      <- paste ("%s%", vWidth$preW, 's%-', vWidth$postW , 's ', sep='')   # with trailinfg space for column seapration
    sprintf (fs, lm, vWidth$pre, vWidth$post)
}

formatDotColumnNonNums <- function (cVals, vWidth, colW) {
    valueW  <- vWidth$valueW
    nonNumW <- vWidth$nonNumW
    lmW     <- colW - valueW                                                    # left margin for numbers                                                   # left margin num values
    preDotW <- vWidth$preW + lmW                                                # pre dot space
    postDotW<- max (0, vWidth$postW-1)                                          # excl dot if present, i.e. post dot space
    if (nonNumW <= max (preDotW, postDotW)) {
        if (postDotW >= preDotW) {                                              # add under decimal parts
            fs          <- paste ("%", preDotW, 's %-', postDotW , 's ', sep='')
            nonNums     <- sprintf (fs, '', cVals)
        } else {                                                                # rigth allign under lm + integer parts
            if (postDotW == 0) {
                fs          <- paste ("%", preDotW, 's ', sep='')
                nonNums     <- sprintf (fs, cVals)
            } else {
                fs          <- paste ("%", preDotW, 's %', postDotW , 's ', sep='')
                nonNums     <- sprintf (fs, cVals, '')
            }

        }
    } else {                                                           # centre largest under numeric values
        margingsW   <- ifelse (nonNumW <= valueW, valueW - nonNumW, colW - nonNumW)
        nnLmW       <- floor (margingsW / 2)
        nnVfs       <- paste ("%", nnLmW,"s%-", colW-nnLmW , 's ', sep='')
        nonNums     <- sprintf (nnVfs, '', cVals)
    }
    nonNums
}

# ------------------------------------------------------------------------------
#                     Common Lines / Table support functions
# ------------------------------------------------------------------------------
allNullsToBeSkipped <- function (cls) {
    l           <- length (cls)
    toBeSkipped <- logical (length = l)
    for (i in 1:l) if (cls[[i]]$skipAllNull) {
        v   <- cls[[i]]$v
        if (is.null(v)) {
            toBeSkipped [i] <- TRUE
        } else if ('list' %in% class(v)) {
            allNulls    <- min (as.logical (sapply (v, is.null)))
            if (allNulls) toBeSkipped [i] <- TRUE
        }
    }
    toBeSkipped
}

processPvalues <- function ( lc)  {                 # line or column
    lc$txt      <- character (length (lc$v))
    lc$vClass   <- 'character'                      # set default vClass
    lc          <- procesPosixLtPValues (lc)        # convert lt to ct to prepare unlist
    lc          <- processNullPValues   (lc)        # and unlist non-null values in nnV
    if (!is.null(lc$nnV)) {
        lc      <- processNonStdPValues (lc)
        if (sum (lc$nnIsStd)) {
            lc  <- processStdPValues  (lc)
        }
    }
    lc
}

procesPosixLtPValues <- function (lc) {                     # line or column
    v  <- lc$v
    if ( "POSIXlt" %in% class (v)) {                        # typeof = list
        lc$v <- as.POSIXct (v)
    } else if (typeof (v) =='list') for (i in 1:length (v)) {
        if ("POSIXlt" %in% class (v[[i]])) {
            lc$v[[i]] <- as.POSIXct (v[[i]])
        }
    }
    lc
}

processNullPValues <- function (lc) {                       # line or column
    # NOTE v may be a list with NULLs and other elements, if any, of the same class
    v  <- lc$v
    if (typeof(v)=='list') {
        # process NULL, if any, and unlist
        isNull  <- as.logical (sapply (v, is.null))
        lc$nnL  <- !isNull
        if (sum (isNull)) {
            if (!is.character(lc$null))                     stop ("nulll must be of type character")
            lc$txt[isNull] <- lc$null
        }
        if (sum (isNull) < length(v)) {          # unlist, non
            aNonNull            <- v [[which.min (isNull)]]
            vAttributes         <- attributes (aNonNull)        # save attributes
            nnV                 <- unlist (v[!isNull])
            attributes(nnV)     <- vAttributes                  # restore attributes stripped by unlist()
            lc$nnV              <- nnV
        }
    } else {
        lc$nnL <- !logical (length(v))
        lc$nnV <- lc$v
    }
    lc
}

processNonStdPValues <- function (lc) {                    # line or column
    # set default replacement if none given.
    if (is.numeric (lc$pInf))   lc$pInf <- as.character ( Inf)
    if (is.numeric (lc$mInf))   lc$mInf <- as.character (-Inf)
    if (is.numeric (lc$nan) )   lc$nan  <- as.character ( NaN)
    if (is.na (lc$na)       )   lc$na   <- as.character ( NA )
    if (!is.null (lc$true)) {
        if (is.logical (lc$true ))  lc$pInf <- as.character (TRUE )
        if (is.logical (lc$false))  lc$mInf <- as.character (FALSE)
    }
    # check replacement values; should be character (if not zero)
    if (lc$zero !=   0  && !is.character (lc$zero))  stop ("zero must be 0 or of type character.")
    if (!is.character (lc$pInf))  stop ("pInf must be Inf or of type character.")
    if (!is.character (lc$mInf))  stop ("mInf must be -Inf or of type character.")
    if (!is.character (lc$na))    stop ("na must be of type character.")
    if (!is.null (lc$true)) {
        if (!is.character(lc$true))   stop ("true must be TRUE or of type character.")
        if (!is.character(lc$false))  stop ("false must be FALSE or of type character.")
    }

    nnV <- lc$nnV
    nnL <- lc$nnL
    txt <- lc$txt
    isZero <- isPInf <- isMInf <- isNaN <- isNa <- isTrue <- isFalse <- logical (length (nnV))

    # na : NOTE R considers NaN as NA
    isNa  <- is.na (nnV)  & !is.nan (nnV)
    if (sum(isNa)) txt[nnL][isNa] <- lc$na

    # if numeric: zero, Inf, NaN and NA
    if (is.numeric(nnV)) {
        # NaN
        isNaN <- is.nan (nnV)                           # is.nan(NA) is FALSE
        if (sum (isNaN)) txt[nnL][isNaN] <- lc$nan

        auxNnV          <- nnV
        if (sum(isNa )) auxNnV[isNa]    <- 1     # a std numeric value; to avoid NA's in e.g. nnN  == -Inf
        if (sum(isNaN)) auxNnV[isNaN]   <- 1     # idem
        # zero
        zero <- lc$zero
        if (zero != 0) {                                # replace any zero
            isZero  <- auxNnV == 0                      # zero replacements only, NA==0 isNA
            if (sum(isZero)) txt[nnL][isZero] <- zero
        }
        # mInf
        mInf    <- lc$mInf
        if (mInf == -Inf) mInf <- '-Inf'
        isMInf  <- auxNnV == -Inf                       # NA == -Inf is NA
        if (sum (isMInf)) txt[nnL][isMInf] <- mInf
        # pInf
        pInf    <- lc$pInf
        if (pInf == Inf) pInf <- 'Inf'
        isPInf  <- auxNnV == Inf
        if (sum(isPInf)) txt[nnL][isPInf] <- pInf
    }

    # summarise logical vectors
    nnIsStd    <- ! (isZero | isPInf | isMInf | isNaN | isNa)

    # logical replacements
    if (sum(nnIsStd) & is.logical(nnV) & !is.null(lc$true)) {
        # true
        isTrue              <- lc$nnV
        txt[nnL][ isTrue]   <- lc$true
        txt[nnL][!isTrue]   <- lc$false
        nnIsStd             <- logical (length(nnV))    # nnIsStd <- FALSE
        lc$vClass           <- 'logical'
    }
    isStd                   <- logical (length (lc$txt))
    isStd[nnL][nnIsStd]     <- TRUE
    lc$txt                  <- txt
    lc$nnIsStd              <- nnIsStd
    lc$isStd                <- isStd
    lc
}

processStdPValues <- function (lc, ...) {               # line or column
    vClass  <- class (lc$nnV)
    if (is.quantity (lc$nnV))   vClass  <- 'quantity'
    if ('POSIXct' %in% vClass)  vClass  <- 'POSIXct'
    if (length (vClass) > 1) {
        stop ("values of class '", vClass, "' are not yet supported")
    }
    stdV    <- lc$nnV[lc$nnIsStd]                       # NOTE this will remove all nnV attributes
    if (vClass == 'numeric') {
        # change numeric to integer if possible (i.e. without rounding)
        stdValIsInt <- !is.na (sapply(stdV, asInteger))
        if (sum(stdValIsInt) == length(stdValIsInt))  vClass <- 'integer'
    }
    # convert to a character vector
    if (vClass == 'POSIXct'    ) {
        stdTxt <- posixToChar (stdV)
    } else if (vClass == 'logical'   | vClass == 'numeric'  | vClass == 'integer' |
               vClass == 'character' | vClass == 'quantity' | vClass == 'Date'    ) {
        stdTxt <- as.character (stdV)
    } else {
        stop ("values of class '", vClass, "' are not yet supported")
    }
    lc$txt[lc$nnL][lc$nnIsStd]  <- stdTxt
    lc$vClass                   <- vClass

    isNa            <- is.na (lc$txt)              # some converted values can be NA
    if (sum (isNa)) {
        lc$txt[isNa]    <- lc$na                   # replace with lc$na
        ls$isStd        <- lc$isStd & !isNa  # adjust isStd
    }
    lc
}

# processTableUnits old name, rplace
processPunit <- function (lc) {         # line or column
    u   <- lc$u
    if (is.null(u)) {
        if (lc$type == 'line')  u  <- 'text'
        else                    u  <- 'symbol'
    } else {
        if (!is.character(u)) stop ("u must be of type character")
        u   <- trimws(u)
    }
    if (u=='symbol' | u=='text' ) {
        if (lc$vClass == 'quantity') {
            u  <- formatQuantityUnit (lc$nnV, unitAs=u)
        } else {
            u   <- NULL             # '' wpould print an extra space
        }
    }
    lc$u  <- u
    lc
}

processQTHeader <- function (table) {
    nCols   <- length (table)
    for (i in 1:nCols) {
        col         <- table[[i]]
        u           <- col$u
        if (!(is.null(u) || u==''))     col$h <- c (col$h, u)
        table[[i]]  <- col
    }
    hRows   <- max (sapply (table, function (X) length(X$h)))
    for (i in 1:nCols)  {                                      # same length for all headers
        col             <- table[[i]]
        nH              <- length (col$h)
        if (nH < hRows) {
            h           <- character (hRows)
            h[1:nH]     <- col$h
            col$h       <- h
            table[[i]]  <- col
        }
    }
    table
}

posixToChar <- function (t) {        # with subsecond accuracy and without trailing zero'
    tc  <- format(t, "%Y-%m-%d %H:%M:%OS9", usetz=FALSE)
    tc <- gsub ('[.]?0*$', '', tc)              # remove trailing '0's and a '.'
    # append tz, if appropriate
    tzone <- attr(t, 'tzone')[1]
    if (is.null(tzone)) tzone <- ''
    paste (tc, tzone, sep=' ')
}
