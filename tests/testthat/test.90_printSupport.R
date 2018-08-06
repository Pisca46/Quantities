#                               print/summary support function
#
# Purpose   :   Test the iv and ivu functions
#
#
# Copyright :   (C) 2016, Vis Consultancy, the Netherlands
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
#               along with edf package for R.  If not, see <http://www.gnu.org/licenses/>.
#
# History   :
#     Nov17 - Created
#     Dec17 - tested except for quantities; test added for logical columns;
#     Jan18 - Header and POSIX printing tests added; testTable() added
# ------------------------------------------------------------------------------

require (quantities)
qEnvir <- quantities:::qEnvir
refPrintsDir <- paste (system.file("refPrints", package="quantities"), '/', sep='')
require (testthat)
context ("Test printSuppport.")

# source ("./R/printSupport.R")
itemWidth                   <- quantities:::itemWidth
formatDotColumn             <- quantities:::formatDotColumn
formatNonDotColumn          <- quantities:::formatNonDotColumn
processNullPValues          <- quantities:::processNullPValues
processNonStdPValues        <- quantities:::processNonStdPValues

# ------------------------------------------------------------------------------
#                     Common Lines / Table support functions
# ------------------------------------------------------------------------------

qL <- newQLines ()
qT <- newQTable ()
# 1                                                                             vNLN numeric list with NULL
vNLN <- list (NULL, -Inf, -1, 1, NaN, 3, Inf, 5, NA, 7)
class (vNLN) <- 'spam'
attr (vNLN, 'ham') <- 'egg'
qL$add ('List', vNLN)
qT$add ('List', vNLN)
# 2                                                                             vAN  all NULL
vAN  <- list (NULL, NULL, NULL)
qL$add  ('allNULL', vAN)
qT$add  ('allNULL', vAN, skipAllNull = FALSE)
# 3                                                                             vNL numeric list without NULL
vNL <- list (0, -Inf, -1 , 1, NaN, 3, Inf, 5, NA, 7)
qL$add ('Numeric', vNL)
qT$add ('Numeric', vNL, mInf='less',  null= '0', na='n.a.', pInf='more' )
# 4                                                                             vNV numeric vector
vNV <- c (0, -Inf, -1 , 1, NaN, 3, Inf, 5, NA, 7)
qL$add ('Numeric', vNV)
qT$add ('Numeric', vNV, mInf='less',  null= '0', na='n.a.', pInf='more' )

test_that("processNullPValues", {
    # vNLN
    lc <- processNullPValues (qL$lines[[1]])
    expect_equal (lc$nnL, !c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
    expect_equal (lc$nnV  , c (-Inf, -1, 1, NaN, 3, Inf, 5, NA, 7))
    expect_equal (class (lc$v)       , 'spam')
    expect_equal (attr  (lc$v, 'ham'), 'egg')
    # vAN
    lc <- processNullPValues (qL$lines[[2]])
    expect_null(lc$text)
    lc <- processNullPValues (qT$table[[2]])
    expect_equal (lc$nnL, !c(TRUE, TRUE, TRUE))
})

test_that("processNonStdPValues", {
    lcNLN   <- processNonStdPValues (processNullPValues (qL$lines[[1]]))
    lcAN    <- processNullPValues (qT$table[[2]])                               # table keeps NULLS
    lcNL    <- processNonStdPValues (processNullPValues (qL$lines[[3]]))
    lcNV    <- processNonStdPValues (processNullPValues (qL$lines[[4]]))
    lcCV    <- processNonStdPValues (processNullPValues (qT$table[[4]]))
    expect_equal (lcNLN$nnIsStd , c (       FALSE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE, FALSE,  TRUE))
    expect_equal (lcNLN$isStd   , c (FALSE, FALSE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE, FALSE,  TRUE))
    expect_null  (lcAN$nnV)
    expect_equal (lcAN$txt      , c ('NULL', 'NULL', 'NULL'))
    # NL, NV, CV
    expect_equal (lcNL$nnIsStd  , c ( TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE, FALSE,  TRUE))   # no zero replacement, firt value is TRUE
    expect_equal (lcNV$nnIsStd  , c ( TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE, FALSE,  TRUE))
    expect_equal (lcCV$nnIsStd  , c ( TRUE, FALSE,  TRUE,  TRUE, FALSE,  TRUE, FALSE,  TRUE, FALSE,  TRUE))
    isNa <- is.na (lcNL$txt)
    expect_equal (is.na (lcNV$txt), isNa)
    expect_equal (lcNL$txt[!isNa], lcNV$txt[!isNa])
    expect_equal (lcNL$txt, lcNV$txt)
    expect_equal (is.na (lcCV$txt), isNa)
})

# 5
vIV <- c (1, 22, 333, NA, 55555)
qL$add ('Char', vIV)
qT$add ('Char', vIV)
# 6                                                                             vCV character vector
vCV <- c ('a', 'bb' , '', 'dddd', ' ', NA, 'ggggggg')
qL$add ('Char', vCV)
qT$add ('Char', vCV)

getStdVals <- function (q, lcN) {
    if (exists ('lines', where =q, inherits=FALSE)) lc <- processNonStdPValues (processNullPValues (q$lines[[lcN]]))
    else                                            lc <- processNonStdPValues (processNullPValues (q$table[[lcN]]))
    stdV <- lc$nnV[lc$nnIsStd]
}

test_that("processStdValues", {
    stdV1   <- getStdVals (qL, 1)
    stdV2   <- getStdVals (qT, 3)
    stdV3   <- getStdVals (qL, 4)
    expect_equal (stdV1, c(   -1, 1, 3, 5, 7))
    expect_equal (stdV2, c(0, -1, 1, 3, 5, 7))
    expect_equal (stdV3, c(0, -1, 1, 3, 5, 7))
    stdV     <- getStdVals (qT, 5)
    expect_equal (stdV, c (1, 22, 333, 55555))
    stdV     <- getStdVals (qT, 6)
    expect_equal (stdV,  c ('a', 'bb' , '', 'dddd', ' ', 'ggggggg'))
})

# ------------------------------------------------------------------------------
#                                 print Qines
# ------------------------------------------------------------------------------
test_that ("itemWidth", {
    qL <- newQLines ()
    qL$add ("123456", 'abc')
    qL$add ("12345678", '', itemOnly=TRUE )
    qL$add ("123", 'abcd')
    expect_equal (itemWidth(qL$lines), 6)
})

# ------------------------------------------------------------------------------
#                              format(Non)DotColomn
# ------------------------------------------------------------------------------
test_that ( "formatDotColumn", {
    fut     <- formatDotColumn                               # function under test

    aCol1   <- fut (h= c ('a','unit'), txt= c ('123.567', '23.567890', '3.', '23'), nonNumL=c(FALSE, FALSE, FALSE, FALSE))
    eCol1   <- c ('    a      ','   unit    ','123.567    ', ' 23.567890 ', '  3        ', ' 23        ')
    expect_equal (nchar(eCol1), c(11, 11, 11, 11, 11, 11))
    expect_equal (aCol1, eCol1)

    aCol2   <- fut ('abcdefghij', c ('123.567', '23.567890', '3.', '23'), nonNumL=c (FALSE, FALSE, FALSE, FALSE))
    eCol2   <- c ('abcdefghij ','123.567    ', ' 23.567890 ', '  3        ', ' 23        ')
    expect_equal (nchar(eCol2), c(11, 11, 11, 11, 11))
    expect_equal (aCol2, eCol2)

    aCol3   <- fut (c('abcdefghijk', ''), c ('123.567', '23.567890', '3.', '23'), nonNumL=c (FALSE, FALSE, FALSE, FALSE))
    eCol3   <- c ('abcdefghijk ', "            ", ' 123.567    ', '  23.567890 ', '   3        ', '  23        ')
    expect_equal (nchar(eCol3), c(12, 12, 12, 12, 12, 12))
    expect_equal (aCol3, eCol3)

    aCol4   <- fut ('hdr1', c('1.3456', '1', 'n.a.', '-', '0.5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol4   <- c(' hdr1  ', '1.3456 ', '1      ', '  n.a. ', '  -    ', '0.5    ')
    expect_equal (nchar(eCol4), c(7, 7, 7, 7, 7, 7))
    expect_equal (aCol4, eCol4)

    aCol5   <- fut ('hdr1a', c('1.3', '1', '', ' ', '0.5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol5   <- c('hdr1a ', '  1.3 ', '  1   ', '      ', '      ', '  0.5 ')
    expect_equal (nchar(eCol5), c(6, 6, 6, 6, 6, 6))
    expect_equal (aCol5, eCol5)

    aCol6   <- fut (c('hdr3','u'), c('1', '2', '', ' ', '5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol6   <- c('hdr3 ', ' u   ', '   1 ', '   2 ', '     ', '     ', '   5 ')
    expect_equal (nchar(eCol6), c(5, 5, 5, 5, 5, 5, 5))
    expect_equal (aCol6, eCol6)

    aCol7   <- fut ('hdr4', c('1.3456', '1', '.', '-', '0.5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol7   <- c(' hdr4  ', '1.3456 ', '1      ', '  .    ', '  -    ', '0.5    ')
    expect_equal (nchar(eCol7), c(7, 7, 7, 7, 7, 7))
    expect_equal (aCol7, eCol7)

    aCol8    <- fut ('hdr5', c('1.3456', '1', 'notAp', '-', '0.5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol8    <- c(' hdr5  ', '1.3456 ', '1      ', 'notAp  ', '-      ', '0.5    ')
    expect_equal (nchar(eCol8), c(7, 7, 7, 7, 7, 7))
    expect_equal (aCol8, eCol8)

    aCol9   <- fut ('large hdr6', c('1.3456', '1', 'notAp', '-', '0.5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol9   <- c('large hdr6 ', '    1.3456 ', '    1      ', 'notAp      ', '    -      ', '    0.5    ')
    expect_equal (nchar(eCol9), c(11, 11, 11, 11, 11, 11))
    expect_equal (aCol9, eCol9)

    aCol10  <- fut ('large hdr7', c('1.3456', '1', 'not appl', '-', '0.5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol10  <- c('large hdr7 ', '    1.3456 ', '    1      ', ' not appl  ', ' -         ', '    0.5    ')
    expect_equal (nchar(eCol10), c(11, 11, 11, 11, 11, 11))
    expect_equal (aCol10, eCol10)

    aCol11  <-fut ('large hdr8', c('1.3456', '1', 'not applic.', '-', '0.5'), nonNumL=c (FALSE, FALSE, TRUE, TRUE, FALSE))
    eCol11  <- c('large hdr8  ', '     1.3456 ', '     1      ', 'not applic. ', '-           ', '     0.5    ')
    expect_equal (nchar(eCol11), c(12, 12, 12, 12, 12, 12))
    expect_equal (aCol11, eCol11)
})

test_that ('formatNonDotColumn', {
    values <- c ('ab', 'def', '', ' ')
    # allign left
    vFormattedL1 <- c ("L   ", "    ", "ab  ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('L', ''   , values), allign='left'  ), vFormattedL1)
    vFormattedL2 <- c ("L2  ", "s   ", "ab  ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('L2', 's' , values), allign='left'), vFormattedL2)
    vFormattedL3 <- c ("L23 ", "ab  ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('L23'     , values    ), allign='left'  ), vFormattedL3)
    vFormattedL4 <- c ("L234 ", "ab   ", "def  ", "     ", "     ")
    expect_equal (formatNonDotColumn   (c ('L234'    , values), allign='left'  ), vFormattedL4)
    #allign rigth
    vFormattedR1 <- c ("  R ", " ab ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('R'       , values), allign='right' ), vFormattedR1)
    vFormattedR2 <- c (" R2 ", " ab ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('R2'      , values), allign='right' ), vFormattedR2)
    vFormattedR3 <- c ("R23 ", " ab ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('R23'     , values), allign='right' ), vFormattedR3)
    vFormattedR4 <- c ("R234 ", "  ab ", " def ", "     ", "     ")
    expect_equal (formatNonDotColumn   (c ('R234'    , values), allign='right' ), vFormattedR4)
    # centre
    vFormattedC1 <- c (" C  ", "ab  ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('C'       , values), allign='centre'), vFormattedC1)
    vFormattedC2 <- c ("C2  ", "ab  ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('C2'      , values), allign='centre'), vFormattedC2)
    vFormattedC3 <- c ("C23 ", "ab  ", "def ", "    ", "    ")
    expect_equal (formatNonDotColumn   (c ('C23'     , values), allign='centre'), vFormattedC3)
    vFormattedC4 <- c ("C234 ", " ab  ", "def  ", "     ", "     ")
    expect_equal (formatNonDotColumn   (c ('C234'    , values), allign='centre'), vFormattedC4)
})

# ------------------------------------------------------------------------------
#                            test agains std output files
# ------------------------------------------------------------------------------
testTable <- function (qT, maxRows=24, name, do='check') {
    file    <- paste (name, 'txt', sep='.')
    ffn     <- paste (refPrintsDir, file, sep='/')
    # do      <- 'show'
    if (do=='create') {
        if (file.exists(file)) file.remove (ffn)
        printQTable (envir=qT, maxRows=maxRows, file=ffn)
    } else if (do=='check') {
        expect_known_output (printQTable (envir=qT, maxRows=maxRows, file=''), ffn, update=TRUE, print=FALSE)
    } else if (do=='show'){
        cat ('\n', name, ":\n")
        printQTable (envir=qT, maxRows=maxRows, file='')
    } else {
        cat ("testTable: do error /n")
    }
}


testLines <- function (qL, name, do='check') {
    file    <- paste (name, 'txt', sep='.')
    ffn     <- paste (refPrintsDir, file, sep='/')
    # do = 'show'
    if (do=='create') {
        if (file.exists(file)) file.remove (ffn)
        printQLines (envir=qL, file=ffn)
    } else if (do=='check') {
        expect_known_output (printQLines (envir=qL, file=''), ffn, update=TRUE, print=FALSE)
    } else if (do=='show'){
        cat ('\n', name, ":\n")
        printQLines (envir=qL, file='')
    } else {
        cat ("testTable: do error /n")
    }
}

# all null lists and empty table / lines section
anl <- vector (mode='list', 3)

qL <- newQLines()
name    <- 'emptyLines'
testLines (qL, name=name,  do='check')

qL <- newQLines()
qL$add ('all nulls one', anl, skipAllNull=TRUE)
qL$add ('all nulls two', anl, skipAllNull=FALSE)
name    <- 'aNullLine'
testLines (qL, name=name,  do='check')

qL <- newQLines()
qL$add ('all nulls one', anl, skipAllNull=TRUE)
name    <- 'noNullLine'
testLines (qL, name=name,  do='check')

qT <- newQTable()
name    <- 'emptyTable'
testTable (qT, maxRows=7, name=name,  do='check')

qT <- newQTable()
qT$add ('anl1'  , anl, skipAllNull=TRUE)
qT$add ('anl2'  , anl, skipAllNull=FALSE)
name    <- 'aNullColumn'
testTable (qT, maxRows=7, name=name,  do='check')

qT <- newQTable()
qT$add ('anl1'  , anl, skipAllNull=TRUE)
name    <- 'noNullColumn'
testTable (qT, maxRows=7, name=name,  do='check')

# headers
hd      <- c('a', 'splitted', 'default alligned', 'caption')
hl      <- c('a', 'splitted', 'left alligned', 'caption')
hc      <- c('a', 'splitted', 'centered alligned', 'caption')
hr      <- c('a', 'splitted', 'rigth alligned', 'caption')
hs      <- 'short'
qT      <- newQTable()
qT$add (hd, 'default')
qT$add (hl, 'left', allign='l')
qT$add (hc, 'centre', allign ='center')
qT$add (hr, 'right', allign='ri')
qT$add (hs, 'long default entry')
name    <- 'tableHeaders'
testTable (qT, maxRows=7, name=name, do='check')

# long table
x       <- c(3.767264e-01, -1.907901e-02, -9.698776e-02, -7.276240e+05,  1.201840e+02,  5.116543e+08, -2.101833e-04,  2.756080, -1.834473e-02,  4.341378e+07)
qT      <- newQTable()
qT$add ('x', x)
name    <- 'truncatedTable'
testTable (qT, maxRows=7, name=name, do='check')

# dot table
qT <- newQTable()
qT$add ('a'          , c (123.567, 23.567890 , 3.  , 23  ), u='unit')
qT$add ('abcdefghij' , c (123.567, 23.567890 , 3.  , 23  ), u='u2'  )
qT$add ('abcdefghijk', c (123.567, 23.567890 , 3.  , 23  )             )
qT$add ('hdr2'       , c(1.3456  , 0         , 0   , 0.5 ), zero='-'   )
qT$add ('hdr3a'      , c(1.3     , 0         , 0   , 0.5 ), zero = ''  )
qT$add ('hdr4'       , c(1.3456  , -Inf      , Inf , 0.5 ), pInf='much')
qT$add ('hdr5'       , c(1.345   , -Inf      , Inf , 0.5 ), mInf='less')
qT$add ('large hdr6' , c(1.3456  , 1         , NA  , 0.5 )             )
qT$add ('large hdr7' , c(1.3456  , 1         , NA  , 0.5 ), na='n.a.'  )
qT$add ('large hdr8' , c(1.3456  , 1         , NA  , 0.5 )             )
name    <- 'dotTable'
testTable (qT, name=name,  do='check')

# non dot text table
qT <- newQTable()
values <- c ('ll', '', 'lll', ' ', 'l')
qT$add ('x'   ,values,  allign='l')
qT$add ('xyz' , values, allign='l')
qT$add ('xyzz', values, allign='l')
values <- c ('rr', '', 'rrr', ' ', 'r')
qT$add ('x'   , values, allign='r')
qT$add ('xyz' , values, allign='r')
qT$add ('xyzz', values, allign='r')
values <- c ('cc', '', 'ccc', ' ', 'c')
qT$add ('x'   , values, allign='c')
qT$add ('xyz' , values, allign='c')
qT$add ('xyzz', values, allign='c')
name    <- 'nonDotTable'
testTable (qT, name=name, do='check')

# logical table columns
qT <- newQTable()
values <- c (TRUE, TRUE, FALSE, FALSE, TRUE, NA, TRUE, FALSE)
qT$add ("CTrue", values, true = 'v')
qT$add ("CFalse", values, false= 'No')
qT$add ("CBot", values, true = 'v', false='x', na='n.a.')
qT$add ("CDef", values)
name    <- 'logicalTable'
testTable (qT, maxRows=7, name=name,  do='check')

# POSIX (with the same UTC time)
uct1 <- as.POSIXct( strptime("2017-04-10 00:00:00"   , "%Y-%m-%d %H:%M:%OS"), "UTC")
uct2 <- as.POSIXct( strptime("2017-04-10 12:00:00"   , "%Y-%m-%d %H:%M:%OS"), "UTC")
uct3 <- as.POSIXct( strptime("2017-04-10 12:00:00.12", "%Y-%m-%d %H:%M:%OS"), "UTC")
uct  <- c (uct1, uct2, uct3)
hst  <- uct; attr(hst, 'tzone') <- 'HST'  # Hawai 10 hours late
emp  <- uct; attr(emp, 'tzone') <- ''
nul  <- uct; attr(nul, 'tzone') <- NULL
na   <- uct; na[2] <- na[3] <- NA
qT <- newQTable()
qT$add ('UCT'  , uct)
qT$add ('HST'  , hst)
qT$add ('EMPTY', emp)
qT$add ('NULL' , nul)
qT$add ('n.a.' , na, na='n.a.')
name    <- 'posixTable'
testTable (qT, maxRows=7, name=name,  do='check')

# example for newQTable
qT <- newQTable ()
qT$add ('channel', c(1,2,3,4))
qT$add ('filtered', c(TRUE, FALSE, TRUE, FALSE), allign = 'c')
qT$add ('sampleRate', c(512, 256, 512, 256), u='/sec')
qT$add (c('signal', 'max'), c(.123, 21.12, 21, 5421))
name    <- 'newTableExample'
testTable (qT, maxRows=7, name=name, do='check')
