#                           add defined quantities
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
# this is a finalising quantity source
#' @include quantityDefinitions.R
#' @include quantities.R
#' @include qPrefix.R
#' @include qDurationCA.R
#' @include qLengthCA.R
#' @include qMassCA.R
NULL

# ------------------------------------------------------------------------------
#                         check existence quantity definition
#
#    NOTE: run saveQdefs() in .private/utils.r to save qDefs for package use.
# ------------------------------------------------------------------------------

addPackageDefs <- function () {
    qDefsDir    <- system.file ("Qdefinitions", package="quantities")
    qDefsFfn    <- paste(qDefsDir, 'qDefs.RData', sep = '/')
    if (file.exists(qDefsFfn)) load (qDefsFfn, envir=qEnvir)

    if (is.null (qEnvir$qDefs$prefix)) {
        # create a bootstrap
        qEnvir$qDefs$prefix             <- list (units=list(), dummy=TRUE)
        qEnvir$qDefs$prefix$units$none  <- list (pe=0)
        addQprefix ()
        cat ("adding quantity definitions: re-added 'prefix'\n")
    }
    # order because of dependencies
    qDefNames <- c ('angle', 'time', 'frequency', 'length', 'area', 'volume', 'mass', 'force', 'energy', 'power')
    #    NOTE: run saveQdefs() in .private/utils.r to save qDefs for package use.
    # check on compleness
    if (!setequal (qDefNames, names(packageQdefs))) stop ("Missing q definitions")
    for (qName in qDefNames) {
        if (is.null (qEnvir$qDefs[[qName]])) {
            packageQdefs[[qName]]()
            cat ("adding quantity definitions: re-added", qName, '\n')
        }
    }
}

addPackageDefs ()
