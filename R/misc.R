#                                   miscwllaneous
#
# Purpose   :   a place for miscellaneus (noon-numice) basic utility function
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
# History    :
#     Oct17 - Created
#     Dec17 - Version 0.1 (Dec 20)
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#                          parameter value processing
# ------------------------------------------------------------------------------
#' process a possible abbreviated enumetered type parameter
#' @param x a character string with the actual paremeter
#' @param type a character vector with the set of possible values
#' @return the unabreviated value or as.character(NA) in case of an error
#' @keywords internal
procesEnumertedParameter <- function (x, type) {
    p <- as.character (NA)
    if (!is.null(x) && length(x)==1 && is.character(x) && is.character(type)) {
        # allow '(' and ')' in names (used in unit names like "horsepower(E)")
        x <- sub ('[(]', '[(]', x)
        x <- sub ('[)]', '[)]', x)
        i <- grep (paste ('^', x, sep=''), type)
        if (length (i) == 1)    p <- type [i]
    }
    p
}
