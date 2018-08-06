#                               duration class test
#
# Purpose   :   Test misc functions
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
#     Oct17 - Created
#     Dec17 - Version 0.1 (Dec 20)
# ------------------------------------------------------------------------------

require (testthat)
context ("Test the misc functions.")

# source ("./R/misc.R")
procesEnumertedParameter    <- quantities:::procesEnumertedParameter

test_that ('procesEnumertedParameter', {
    na <- as.character (NA)
    type <- c('January', 'February', 'March', 'May', 'April' ,'June', 'July')
    expect_equal (procesEnumertedParameter ('May', type), 'May')
    expect_equal (procesEnumertedParameter ('Ma', type), na)
    expect_equal (procesEnumertedParameter ('Ja', type), 'January')
    expect_equal (procesEnumertedParameter ('XX', type), na)
    expect_equal (procesEnumertedParameter (NULL, type), na)
    expect_equal (procesEnumertedParameter ( NA , type), na)
})
