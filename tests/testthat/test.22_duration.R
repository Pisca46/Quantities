#                               duration class test
#
# Purpose   :   Test duration functions
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
#     Oct17 - Created
#     Dec17 - Version 0.1 (Dec 20)
#     Mar18 - Revised
# ------------------------------------------------------------------------------

require (quantities)
qEnvir <- quantities:::qEnvir
require (testthat)
context ("Duration testing.")

# ------------------------------------------------------------------------------
#                             is.time & is.frequancy
# ------------------------------------------------------------------------------
test_that ("is.time", {
    x <- as.time (123, 'kilo.year')
    expect_true (is.time(x))
})

test_that ("frequency", {
    x <- as.quantity (123, 'time$pico.second^-1')
    expect_true (is.frequency(x))
})
# ------------------------------------------------------------------------------
#                           as.time & as.frequancy
# ------------------------------------------------------------------------------

na  <- as.numeric (NA)

# ------------------------------------------------------------------------------
#                         conversions to and from hhmmss
# ------------------------------------------------------------------------------
test_that ("secToHms",{
    # sub minute
    expect_equal(secToHms (1.2345678                   ), '00:00:01.23457')
    expect_equal(secToHms (1.234567,       formal=FALSE), '1.23457')
    # sub hour
    expect_equal(secToHms (123                         ), '00:02:03' )
    # expect_equal(secToHms ('123'                     ), '00:02:03' )         # include ???
    expect_equal(secToHms (123,            formal=FALSE), '2:3'      )
    expect_equal(secToHms (-123,           formal=FALSE), '-2:3'     )
    expect_equal(secToHms (123.45,         formal=FALSE), '2:3.45'   )
    expect_equal(secToHms (123.1234567,    formal=FALSE), '2:3.123'  )
    expect_equal(secToHms (123.1236,       formal=FALSE), '2:3.124'  )
    expect_equal(secToHms (123.1234567                 ), '00:02:03.123')
    expect_equal(secToHms (123.1234564                 ), '00:02:03.123')
    # hour
    expect_equal(secToHms (3600                         ), '01:00:00')
    expect_equal(secToHms (3600,           formal=FALSE), '1:0:0'    )
    # illegals
    expect_error(secToHms (NULL)            , "sec must be numerical")
    expect_error(secToHms (FALSE)           , "sec must be numerical")
    expect_error(secToHms (list())          , "sec must be numerical")
    expect_error(secToHms ('spam')          , "sec must be numerical")
    expect_error(secToHms (NA)              , "sec must be numerical")
    expect_equal(secToHms (as.numeric(NA))  , '')

    # vectors
    expect_equal(secToHms (c(123, 124.5)                ), c ('00:02:03', '00:02:04.5'))
    expect_equal(secToHms (c(123, -124.5)               ), c ('00:02:03', '-00:02:04.5'))
    expect_equal(secToHms (c(123, NA)                   ), c ('00:02:03', ''))
})

test_that ("timeToHms", {
    expect_equal(timeToHms(as.time (85)                 ), '00:01:25')
    expect_equal(timeToHms(as.time (25.5,'minute')      ), '00:25:30')
})

test_that ('hmsToSec', {
    na <- as.numeric (NA)
    expect_equal(hmsToSec ('2:3 ')   , as.quantity (123  , 'time$second'))
    expect_equal(hmsToSec ('2.5:3.5'), as.quantity (153.5, 'time$second'))
    expect_equal(hmsToSec ('2:')     , as.quantity (120  , 'time$second'))
    expect_equal(hmsToSec ('2:3:4')  , as.quantity (7384 , 'time$second'))
    expect_equal(hmsToSec ('-2:3:4') ,-as.quantity (7384 , 'time$second'))
    expect_equal(hmsToSec ('2:3:')   , as.quantity (7380 , 'time$second'))
    expect_equal(hmsToSec ('2::4')   , as.quantity (7204 , 'time$second'))
    expect_equal(hmsToSec ('2::')    , as.quantity (7200 , 'time$second'))

    expect_equal(hmsToSec ('2:b')    , as.quantity (na, 'time$second'))
    expect_equal(hmsToSec ('b:2')    , as.quantity (na, 'time$second'))
    expect_equal(hmsToSec ('b:')     , as.quantity (na, 'time$second'))
    expect_equal(hmsToSec ('b:2:1')  , as.quantity (na, 'time$second'))

    naQ <- as.quantity (na, 'time$second')
    expect_warning (hmsToSec (27)     , "hhmmss must be character, NAs introduced by coercion")
    expect_equal   (hmsToSec (27)     , naQ)
    expect_warning (hmsToSec ('spam') , "NAs introduced by coercion")
    expect_equal   (hmsToSec ('spam') , naQ)
    expect_warning (hmsToSec (TRUE)   , "hhmmss must be character, NAs introduced by coercion")
    expect_equal   (hmsToSec (TRUE)   , naQ)
    expect_warning (hmsToSec (NULL)   , "hhmmss must be character, NAs introduced by coercion")
    expect_equal   (hmsToSec (NULL)   , naQ)
    #
    # vectors
})
# ------------------------------------------------------------------------------
#                         conversions to and from yymmdd
# ------------------------------------------------------------------------------
test_that ("dayToYmd",{
    # year errors   "Year must be (an abbreviation of) 'Julian' or 'Gregorian' (default)."
    expect_error (dayToYmd (1, year='spam'), "Year must be [(]an abbreviation of) 'Julian' or 'Gregorian' [(]default).")
    # sub day
    expect_equal(dayToYmd (0.2345678                   ), '00:00:00.2346')
    # sub month
    expect_equal(dayToYmd (1.2345678                   ), '00:00:01.235')
    expect_equal(dayToYmd (1.2345678,     year='julian'), '00:00:01.235')
    expect_equal(dayToYmd (1.234567,       formal=FALSE), '1.235')
    # sub year
    expect_equal(dayToYmd (123                         ), '00:04:01.252')
    expect_equal(dayToYmd (123,            formal=FALSE), '4:1.252'     )
    expect_equal(dayToYmd (-123,           formal=FALSE), '-4:1.252'    )
    expect_equal(dayToYmd (123.45,         formal=FALSE), '4:1.703'  )
    expect_equal(dayToYmd (123.1234567,    formal=FALSE), '4:1.376' )
    expect_equal(dayToYmd (123.1236,       formal=FALSE), '4:1.376')
    expect_equal(dayToYmd (123.1234567                 ), '00:04:01.376')
    expect_equal(dayToYmd (123.1234564                 ), '00:04:01.376')
    # year +
    expect_equal(dayToYmd (12345678                    ), '33801:03:24.95')
    expect_equal(dayToYmd (12345678,      year='julian'), '33800:07:14.94')
    # illegals
    expect_error(dayToYmd (NULL)            , "day must be numerical")
    expect_error(dayToYmd (FALSE)           , "day must be numerical")
    expect_error(dayToYmd (list())          , "day must be numerical")
    expect_error(dayToYmd ('spam')          , "day must be numerical")
    expect_error(dayToYmd (NA)              , "day must be numerical")
    expect_equal(dayToYmd (as.numeric(NA))  , '')
    # vectors
    expect_equal(dayToYmd (c(123, 124.5)                ), c ('00:04:01.252', '00:04:02.752'))
    expect_equal(dayToYmd (c(123, -124.5)               ), c ('00:04:01.252', '-00:04:02.752'))
    expect_equal(dayToYmd (c(123, NA)                   ), c ('00:04:01.252', ''))
})
