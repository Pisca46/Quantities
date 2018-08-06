#                               quantity class test
#
# Purpose   :   Test quantity class functions
#
#
# Copyright :   (C) 2017-2018, Vis Consultancy, the Netherlands
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
#     Mar-Jul18 - Revised
# ------------------------------------------------------------------------------

require (quantities)
qEnvir <- quantities:::qEnvir
require (testthat)
context ("Testing quantity functions")

# ------------------------------------------------------------------------------
#                             quantity specifications
# ------------------------------------------------------------------------------
test_that ("quantity specification parsing parameter errors", {
    pQu     <- quantities:::parseQunit
    expect_equal (pQu (5)$err                      , "Illegal quantity specification '5'")
    expect_equal (pQu (character(0))$err           , "Illegal quantity specification ''")
    expect_equal (pQu ('')$err                     , "Illegal quantity specification ''")
    expect_equal (pQu ('time^1^2')$err             , "Illegal time^1^2 only one '^' allowed")
    expect_equal (pQu ('time^a')$err               , "Illegal non-integer exponent in 'time^a'")
    expect_equal (pQu ('time$a$b')$err             , "Illegal time$a$b only one '$' allowed")
    expect_equal (pQu ('spam')$err                 , "Illegal quantity 'spam'")
    expect_equal (pQu ('prefix^2')$err             , "The exponent for the 'prefix' quantity must be ommited or 1")
    expect_equal (pQu ('time$k.k.day')$err         , "Illegal k.k.day only one '.' allowed")
    expect_equal (pQu ('prefix$mega.kilo time')$err, "The unit prefix for the 'prefix' quantity must be ommited or 'none'")
    expect_equal (pQu ('prefix$spam time')$err     , "Illegal 'prefix' unit 'spam'")
    expect_equal (pQu ('time$spam')$err            , "Illegal 'time' unit 'spam'")
    expect_equal (pQu ('time$spam.day')$err        , "Illegal unit prefix 'spam'")
    expect_equal (pQu ('prefix')$err               , "only a 'prefix', no quantity specified")
    expect_equal (pQu ('time prefix')$err          , "'prefix' may be used as quantity at the first postion only")
})

test_that("simple quantity specification parsing", {
    pQu     <- quantities:::parseQunit
    qs1     <- pQu ('length')[[1]]
    expect_equal (qs1$q   , 'length'        )
    expect_equal (qs1$p   , 'none'          )
    expect_equal (qs1$pe  ,  0              )
    expect_equal (qs1$u   , 'metre'         )
    expect_equal (qs1$e   ,  1              )
    expect_equal (qs1$unit, 'length$metre'  )
    # user spec as vector
    qs  <- pQu (c("prefix$mega", "mass$pico.gram^2"))
    qs1 <- qs[[1]]
    expect_equal (qs1$q   , 'prefix'        )
    expect_equal (qs1$p   , 'none'          )
    expect_equal (qs1$u   , 'mega'          )
    expect_equal (qs1$pe  ,  6              )
    expect_equal (qs1$e   ,  1              )
    expect_equal (qs1$unit, 'prefix$mega'   )
    qs2 <- qs[[2]]
    expect_equal (qs2$q   , 'mass'          )
    expect_equal (qs2$p   , 'pico'          )
    expect_equal (qs2$pe  , -12             )
    expect_equal (qs2$u   , 'gram'          )
    expect_equal (qs2$e   ,  2              )
    expect_equal (qs2$unit, "mass$pico.gram^2" )

    # user spec as char string
    qs  <- pQu ("prefix$mega mass$pico.gram^2")
    qs1 <- qs[[1]]
    expect_equal (qs1$q   , 'prefix'        )
    expect_equal (qs1$p   , 'none'          )
    expect_equal (qs1$u   , 'mega'          )
    expect_equal (qs1$pe  ,  6              )
    expect_equal (qs1$e   ,  1              )
    expect_equal (qs1$unit, 'prefix$mega'   )
    qs2 <- qs[[2]]
    expect_equal (qs2$q   , 'mass'          )
    expect_equal (qs2$p   , 'pico'          )
    expect_equal (qs2$pe  , -12             )
    expect_equal (qs2$u   , 'gram'          )
    expect_equal (qs2$e   ,  2              )
    expect_equal (qs2$unit, "mass$pico.gram^2" )

    # default mass = kilo.gram
    qs1  <- pQu ("mass^2")[[1]]
    expect_equal (qs1$q   , 'mass'          )
    expect_equal (qs1$p   , 'kilo'          )
    expect_equal (qs1$pe  ,  3              )
    expect_equal (qs1$u   , 'gram'          )
    expect_equal (qs1$e   ,  2              )
    expect_equal (qs1$unit, "mass$kilo.gram^2")
})

test_that ("derived quantity parsing", {
    pQs     <- quantities:::parseQunit
    eQs     <- quantities:::expandPqs
    aUs     <- quantities:::assemblePunit
    e       <- as.quantity (87, 'energy')
    p1      <- as.quantity (88, 'energy time$hour^-1')
    p2      <- as.quantity (88, 'energy time$second^-1')
    p3      <- as.quantity (89, 'power$watt')
    p4      <- as.quantity (89, 'power$horsepower(E)')
    eqsE    <- eQs (pQs (e))
    expect_equal (aUs (eqsE$pqs ) , c("mass$kilo.gram", "length$metre^2", "time$second^-2"                  ))
    expect_equal (eqsE$pe         , 0                                                                        )
    expect_equal (eqsE$f          , 1                                                                        )
    eqsP1   <- eQs (pQs (p1))
    expect_equal (aUs (eqsP1$pqs) , c("mass$kilo.gram", "length$metre^2", "time$second^-2", "time$hour^-1"  ))
    expect_equal (eqsP1$pe        , 0                                                                        )
    expect_equal (eqsP1$f         , 1                                                                        )
    eqsP2   <- eQs (pQs (p2))
    expect_equal (aUs (eqsP2$pqs) , c("mass$kilo.gram", "length$metre^2", "time$second^-2", "time$second^-1"))  # no merging
    expect_equal (eqsP2$pe        , 0                                                                        )
    expect_equal (eqsP2$f         , 1                                                                        )
    eqsP3     <- eQs (pQs (p3))
    expect_equal (aUs (eqsP3$pqs) , c("mass$kilo.gram", "length$metre^2", "time$second^-2", "time$second^-1"))
    expect_equal (eqsP3$pe        , 0                                                                        )
    expect_equal (eqsP3$f         , 1                                                                        )
    eqsP4     <- eQs (pQs (p4))
    expect_equal (aUs (eqsP4$pqs) , c("mass$kilo.gram", "length$metre^2", "time$second^-2", "time$second^-1"))
    expect_equal (eqsP4$pe        , 0                                                                        )
    expect_equal (eqsP4$f         , 746                                                                      )
    # test recursive expansion
    watt                                    <- qEnvir$qDefs[['power']]$units$watt
    qEnvir$qDefs[['power']]$units$watt$def <- "energy$joule time$second^-1"
    qEnvir$qDefs[['power']]$units$watt$pqs <- pQs (qEnvir$qDefs[['power']]$units$watt$def)
    eqsP4                                   <- eQs (pQs (p4))
    expect_equal (aUs (eqsP4$pqs) , c("mass$kilo.gram", "length$metre^2", "time$second^-2", "time$second^-1"))
    expect_equal (eqsP4$pe        , 0                                                                        )
    expect_equal (eqsP4$f         , 746                                                                      )
    qEnvir$qDefs[['power']]$units$watt     <<- watt
})

# ------------------------------------------------------------------------------
#                                as / is quantity
# ------------------------------------------------------------------------------
test_that ('assemble p unit', {
    aPu  <- quantities:::assemblePunit
    pqs1 <- list (q='prefix', pe=15, u='peta', e=1)      # unbit is ommitted
    pqs2 <- list (q='time', pe=0, p='none', u='second', e=2)      # unbit is ommitted
    pqs  <- list (pqs1, pqs2)
    expect_equal (aPu (pqs, formal=FALSE), c ("prefix$peta", "time$second^2"     ))
    expect_equal (aPu (pqs, formal=TRUE) , c ("prefix$peta", "time$none.second^2"))
})

test_that ("as quantity", {
    q <- as.quantity (5, 'power')
    expect_equal (class(q), c('quantity', 'numeric'))
    expect_equal (as.numeric(q), 5)
    expect_equal (attr(q, 'unit'), 'power$watt')
    q <-as.quantity (220, c("prefix$kilo", "length$metre", "time$second^-2"))
    expect_equal (class(q), c('quantity', 'numeric'))
    expect_equal (as.numeric(q), 220)
    expect_equal (attr(q, 'unit'), c("prefix$kilo", "length$metre", "time$second^-2"))
    q <-as.quantity (221, "length$kilo.metre time$second^-2")
    expect_equal (class(q), c('quantity', 'numeric'))
    expect_equal (as.numeric(q), 221)
    expect_equal (attr(q, 'unit'), c("length$kilo.metre", "time$second^-2"))
})

test_that ("is quantity", {
    q <-as.quantity (5000, c("prefix$kilo", "angle$radian^-2"))
    expect_true (is.quantity (q))

    # details
    # pQu <- quantities:::parseQunit
    # aPu <- quantities:::assemblePunit
    x <- 5
    expect_false (is.quantity (x))
    class(x) <- c('qua')
    expect_false (is.quantity(x))
    class(x) <- c('quantity', 'numeric')
    expect_false (is.quantity(x))

    y <- as.numeric (NA)
    class(y) <- c('quantity', 'numeric')
    expect_false (is.quantity(y))
    attr(y, 'unit') <- 'length$inch'
    expect_true (is.quantity(y))

    z <- c (x, y)                                # c() removes attributes and class info
    expect_false (is.quantity(z))
    class(z) <- c('quantity', 'numeric')
    attr (z, 'unit') <- "prefix$kilo length$inch time$milli.second^-2"
    expect_true (is.quantity(z))
})

test_that ("as.quantity scale conversions", {
    # prefix conversions
    a1  <- as.quantity (1000000, 'length$metre^2')
    a2  <- as.quantity (a1, 'prefix$kilo length$metre^2')
    a3  <- as.quantity (a1, 'length$kilo.metre^2')
    expect_equal (format(a2), "1000 k m²")
    expect_equal (format(a3), "1 km²")
    # unit conversions
    l <- as.quantity (3600, 'angle$kilo.degree')
    l <- as.quantity (l, 'angle$mega.revolution')
    expect_equal (format(l), "0.01 Mrev")

    l <- as.quantity (3600, c('prefix$kilo', 'angle$degree'))
    l <- as.quantity (l, c('prefix$kilo', 'angle$kilo.revolution'))
    expect_equal (format(l), "10 krev")
    l <- as.quantity (3, c('time$hour', 'time$second'))
    l <- as.quantity (l, 'time$minute^2')
    expect_equal (format(l), '3 min²')
    l <- as.quantity (3, c('length$metre^-1', 'length$kilo.foot^-1'))
    l <- as.quantity (l, 'length$metre^-2')
    expect_equal (format(l), "0.00984252 m⁻²")
})

test_that ("as.quantity with derived scale conversions", {          # check with https://www.rapidtables.com/convert/
    pQs     <- quantities:::parseQunit
    eQs     <- quantities:::expandPqs
    aUs     <- quantities:::assemblePunit
    #
    e1Qs    <- 'energy$kilo.joule'
    e1      <- as.quantity (5 , e1Qs)
    e2      <- as.quantity (e1, "mass$mega.gram length$metre^2 time$second^-2")
    expect_equal (format(e2), "5 Mgm²s⁻²")
    e1bis   <- as.quantity (e2, e1Qs)
    expect_equal (e1, e1bis)
    # more complex
    e1hp    <- as.quantity (1/746 , 'power$horsepower(E)')
    e1w     <- as.quantity (e1hp  , 'power$watt')
    expect_equal (format (e1w), '1 W')

    e2hpm   <- as.quantity (3/746, 'power$horsepower(E) time$minute')
    e2hps   <- as.quantity (e2hpm, 'power$horsepower(E) time$second')
    expect_equal (format (e2hps), "0.2412869 hp(E)s")

    e2ws    <- as.quantity (e2hps, 'power$watt time$second')
    expect_equal (format(e2ws), "180 Ws")
    e2kwh   <- as.quantity (e2hps, 'power$kilo.watt time$hour')
    expect_equal (format(e2kwh), "5e-05 kWh")
    e2kJ    <- as.quantity (e2hps, 'energy$kilo.joule')
    expect_equal (format(e2kJ), "0.18 kJ")
    e2hpmB  <- as.quantity (e2kJ, 'power$horsepower(E) time$minute')
    expect_equal (e2hpm, e2hpmB)
})
# ------------------------------------------------------------------------------
#                               qDim
# ------------------------------------------------------------------------------

test_that ("qDim", {
    x           <- as.quantity (85, "prefix$kilo length$inch time$hour time$milli.second^-2")
    d           <- c (1, -1)
    names(d)    <- c('length', 'time')
    expect_equal (qDim (x), d)

    x           <- as.quantity (.14, "length$metre length$metre^-1")
    d           <- 0
    names(d)    <- 'length'
    expect_equal (qDim (x), d)

    x <- as.quantity (.14, "prefix$pico length$metre length$metre^-1")
    expect_equal (qDim (x), d)
})

test_that ("Condense dim", {
    cd <- quantities:::condenseDim
    dim <- c (mass=1, length=2, time=-2)
    expect_equal (cd (dim), c(energy=1))
    dim <- c (mass=1, length=2, time=-3)
    expect_equal (cd (dim), c(power=1))
    dim <- c (mass=1, length=2, time=-4)
    expect_equal (cd (dim), c(power=1, time=-1))  # not c(energy=1, time=-2)

})

test_that ('normaliseQValue', {
    nQv <- quantities:::normaliseQValue
    expect_equal (nQv (as.quantity(30000, "time$mega.second"    )), as.quantity (30, "time$giga.second"         ))
    expect_equal (nQv (as.quantity(30000, "time$mega.second^2"  )), as.quantity (30, "prefix$peta time$second^2"))
    expect_equal (nQv (as.quantity(.03  , "time$mega.second"    )), as.quantity (30, "time$kilo.second"         ))
    expect_equal (nQv (as.quantity(.001 , "time$mega.second"    )), as.quantity ( 1, "time$kilo.second"         ))
    expect_equal (nQv (as.quantity(30000, "time$milli.second^-1")), as.quantity (30, "time$micro.second^-1"     ))
    # with prefix
    expect_equal (nQv (as.quantity(30000, "prefix$milli time$mega.second"    )), as.quantity (30, "time$mega.second"         ))
    expect_equal (nQv (as.quantity(30000, "prefix$pico  time$mega.second^2"  )), as.quantity (30, "prefix$kilo time$second^2"))
    expect_equal (nQv (as.quantity(.03  , "prefix$mega  time$mega.second"    )), as.quantity (30, "time$giga.second"         ))
    expect_equal (nQv (as.quantity(.001 , "prefix$kilo  time$mega.second"    )), as.quantity ( 1, "time$mega.second"         ))
    expect_equal (nQv (as.quantity(30000, "prefix$kilo  time$milli.second^-1")), as.quantity (30, "time$nano.second^-1"      ))
})

# ------------------------------------------------------------------------------
#                                 scale conversions
# ------------------------------------------------------------------------------
# adjustQprefix <- function (x, to, mode, ps)
# internal function parameters are assumed to be correct and correctly ordered
test_that ("adjustQprefix", {
    ap  <- quantities:::adjustQprefix
    ps  <- names (qEnvir$qDefs$prefix$units)
    ps3 <- setdiff(ps, c('hecto', 'deca', 'deci', 'centi'))
    x   <- as.quantity (123, "time$second")
    expect_equal (ap (x, 1  , mode='min'     , ps=ps ), as.quantity (1.23 , "time$hecto.second"))
    expect_equal (ap (x, 1  , mode='fraction', ps=ps ), as.quantity (1.23 , "time$hecto.second"))
    expect_equal (ap (x, 1  , mode='max'     , ps=ps ), as.quantity (.123 , "time$kilo.second" ))
    expect_equal (ap (x, 1  , mode='min'     , ps=ps3), as.quantity (123  , "time$second"      ))
    expect_equal (ap (x, 1  , mode='fraction', ps=ps3), as.quantity (.123 , "time$kilo.second" ))
    expect_equal (ap (x, 1  , mode='max'     , ps=ps3), as.quantity (.123 , "time$kilo.second" ))
    # check no rounding
    expect_equal (ap (x, 123, mode='min'     , ps=ps ), as.quantity (123  , "time$second"      ))
    expect_equal (ap (x, 123, mode='fraction', ps=ps ), as.quantity (123  , "time$second"      ))
    expect_equal (ap (x, 123, mode='max'     , ps=ps ), as.quantity (123  , "time$second"      ))
    # check rounding
    u   <- as.quantity (round(10^.5 - .001, digits = 3), "time$second")
    v   <- as.quantity (round(10^.5 + .001, digits = 3), "time$second")
    y   <- as.quantity ((1+10)/2 - .001, 'time$second')
    z   <- as.quantity ((1+10)/2 + .001, 'time$second')
    expect_equal (ap (u, 1  , mode='fraction', ps=ps ), as.quantity (3.161, "time$second"))
    expect_equal (ap (v, 1  , mode='fraction', ps=ps ), as.quantity (.3163, "time$deca.second"))
    expect_equal (ap (y, 1  , mode='round'   , ps=ps ), as.quantity (5.499, "time$second"))
    expect_equal (ap (z, 1  , mode='round'   , ps=ps ), as.quantity (.5501, "time$deca.second"))
    # check prefix constraints
    expect_equal (ap (x, 123, mode='min'     , ps=c('deca', 'deci')), as.quantity (1230 , "time$deci.second"      ))
    expect_equal (ap (x, 123, mode='max'     , ps=c('deca', 'deci')), as.quantity (12.3 , "time$deca.second"      ))
})

# adjustQunit <- function (x, to, mode, us)
# internal function parameters are assumed to be correct and correctly ordered
test_that ("adjustQunit", {
    au  <- quantities:::adjustQunit
    us  <- names (qEnvir$qDefs$time$units)
    x   <- as.quantity (84, "time$hour")
    expect_equal (au (x, 1  , mode='min'     , us=us ), as.quantity (3.5  , "time$day"  ))
    expect_equal (au (x, 1  , mode='round'   , us=us ), as.quantity (3.5  , "time$day"  ))
    expect_equal (au (x, 1  , mode='fraction', us=us ), as.quantity (.5   , "time$week" ))
    expect_equal (au (x, 1  , mode='max'     , us=us ), as.quantity (.5   , "time$week" ))
    # check no rounding
    expect_equal (au (x, 84, mode='min'     , us=us ), as.quantity (84    , "time$hour" ))
    expect_equal (au (x, 84, mode='fraction', us=us ), as.quantity (84    , "time$hour" ))
    expect_equal (au (x, 84, mode='round'   , us=us ), as.quantity (84    , "time$hour" ))
    expect_equal (au (x, 84, mode='max'     , us=us ), as.quantity (84    , "time$hour" ))
    # check rounding
    u   <- as.quantity (round(24^.5 - .001, digits = 3), "time$hour")
    v   <- as.quantity (round(24^.5 + .003, digits = 3), "time$hour")
    y   <- as.quantity ((1+24)/2 - .001, 'time$hour')
    z   <- as.quantity ((1+24)/2 + .001, 'time$hour')
    expect_equal (au (u, 1  , mode='fraction', us=us ), as.quantity (4.898   , "time$hour"))
    expect_equal (au (v, 1  , mode='fraction', us=us ), as.quantity (.20425  , "time$day" ))
    expect_equal (au (y, 1  , mode='round'   , us=us ), as.quantity (12.499  , "time$hour"))
    expect_equal (au (z, 1  , mode='round'   , us=us ), as.quantity (0.520875, "time$day" ))
    # check unit constraints
    expect_equal (au (x, 84, mode='min'     , us=c('day', 'minute')), as.quantity (5040, "time$minute"))
    expect_equal (au (x, 84, mode='max'     , us=c('day', 'minute')), as.quantity (3.5 , "time$day"   ))
})

# adjustQprefixAndUnit <- function (x, to, mode, ps, us)
# internal function parameters are assumed to be correct and correctly ordered
test_that ("adjustQprefixAndUnit", {
    apu <- quantities:::adjustQprefixAndUnit
    ps  <- names (qEnvir$qDefs$prefix$units)
    us  <- names (qEnvir$qDefs$time$units)
    x   <- as.quantity (15, "time$minute")
    expect_equal (apu (x, to=1, mode='max', ps=ps, us=us), as.quantity (.25 , "time$hour"       ))
    expect_equal (apu (x, to=1, mode='min', ps=ps, us=us), as.quantity (1.5 , "time$deca.minute"))
})

# adjustQscale <- function (x, to=NULL,  mode='fraction', prefix=NULL, unit=NULL)
test_that ("adjustQscale", {
    aqs <- adjustQscale
    ps  <- names (qEnvir$qDefs$prefix$units)
    us  <- names (qEnvir$qDefs$time$units)
    x   <- as.quantity (84, "time$hour")
    expect_error (aqs (85, to=1  , mode='r'), "x must be a quantity")
    expect_error (aqs (x , to='a', mode='r'), "'to' must be NULL or numeric" )
    expect_error (aqs (x , to=1  , mode='b'), "mode must be eihter 'min', 'max', 'round', or 'fraction'" )
    # to == NULL
    expect_equal (aqs (x         , mode='f'                        ), x)
    expect_equal (aqs (x         , mode='f' , prefix='ki'           ), as.quantity (.084 , "time$kilo.hour" ))
    expect_equal (aqs (x         , mode='f' ,              unit='mi'), as.quantity (5040 , "time$minute"    ))
    expect_equal (aqs (x         , mode='f' , prefix='ki', unit='mi'), as.quantity (5.04 , "time$kilo.minute"))
    # '='
    expect_equal (aqs (x         , mode='f' , prefix='=' , unit='=' ), x)
    expect_equal (aqs (x         , mode='f' , prefix='ki', unit='=' ), as.quantity (.084 , "time$kilo.hour"  ))
    expect_equal (aqs (x, to=1   , mode='f' , prefix='10', unit='=' ), as.quantity (.084 , "time$kilo.hour"  ))
    expect_equal (aqs (x         , mode='f' , prefix='=' , unit='mi'), as.quantity (5040 , "time$minute"     ))
    x   <- as.quantity (15, "time$minute")
    expect_equal (aqs (x, to=1   , mode='ma', prefix=ps  , unit=us  ), as.quantity (.25  , "time$hour"       ))
    expect_equal (aqs (x, to=1   , mode='mi', prefix=ps  , unit=us  ), as.quantity (1.5  , "time$deca.minute"))
})

# ------------------------------------------------------------------------------
#                               quantity operations
# ------------------------------------------------------------------------------
test_that ("Qsum", {
    expect_equal (qSum (3, 5) , 8)
    expect_equal (qSum ()     , 0)
    expect_error (qSum (3,'5'), "invalid 'type' [(]character) of argument")
    xQs <- 'time$day'
    yQs <- 'time$hour'
    zQs <- 'time$kilo.second'
    x   <- as.quantity (2, xQs)     # 2 days
    y   <- as.quantity (12, yQs)    # 12 hours
    z   <- as.quantity (21.6, zQs)  # 6 hours
    expect_equal (qSum (x,y), as.quantity (2.5  , xQs))
    expect_equal (qSum (y,x), as.quantity (60   , yQs))
    expect_equal (qSum (x,z), as.quantity (2.25 , xQs))
    expect_equal (qSum (y,z), as.quantity (18   , yQs))
    expect_equal (qSum (z,x), as.quantity (194.4, zQs))
    # more complex
    expect_equal (qSum (x,y,z),             as.quantity (2.75  , xQs))
    expect_equal (qSum (z,y,x),             as.quantity (237.6 , zQs))
    expect_equal (qSum (z,y,x, unit=xQs),   as.quantity (2.75  , xQs))

    # errors
    l <- as.quantity (5, 'length$mega.foot')
    expect_error (qSum(x,l), "All quantities must have the same dimension")
    # currently qSum (x,l) doeasn't produce a warning but an errer
    # z  <- qSum (x,l)
    # expect_equal (as.numeric (z), as.numeric(NA))
    # expect_equal (attr (z, 'unit'), xQs)
    # z  <- qSum (x,l)
    # expect_equal (as.numeric (z), as.numeric(NA))
    # expect_equal (attr (z, 'unit'), xQs)
})

test_that ('qMax', {
    expect_equal (qMax (3, 5, 2), 5)
    xQs <- 'time$day'
    yQs <- 'time$hour'
    zQs <- 'time$kilo.second'
    x   <- as.quantity (2, xQs)     # 2 days
    y   <- as.quantity (12, yQs)    # 12 hours
    z   <- as.quantity (21.6, zQs)  # 6 hours
    expect_equal (qMax (z,y,x),             as.quantity ( 2 , xQs))
    expect_equal (qMax (z,y,x,  unit=yQs),  as.quantity (48 , yQs))
})

test_that ('qMin', {
    expect_equal (qMax (3, 5, 2), 5)
    xQs <- 'time$day'
    yQs <- 'time$hour'
    zQs <- 'time$kilo.second'
    x   <- as.quantity (2, xQs)     # 2 days
    y   <- as.quantity (12, yQs)    # 12 hours
    z   <- as.quantity (21.6, zQs)  # 6 hours
    expect_equal (qMin (z,y,x),             as.quantity (21.6 , zQs))
    expect_equal (qMin (z,y,x,  unit=yQs),  as.quantity ( 6   , yQs))
})

test_that ("qProd", {
    expect_equal (qProd (3, 5), 15)

    x   <- as.quantity (2 , 'time$day')
    y   <- as.quantity (12, 'time$hour')
    expect_equal (format (qProd (x, y, unit='time$second^2' )), '7464960000 s²')
    expect_equal (format (qProd (x, y, unit='time$day^2'    )), '1 d²')
    expect_equal (format (qProd (x, y, unit=NULL            )), '7.46496 G s²')

    f <- as.quantity (10, 'mass$kilo.gram length$metre time$second^-2')
    expect_equal (format (qProd (1, f                       )), "10 N")

    x <- as.quantity (10, 'force$newton')
    y <- as.quantity (2 , 'length$metre')
    z <- as.quantity ( 5, 'time$second^-1')
    expect_equal (format (qProd (7, x,y)), "140 J")
    expect_equal (format (qProd (x,y, z)), "100 W")
    format (qProd (x,y, z))
})

test_that ("Qdiv", {
    expect_equal (qDiv (15, 5), 3)

    x   <- as.quantity (72000, 'length$metre')
    y   <- as.quantity (12, 'time$hour')
    expect_equal (format ( qDiv (x, y)), "1.666667 ms⁻¹")
    expect_equal (format ( qDiv (x, y, unit='length$kilo.metre time$hour^-1')), '6 kmh⁻¹')

    x   <- as.quantity (2, 'time$day')
    y   <- as.quantity (12, 'time$hour')
    expect_equal (format ( qDiv (x, y)), '4  s⁰')      # one space too much
})

test_that ("qUnlist", {
    x   <- as.quantity ( 2, 'time$day' )
    y   <- as.quantity (12, 'time$hour')
    l   <- list (NULL, x, NULL, y)
    z   <- qUnlist (l)
    expect_equal (as.numeric (z), c(2, .5))
    expect_equal (qUnit (z), 'time$day')
    z   <- qUnlist (l, skipNull=FALSE, unit='time$minute')
    expect_equal (as.numeric (z), c(NA, 2*24*60, NA, 720))
    expect_equal (qUnit (z), 'time$minute')
})

# ------------------------------------------------------------------------------
#                                 format quantity
# ------------------------------------------------------------------------------

test_that ('asPlural', {
    uAsPlural   <- quantities:::uAsPlural
    pQu         <- quantities:::parseQunit
    x           <- as.quantity (1, 'time$mega.year')
    pqsX        <- pQu (attr(x, 'unit'))
    expect_false (uAsPlural (1  , pqsX))
    expect_false (uAsPlural (0.5, pqsX))
    expect_true  (uAsPlural (0  , pqsX))
    expect_true  (uAsPlural (1.1, pqsX))
    expect_true  (uAsPlural (-2 , pqsX))
    # check qps variants
    x       <- as.quantity (2, c('time$mega.year', 'length$foot'))
    pqsX    <- pQu (attr(x, 'unit'))
    expect_false (uAsPlural (2  , pqsX))

    x       <- as.quantity (2, c('time$mega.year', 'length$foot','time$minute^-1'))
    pqsX    <- pQu (attr(x, 'unit'))
    expect_false (uAsPlural (2  , pqsX))

    x       <- as.quantity (2, c('length$foot','time$minute^-1'))
    pqsX    <- pQu (attr(x, 'unit'))
    expect_true (uAsPlural (2  , pqsX))

    x       <- as.quantity (2, c('prefix$mega', 'time$year'))
    pqsX    <- pQu (attr(x, 'unit'))
    expect_true (uAsPlural (2  , pqsX))
})

test_that ("format.quantity", {
    asq <- quantities:::as.quantity
    fq  <- quantities:::format.quantity
    x   <- asq  (123, 'time$micro.second')
    xT  <- fq (x, unitAs='text')
    expect_equal (xT, "123 microseconds")
    xS  <- fq (x, unitAs='symbol')
    expect_equal (xS, "123 \u00B5s")

})

test_that ("format quantity: merge prefixes", {
    x <- as.quantity (123, "prefix$kilo length$kilo.metre")
    expect_equal (format(x, unitAs='symbol'), "123 Mm")
    expect_equal (format(x, unitAs='text'  ), "123 megametres")
    x <- as.quantity (123, "prefix$kilo length$kilo.metre^2")
    expect_equal (format(x, unitAs='symbol'), "123 k km²")
    expect_equal (format(x, unitAs='text'  ), "123 kilo kilometres²")
    x <- as.quantity (123, "prefix$kilo length$metre^2")
    expect_equal (format(x, unitAs='symbol'), "123 k m²")
    expect_equal (format(x, unitAs='text'  ), "123 kilo metres²")
    # with rounding
    x <- as.quantity (123, "prefix$hecto length$metre")
    expect_equal (format(x, unitAs='text'  ), "123 hectometres")
    x <- as.quantity (123, "prefix$hecto length$deca.metre")
    expect_equal (format(x, unitAs='text'  ), "123 kilometres")
    x <- as.quantity (123, "prefix$hecto length$hecto.metre")
    expect_equal (format(x, unitAs='text'  ), "1.23 megametres")
    x <- as.quantity (123, "prefix$deca length$deca.metre")
    expect_equal (format(x, unitAs='text'  ), "12.3 kilometres")
    x <- as.quantity (1.23, "prefix$deca length$deca.metre")
    expect_equal (format(x, unitAs='text'  ), "123 metres")
})
