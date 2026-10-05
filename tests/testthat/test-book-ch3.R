# Worked examples from "Applied Statistical Methods", chapter 3 (summarising a single variable).

tickets <- c(25, 35, 13, 21, 24, 37, 26, 46, 58, 30, 32, 13, 12, 38, 41, 43, 44, 27, 53, 27)
spend13 <- c(18.1, 31.0, 45.4, 57.95, 60.4, 62.3, 66.7, 72.8, 74.6, 82.85, 95.1, 111.5, 148.0)
pharmacy <- c(55.5, 60.4, 62.3, 66.7, 72.8, 74.6, 81.1, 90.5, 91.3)

test_that("T&E 3.1 / 3.3 / 3.4 / 3.14: mode, median, mean, variance, SD", {
  r <- desc_summary(tickets)
  expect_equal(r$mode, c("13", "27"))
  expect_equal(r$median, 31)
  expect_equal(r$mean, 32.25)
  expect_equal(round(r$variance, 4), 167.0395)
  expect_equal(round(r$sd, 4), 12.9244)
  ages <- c(35, 25, 54, 60, 44, 62, 27, 55, 61, 24, 33)
  a <- desc_summary(ages)
  expect_length(a$mode, 0)                      # no mode: all values equally frequent
  expect_equal(a$median, 44)
  expect_true(any(grepl("none: all values have the same frequency", a$lines)))
})

test_that("T&E 3.5: mean of discrete data from the frequency distribution", {
  items <- c(4, 3, 3, 1, 0, 1, 2, 3, 3, 4, 2, 5, 4, 3, 4, 5)
  f <- desc_freq(items)
  expect_equal(f$mean, 2.9375)
  expect_equal(f$mean, mean(items))
  expect_equal(f$variance, var(items))         # n/(n-1) correction
  expect_equal(f$median, "3")
})

test_that("T&E 3.3 (ordinal) and 3.8 (ordinal quartiles): cumulative rule", {
  sat12 <- rep(c("VLow", "Low", "Neutral", "High", "VHigh"), c(2, 2, 2, 3, 3))
  o <- desc_freq(sat12, order = c("VLow", "Low", "Neutral", "High", "VHigh"))
  expect_equal(o$median, "Neutral")            # F(Neutral) = 6/12 = 0.5: no averaging for ordinal data
  sat14 <- rep(c("VLow", "Low", "Neutral", "High", "VHigh"), c(1, 3, 6, 1, 3))
  o14 <- desc_freq(sat14, order = c("VLow", "Low", "Neutral", "High", "VHigh"))
  expect_equal(o14$mode, "Neutral")
  expect_equal(unname(o14$quartiles), c("Low", "High"))   # F: 0.07, 0.29, 0.71, 0.79 -> Q1 = Low, Q3 = High
})

test_that("T&E 3.6: central tendency for grouped data and classes (influencer marketing)", {
  macro <- desc_freq(c("6" = .10, "7" = .20, "8" = .33, "9" = .22, "10" = .15))
  expect_equal(macro$mean, 8.12); expect_equal(macro$median, "8"); expect_equal(macro$mode, "8")
  mega <- desc_freq(c("6" = .20, "7" = .35, "8" = .21, "9" = .19, "10" = .05))
  expect_equal(mega$mean, 7.54); expect_equal(mega$median, "7")
  v_mega <- desc_classes(breaks = c(0, 300, 500, 700, 800, 1000, Inf), prop = c(0, .05, .19, .28, .31, .17))
  expect_equal(round(v_mega$median, 3), 792.857)
  expect_true(is.na(v_mega$mean))                                    # open-ended last class
  expect_equal(v_mega$modal_class, c(700, 800))
  v_macro <- desc_classes(breaks = c(0, 300, 500, 700, 800, 1000, Inf), prop = c(.07, .43, .24, .12, .08, .06))
  expect_equal(v_macro$median, 500)
  expect_equal(v_macro$modal_class, c(300, 500))
  e_macro <- desc_classes(breaks = c(20, 30, 35, 40, 45, 50, 100), prop = c(0, 0, .09, .28, .38, .25))
  expect_equal(round(e_macro$median, 2), 46.71)
  expect_equal(e_macro$mean, 52.075)          # the book prints 52.75, but its own sum is 52.075
  e_mega <- desc_classes(breaks = c(20, 30, 35, 40, 45, 50, 100), prop = c(.26, .33, .20, .11, .06, .04))
  expect_equal(e_mega$mean, 35.25)
  expect_equal(round(e_mega$median, 2), 33.64) # the median class is [30, 35); the book's 38.64 uses 35 as lower limit
})

test_that("T&E 3.8 / 3.10 / 3.11: quartiles, five-number summary and enhanced boxplot", {
  r <- desc_summary(spend13)
  expect_equal(r$median, 66.7)
  expect_equal(unname(r$quartiles), c(57.95, 82.85))
  expect_equal(unname(r$hand_quartiles), c(57.95, 82.85))
  expect_equal(unname(r$fivenum), c(18.1, 57.95, 66.7, 82.85, 148))
  expect_equal(r$whiskers, c(31, 111.5))
  expect_equal(r$outliers, c(18.1, 148))
  expect_true(any(grepl("\\[20.6, 120.2\\]", r$lines)))
  expect_true(any(grepl("right-skewed", r$lines)))
})

test_that("T&E 3.9: quartiles of grouped data (household size; age of mothers)", {
  hh1960 <- desc_freq(c("1" = .131, "2" = .278, "3" = .189, "4" = .176, "5" = .115, "6" = .057, "7 or more" = .054))
  expect_equal(unname(hh1960$quartiles), c("2", "4")); expect_equal(hh1960$median, "3")
  expect_null(hh1960$mean)
  hh2020 <- desc_freq(c("1" = .282, "2" = .348, "3" = .151, "4" = .127, "5" = .058, "6" = .023, "7 or more" = .012))
  expect_equal(unname(hh2020$quartiles), c("1", "3")); expect_equal(hh2020$median, "2")
  br <- c(10, 15, 20, 25, 30, 35, 40, 45, 54)
  b00 <- desc_classes(breaks = br, prop = c(.0051, .2264, .2877, .2424, .1645, .0616, .0116, .0007))
  expect_equal(round(unname(b00$quartiles), 4), c(20.3215, 29.7607))
  expect_equal(round(b00$median, 3), 24.666)    # book: 24.667 (rounding)
  expect_equal(round(b00$mean, 2), 25.35)
  b20 <- desc_classes(breaks = br, prop = c(.0012, .0966, .2619, .2843, .2425, .094, .0178, .0017))
  expect_equal(round(unname(b20$quartiles), 4), c(22.9057, 32.1856))
  expect_equal(round(b20$median, 4), 27.4675)
  expect_equal(round(b20$mean, 2), 27.66)
})

test_that("T&E 3.12 / 3.13: range and IQR", {
  r <- desc_summary(pharmacy)
  expect_equal(r$range, 35.8)
  expect_equal(unname(r$quartiles), c(62.3, 81.1))
  expect_equal(r$iqr, 81.1 - 62.3)
})

test_that("T&E 3.15: variance of a variable measured in classes", {
  br <- c(0, 50, 100, 150, 200, 300, 400, 600, 800)
  counts <- desc_classes(breaks = br, freq = c(484, 858, 571, 394, 408, 225, 142, 32))
  expect_equal(round(counts$mean, 4), 157.6509)
  expect_equal(round(counts$variance, 2), 17014.00)
  rounded <- c(.155, .276, .183, .127, .131, .072, .046, .010)
  with_n <- desc_classes(breaks = br, prop = rounded, n = 3114)
  expect_equal(with_n$mean, 157.625)
  expect_equal(round(with_n$variance, 2), 16965.43)
  expect_equal(round(sqrt(with_n$variance), 4), 130.2514)
  no_n <- desc_classes(breaks = br, prop = rounded)
  expect_equal(round(no_n$variance, 2), 16959.98)
})

test_that("T&E 3.16 / 3.17: SD depends on the unit, CV does not", {
  sec <- desc_summary(tickets * 60)
  expect_equal(sec$mean, 1935)
  expect_equal(sec$variance, var(tickets) * 60^2)        # book: 167.0395 x 60^2 = 601342.2 (from the rounded s^2)
  expect_equal(sec$cv, desc_summary(tickets)$cv)
  sc <- c(2.2, 3.9, 3.1, 3.4, 2.3, 2.6, 2.2, 3.8, 1.6)
  cars <- c(19.9, 21.7, 20.7, 18.1, 27.2, 18.9, 17.7, 27.6, 22, 27.1, 26.2)
  cv <- desc_cv(scooters = sc, cars = cars)$table
  expect_equal(round(cv$mean, 2), c(2.79, 22.46)); expect_equal(round(cv$sd, 2), c(0.80, 3.86))
  assets <- desc_cv(mean = c(A = .362, B = .072, C = .075, D = .151), sd = c(.609, .151, .137, .272))$table
  expect_equal(round(assets$CV[1], 3), 1.682)
  expect_equal(assets$CV, c(.609 / .362, .151 / .072, .137 / .075, .272 / .151))
})
