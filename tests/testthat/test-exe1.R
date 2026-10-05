# Module 1 exercises (graphs and central tendency), official solutions. The course data are not
# shipped: the variables are rebuilt from the frequencies reported in the solutions.

test_that("Ex 1.1: binary and nominal variables (SmokingArea, District), mode only", {
  sm <- q(desc_freq(factor(rep(c("No", "Yes"), c(51, 49)))))
  expect_equal(sm$type, "binary")
  expect_true(any(grepl("approximately the same weight", sm$wording)))
  expect_true(any(grepl("almost the same frequency, so the mode is poorly representative", sm$wording)))
  di <- q(desc_freq(factor(rep(c("Lodi", "Milano", "Pavia"), c(35, 33, 32)))))
  expect_equal(di$type, "nominal")
  expect_equal(di$mode, "Lodi")
  expect_null(di$median)                                    # no median for a nominal variable
  expect_true(any(grepl("the mode is the only measure of central tendency", di$wording)))
  expect_true(any(grepl("alphabetical order, so their position on the axis carries no information", di$wording)))
})

test_that("Ex 1.1 e/g: exact shares from raw data with the mean(condition) line", {
  emp <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 13), c(2, 3, 12, 28, 23, 14, 10, 2, 4, 1, 1))
  f <- q(desc_freq(emp, at_most = 4))
  expect_equal(f$values$at_most, 0.45)
  expect_equal(f$type, "discrete")
  expect_true(any(grepl("spike plot", f$wording)))
  s <- q(desc_summary(emp, below = 5))
  expect_equal(s$shares$below, 0.45)
  expect_true(any(grepl("in R: mean(emp < 5)", s$lines, fixed = TRUE)))
  sales <- c(8000, 14000, 15000, 22000, 29999, 30000, 41000)
  b <- q(desc_summary(sales, between = c(15000, 30000)))
  expect_equal(b$shares$between, 3 / 7)                     # 15000 <= X < 30000
  expect_true(any(grepl("mean(sales >= 15000 & sales < 30000)", b$lines, fixed = TRUE)))
})

test_that("Ex 1.1 i: Brescia classes, approximate mean 25350 and median 21904.76", {
  r <- q(desc_classes(prop = c(21, 63, 16), breaks = c(0, 15000, 30000, 90000)))
  expect_equal(r$mean, 25350)
  expect_equal(round(r$median, 2), 21904.76)
  expect_true(any(grepl("right-skewed", r$wording)))
  expect_true(any(grepl("0.000042", r$lines, fixed = TRUE)))   # densities with significant digits
})

test_that("Ex 1.2: ordinal variable stored as text (Age, History)", {
  age <- rep(c("Young", "Middle", "Senior"), c(216, 390, 144))
  a <- q(desc_freq(age))
  expect_true(any(grepl("looks ordinal (Young < Middle < Senior)", a$notes, fixed = TRUE)))
  expect_true(any(grepl("qualitative ordinal variable (Young < Middle < Senior), but it is stored as text", a$wording, fixed = TRUE)))
  o <- q(desc_freq(age, order = c("Young", "Middle", "Senior"), at_most = "Middle"))
  expect_equal(o$median, "Middle")
  expect_equal(round(o$values$at_most, 3), 0.808)          # solution: 81%
  af <- q(desc_freq(factor(age, levels = c("Young", "Middle", "Senior"))))
  expect_equal(af$type, "ordinal")                          # factor with levels in order
  h <- q(desc_freq(rep(c("High", "Low", "Medium", "None"), c(230, 180, 150, 190))))
  expect_true(any(grepl("None < Low < Medium < High", h$notes, fixed = TRUE)))
  ch <- q(desc_freq(rep(0:3, c(360, 184, 111, 95))))
  expect_equal(ch$median, "1")
})

test_that("Ex 1.3: Country (combined share, mode), variable types of the data frame", {
  ct <- q(desc_freq(rep(c("France", "Germany", "United Kingdom", "United States"), c(6603, 9896, 8119, 10248)),
                    event = c("France", "Germany", "United Kingdom")))
  expect_equal(round(ct$event_share, 3), 0.706)
  expect_true(any(grepl("Germany (28.4%), has almost the same frequency", ct$wording, fixed = TRUE)))
  set.seed(1); n <- 200
  ch <- data.frame(index = 0:(n - 1), Date = sprintf("%02d/%02d/16", sample(1:12, n, TRUE), sample(1:28, n, TRUE)),
                   Year = sample(c(2015, 2016), n, TRUE), Month = sample(month.name, n, TRUE),
                   Quantity = sample(1:4, n, TRUE), Revenue = round(rexp(n, 1 / 900), 2) + 0.5,
                   Country = sample(c("France", "Germany"), n, TRUE), Age = sample(17:87, n, TRUE), stringsAsFactors = FALSE)
  v <- q(desc_vars(ch))
  expect_equal(v$table$type, c("identifier", "qualitative ordinal", "qualitative ordinal", "qualitative ordinal",
                               "quantitative discrete", "quantitative continuous", "qualitative nominal", "quantitative discrete"))
  expect_true(any(grepl("So there are 7 statistical variables: 4 qualitative", v$wording, fixed = TRUE)))
  expect_true(any(grepl("Quantity, Age are discrete and the others continuous", v$wording, fixed = TRUE)))
  expect_equal(v$rbase, "str(ch)")
})

test_that("Ex 1.4: typed table with a zero-count value; comparison with another period", {
  qn <- c("1" = 5401, "2" = 7340, "3" = 8238, "4" = 2561, "5" = 0, "6" = 700)
  r <- q(desc_freq(qn, compare = rep(1:4, c(6347, 7118, 14070, 7331))))
  expect_equal(r$mode, "3"); expect_equal(r$median, "2")
  expect_equal(round(r$mean, 4), 2.4439)                    # solution: 2.447 with rounded proportions
  expect_true(any(grepl("5, with frequency 0, keeps its empty position", r$wording, fixed = TRUE)))
  expect_true(any(grepl("compare them through relative frequencies (percentages), not counts", r$wording, fixed = TRUE)))
  expect_true(any(grepl("Mode: 3 vs 3; median: 2 vs 3; mean: 2.44 vs 2.64", r$wording, fixed = TRUE)))
  expect_equal(nrow(r$comparison), 6)
})

test_that("Ex 1.5: time in classes (shares, modal class, two peaks, subgroups)", {
  r <- q(desc_classes(freq = c(122, 420, 294, 176, 571, 217), breaks = c(0, 10, 20, 30, 60, 90, 150),
                      at_most = c(5, 30), between = c(15, 50), split = 30))
  expect_equal(round(100 * r$values$at_most, 2), c(3.39, 46.44))
  expect_equal(round(100 * r$values$between, 2), 34.52)
  expect_equal(r$modal_class, c(10, 20))
  expect_equal(round(r$mean, 2), 50.58)
  expect_equal(round(r$median, 2), 40.91)                   # solution 41.01 with the density rounded to 0.00327
  expect_equal(round(unname(r$split$below[c("median", "mean")]), 2), c(17.05, 17.06))
  expect_equal(round(unname(r$split$above[c("median", "mean")]), 2), c(76.08, 79.65))   # solution 76.06 (rounded)
  expect_true(any(grepl("[60, 90) has the highest frequency (31.7%)", r$wording, fixed = TRUE)))
  expect_true(any(grepl("2 peaks ([10, 20) and [60, 90))", r$wording, fixed = TRUE)))
  expect_true(any(grepl("fall in [30, 60), a class with low density", r$wording, fixed = TRUE)))
  expect_true(any(grepl("approximately 61 units", r$lines, fixed = TRUE)))
})

test_that("Ex 1.6: densities read off a histogram", {
  b <- c(0, 20, 40, 60, 80, 120, 160, 300)
  r <- q(desc_classes(density = c(0.022, 0.01, 0.005, 0.003, 0.002, 0.001, 0.0006), breaks = b, at_most = 40))
  expect_equal(r$median, 26)
  expect_equal(round(r$mean, 2), 52.52)
  expect_equal(r$values$at_most, 0.64)                      # so Freq(X > 40) = 0.36, not 50%
  expect_true(any(grepl("sum to 1.004", r$notes, fixed = TRUE)))
  r2 <- q(desc_classes(density = c(0.022, 0.01, 0.005, 0.003, 0.002, 0.001, NA), breaks = b))
  expect_equal(r2$table$p_k[7], 0.08)
  expect_error(desc_classes(density = c(NA, NA, 0.1), breaks = c(0, 1, 2, 3)), "At most one density")
})

test_that("menu item for the data-frame overview", {
  in_global(list(dd = data.frame(id = 1:20, g = rep(c("a", "b"), 10), v = rnorm(20))), {
    s <- scripted(c("1", "q"), sc(1, 9))
    expect_true(any(grepl("desc_vars(data = dd)", s$out, fixed = TRUE)))
    expect_true(grepl("1 qualitative (g) and 1 quantitative (v)", gsub("\\s+", " ", paste(s$out, collapse = " ")), fixed = TRUE))
  })
})
