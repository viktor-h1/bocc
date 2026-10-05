# Worked examples from "Applied Statistical Methods" (Piccarreta, Tonini, Trentini), chapter 2.

test_that("Theory & Examples 2.1: frequency tables of qualitative variables", {
  BestFor <- c("P", "R", "R", "S", "P", "B", "B", "P", "P", "S", "S", "R", "R", "R", "P")
  Rate <- c("H", "M", "VH", "VH", "VH", "M", "VH", "H", "H", "VH", "VH", "VH", "H", "M", "VH")
  b <- desc_freq(BestFor)
  expect_equal(b$table$Count, c(2, 5, 5, 3))
  expect_equal(round(b$table$Prop, 4), c(0.1333, 0.3333, 0.3333, 0.2))
  r <- desc_freq(Rate, order = c("M", "H", "VH"), at_least = "H")
  expect_equal(r$table$Count, c(3, 4, 8))
  expect_equal(r$values$at_least, 0.8)          # "proportion of clients with satisfaction higher than M is 0.8"
})

test_that("Example 2.5: ordinal levels must be ordered before cumulating", {
  Satisf <- rep(c("VLow", "Low", "QLow", "Med", "QHigh", "High", "VHigh"), c(72, 218, 422, 556, 692, 757, 397))
  alpha <- desc_freq(Satisf)
  expect_equal(alpha$table[[1]], c("High", "Low", "Med", "QHigh", "QLow", "VHigh", "VLow"))   # the book's "wrong" order
  expect_true(any(grepl("order = c", alpha$notes)))
  ok <- desc_freq(Satisf, order = c("VLow", "Low", "QLow", "Med", "QHigh", "High", "VHigh"))
  expect_equal(round(ok$table$Cum.Prop, 2), c(0.02, 0.09, 0.23, 0.41, 0.63, 0.87, 1))
  expect_equal(ok$table$Cum.Count, c(72, 290, 712, 1268, 1960, 2717, 3114))
})

test_that("Examples 2.2 / 2.6: NMonths cumulative frequencies, F(x) for any x", {
  NMonths <- rep(2:12, c(7, 32, 71, 117, 147, 206, 248, 256, 314, 398, 1318))
  f <- desc_freq(NMonths, at_least = 10)
  expect_equal(f$table[[1]], as.character(2:12))                  # numeric order, not "10" < "2"
  expect_equal(round(f$values$at_least, 3), 0.652)                # Freq(NMonths >= 10) = 0.652
  expect_equal(round(desc_freq(NMonths, at_most = 3)$values$at_most, 3), 0.013)    # F(3)
  expect_equal(desc_freq(NMonths, at_most = 3.5)$values$at_most, desc_freq(NMonths, at_most = 3)$values$at_most)
  expect_equal(round(desc_freq(NMonths, between = c(10, 12))$values$between, 3), 0.652)
})

test_that("Theory & Examples 2.2 / 2.3: grouping 20 ticket times into intervals", {
  tickets <- c(25, 35, 13, 21, 24, 37, 26, 46, 58, 30, 32, 13, 12, 38, 41, 43, 44, 27, 53, 27)
  eq <- desc_classes(tickets, breaks = 5)
  expect_equal(eq$table$w_k, rep(9.2, 5))                         # w = (58 - 12) / 5
  expect_equal(eq$table$class[1], "[12, 21.2)")
  expect_equal(eq$table$class[5], "[48.8, 58]")                   # last class closed
  expect_equal(sum(eq$table$f_k), 20)
  r <- desc_classes(tickets, breaks = c(10, 20, 30, 40, 50, 60))
  expect_equal(r$table$f_k, c(3, 6, 5, 4, 2))
  expect_equal(r$table$p_k, c(0.15, 0.30, 0.25, 0.20, 0.10))
  expect_equal(r$table$c_k, c(0.015, 0.030, 0.025, 0.020, 0.010))
  expect_equal(r$table$class[5], "[50, 60]")
})

test_that("Examples 2.4 / 2.7: MonthExp measured in classes", {
  set.seed(7)
  MonthExp <- sample(rep(c("[0,50)", "[50,100)", "[100,150)", "[150,200)", "[200,300)", "[300,400)", "[400,600)", "[600,800]"),
                         c(484, 858, 571, 394, 408, 225, 142, 32)))
  r <- desc_classes(MonthExp, at_most = 250, between = c(100, 250))
  expect_equal(r$table$f_k, c(484, 858, 571, 394, 408, 225, 142, 32))        # sorted by class, not alphabetically
  expect_equal(round(r$table$c_k, 5), c(0.00311, 0.00551, 0.00367, 0.00253, 0.00131, 0.00072, 0.00023, 0.00005))
  expect_equal(round(r$table$F_k, 3), c(0.155, 0.431, 0.614, 0.741, 0.872, 0.944, 0.990, 1))
  expect_equal(r$values$at_most, (484 + 858 + 571 + 394 + 408 / 2) / 3114)   # Freq(X <= 250) ~ 0.806
  expect_equal(round(r$values$between, 4), 0.3754)                          # p3 + p4 + p5/2 ~ 0.3755 in the book
  expect_match(statcram:::.kind_tag(MonthExp), "measured in classes")
  expect_equal(statcram:::.cats(MonthExp)[1:2], c("[0,50)", "[50,100)"))
})
