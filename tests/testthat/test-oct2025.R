# First partial, 14 Oct 2025 (Bidding data, not shipped): the answer framework of the official
# solutions, checked on synthetic data with the same features, plus the exact numbers of Q3.

test_that("shape rule: symmetric centre with low outliers; strongly skewed by many outliers", {
  a <- c(qnorm(ppoints(200), 40, 5), rep(c(15, 16, 17), 3))
  q <- quantile(a, c(.25, .5, .75), names = FALSE)
  sa <- statcram:::.shape_info(a, q[1], q[2], q[3])
  expect_equal(sa$phrase, "the central part is fairly symmetric, although there are some outliers, mainly on the lower side")
  b <- c(qnorm(ppoints(100), 55, 5), seq(10, 25, length.out = 15))
  q <- quantile(b, c(.25, .5, .75), names = FALSE)
  sb <- statcram:::.shape_info(b, q[1], q[2], q[3])
  expect_match(sb$phrase, "^strongly left-skewed because of the numerous lower outliers")
})

test_that("Q1a/1b/1c framework: compare groups, centre measures, extreme value", {
  set.seed(5)
  agg <- qnorm(ppoints(140), 56, 6.5)
  age <- c(qnorm(ppoints(285), 41, 5.5), seq(18, 25, length.out = 15))
  air <- c(qnorm(ppoints(190), 57, 6), seq(14, 30, length.out = 30))
  d <- data.frame(bid = c(agg, age, air), ch = rep(c("Aggregator", "Agency", "Airline"), c(140, 300, 220)))
  r <- q(desc_compare(d$bid, d$ch, value = 35))
  w <- r$wording
  expect_equal(unname(w[names(w) == "frame"]), "We compare the conditional distributions of bid | ch in 3 groups (Agency: n = 300, Aggregator: n = 140, Airline: n = 220).")
  expect_true(any(grepl("differ in location, variability and shape", w)))
  expect_true(any(grepl("Agency has the lowest measures of location", w)))
  expect_true(any(grepl("its central 50% is more concentrated", w)))
  expect_true(any(grepl("is the maximum value reached by the lowest 50% of its values", w)))
  expect_true(any(grepl("Airline: strongly left-skewed because of the numerous lower outliers", w, fixed = TRUE)))
  expect_true(any(grepl("report both the means and the medians", w)))
  expect_true(any(grepl("are aligned and do not reflect the long tail of Airline", w)))
  expect_true(any(grepl("Within Aggregator an extremely low value is below Q1 - 1.5 IQR", w, fixed = TRUE)))
  expect_true(all(c("frame", "tool", "result", "meaning", "conclusion", "caveat") %in% names(w)))
})

test_that("Q2 framework: scatterplot first, direction, non-linearity, r not reliable", {
  x <- seq(10, 92, length.out = 300)
  y <- 80 - 0.5 * x - 0.02 * (x - 60)^2 + rep(c(-3, 0, 3), 100)
  r <- q(desc_cor(x, y))
  w <- r$wording
  expect_match(w[["tool"]], "^The fundamental tool is the scatterplot")
  expect_true(any(grepl("linked by an inverse relationship: as x increases, y tends to decrease", w, fixed = TRUE)))
  expect_true(any(grepl("compromise that describes the centre of the data but not the tails", w, fixed = TRUE)))
  expect_true(any(grepl("is not particularly reliable", w, fixed = TRUE)))
  expect_true(any(grepl("the relationship is NOT linear", r$lines, fixed = TRUE)))
  lin <- q(desc_cor(x, 3 + 2 * x + rep(c(-3, 0, 3), 100)))
  expect_true(any(grepl("is a reliable measure of the strength", lin$wording, fixed = TRUE)))
})

test_that("Q3 framework: SD from classes with the mean of the squares; CV with mixed inputs", {
  r <- q(desc_classes(freq = c(150, 300, 200, 200, 150), breaks = c(0, 2, 5, 6, 7, 10)))
  expect_true(any(grepl("Mean of the squares sum(m_k^2 p_k) = 1^2 x 0.15 + 3.5^2 x 0.3 + 5.5^2 x 0.2 + 6.5^2 x 0.2 + 8.5^2 x 0.15 = 29.1625;  mean^2 = 4.875^2 = 23.765625",
                        r$lines, fixed = TRUE)))
  expect_equal(round(sqrt(r$variance), 4), 2.3243)
  set.seed(2); bid <- rnorm(668, 47.48, 11.82)
  cv <- q(desc_cv(Bid = bid, mean = 4.875, sd = 2.3243, names = "classes"))
  expect_equal(round(cv$table$CV[2], 4), 0.4768)
  expect_true(any(grepl("Relative to its mean, classes is the most dispersed", cv$wording, fixed = TRUE)))
  cv2 <- q(desc_cv(Bid = bid, mean = 5.4, var = 6.7))
  expect_equal(round(cv2$table$CV[2], 5), 0.47934)
  expect_true(any(grepl("sqrt(6.7) / |5.4|", cv2$lines, fixed = TRUE)))
})

test_that("Q4/Q5 framework: percentile phrasing, conditional share printed once", {
  s <- q(desc_summary(1:100, probs = c(0.1, 0.9)))
  expect_true(any(grepl("P10 is the largest value among the 10% of units with the lowest values", s$wording, fixed = TRUE)))
  ch <- rep(c("Aggregator", "Agency"), c(142, 302)); lt <- c(rep(c("MediumTerm", "Early"), c(72, 70)), rep(c("MediumTerm", "Late"), c(76, 226)))
  x <- q(desc_crosstab(ch, lt, y_event = "MediumTerm"))
  expect_true(any(grepl("Freq(lt = MediumTerm | ch = Aggregator) = 0.507   (72 of 142)", x$lines, fixed = TRUE)))
  expect_true(any(grepl("It would be wrong to answer based on joint counts", x$wording, fixed = TRUE)))
})

test_that("Q6 framework: estimates and SEs by group, the more reliable estimator", {
  set.seed(3)
  f <- c(rnorm(142, 56, 12), rnorm(224, 53.5, 22)); g <- rep(c("Aggregator", "Airline"), c(142, 224))
  e <- q(est_mean(f, group = g))
  expect_equal(unname(e$se), c(sd(f[1:142]) / sqrt(142), sd(f[143:366]) / sqrt(224)))
  expect_true(any(grepl("cannot be determined, as the population variances are unknown", e$wording, fixed = TRUE)))
  expect_true(any(grepl("The estimator for g = Aggregator has the smallest standard error", e$wording, fixed = TRUE)))
  expect_true(any(grepl("No conclusion can be drawn on the reliability of the specific realised estimates", e$wording, fixed = TRUE)))
})

test_that("labels follow the seven moves and can be switched off; phrasing notes", {
  out <- capture.output(print(desc_cor(1:20, (1:20)^1.1)))
  expect_true(any(grepl("^Frame: ", out))); expect_true(any(grepl("^Conclusion: ", out)))
  old <- options(statcram.labels = FALSE)
  out2 <- capture.output(print(desc_cor(1:20, (1:20)^1.1)))
  options(old)
  expect_false(any(grepl("^Frame: ", out2)))
  w <- statcram:::.order_wording(c(caveat = "c", frame = "f", "u", tool = "t"))
  expect_equal(w$text, c("f", "t", "u", "c"))
  expect_equal(w$label, c("Frame", "Tool", "Meaning", "Caveat"))
  txt <- utils::capture.output(nt <- sc_notes("phrasing"))
  expect_true(any(grepl("Seven moves, always in this order", nt, fixed = TRUE)))
  for (fn in list(quote(rv_iid(12, sqrt(380), 80, above = 15)), quote(ci_mean(xbar = 10, s = 2, n = 50)),
                  quote(n_prop(width = 0.09, conf = 0.99)), quote(desc_prop(count = 52, n = 300)), quote(prob_normal(0, 1, above = 1.96))))
    expect_true(all(c("frame", "tool", "result") %in% names(q(eval(fn))$wording)), info = deparse(fn))
  expect_true(any(grepl("in about 8.4% of all possible samples of size 80, Xbar > 15", q(rv_iid(12, sqrt(380), 80, above = 15))$wording, fixed = TRUE)))
})
