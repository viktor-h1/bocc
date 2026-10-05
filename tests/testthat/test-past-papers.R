# Questions from the 2025/26 past papers (official solutions), reproduced from the numbers
# given in the solutions. Where the solution used a rounded printed table, statcram uses the
# exact values, so those checks use a tolerance or the exact value (commented).

near <- function(object, expected, tol = 0.001) expect_lte(max(abs(unname(object) - expected)), tol)

test_that("Midterm Oct 2025, Q3: SD and CV of bids grouped in classes", {
  r <- q(desc_classes(freq = c(150, 300, 200, 200, 150), breaks = c(0, 2, 5, 6, 7, 10)))
  expect_equal(r$mean, 4.875)
  expect_equal(round(r$variance, 4), 5.4023)                 # n/(n-1) x (29.1625 - 4.875^2)
  expect_equal(round(sqrt(r$variance), 4), 2.3243)
  expect_equal(round(sqrt(r$variance) / r$mean, 4), 0.4768)  # CV
  expect_true(any(grepl("without the n/(n-1) factor: 29.1625 - 4.875^2 = 5.3969, SD = 2.3231", r$lines, fixed = TRUE)))
})

test_that("Second partial Jan 2026: sample size for a 99% CI of width 0.09", {
  expect_equal(q(n_prop(width = 0.09, conf = 0.99))$n, 820)
})

test_that("General Jan 2026: income classes (modal class, upper percentile, count below 30)", {
  r <- q(desc_classes(freq = c(96, 85, 101, 87, 72), breaks = c(0, 20, 40, 70, 120, 500), at_most = 30, probs = c(0.9, 0.95)))
  expect_equal(round(r$table$c_k, 5), c(0.01088, 0.00964, 0.00763, 0.00395, 0.00043))
  expect_equal(r$modal_class, c(0, 20))
  # solution: 120 + (0.9 - 0.84) / 0.00043 = 259.5 with the rounded table; exact F_4 = 369/441
  expect_equal(unname(r$quantiles["p90"]), 120 + (0.9 - 369 / 441) / (72 / 441 / 380))
  near(r$values$at_most, 0.315, 0.001)                       # 0.22 + 0.19 / 2 in the solution
  expect_true(any(grepl("approximately 139 units", r$lines, fixed = TRUE)))
})

test_that("General Jan 2026: rejection region with sigma known and P(revise) when mu = 635", {
  r <- q(test_mean(xbar = 617.9683, sigma = 120, n = 441, mu0 = 630, alt = "<", alpha = 0.01))
  expect_equal(round(r$cutoff, 4), 616.7066)
  expect_equal(r$decision, "fail to reject H0")
  expect_true(any(grepl("in R: pnorm(-2.1055)", r$lines, fixed = TRUE)))
  p <- q(power_mean(630, 635, 120, 441, alpha = 0.01, alt = "<"))
  expect_true(p$in_h0)
  expect_equal(round(p$p_reject, 5), 0.00068)
  expect_true(any(grepl("in R: pnorm(616.706584, mean = 635, sd = 120/sqrt(441))", p$lines, fixed = TRUE)))
  g <- q(chisq_gof(c(200, 140, 101), p = c(0.4, 0.3, 0.3)))
  expect_equal(unname(g$expected), c(176.4, 132.3, 132.3))
  expect_true(any(grepl("in R: 1 - pchisq(", g$lines, fixed = TRUE)))
})

test_that("Second partial Jan 30 2026: promotion test at alpha = 0.10 and power at 875", {
  r <- q(test_mean(xbar = 890.89, sigma = 200, n = 950, mu0 = 850, alt = ">", alpha = 0.10))
  expect_equal(round(r$cutoff, 4), 858.3158)
  expect_equal(round(r$statistic, 2), 6.30)
  p <- q(power_mean(850, 875, 200, 950, alpha = 0.10, alt = ">"))
  expect_equal(round(p$power, 7), 0.9949328)
  expect_true(any(grepl("1 - pnorm(858.315805, mean = 875, sd = 200/sqrt(950))", p$lines, fixed = TRUE)))
})

test_that("General Jan 30 2026: in-store proportion, test, beta; classes; mixed two-sample test", {
  e <- q(desc_prop(count = 188, n = 950))
  expect_equal(round(e$estimate, 4), 0.1979)
  expect_equal(round(e$se, 5), 0.01293)
  expect_true(grepl("X_i = 1 if the i-th unit", e$wording))
  t <- q(test_prop(count = 188, n = 950, p0 = 0.23, alt = "<"))
  expect_equal(round(t$cutoff, 7), 0.2075418)
  expect_equal(t$decision, "reject H0")
  b <- q(power_prop(0.23, 0.20, 950, alpha = 0.05, alt = "<"))
  expect_equal(round(b$beta, 7), 0.2805745)
  expect_true(any(grepl("1 - pnorm(0.207542, mean = 0.2, sd = sqrt(0.2*(1-0.2)/950))", b$lines, fixed = TRUE)))
  inc <- q(desc_classes(prop = c(0.06, 0.12, 0.24, 0.28, 0.18, 0.12), breaks = c(10, 25, 30, 35, 40, 45, 60)))
  expect_equal(round(inc$table$c_k, 3), c(0.004, 0.024, 0.048, 0.056, 0.036, 0.008))
  expect_equal(inc$mean, 36.6)
  expect_equal(round(inc$median, 4), 36.4286)
  # this year raw data (n = 950, mean 890.89), last year summary numbers only
  set.seed(1); x <- as.numeric(scale(rnorm(950)) * 548.95 + 890.89)
  m <- q(test_2means(x, xbar2 = 820, s2 = 550, n2 = 500, case = "pooled", alt = ">"))
  expect_equal(round(m$estimate, 2), 70.89)
  near(m$se, 30.35, 0.005)
  near(m$statistic, 2.3359, 0.001)
  near(m$p_value, 0.0098, 0.0001)
  expect_equal(m$ubstats, statcram:::.ub_raw_note)
  m2 <- q(test_2means(xbar1 = mean(x), s1 = sd(x), n1 = 950, xbar2 = 820, s2 = 550, n2 = 500, case = "pooled", alt = ">"))
  expect_equal(m$statistic, m2$statistic)
  expect_equal(m$p_value, m2$p_value)
})

test_that("General Mar 2026: proportion CI, pooled two-means test", {
  r <- q(ci_prop(phat = 0.34, n = 360, conf = 0.90))
  expect_equal(round(r$ci, 2), c(0.30, 0.38))
  expect_gt(diff(q(ci_prop(phat = 0.34, n = 360, conf = 0.95))$ci), diff(r$ci))   # larger confidence -> larger ME
  t <- q(test_2means(xbar1 = 197.11, s1 = 63.92, n1 = 150, xbar2 = 187.66, s2 = 67.1, n2 = 106, case = "pooled",
                     alt = ">", alpha = 0.10))
  expect_equal(round(t$statistic, 2), 1.14)
  expect_equal(t$df, 254)
  near(t$p_value, 0.1267, 0.002)
  expect_equal(t$decision, "fail to reject H0")
})

test_that("General Sep 2026: one-sided mean test, median and top share from classes, CI", {
  r <- q(test_mean(xbar = 66.52, s = 1.222 * sqrt(300), n = 300, mu0 = 70, alt = "<", alpha = 0.01))
  expect_equal(round(r$statistic, 3), -2.848)
  expect_equal(r$decision, "reject H0")
  l <- q(desc_classes(prop = c(0.44, 0.36, 0.20), lower = c(0, 70, 80), upper = c(70, 80, 100), at_least = 85))
  expect_equal(round(l$median, 3), 71.667)
  expect_equal(l$values$at_least, 0.15)
  expect_equal(round(q(ci_prop(count = 52, n = 300, conf = 0.90))$ci, 2), c(0.14, 0.21))
})

test_that("exam wording: CI meaning, p-value meaning, R calls in tests", {
  ci <- q(ci_mean(xbar = 10, s = 2, n = 50, conf = 0.90))
  expect_true(any(grepl("With a level of confidence of 90%, we can conclude that", ci$wording, fixed = TRUE)))
  expect_true(any(grepl("NOT contained in", ci$wording, fixed = TRUE)))
  expect_true(any(grepl("alpha = 0.1 rejects H0: parameter = v", ci$wording, fixed = TRUE)))
  t <- q(test_paired(dbar = 12, sd_d = 40, n = 100, d0 = 10, alt = ">"))
  expect_true(any(grepl("in R: 1 - pt(0.5, df = 99)", t$lines, fixed = TRUE)))
  expect_true(any(grepl("Meaning of the p-value", t$wording)))
  expect_true(any(grepl("no probability can be attached to the decision", t$wording)))
  c2 <- q(chisq_indep(matrix(c(20, 30, 25, 25, 10, 40), nrow = 2)))
  expect_true(any(grepl("in R: 1 - pchisq(", c2$lines, fixed = TRUE)))
  expect_true(any(grepl("Meaning of the p-value: P(chi-square(2)", c2$wording, fixed = TRUE)))
  p2 <- q(test_2props(count1 = 45, n1 = 100, count2 = 30, n2 = 90, alt = ">"))
  expect_true(any(grepl("in R: 1 - pnorm(", p2$lines, fixed = TRUE)))
})

test_that("descriptive checks: extreme values, percentile reading, groups, conditional percentages", {
  s <- q(desc_summary(1:9, value = c(14, 5)))                 # Q1 = 3, Q3 = 7, fences -3 and 13
  expect_true(any(grepl("14: 14 > 13 -> extreme HIGH value", s$lines, fixed = TRUE)))
  expect_true(any(grepl("5: inside [-3, 13] -> not extreme", s$lines, fixed = TRUE)))
  expect_true(any(grepl("central (most typical) 90% lie between", s$wording, fixed = TRUE)))
  expect_true(any(grepl("central (most typical) 80% lie between", s$wording, fixed = TRUE)))
  set.seed(4)
  d <- data.frame(bid = c(rnorm(40, 40, 3), rnorm(40, 56, 6), rnorm(40, 55, 9)), ch = rep(c("Agency", "Aggregator", "Airline"), each = 40))
  g <- q(desc_compare(d$bid, d$ch, value = 35))
  expect_true(any(grepl("Q3 of Agency .* < Q1 of Aggregator", g$lines)))
  expect_true(any(grepl("bid = 35 within Aggregator: fences", g$lines, fixed = TRUE)))
  expect_true(any(grepl("Shape: Agency:", g$wording, fixed = TRUE)))
  expect_true("p5" %in% names(g$table) && "p95" %in% names(g$table))
  x <- q(desc_crosstab(rep(c("A", "B"), c(30, 70)), rep(c("y", "n", "y", "n"), c(20, 10, 30, 40))))
  expect_true(any(grepl("CONDITIONAL distributions", x$wording, fixed = TRUE)))
})

test_that("regression: I() equation, significance, F formula, dummy differences, effects, prediction, comparison", {
  set.seed(9); n <- 300
  d <- data.frame(Age = round(runif(n, 20, 70)), Empl = sample(c("Empl", "Stud", "Unemp"), n, TRUE))
  d$Income <- 10 + 0.8 * d$Age + rnorm(n, 0, 6)
  d$Risk <- 50 + 15 * (d$Empl == "Stud") + 14 * (d$Empl == "Unemp") - 0.3 * d$Income + rnorm(n, 0, 8)
  m1 <- q(reg_fit(Risk ~ Empl + Age, data = d))
  m2 <- q(reg_fit(Risk ~ Empl + Age + Income, data = d))
  expect_true(any(grepl("I(Empl = Stud)", m1$lines, fixed = TRUE)))
  expect_true(any(grepl("significant at any usual level", m1$wording)))
  expect_true(any(grepl("Stud vs Unemp:", m1$wording, fixed = TRUE)))
  sm <- summary(m1$model)
  fl <- grep("^F = \\(SSR / k\\)", m1$lines, value = TRUE)
  expect_match(fl, sprintf("= %s$", statcram:::.f(sm$fstatistic[1])))
  e <- q(reg_effect(m2, "Income", change = 10, conf = 0.90))
  expect_equal(e$effect_ci, 10 * as.numeric(confint(m2$model, "Income", level = 0.90)))
  expect_true(grepl("increase of 10 units in Income", e$wording[1]))
  expect_error(reg_effect(m2, "nope"), "term must be one of")
  p <- q(reg_predict(m2, Empl = "Empl", Age = 40, Income = 40, value = 70))
  expect_false(p$extrapolation)
  inside <- 70 >= p$predictions$pi_lower && 70 <= p$predictions$pi_upper
  expect_true(any(grepl(if (inside) "not unexpected" else "unexpected \\(anomalous\\)", p$wording)))
  ex <- q(reg_predict(m2, Empl = "Empl", Age = 40, Income = 500))
  expect_true(ex$extrapolation)
  expect_true(any(grepl("EXTRAPOLATION: Income = 500 is outside the observed range", ex$notes, fixed = TRUE)))
  cmp <- q(reg_compare(m1, m2))
  expect_true(any(grepl("Age is significant in model 1", cmp$wording, fixed = TRUE)))
  expect_true(any(grepl("F = t^2", cmp$wording, fixed = TRUE)))
  ck <- q(reg_check(m2))
  expect_true(any(grepl("plot(mod, which = 1)", ck$lines, fixed = TRUE)))
  expect_true(any(grepl("Var(eps_i) = sigma^2", ck$wording, fixed = TRUE)))
})

test_that("menu: CI for a coefficient change and judging a predicted value", {
  set.seed(3)
  dd <- data.frame(y = rnorm(40, 10), x = rnorm(40))
  in_global(list(dd = dd), {
    eval(quote(fitq <- reg_fit(y ~ x, data = dd)), .GlobalEnv)
    s <- scripted(c("1", "1", "10", "0.9", "q"), sc(7, 6))
    expect_length(s$left, 0)
    expect_true(any(grepl("reg_effect(model = fitq, term = \"x\", change = 10, conf = 0.9)", s$out, fixed = TRUE)))
  })
})

test_that("menu: value check in the summary and in the group comparison", {
  hb <- data.frame(t = c(120, 130, 135, 150, 160, 170, 175, 180, 210, 75), tier = rep(c("High", "Low"), each = 5))
  in_global(list(hb = hb), {
    s <- scripted(c("hb$t", "75", "q"), sc(1, 3))
    expect_length(s$left, 0)
    expect_true(any(grepl("desc_summary(x = hb$t, value = 75)", s$out, fixed = TRUE)))
    s2 <- scripted(c("hb$t", "hb$tier", "n", "", "q"), sc(1, 4))
    expect_length(s2$left, 0)
    expect_true(any(grepl("^desc_compare\\(x = hb\\$t, group = hb\\$tier\\)$", s2$out)))
  })
})
