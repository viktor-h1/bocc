# Questions from the 2024/25 and 2025/26 past papers (official solutions), reproduced from the numbers
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
  expect_true(any(grepl("X_i = 1 if the i-th unit", e$wording)))
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
  expect_true(any(grepl("Shape - Agency:", g$wording, fixed = TRUE)))
  expect_true("p5" %in% names(g$table) && "p95" %in% names(g$table))
  x <- q(desc_crosstab(rep(c("A", "B"), c(30, 70)), rep(c("y", "n", "y", "n"), c(20, 10, 30, 40))))
  expect_true(any(grepl("conditional distributions of", x$wording, fixed = TRUE)))
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

# ---- 2024/25 papers ---------------------------------------------------------------------------

test_that("First partial Oct 2024: CLT probability with its R call, classes, two proportions' SEs", {
  r <- q(rv_iid(12, sqrt(380), 80, above = 15))
  expect_equal(round(r$values[[1]], 4), 0.0843)              # solution rounds to 0.08
  expect_true(any(grepl("in R: 1 - pnorm(15, mean = 12, sd = sqrt(380/80))", r$lines, fixed = TRUE)))
  cl <- q(desc_classes(prop = c(0.20, 0.42, 0.16, 0.20, 0.02), breaks = c(0, 1, 5, 10, 50, 200), n = 550))
  expect_equal(cl$mean, 11.06)
  ex2 <- sum(c(0.5, 3, 7.5, 30, 125)^2 * c(0.20, 0.42, 0.16, 0.20, 0.02))
  expect_equal(round(ex2, 2), 505.33)
  expect_equal(cl$variance, 550 / 549 * (ex2 - 11.06^2))
  a <- q(desc_prop(count = 176, n = 550)); b <- q(desc_prop(count = 95, n = 550))
  expect_equal(round(c(a$se, b$se), 3), c(0.020, 0.016))
  expect_true(any(grepl("nothing can be concluded about how close this specific estimate", a$wording, fixed = TRUE)))
})

test_that("Second partial Jan 8 2025: sleep test cut-off, beta, effect of 10 years less", {
  t <- q(test_mean(xbar = 7.05, sigma = 0.5, n = 374, mu0 = 7, alt = ">", alpha = 0.01))
  expect_equal(round(t$cutoff, 2), 7.06)
  p <- q(power_mean(7, 7.1, 0.5, 374, alpha = 0.01, alt = ">"))
  near(p$beta, 0.0609, 0.001)                                # 0.0609 with the cut-off rounded to 7.06
  set.seed(8); d <- data.frame(age = runif(120, 20, 70)); d$sleep <- 8 - 0.02 * d$age + rnorm(120, 0, 0.6)
  m <- q(reg_fit(sleep ~ age, data = d))
  e <- q(reg_effect(m, "age", change = -10))
  expect_true(grepl("a decrease of 10 units in age", e$wording[1], fixed = TRUE))
  expect_false(grepl("increase of -10", e$wording[1], fixed = TRUE))
  expect_true(any(grepl("(a decrease of 10 units)", e$lines, fixed = TRUE)))
  expect_equal(e$effect_ci, sort(-10 * as.numeric(confint(m$model, "age"))))
})

test_that("General Jan 8 2025: power of a proportion test; paired test from summaries and mixed input", {
  t <- q(test_prop(count = 150, n = 400, p0 = 0.35, alt = ">", alpha = 0.01))
  expect_equal(round(t$cutoff, 4), 0.4055)
  pw <- q(power_prop(0.35, 0.38, 400, alpha = 0.01, alt = ">"))
  expect_equal(round(pw$power, 3), 0.147)
  pd <- q(test_paired(mean1 = 414, mean2 = 402.89, s1 = 48, s2 = 45.61, r = 0.71, n = 161, alt = ">"))
  expect_equal(round(pd$statistic, 3), 3.947)
  near(pd$p_value, 5.9e-05, 1e-06)
  expect_true(any(grepl("in R: 1 - pt(3.9472, df = 160)", pd$lines, fixed = TRUE)))
  expect_true(any(grepl("GENERIC estimate", pd$wording, fixed = TRUE)))
  # "before" as raw data in the data frame, "after" mean, s and r from the text
  set.seed(4); before <- as.numeric(scale(rnorm(161)) * 48 + 414)
  mx <- q(test_paired(before, mean2 = 402.89, s2 = 45.61, r = 0.71, alt = ">"))
  expect_equal(mx$statistic, pd$statistic)
  expect_true(any(grepl("First measurement (before) from the data", mx$notes, fixed = TRUE)))
  ci <- q(ci_paired(before, mean2 = 402.89, s2 = 45.61, r = 0.71))
  expect_true(any(grepl("how much a specific unit changes", ci$wording, fixed = TRUE)))
})

test_that("Second partial Jan 28 2025: cut-off on the p-hat scale; P(reject) for a value in H0", {
  pw <- q(power_prop(0.2, 0.18, 500, alpha = 0.05, alt = ">"))
  expect_equal(round(pw$cutoff, 4), round(0.2 + qnorm(0.95) * sqrt(0.16 / 500), 4))
  expect_equal(round(pw$cutoff, 4), 0.2294)
  expect_true(pw$in_h0)
  expect_equal(round(pw$p_reject, 5), 0.00201)               # solution: 0.00202
  c2 <- q(chisq_indep(matrix(c(30, 20, 25, 25), 2)))
  expect_true(any(grepl("% of all possible samples", c2$wording, fixed = TRUE)))
})

test_that("General Jan 29 2025: salary test beta, GOF, two proportions with the old sample", {
  p <- q(power_mean(2500, 2400, 850, 87, alpha = 0.05, alt = "<"))
  expect_equal(round(p$cutoff, 1), 2350.1)
  expect_equal(round(p$beta, 3), 0.708)
  expect_true(any(grepl("in R: 1 - pnorm(2350.105204, mean = 2400, sd = 850/sqrt(87))", p$lines, fixed = TRUE)))
  g <- q(chisq_gof(c(262, 168, 70), p = c(0.5, 0.4, 0.1)))
  expect_equal(unname(g$expected), c(250, 200, 50))
  expect_equal(round(g$statistic, 3), 13.696)
  expect_equal(round(g$p_value, 5), 0.00106)
  t <- q(test_2props(count1 = 206, n1 = 500, phat2 = 0.5, n2 = 100, alt = "<"))
  expect_equal(round(t$pooled, 4), 0.4267)
  expect_equal(round(t$statistic, 3), -1.624)
  expect_equal(round(t$p_value, 4), 0.0522)
  expect_true(any(grepl("in R: pnorm(-1.6242)", t$lines, fixed = TRUE)))
  used <- rep(c("yes", "no"), c(206, 294))
  tm <- q(test_2props(used, event = "yes", phat2 = 0.5, n2 = 100, alt = "<"))
  expect_equal(tm$statistic, t$statistic)
  cm <- q(ci_2props(used, event = "yes", phat2 = 0.5, n2 = 100))
  expect_equal(length(cm$ci), 2)
})

test_that("General Jul 2025: CI with sigma known; P(P-hat >= 0.30) with its R call", {
  ci <- q(ci_mean(xbar = 275.3343, sigma = 500, n = 390, conf = 0.90))
  expect_equal(round(ci$ci, 2), c(233.69, 316.98), tolerance = 0.011)   # solution (233.68, 316.99)
  r <- q(rv_prop(0.3397, 1200, above = 0.30))
  expect_equal(round(r$values[[1]], 3), 0.998)
  expect_true(any(grepl("in R: 1 - pnorm(0.3, mean = 0.3397, sd = sqrt(0.3397*(1-0.3397)/1200))", r$lines, fixed = TRUE)))
  e <- q(est_mean(c(3, 5, 7, 9, 11)))
  expect_true(any(grepl("tends to 0 as n grows (consistency)", e$wording, fixed = TRUE)))
})

test_that("General Sep 2025: cut-off and beta for a proportion test", {
  pw <- q(power_prop(0.25, 0.3, 500, alpha = 0.01, alt = ">"))
  expect_equal(round(pw$cutoff, 3), 0.295)
  expect_equal(round(pw$beta, 3), 0.405)                     # solution: 0.404 with the cut-off rounded
})

test_that("probability engine: every probability and quantile prints its R call", {
  n1 <- q(prob_normal(mean = 10, sd = 2, between = c(8, 12), quantile = 0.9))
  expect_true(any(grepl("in R: pnorm(12, mean = 10, sd = 2) - pnorm(8, mean = 10, sd = 2)", n1$lines, fixed = TRUE)))
  expect_true(any(grepl("in R: qnorm(0.9, mean = 10, sd = 2)", n1$lines, fixed = TRUE)))
  t1 <- q(prob_t(df = 12, above = 2))
  expect_true(any(grepl("in R: 1 - pt(2, df = 12)", t1$lines, fixed = TRUE)))
  c1 <- q(prob_chisq(df = 3, below = 7.81))
  expect_true(any(grepl("in R: pchisq(7.81, df = 3)", c1$lines, fixed = TRUE)))
})

test_that("grouped conditional shares in a crosstab (Freq(high or very high | group))", {
  d <- data.frame(Content = rep(c("news", "offers", "video"), c(40, 50, 30)),
                  Shares = c(rep(c("low", "medium", "high", "veryhigh"), c(10, 14, 10, 6)),
                             rep(c("low", "medium", "high", "veryhigh"), c(5, 7, 19, 19)),
                             rep(c("low", "medium", "high", "veryhigh"), c(12, 9, 6, 3))))
  r <- q(desc_crosstab(d$Content, d$Shares, order_y = c("low", "medium", "high", "veryhigh"), y_event = c("high", "veryhigh")))
  rp <- prop.table(table(d$Content, d$Shares), 1)
  expect_equal(unname(r$event_share), unname(rp[, "high"] + rp[, "veryhigh"]))
  expect_true(any(grepl("Freq(Shares = high or veryhigh | Content = offers) = 0.38 + 0.38 = 0.76   (38 of 50)", r$lines, fixed = TRUE)))
  expect_true(any(grepl("Marginal (all units): Freq(Shares = high or veryhigh) = 63 / 120 = 0.525", r$lines, fixed = TRUE)))
  expect_true(any(grepl("equal to the marginal share 0.525", r$wording, fixed = TRUE)))
  expect_true(any(grepl("with Content = offers, the share with Shares = high or veryhigh is 76%", r$wording, fixed = TRUE)))
  expect_error(desc_crosstab(d$Content, d$Shares, y_event = "huge"), "y_event not found")
})

test_that("outlier shares, P95 vs the fence, top-15% reading, outlier % by group", {
  x <- c(1:18, 60, 80)                                       # Q1 5.75, Q3 15.25, upper fence 29.5
  s <- q(desc_summary(x, probs = c(0.05, 0.85, 0.95)))
  expect_true(any(grepl("Share of outliers: 0 low (0%) and 2 high (10%) out of n = 20", s$lines, fixed = TRUE)))
  expect_true(any(grepl("P95 is above the fence", s$wording, fixed = TRUE)))
  expect_true(any(grepl("the top 15% of the units have x above", s$wording, fixed = TRUE)))
  g <- q(desc_compare(c(x, 1:20), rep(c("A", "B"), each = 20)))
  expect_true(any(grepl("outliers", g$wording)))
})

test_that("SE meaning in tests and 'averages, not one unit' in difference intervals", {
  t <- q(test_mean(xbar = 10, s = 2, n = 50, mu0 = 9))
  expect_true(any(grepl("expected distance of a GENERIC estimate", t$wording, fixed = TRUE)))
  c2 <- q(ci_2means(xbar1 = 10, s1 = 2, n1 = 40, xbar2 = 9, s2 = 2.5, n2 = 45, case = "pooled"))
  expect_true(any(grepl("specific unit", c2$wording, fixed = TRUE)))
})

test_that("regression: baseline intercept, baseline order, low R2, strong assumptions, dummies in reg_compare", {
  set.seed(3); n <- 150
  d <- data.frame(Branch = sample(c("B", "A", "C"), n, TRUE), AgeC = sample(c("young", "adult"), n, TRUE))
  d$Age <- 35 + 12 * (d$Branch == "C") + rnorm(n, 0, 6)
  d$y <- 5 + 0.8 * d$Age + rnorm(n, 0, 6)
  m <- q(reg_fit(y ~ Branch + AgeC, data = d))
  expect_true(any(grepl("estimated mean of y for units with Branch = A and AgeC = adult", m$wording, fixed = TRUE)))
  expect_true(any(grepl("The baseline of each factor is its FIRST level (Branch = A, AgeC = adult)", m$wording, fixed = TRUE)))
  lo <- q(reg_fit(y ~ AgeC, data = d))
  expect_true(lo$r_squared < 0.3)
  expect_true(any(grepl("not advisable for predicting individual values", lo$wording, fixed = TRUE)))
  m1 <- q(reg_fit(y ~ Branch, data = d)); m2 <- q(reg_fit(y ~ Branch + Age, data = d))
  cmp <- q(reg_compare(m1, m2))
  expect_true(any(grepl("The groups Branch = C and Branch = A (baseline) differ in the added variable(s)", cmp$wording, fixed = TRUE)))
  ck <- q(reg_check(m2))
  expect_true(any(grepl("Cor(eps_i, eps_j) = 0", ck$lines, fixed = TRUE)))
  expect_true(any(grepl("iid N(0, sigma^2)", ck$wording, fixed = TRUE)))
})

test_that("menu: crosstab asks for a group of Y categories", {
  dc <- data.frame(g = rep(c("Old", "Young"), each = 20), t = rep(c("Low", "High", "Med", "High"), 10))
  in_global(list(dc = dc), {
    s <- scripted(c("1", "dc$g", "dc$t", "", "1", "Med High", "q"), sc(1, 7))
    expect_length(s$left, 0)
    expect_true(any(grepl("y_event = c(\"Med\", \"High\")", s$out, fixed = TRUE)))
    expect_true(any(grepl("Freq(t = Med or High | g = Old)", s$out, fixed = TRUE)))
  })
})
