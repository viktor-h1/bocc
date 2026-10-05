# Worked examples from "Applied Statistical Methods", chapter 6 (estimation and confidence intervals).
# Summary numbers in the book's examples are rounded, so those checks use tolerance 0.01-0.02.

near <- function(object, expected, tol = 0.01) expect_lte(max(abs(unname(object) - expected)), tol)

test_that("T&E 6.1: SE of the mean, n for a target SE, CIs with sigma known, n for a width", {
  e <- q(est_mean(xbar = 82, sigma = 20, n = 64, se_target = 1.5))
  expect_equal(e$se, 2.5)
  expect_equal(e$n_needed, 178)
  expect_equal(round(q(ci_mean(xbar = 82, sigma = 20, n = 64))$ci, 5), c(77.10009, 86.89991))
  expect_equal(round(q(ci_mean(xbar = 82, sigma = 20, n = 64, conf = 0.99))$ci, 5), c(75.56043, 88.43957))
  # width <= 4: n >= (1.96 x 20 / 2)^2 = 384.16 -> 385 (the book prints 368.64 / 369)
  expect_equal(q(n_mean(width = 4, sigma = 20))$n, 385)
  expect_equal(q(n_mean(margin = 2, sigma = 20))$n, 385)
  r <- q(ci_mean(xbar = 87, sigma = 20, n = 370))
  expect_equal(round(r$ci, 5), c(84.96213, 89.03787))
  expect_lte(diff(r$ci), 4.08)
  expect_null(r$ci_t)
  expect_equal(rownames(r$table), "Normal")
})

test_that("Example 6.1: estimated standard error s / sqrt(n)", {
  e <- q(est_mean(xbar = 500, s = 252.302, n = 3637))        # xbar is not needed for the SE
  expect_equal(round(e$se, 4), 4.1836)
  expect_true(grepl("expected deviation of a GENERIC estimate", paste(e$wording, collapse = " ")))
})

test_that("Example 6.2: 99% CIs for a mean, sigma known and unknown (z and t rows)", {
  near(q(ci_mean(xbar = 4602.34, sigma = 1650, n = 247, conf = 0.99))$ci, c(4331.92, 4872.77))
  r <- q(ci_mean(xbar = 4602.34, s = 1575.3, n = 247, conf = 0.99))
  near(r$ci_z, c(4344.16, 4860.53))
  near(r$ci_t, c(4342.14, 4862.55))
  expect_equal(r$ci, r$ci_t)                                 # default method = "t"
  expect_equal(q(ci_mean(xbar = 4602.34, s = 1575.3, n = 247, conf = 0.99, method = "z"))$ci, r$ci_z)
  expect_equal(rownames(r$table), c("Normal.Approx", "Student-t"))
})

test_that("T&E 6.2: CI from the sums of x and x^2", {
  r <- q(ci_mean(sum_x = 2755, sum_x2 = 585203, n = 15))
  expect_equal(round(r$estimate, 4), 183.6667)
  expect_equal(round(r$critical, 6), 2.144787)
  expect_equal(round(r$ci, 4), c(142.0142, 225.3191))
  expect_true(any(grepl("s^2 = [sum(x_i^2) - n xbar^2] / (n - 1)", r$lines, fixed = TRUE)))
  expect_true(any(grepl("5657.2381", r$lines, fixed = TRUE)))
  # same sums from raw data
  x <- c(rep(150, 7), rep(220, 8)); x[1] <- x[1] + 5
  expect_equal(q(ci_mean(sum_x = sum(x), sum_x2 = sum(x^2), n = 15))$ci, q(ci_mean(x))$ci)
  expect_error(ci_mean(sum_x = 10), "sample size n")
})

test_that("T&E 6.3 / Kerry exit poll: CIs for a proportion and the n for a width", {
  r <- q(ci_prop(count = 120, n = 2000, conf = 0.90))
  expect_equal(round(r$se, 5), 0.00531)
  expect_equal(round(r$ci, 4), c(0.0513, 0.0687))
  expect_equal(round(q(ci_prop(phat = 0.514, n = 2862))$ci, 6), c(0.495689, 0.532311))
  expect_equal(round(q(ci_prop(phat = 0.514, n = 2862, conf = 0.99))$ci, 7), c(0.4899352, 0.5380648))
  # width <= 0.01 -> margin 0.005: 1.96^2 x 0.25 / 0.005^2 = 38416 (38415 with the exact z); the book prints 9604
  expect_equal(q(n_prop(width = 0.01))$n, 38415)
  expect_equal(q(n_prop(margin = 0.01))$n, 9604)
  expect_equal(colnames(r$table), c("n", "phat", "s_X", "se", "Lower", "Upper"))
})

test_that("T&E 6.4: the four UBStats intervals for two means", {
  r <- q(ci_2means(xbar1 = 1120, s1 = 310, n1 = 85, xbar2 = 970, s2 = 280, n2 = 100))
  iv <- r$intervals
  expect_equal(nrow(iv), 4)
  expect_equal(iv$variances, c("equal", "equal", "different", "different"))
  expect_equal(iv$method, c("Normal.Approx", "Student-t", "Normal.Approx", "Student-t"))
  expect_equal(round(iv$se, 5), c(43.39565, 43.39565, 43.75601, 43.75601))
  expect_equal(round(iv$Lower, 5), c(64.94609, 64.37987, 64.23980, 63.62884))
  expect_equal(round(iv$Upper, 5), c(235.05391, 235.62013, 235.76020, 236.37116))
  expect_equal(round(iv$df[4], 4), 171.0868)
  expect_equal(iv$df[2], 183)
  expect_true(any(grepl("86524.5902", r$lines, fixed = TRUE)))
  expect_null(r$ci)
  # a chosen case gives that interval with its steps
  w <- q(ci_2means(xbar1 = 1120, s1 = 310, n1 = 85, xbar2 = 970, s2 = 280, n2 = 100, case = "welch"))
  expect_equal(w$ci, c(iv$Lower[4], iv$Upper[4]))
  expect_equal(q(ci_2means(xbar1 = 1120, s1 = 310, n1 = 85, xbar2 = 970, s2 = 280, n2 = 100, case = "pooled"))$ci,
               c(iv$Lower[2], iv$Upper[2]))
  expect_equal(q(ci_2means(xbar1 = 1120, s1 = 310, n1 = 85, xbar2 = 970, s2 = 280, n2 = 100, case = "large"))$ci,
               c(iv$Lower[3], iv$Upper[3]))
  # the test without a case shows the same four rows (chapter 7)
  tt <- q(test_2means(xbar1 = 1120, s1 = 310, n1 = 85, xbar2 = 970, s2 = 280, n2 = 100))$tests
  expect_equal(tt$se, iv$se)
  expect_equal(tt$stat, 150 / iv$se)
})

test_that("Example 6.4: Area A vs B and Push, 99% intervals", {
  a <- q(ci_2means(xbar1 = 35.39, s1 = 7.66, n1 = 3053, xbar2 = 35.83, s2 = 7.97, n2 = 2923, conf = 0.99))$intervals
  near(c(a$Lower[3], a$Upper[3]), c(-0.95, 0.09), 0.02)
  p <- q(ci_2means(xbar1 = 37.15, s1 = 7.81, n1 = 2836, xbar2 = 34.21, s2 = 7.56, n2 = 3140, conf = 0.99))
  near(c(p$intervals$Lower[3], p$intervals$Upper[3]), c(2.43, 3.45), 0.02)
  expect_true(any(grepl("All the intervals exclude 0", p$wording)))
})

test_that("Example 6.5: LocalPromo1, all eight intervals", {
  r <- q(ci_2means(xbar1 = 4148, s1 = 1166.48, n1 = 58, xbar2 = 4741.77, s2 = 1658.73, n2 = 189))$intervals
  near(r$se[c(1, 3)], c(233.89, 194.98))
  near(r$Lower, c(-1052.19, -1054.47, -975.93, -979.41), 0.02)
  near(r$Upper, c(-135.36, -133.08, -211.62, -208.13), 0.02)
  e <- q(ci_2means(xbar1 = 4110.85, s1 = 937.97, n1 = 20, xbar2 = 4668.72, s2 = 1925.55, n2 = 32))
  near(e$intervals$se[c(1, 3)], c(462.54, 399.82))
  near(e$intervals$Lower, c(-1464.43, -1486.9, -1341.5, -1361.86), 0.02)
  near(e$intervals$Upper, c(348.69, 371.17, 225.76, 246.12), 0.02)
  expect_true(any(grepl("All the intervals contain 0", e$wording)))
})

test_that("Example 6.6: paired 90% interval and Dept1 - Dept2", {
  p <- q(ci_paired(dbar = 9.44, sd_d = 12.69, n = 130, conf = 0.90))
  near(p$ci_z, c(7.61, 11.27))
  near(p$ci_t, c(7.59, 11.28))
  d <- q(ci_2means(xbar1 = 11.51, s1 = 11.2, n1 = 96, xbar2 = 3.59, s2 = 14.87, n2 = 34, conf = 0.90))$intervals
  near(d$se[c(1, 3)], c(2.44, 2.79))
  near(d$Lower, c(3.9, 3.87, 3.33, 3.23), 0.02)
  near(d$Upper, c(11.94, 11.97, 12.52, 12.61), 0.02)
})

test_that("T&E 6.5: paired interval from two means, SDs and the covariance; sigma_D known", {
  r <- q(ci_paired(mean1 = 155, mean2 = 122, s1 = 19.9, s2 = 24.5, cov = 23.4, n = 110, conf = 0.90))
  expect_equal(r$table$s_D[1]^2, 949.46)
  expect_equal(round(r$ci_t, 3), c(28.126, 37.874))
  near(r$ci_z, c(28.167, 37.833), 0.002)                   # the book uses z = 1.645
  k <- q(ci_paired(dbar = 33, sd_d = 30, n = 110, sigma_d = sqrt(949.46), conf = 0.90))
  expect_equal(k$ci, r$ci_z)
  expect_equal(rownames(k$table), "Normal")
  expect_true("sigma_D" %in% names(k$table))
})

test_that("T&E 6.6: two proportions and the n per group for a width", {
  r <- q(ci_2props(count1 = 145, n1 = 200, count2 = 163, n2 = 250))
  expect_equal(round(r$ci, 8), c(-0.01253304, 0.15853304))
  expect_equal(colnames(r$table), c("n_x", "n_y", "phat_x", "phat_y", "phat_x-phat_y", "s_X", "s_Y", "se", "Lower", "Upper"))
  expect_equal(q(n_2props(width = 0.08))$n, 1201)
  expect_equal(q(n_2props(margin = 0.04))$n, 1201)
  expect_lt(q(n_2props(margin = 0.04, p1 = 0.2, p2 = 0.3))$n, 1201)
  expect_error(n_2props(), "margin")
})

test_that("Examples 6.7 / 6.8: differences of proportions", {
  near(q(ci_2props(phat1 = 0.889, n1 = 757, phat2 = 0.9035, n2 = 964, conf = 0.99))$ci, c(-0.0528, 0.0238), 0.001)
  near(q(ci_2props(phat1 = 0.8005, n1 = 757, phat2 = 0.8973, n2 = 964, conf = 0.99))$ci, c(-0.1419, -0.0517), 0.001)
  r <- q(ci_2props(phat1 = 0.12, n1 = 1150, phat2 = 0.08, n2 = 1200, conf = 0.90))
  expect_equal(round(r$se, 4), 0.0124)
  near(r$ci, c(0.0196, 0.0604), 0.001)
})

test_that("sample size for a mean with sigma unknown uses t and warns it is approximate", {
  z <- q(n_mean(margin = 5, s = 20))
  t <- q(n_mean(margin = 5, s = 20, n = 30))
  expect_equal(z$n, ceiling((qnorm(0.975) * 20 / 5)^2))
  expect_equal(t$n, ceiling((qt(0.975, 29) * 20 / 5)^2))
  expect_true(grepl("approximate", t$notes))
  expect_error(n_mean(margin = 5), "sigma")
})

test_that("UBStats calls printed under each result", {
  set.seed(6)
  df <- data.frame(spend = rnorm(40, 50, 8), after = rnorm(40, 52, 8),
                   Banner = rep(c("Original", "Redesigned"), 20), Click = sample(c(TRUE, FALSE), 40, TRUE),
                   loy = sample(c("H", "M", "L"), 40, TRUE))
  expect_equal(q(ci_mean(df$spend))$ubstats, "CI.mean(x = df$spend, conf.level = 0.95)")
  expect_equal(q(ci_mean(df$spend, sigma = 8, conf = 0.9))$ubstats, "CI.mean(x = df$spend, sigma = 8, conf.level = 0.9)")
  expect_equal(q(ci_prop(df$loy, event = "H"))$ubstats, "CI.prop(x = df$loy, success = \"H\", conf.level = 0.95)")
  expect_equal(q(ci_prop(df$Click))$ubstats, "CI.prop(x = df$Click, conf.level = 0.95)")
  expect_equal(q(ci_prop(df$loy, event = c("H", "M")))$ubstats, "CI.prop(x = df$loy %in% c(\"H\", \"M\"), conf.level = 0.95)")
  expect_equal(q(ci_paired(df$after, df$spend))$ubstats,
               "CI.diffmean(x = df$after, y = df$spend, type = \"paired\", conf.level = 0.95)")
  expect_equal(q(ci_2means(df$spend, group = df$Banner))$ubstats, "CI.diffmean(x = df$spend, by = df$Banner, conf.level = 0.95, var.test = TRUE)")
  expect_equal(q(ci_2means(df$spend, group = df$Banner, levels = c("Redesigned", "Original"), case = "welch"))$ubstats,
               "CI.diffmean(x = df$spend[df$Banner == \"Redesigned\"], y = df$spend[df$Banner == \"Original\"], conf.level = 0.95, var.test = TRUE)")
  expect_equal(q(ci_2props(df$Click, group = df$Banner))$ubstats, "CI.diffprop(x = df$Click, by = df$Banner, conf.level = 0.95)")
  expect_equal(q(ci_2props(df$loy, group = df$Banner, event = "H", conf = 0.99))$ubstats,
               "CI.diffprop(x = df$loy, by = df$Banner, success.x = \"H\", conf.level = 0.99)")
  expect_match(q(est_mean(df$spend))$ubstats, "^CI\\.mean\\(x = df\\$spend\\)")
  expect_equal(q(ci_mean(xbar = 1, s = 1, n = 10))$ubstats, statcram:::.ub_raw_note)
  out <- capture.output(print(q(ci_mean(df$spend))))
  expect_true(any(grepl("UBStats (course package)", out, fixed = TRUE)))
  old <- options(statcram.ubstats = FALSE); on.exit(options(old))
  out2 <- capture.output(print(q(ci_mean(df$spend))))
  expect_false(any(grepl("UBStats (course package)", out2, fixed = TRUE)))
})

test_that("the four intervals agree with base R for raw data", {
  set.seed(61)
  a <- rnorm(25, 10, 2); b <- rnorm(30, 9, 3)
  iv <- q(ci_2means(a, b))$intervals
  expect_equal(c(iv$Lower[2], iv$Upper[2]), as.numeric(t.test(a, b, var.equal = TRUE)$conf.int))
  expect_equal(c(iv$Lower[4], iv$Upper[4]), as.numeric(t.test(a, b)$conf.int))
  expect_equal(iv$df[4], unname(t.test(a, b)$parameter))
  m <- q(ci_mean(a))
  expect_equal(m$ci_t, as.numeric(t.test(a)$conf.int))
})

test_that("menu: CI from the sums, all four two-mean intervals, est_mean, n_2props", {
  s <- scripted(c("s", "2", "2", "15", "2755", "585203", "0.95", "q"), sc(3, 1))
  expect_length(s$left, 0)
  expect_true(any(grepl("ci_mean(conf = 0.95, method = \"t\", n = 15, sum_x = 2755, sum_x2 = 585203)", s$out, fixed = TRUE)))
  expect_true(any(grepl("142.0142, 225.3191", s$out, fixed = TRUE)))
  s2 <- scripted(c("3", "1120", "85", "970", "100", "5", "310", "280", "0.95", "q"), sc(3, 4))
  expect_length(s2$left, 0)
  expect_true(any(grepl("Unknown variances assumed to be different", s2$out)))
  expect_equal(sum(grepl("^  (Normal.Approx|Student-t) ", s2$out)), 4)
  expect_true(any(grepl("^ci_2means\\(conf = 0.95, xbar1 = 1120", s2$out)))
  s3 <- scripted(c("1", "s", "1", "1", "64", "82", "20", "y", "1.5", "q"), sc(3, 6))
  expect_true(any(grepl("est_mean(xbar = 82, n = 64, sigma = 20, se_target = 1.5)", s3$out, fixed = TRUE)))
  expect_true(any(grepl("n = 178", s3$out, fixed = TRUE)))
  s4 <- scripted(c("2", "0.08", "", "", "0.95", "q"), sc(5, 5))
  expect_true(any(grepl("Required n = 1201 in EACH group", s4$out, fixed = TRUE)))
  s5 <- scripted(c("2", "4", "1", "20", "0.95", "q"), sc(5, 3))
  expect_length(s5$left, 0)
  expect_true(any(grepl("Required n = 385", s5$out, fixed = TRUE)))
})
