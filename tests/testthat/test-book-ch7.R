# Worked examples from "Applied Statistical Methods", chapter 7 (hypothesis testing).
# The book rounds intermediate values (p-hats, cut-offs), so those checks use a small tolerance.

near <- function(object, expected, tol = 0.001) expect_lte(max(abs(unname(object) - expected)), tol)

test_that("T&E 7.1: right-tailed Z test, sigma known; beta(21)", {
  r <- q(test_mean(xbar = 17.9, sigma = sqrt(170), n = 50, mu0 = 15, alt = ">", alpha = 0.01))
  expect_equal(round(r$critical, 3), 2.326)
  expect_equal(round(r$cutoff, 2), 19.29)
  expect_equal(round(r$p_value, 8), 0.05788884)
  expect_equal(r$decision, "fail to reject H0")
  expect_true(any(grepl("H0: mu <= 15  (or mu = 15)", r$lines, fixed = TRUE)))
  expect_true(any(grepl("boundary value mu = 15", r$lines, fixed = TRUE)))
  expect_equal(rownames(r$table), "Normal")
  b <- q(power_mean(mu0 = 15, mu1 = 21, sigma = sqrt(170), n = 50, alpha = 0.01, alt = ">"))
  near(b$beta, 0.1768652)
  expect_false(b$in_h0)
})

test_that("T&E 7.2: left-tailed test, power at 6, true value inside H0", {
  r <- q(test_mean(xbar = 7.05, sigma = 3, n = 50, mu0 = 8, alt = "<", alpha = 0.025))
  expect_equal(round(r$cutoff, 6), 7.168458)
  expect_equal(round(r$p_value, 8), 0.01257238)
  expect_equal(r$decision, "reject H0")
  near(q(power_mean(8, 6, 3, 50, alpha = 0.025, alt = "<"))$power, 0.9970474, 1e-4)
  h <- q(power_mean(8, 10, 3, 50, alpha = 0.025, alt = "<"))     # mu = 10 satisfies H0: mu >= 8
  expect_true(h$in_h0)
  expect_true(is.na(h$beta))
  expect_equal(round(h$p_not_reject, 6), 1)
  expect_lt(h$p_reject, 0.025)
  expect_true(any(grepl("CORRECT decision", h$lines)))
})

test_that("T&E 7.3: two-sided test, cut-offs, p-value and beta", {
  r <- q(test_mean(xbar = 748.8, sigma = 4, n = 150, mu0 = 750, alt = "!=", alpha = 0.05))
  expect_equal(round(r$cutoff, 4), c(749.3599, 750.6401))
  expect_equal(round(r$p_value, 10), 0.0002385635)
  expect_true(any(grepl("^H0: mu = 750      H1: mu != 750$", r$lines)))
  near(q(power_mean(750, 752, 4, 150, 0.05, "!="))$beta, 1.537052e-05, 1e-6)
  # the book prints 0.11 and 0.33; the exact values are 0.135 and 0.666
  expect_equal(round(q(power_mean(750, 751, 4, 150, 0.05, "!="))$beta, 3), 0.135)
  expect_equal(round(q(power_mean(750, 750.5, 4, 150, 0.05, "!="))$beta, 3), 0.666)
})

test_that("T&E 7.4: sigma unknown, z and t rows", {
  r <- q(test_mean(xbar = 17.9, s = sqrt(190), n = 50, mu0 = 15, alt = ">", alpha = 0.01))
  near(r$statistic, 1.488)
  near(r$p_z, 0.0684)
  near(r$p_t, 0.07157932)
  expect_equal(round(r$critical, 6), 2.404892)                 # t(49), default method = "t"
  expect_equal(rownames(r$table), c("Normal.Approx", "Student-t"))
  expect_equal(r$table$`p-value`, c("0.0684", "0.0716"))
  expect_equal(q(test_mean(xbar = 17.9, s = sqrt(190), n = 50, mu0 = 15, alt = ">", method = "z"))$p_value, r$p_z)
})

test_that("Example 7.1: TEST.mean on a large sample", {
  r <- q(test_mean(xbar = 35.605, s = 7.817, n = 5976, mu0 = 34, alt = ">"))
  expect_equal(round(r$se, 3), 0.101)
  expect_equal(round(r$statistic, 3), 15.872)
  expect_equal(r$table$`p-value`, c("<0.0001", "<0.0001"))
})

test_that("T&E 7.5: one proportion, p-value and beta(0.35)", {
  r <- q(test_prop(count = 58, n = 150, p0 = 0.42, alt = "<"))
  near(r$statistic, -0.8263)
  near(r$p_value, 0.2043097)
  b <- q(power_prop(0.42, 0.35, 150, alpha = 0.05, alt = "<"))
  near(b$cutoff, 0.3537)
  near(b$beta, 0.4621545)
})

test_that("Examples 7.2 / 7.3: TEST.prop tables (s_X and se under H0)", {
  r <- q(test_prop(phat = 0.4075, n = 3114, p0 = 0.4, alt = ">"))
  expect_equal(round(r$table$s_X, 4), 0.4899)
  expect_equal(round(r$se, 4), 0.0088)
  near(r$statistic, 0.856, 0.005)
  near(r$p_value, 0.196, 0.002)
  near(q(test_prop(phat = 0.4075, n = 3114, p0 = 0.4, alt = "<"))$p_value, 0.804, 0.002)
  c3 <- q(test_prop(phat = 0.7264, n = 5007, p0 = 0.75, alt = ">"))
  expect_equal(round(c3$se, 4), 0.0061)
  near(c3$statistic, -3.8593, 0.005)
  expect_equal(c3$table$`p-value`, "0.9999")
  expect_true(any(grepl("H0: p <= 0.75  (or p = 0.75)", c3$lines, fixed = TRUE)))
})

test_that("T&E 7.6: paired test with a correlation, sigma_D known", {
  r <- q(test_paired(mean1 = 0.362, mean2 = 0.201, s1 = 0.338, s2 = 0.424, r = 0.598, n = 44, d0 = 0.05, alt = ">"))
  expect_equal(round(r$table$s_D[1]^2, 4), 0.1226)
  near(r$statistic, 2.1028)
  near(r$p_z, 0.0177)
  near(r$p_t, 0.0207)
  k <- q(test_paired(dbar = 0.161, sd_d = 0.35, n = 44, sigma_d = sqrt(0.18), d0 = 0.05, alt = ">"))
  near(k$statistic, 1.7355)
  near(k$p_value, 0.0413)
  expect_equal(rownames(k$table), "Normal")
})

test_that("T&E 7.6: drug vs placebo, the four tests and known variances", {
  book <- list(low = c(0.16, 0.35, 44, 0.1752, 1.1075, 0.1340, 0.1356, 0.9435, 0.1727),
               medium = c(0.21, 0.37, 49, 0.1800, 1.6813, 0.0464, 0.0481, 1.4423, 0.0746),
               high = c(0.26, 0.41, 44, 0.1985, 2.0809, 0.0187, 0.0202, 1.887, 0.0296))
  for (b in book) {
    r <- q(test_2means(xbar1 = b[1], s1 = b[2], n1 = b[3], xbar2 = 0.06, s2 = 0.48, n2 = 42, alt = ">", alpha = 0.025))
    t <- r$tests
    expect_true(any(grepl(sprintf("] / %s = %s", b[3] + 40, statcram:::.f(b[4])), r$lines, fixed = TRUE)))
    near(t$stat[1], b[5]); near(t$p[1:2], b[6:7])
    k <- q(test_2means(xbar1 = b[1], sigma1 = sqrt(0.18), n1 = b[3], xbar2 = 0.06, sigma2 = sqrt(0.3), n2 = 42, alt = ">", alpha = 0.025))
    near(k$statistic, b[8]); near(k$p_value, b[9])
  }
  w <- q(test_2means(xbar1 = 0.16, s1 = 0.35, n1 = 44, xbar2 = 0.06, s2 = 0.48, n2 = 42, alt = ">", case = "large"))
  near(w$statistic, 1.0996); near(w$p_value, 0.1358)
  expect_equal(w$p_value, w$tests$p[3])
  pw <- q(power_2means(d1 = 0.21, sigma1 = sqrt(0.18), sigma2 = sqrt(0.3), n1 = 44, n2 = 42, alpha = 0.025, alt = ">"))
  near(pw$cutoff, 0.2077)
  near(pw$beta, 0.4913435)
})

test_that("Examples 7.4 / 7.5: TEST.diffmean on summary numbers", {
  t <- q(test_2means(xbar1 = 35.40, s1 = 7.66, n1 = 3053, xbar2 = 35.83, s2 = 7.97, n2 = 2923))$tests  # book diff -0.43
  near(t$se, rep(0.2, 4), 0.005)
  near(t$stat, rep(-2.13, 4), 0.006)
  near(t$p, c(0.033, 0.033, 0.0332, 0.0332), 0.001)
  push <- q(test_2means(xbar1 = 37.15, s1 = 7.81, n1 = 2836, xbar2 = 34.21, s2 = 7.56, n2 = 3140, alt = ">"))$tests
  near(push$stat, c(14.78, 14.78, 14.75, 14.75), 0.005)
  expect_equal(push$decision, rep("reject H0", 4))
  p <- q(test_paired(dbar = 9.44, sd_d = 12.69, n = 130, d0 = 7, alt = ">"))
  near(p$se, 1.11, 0.005); near(p$statistic, 2.19, 0.005)
  near(c(p$p_z, p$p_t), c(0.0143, 0.0152))
  d <- q(test_2means(xbar1 = 11.51, s1 = 11.2, n1 = 96, xbar2 = 3.59, s2 = 14.87, n2 = 34))$tests
  near(d$se, c(2.44, 2.44, 2.79, 2.79), 0.006)
  near(d$stat, c(3.24, 3.24, 2.83, 2.83), 0.005)
  near(d$p, c(0.0012, 0.0015, 0.0046, 0.0067), 0.0002)
})

test_that("T&E 7.7: two proportions, pooled p, two- vs one-sided", {
  r <- q(test_2props(count1 = 115, n1 = 526, count2 = 77, n2 = 389))
  expect_equal(round(r$pooled, 4), 0.2098)
  near(r$statistic, 0.7602)
  near(r$p_value, 0.44, 0.01)
  b <- q(test_2props(count1 = 63, n1 = 115, count2 = 32, n2 = 77))
  expect_equal(round(b$pooled, 4), 0.4948)
  near(b$statistic, 1.7957)
  expect_equal(b$decision, "fail to reject H0")
  expect_equal(q(test_2props(count1 = 63, n1 = 115, count2 = 32, n2 = 77, alt = ">"))$decision, "reject H0")
  expect_true("se_0" %in% names(b$table))
})

test_that("Examples 7.7 / 7.8: TEST.diffprop, pooled se_0 and pdiff0", {
  r <- q(test_2props(count1 = 138, n1 = 1150, count2 = 96, n2 = 1200, alt = ">"))
  expect_equal(round(c(r$table$s_X, r$table$s_Y), 4), c(0.325, 0.2713))
  expect_equal(round(r$se, 4), 0.0124)
  expect_equal(round(r$statistic, 4), 3.2372)
  expect_equal(r$table$`p-value`, "0.0006")
  a <- q(test_2props(phat1 = 0.889, n1 = 757, phat2 = 0.9035, n2 = 964))
  near(a$se, 0.0148); near(a$statistic, -0.9824); near(a$p_value, 0.3259)
  near(q(test_2props(phat1 = 0.8005, n1 = 757, phat2 = 0.8973, n2 = 964))$statistic, -5.6554, 0.002)
  d <- q(test_2props(phat1 = 0.8973, n1 = 964, phat2 = 0.8005, n2 = 757, d0 = 0.05, alt = ">"))
  expect_equal(round(d$se, 4), 0.0175)
  near(d$statistic, 2.6716, 0.002)
  near(d$p_value, 0.0038, 0.0001)
  expect_null(d$pooled)
  expect_true("se" %in% names(d$table))
  expect_true(any(grepl("no pooling", d$lines)))
  l <- q(test_2props(phat1 = 0.2219, n1 = 757, phat2 = 0.1909, n2 = 964))
  near(l$statistic, 1.5855, 0.005); near(l$p_value, 0.1129, 0.001)
})

test_that("T&E 7.8 / Example 7.9: chi-square goodness of fit", {
  r <- q(chisq_gof(c(151, 117, 140, 162), alpha = 0.01))
  expect_equal(round(r$critical, 5), 11.34487)
  expect_equal(round(r$statistic, 6), 7.782456)
  near(r$p_value, 0.05072729, 1e-5)
  expect_equal(r$rbase, "chisq.test(x = c(151, 117, 140, 162))")
  s <- q(chisq_gof(c(838, 1829, 1022, 1318), p = c(0.1, 0.4, 0.25, 0.25)))
  expect_equal(round(s$statistic, 2), 287.98)
  expect_equal(round(unname(s$residuals), 6), c(15.073966, -3.883569, -6.493767, 1.872523))
  expect_equal(round(unname(s$contributions), 6), c(227.224466, 15.082105, 42.169013, 3.506341))
  expect_equal(s$rbase, "chisq.test(x = c(838, 1829, 1022, 1318), p = c(0.1, 0.4, 0.25, 0.25))")
  small <- q(chisq_gof(c(0.17, 0.37, 0.20, 0.26) * 150, p = c(0.1, 0.4, 0.25, 0.25)))
  expect_equal(round(small$statistic, 4), 9.2475)
  near(small$p_value, 0.02617, 1e-5)
})

test_that("T&E 7.9: chi-square independence on a typed cross-table", {
  m <- matrix(c(15, 20, 6, 7, 30, 10, 6, 15, 20, 6, 10, 20), nrow = 4, ncol = 3, byrow = TRUE)
  r <- q(chisq_indep(m, alpha = 0.001))
  expect_equal(round(r$expected[1, 1], 3), 8.448)
  expect_equal(round(r$statistic, 3), 27.921)
  expect_equal(r$df, 6)
  expect_equal(round(r$critical, 5), 22.45774)
  expect_equal(signif(r$p_value, 4), 9.725e-05)
  expect_equal(round(sum(((m - r$expected)^2 / r$expected)[1, ]), 2), 9.68)   # book: 9.684 from rounded cells
  expect_equal(r$rbase, "chisq.test(x = m)")
  expect_equal(round(r$cramer_v, 4), round(sqrt(r$statistic / (165 * 2)), 4))
  expect_equal(r$p_value, suppressWarnings(chisq.test(m))$p.value)
})

test_that("Levene test as UBStats (deviations from the medians)", {
  set.seed(7)
  df <- data.frame(Time = c(rnorm(30, 35, 6), rnorm(25, 34, 10)), Area = rep(c("A", "B"), c(30, 25)))
  r <- q(test_levene(df$Time, group = df$Area))
  z <- abs(df$Time - ave(df$Time, df$Area, FUN = median))
  a <- anova(lm(z ~ df$Area))
  expect_equal(r$statistic, a$`F value`[1])
  expect_equal(r$p_value, a$`Pr(>F)`[1])
  expect_equal(r$df, c(1, 53))
  expect_equal(r$s2, c(var(df$Time[df$Area == "A"]), var(df$Time[df$Area == "B"])))
  expect_match(r$ubstats, "^TEST\\.diffmean\\(x = df\\$Time, by = df\\$Area, var\\.test = TRUE\\)")
  expect_error(test_levene(), "raw data")
  t2 <- q(test_2means(df$Time, group = df$Area))
  expect_equal(t2$levene$F, r$statistic)
  expect_true(any(grepl("Levene test for homogeneity of variance", t2$lines)))
  expect_null(q(test_2means(df$Time, group = df$Area, var_test = FALSE))$levene)
  expect_null(q(test_2means(xbar1 = 1, s1 = 1, n1 = 10, xbar2 = 2, s2 = 1, n2 = 10))$levene)
})

test_that("UBStats TEST.* calls", {
  set.seed(8)
  df <- data.frame(Time = rnorm(40, 35, 8), Area = rep(c("A", "B"), 20), Click = sample(c(TRUE, FALSE), 40, TRUE),
                   Banner = rep(c("Original", "Redesigned"), each = 20),
                   Outcome = sample(c("Operator", "Left.Queue", "Left.Aut_Resp"), 40, TRUE),
                   Post = rnorm(40, 57), Pre = rnorm(40, 48))
  expect_equal(q(test_mean(df$Time, mu0 = 34, alt = ">"))$ubstats, "TEST.mean(x = df$Time, mu0 = 34, alternative = \"greater\")")
  expect_equal(q(test_mean(df$Time, mu0 = 34, sigma = 8))$ubstats, "TEST.mean(x = df$Time, sigma = 8, mu0 = 34)")
  expect_equal(q(test_2means(df$Time, group = df$Area))$ubstats, "TEST.diffmean(x = df$Time, by = df$Area, var.test = TRUE)")
  expect_equal(q(test_2means(df$Time, group = df$Area, var_test = FALSE, d0 = 1, alt = "<"))$ubstats,
               "TEST.diffmean(x = df$Time, by = df$Area, mdiff0 = 1, alternative = \"less\")")
  expect_equal(q(test_paired(df$Post, df$Pre, d0 = 7, alt = ">"))$ubstats,
               "TEST.diffmean(x = df$Post, y = df$Pre, type = \"paired\", mdiff0 = 7, alternative = \"greater\")")
  expect_equal(q(test_prop(df$Outcome, event = "Operator", p0 = 0.75, alt = ">"))$ubstats,
               "TEST.prop(x = df$Outcome, success = \"Operator\", p0 = 0.75, alternative = \"greater\")")
  expect_equal(q(test_prop(df$Click, p0 = 0.4))$ubstats, "TEST.prop(x = df$Click, p0 = 0.4)")
  expect_equal(q(test_2props(df$Click, group = df$Banner, alt = "<"))$ubstats,
               "TEST.diffprop(x = df$Click, by = df$Banner, alternative = \"less\")")
  expect_equal(q(test_2props(df$Click, group = df$Banner, levels = c("Redesigned", "Original"), alt = ">"))$ubstats,
               "TEST.diffprop(x = df$Click[df$Banner == \"Redesigned\"], y = df$Click[df$Banner == \"Original\"], alternative = \"greater\")")
  expect_equal(q(test_2props(df$Outcome, group = df$Banner, event = "Operator", d0 = 0.05))$ubstats,
               "TEST.diffprop(x = df$Outcome, by = df$Banner, success.x = \"Operator\", pdiff0 = 0.05)")
  expect_equal(q(chisq_indep(df$Area, df$Click))$rbase, "chisq.test(x = df$Area, y = df$Click, correct = FALSE)")
  expect_equal(q(chisq_gof(df$Outcome, p = c(0.3, 0.3, 0.4)))$rbase, "chisq.test(x = table(df$Outcome), p = c(0.3, 0.3, 0.4))")
  expect_equal(q(test_mean(xbar = 1, s = 1, n = 10, mu0 = 0))$ubstats, statcram:::.ub_raw_note)
})

test_that("composite hypotheses, p-value reading and z/t disagreement", {
  r <- q(test_mean(xbar = 17.9, s = sqrt(190), n = 50, mu0 = 15, alt = ">", alpha = 0.07))
  expect_true(any(grepl("rejected at alpha = 0.1; not rejected at alpha = 0.05", r$lines, fixed = TRUE)))
  expect_true(any(grepl("different decisions", r$wording)))               # z rejects at 0.07, t does not
  l <- q(test_prop(count = 58, n = 150, p0 = 0.42, alt = "<"))
  expect_true(any(grepl("H0: p >= 0.42  (or p = 0.42)", l$lines, fixed = TRUE)))
  expect_true(grepl("H0: p >= 0.42 against H1: p < 0.42", l$wording))
})

test_that("menu: two-means tests with all four rows, two proportions with d0, Levene, power_2means", {
  s <- scripted(c("3", "0.16", "44", "0.06", "42", "5", "0.35", "0.48", "2", "0.025", "q"), sc(4, 4))
  expect_length(s$left, 0)
  expect_true(any(grepl("^test_2means\\(alt = \">\", alpha = 0.025, xbar1 = 0.16", s$out)))
  expect_equal(sum(grepl("^  (Normal.Approx|Student-t) ", s$out)), 4)
  s2 <- scripted(c("3", "964", "", "0.8973", "757", "", "0.8005", "0.05", "2", "0.05", "q"), sc(4, 5))
  expect_length(s2$left, 0)
  expect_true(any(grepl("d0 = 0.05)", s2$out, fixed = TRUE)))
  expect_true(any(grepl("p-value = 0.0038", s2$out, fixed = TRUE)))
  s3 <- scripted(c("", "0.21", "sqrt(0.18)", "sqrt(0.3)", "44", "42", "0.025", "2", "q"), sc(5, 6))
  expect_length(s3$left, 0)
  expect_true(any(grepl("^power_2means\\(d1 = 0.21, sigma1 = 0.4242.*n1 = 44, n2 = 42, alpha = 0.025, alt = \">\"\\)$", s3$out)))
  expect_true(any(grepl("beta = P(Xbar - Ybar <= 0.2077", s3$out, fixed = TRUE)))
  gd <- data.frame(t = c(1, 5, 2, 8, 3, 9, 4, 1), g = rep(c("a", "b"), 4))
  in_global(list(gd = gd), {
    s4 <- scripted(c("2", "gd$t[gd$g == \"a\"]", "gd$t[gd$g == \"b\"]", "0.05", "q"), sc(4, 6))
    expect_length(s4$left, 0)
    expect_true(any(grepl("Levene test (equal variances?)", s4$out, fixed = TRUE)))
  })
})
