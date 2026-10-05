# Every procedure is checked against base R (or against values from 0.7.2).

set.seed(1)
a <- rnorm(30, 10, 2)
b <- rnorm(25, 11, 3)

test_that("one mean: t test and CI match t.test, summary numbers match raw data", {
  for (alt in c("<", ">", "!=")) {
    base_alt <- c("<" = "less", ">" = "greater", "!=" = "two.sided")[[alt]]
    r <- test_mean(a, mu0 = 9, alt = alt)
    t0 <- t.test(a, mu = 9, alternative = base_alt)
    expect_equal(r$statistic, unname(t0$statistic))
    expect_equal(r$p_value, t0$p.value)
  }
  expect_equal(ci_mean(a, conf = 0.9)$ci, as.numeric(t.test(a, conf.level = 0.9)$conf.int))
  expect_equal(ci_mean(a, conf = 90)$ci, ci_mean(a, conf = 0.9)$ci)
  r1 <- test_mean(a, mu0 = 9); r2 <- test_mean(xbar = mean(a), s = sd(a), n = 30, mu0 = 9)
  expect_equal(r1$p_value, r2$p_value)
})

test_that("one mean: known sigma and large-sample Z", {
  r <- test_mean(xbar = 2400, sigma = 850, n = 100, mu0 = 2500, alt = "<")
  expect_equal(r$statistic, (2400 - 2500) / 85)
  expect_equal(r$p_value, pnorm((2400 - 2500) / 85))
  z <- test_mean(a, mu0 = 9, method = "z")
  expect_equal(z$p_value, 2 * (1 - pnorm(abs((mean(a) - 9) / (sd(a) / sqrt(30))))))
  expect_null(z$df)
})

test_that("two means: pooled and Welch match t.test; groups via group = work", {
  p <- test_2means(a, b, case = "pooled", alt = "<")
  expect_equal(p$p_value, t.test(a, b, var.equal = TRUE, alternative = "less")$p.value)
  w <- test_2means(a, b, case = "welch")
  t0 <- t.test(a, b)
  expect_equal(w$p_value, t0$p.value)
  expect_equal(w$df, unname(t0$parameter))
  expect_equal(ci_2means(a, b, case = "welch")$ci, as.numeric(t0$conf.int))
  df <- data.frame(y = c(a, b), g = rep(c("A", "B"), c(30, 25)))
  expect_equal(test_2means(df$y, group = df$g, case = "welch")$p_value, t0$p.value)
  s <- test_2means(xbar1 = mean(a), s1 = sd(a), n1 = 30, xbar2 = mean(b), s2 = sd(b), n2 = 25, case = "welch")
  expect_equal(s$p_value, t0$p.value)
  expect_error(test_2means(a, b), "pooled")
})

test_that("paired: raw, differences, summary numbers and covariance form agree with t.test", {
  x <- a[1:20]; y <- a[1:20] + rnorm(20, 0.5)
  t0 <- t.test(y - x, alternative = "greater")
  expect_equal(test_paired(y, x, alt = ">")$p_value, t0$p.value)
  expect_equal(test_paired(y - x, alt = ">")$p_value, t0$p.value)
  expect_equal(test_paired(dbar = mean(y - x), sd_d = sd(y - x), n = 20, alt = ">")$p_value, t0$p.value)
  expect_equal(test_paired(mean1 = mean(y), mean2 = mean(x), s1 = sd(y), s2 = sd(x), n = 20,
                           cov = cov(y, x), alt = ">")$p_value, t0$p.value)
  expect_equal(test_paired(mean1 = mean(y), mean2 = mean(x), s1 = sd(y), s2 = sd(x), n = 20,
                           r = cor(y, x), alt = ">")$p_value, t0$p.value)
  expect_equal(ci_paired(y, x)$ci, as.numeric(t.test(y - x)$conf.int))
})

test_that("proportions match prop.test (no continuity correction)", {
  r <- test_prop(count = 120, n = 400, p0 = 0.25, alt = ">")
  p0 <- prop.test(120, 400, 0.25, alternative = "greater", correct = FALSE)
  expect_equal(r$statistic^2, unname(p0$statistic))
  expect_equal(r$p_value, p0$p.value)
  expect_equal(test_prop(count = 120, n = 400, p0 = 25, alt = ">")$p_value, p0$p.value)
  r2 <- test_2props(count1 = 45, n1 = 100, count2 = 30, n2 = 90)
  p2 <- prop.test(c(45, 30), c(100, 90), correct = FALSE)
  expect_equal(r2$p_value, p2$p.value)
  expect_equal(ci_2props(count1 = 45, n1 = 100, count2 = 30, n2 = 90)$ci, as.numeric(p2$conf.int))
  ci <- ci_prop(count = 120, n = 400, conf = 0.95)
  expect_equal(ci$ci, 0.3 + c(-1, 1) * qnorm(0.975) * sqrt(0.3 * 0.7 / 400))
  d <- desc_prop(count = 120, n = 400)
  expect_equal(d$se, sqrt(0.3 * 0.7 / 400))
})

test_that("chi-square matches chisq.test and the 0.7.2 invariant", {
  expect_equal(chisq_gof(c(151, 117, 140, 162))$statistic, 7.78245614035088)
  m <- matrix(c(20, 30, 25, 25, 10, 40), nrow = 2)
  c0 <- chisq.test(m, correct = FALSE)
  r <- chisq_indep(m)
  expect_equal(r$statistic, unname(c0$statistic))
  expect_equal(r$p_value, c0$p.value)
  x1 <- sample(c("a", "b", "c"), 200, TRUE); x2 <- sample(c("u", "v"), 200, TRUE)
  expect_equal(chisq_indep(x1, x2)$statistic, unname(chisq.test(table(x1, x2), correct = FALSE)$statistic))
  g <- chisq_gof(c(A = 30, B = 50, C = 20), p = c(0.3, 0.5, 0.2))
  expect_equal(g$statistic, unname(chisq.test(c(30, 50, 20), p = c(0.3, 0.5, 0.2))$statistic))
  expect_equal(chisq_gof(c(A = 30, B = 50, C = 20), p = c(30, 50, 20))$statistic, g$statistic)
  expect_match(chisq_gof(c(30, 50, 20))$notes[1], "typed counts")
  raw <- rep(c(0, 1, 2), c(40, 30, 30))
  expect_equal(chisq_gof(raw)$statistic, unname(chisq.test(c(40, 30, 30))$statistic))
})

test_that("probability, random variables, power and sample size", {
  expect_equal(prob_normal(80, 12, above = 100)$values[[1]], 1 - pnorm(100, 80, 12))
  expect_equal(prob_normal(between = c(-1.96, 1.96))$values[[1]], pnorm(1.96) - pnorm(-1.96))
  expect_equal(prob_t(10, quantile = 0.975)$values[[1]], qt(0.975, 10))
  expect_equal(rv_linear(a = 1, mu_x = 10, sd_x = 1, b = 2, mu_y = 8, sd_y = 1, rho = 0.2)$variance, 5.8)
  expect_equal(rv_discrete(0:3, c(0.1, 0.3, 0.4, 0.2))$mean, 1.7)
  expect_equal(rv_iid(mu = 50, sigma = 10, n = 25, above = 53)$values[[1]], 1 - pnorm(53, 50, 2))
  expect_equal(n_mean(margin = 3, sigma = 12)$n, 62)
  expect_equal(n_prop(margin = 0.03)$n, ceiling(qnorm(0.975)^2 * 0.25 / 0.03^2))
  pw <- power_mean(mu0 = 2500, mu1 = 2400, sigma = 850, n = 100, alt = "<")
  expect_equal(pw$power, pnorm(2500 + qnorm(0.05) * 85, 2400, 85))
})

test_that("grouped (class-interval) data reproduce the 0.7.2 values", {
  g <- desc_classes(breaks = c(10, 25, 30, 35, 40, 45, 60), prop = c(6, 12, 24, 28, 18, 12))
  expect_equal(g$mean, 36.6)
  expect_equal(g$modal_class, c(35, 40))
  expect_equal(unname(g$quantiles["p50"]), 35 + (0.5 - 0.42) / (0.28 / 5))
  expect_equal(desc_classes(lower = c(0, 10), upper = c(10, 30), freq = c(5, 5), below = 20)$share_below, 0.75)
})

test_that("regression matches lm / predict", {
  fit <- reg_fit(mpg ~ wt + hp, data = mtcars)
  m <- lm(mpg ~ wt + hp, mtcars)
  expect_equal(fit$coefficients$estimate, unname(coef(m)))
  pr <- reg_predict(fit, wt = 3, hp = 120)
  expect_equal(pr$predictions$pi_lower, unname(predict(m, data.frame(wt = 3, hp = 120), interval = "prediction")[, "lwr"]))
  expect_equal(pr$predictions$ci_upper, unname(predict(m, data.frame(wt = 3, hp = 120), interval = "confidence")[, "upr"]))
  rt <- reg_test(fit, "wt", value = 0)
  expect_equal(rt$p_value, coef(summary(m))["wt", 4])
  cmp <- reg_compare(reg_fit(mpg ~ wt, data = mtcars), fit)
  expect_equal(cmp$f_p_value, anova(lm(mpg ~ wt, mtcars), m)$`Pr(>F)`[2])
  expect_s3_class(reg_check(fit), "sc_result")
})

test_that("results print hypotheses, steps, decision, wording and the re-run call", {
  out <- capture.output(print(test_mean(a, mu0 = 9, alt = ">")))
  expect_true(any(grepl("H0: mu = 9", out)))
  expect_true(any(grepl("SE = s / sqrt\\(n\\)", out)))
  expect_true(any(grepl("REJECT H0", out)))
  expect_true(any(grepl("Exam wording", out)))
  expect_true(any(grepl("^test_mean\\(x = a, mu0 = 9, alt = \">\"\\)$", out)))
  expect_output(expect_true(sc_selftest(verbose = FALSE)), "PASSED")
})
