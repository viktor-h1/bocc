# Worked examples from "Applied Statistical Methods", chapter 4 (bivariate descriptive analysis).

gen_trust <- matrix(c(37, 35, 19, 8, 4,  36, 38, 31, 12, 4,  31, 22, 62, 36, 20,  21, 19, 72, 68, 32,  11, 14, 68, 98, 82),
                    nrow = 5, byrow = TRUE,
                    dimnames = list(Generation = c("Silent", "BabyBoomer", "GenX", "Millennial", "GenZ"),
                                    Trust = c("VUnlikely", "Unlikely", "Neutral", "Likely", "VLikely")))
trust_order <- c("VUnlikely", "Unlikely", "Neutral", "Likely", "VLikely")

test_that("T&E 4.1: joint, marginal and conditional distributions; conditional modes and medians", {
  r <- desc_crosstab(gen_trust, order_y = trust_order)
  expect_equal(round(rowSums(r$joint), 3), c(Silent = 0.117, BabyBoomer = 0.138, GenX = 0.194, Millennial = 0.241, GenZ = 0.310))
  expect_equal(round(colSums(r$joint), 3), c(VUnlikely = 0.155, Unlikely = 0.145, Neutral = 0.286, Likely = 0.252, VLikely = 0.161))
  expect_equal(unname(round(r$row_cond["Silent", ], 2)), c(0.36, 0.34, 0.18, 0.08, 0.04))   # book: 0.36 0.34 0.19 0.08 0.04 (rounding)
  expect_equal(unname(round(r$col_cond[, "VLikely"], 2)), c(0.03, 0.03, 0.14, 0.23, 0.58))
  expect_equal(r$cond_summary$median, c("Unlikely", "Unlikely", "Neutral", "Neutral", "Likely"))
  expect_equal(r$cond_summary$mode, c("VUnlikely", "Unlikely", "Neutral", "Neutral", "Likely"))
  # (0.022 + 0.082 + 0.077) / 0.241 = 0.751: moderate trust among Millennials
  expect_equal(round(sum(r$row_cond["Millennial", 2:4]), 3), 0.75)
})

test_that("chi-square and Cramer's V as descriptive measures of association", {
  r <- desc_crosstab(gen_trust)
  expect_equal(r$chisq, unname(chisq.test(gen_trust, correct = FALSE)$statistic))
  expect_equal(r$cramer_v, sqrt(r$chisq / (880 * 4)))
  indep <- outer(c(10, 20), c(3, 6, 9))                       # p_kj = R_k C_j exactly
  expect_equal(desc_crosstab(indep)$cramer_v, 0)
  perfect <- diag(c(10, 20, 30)); dimnames(perfect) <- list(c("a", "b", "c"), c("x", "y", "z"))
  expect_equal(desc_crosstab(perfect)$cramer_v, 1)
})

test_that("Example 4.1: conditional summaries of Satisf | Reason", {
  m <- matrix(c(64, 99, 242, 124, 86,  46, 109, 224, 467, 455,  7, 23, 119, 204, 402,  26, 23, 50, 323, 542),
              nrow = 4, byrow = TRUE,
              dimnames = list(Reason = c("Activ/Transf", "Admin", "Landline", "Mobile"), Satisf = c("VLow", "Low", "Med", "High", "VHigh")))
  r <- desc_crosstab(m, order_y = c("VLow", "Low", "Med", "High", "VHigh"))
  s <- r$cond_summary
  expect_equal(s$mode, c("Med", "High", "VHigh", "VHigh"))
  # book: 0.3935 0.3590 0.5310 0.5622; the printed Landline row sums to 755 (not 757), so 402/755 = 53.25%
  expect_equal(s$`mode%`, c("39.35%", "35.9%", "53.25%", "56.22%"))
  expect_equal(s$Q1, c("Low", "Med", "High", "High"))
  expect_equal(s$median, c("Med", "High", "VHigh", "VHigh"))
  expect_equal(s$Q3, c("High", "VHigh", "VHigh", "VHigh"))
})

test_that("raw data: crosstab from two vectors, ordering, classification into intervals", {
  set.seed(4)
  x <- sample(c("a", "b"), 60, TRUE); y <- sample(c("Low", "Med", "High"), 60, TRUE)
  r <- desc_crosstab(x, y, order_y = c("Low", "Med", "High"))
  expect_equal(colnames(r$counts), c("Low", "Med", "High"))
  expect_equal(sum(r$counts), 60)
  sc <- c(-5, 3, 12, 25, 38, 44, 51, 67, 72, 95)
  pl <- rep(c("A", "B"), 5)
  r2 <- desc_crosstab(sc, pl, breaks_x = c(-10, 0, 50, 100))
  expect_equal(rownames(r2$counts), c("[-10, 0)", "[0, 50)", "[50, 100]"))
  expect_equal(unname(rowSums(r2$counts)), c(1, 5, 4))
})

test_that("T&E 4.2 / Example 4.2: conditional summaries of a numerical variable", {
  set.seed(5)
  y <- c(rnorm(30, 20, 5), rnorm(40, 30, 8)); g <- rep(c("Facebook", "Instagram"), c(30, 40))
  y[3] <- NA
  r <- desc_compare(y, g, probs = c(0.05, 0.95))
  fb <- r$table[r$table$group == "Facebook", ]
  expect_equal(fb$n, 29); expect_equal(fb$n.a, 1)
  expect_equal(fb$median, median(y[1:30], na.rm = TRUE))
  expect_equal(fb$p95, unname(quantile(y[1:30], 0.95, na.rm = TRUE)))
  expect_equal(fb$sd, sd(y[1:30], na.rm = TRUE))
  g2 <- rep(c("M", "F"), 35)
  r2 <- desc_compare(y, g, group2 = g2)
  expect_true(all(c("Facebook / M", "Instagram / F") %in% r2$table$group))
})

test_that("4.4: covariance, correlation and regression line match R; joint-table covariance", {
  set.seed(6)
  x <- rnorm(80, 20, 5); y <- 67.5 + 15.4 * x + rnorm(80, 0, 60)
  r <- desc_cor(x, y)
  expect_equal(r$covariance, cov(x, y)); expect_equal(r$correlation, cor(x, y))
  expect_equal(c(r$intercept, r$slope), unname(coef(lm(y ~ x))))
  expect_equal(r$slope, cor(x, y) * sd(y) / sd(x))
  pop <- desc_cor(x, y, population = TRUE)
  expect_equal(pop$covariance, cov(x, y) * 79 / 80); expect_equal(pop$correlation, cor(x, y))
  d <- data.frame(a = x, b = y, c = rnorm(80))
  mm <- desc_cor(d)
  expect_equal(mm$correlation, cor(d)); expect_equal(mm$covariance, cov(d))
  jt <- matrix(c(10, 5, 0, 5, 20, 5, 0, 5, 10), 3, dimnames = list(X = c("0", "1", "2"), Y = c("1", "2", "3")))
  xx <- rep(rep(0:2, 3), as.vector(jt)); yy <- rep(rep(1:3, each = 3), as.vector(jt))
  j <- desc_cor(jt)
  expect_equal(j$covariance, cov(xx, yy)); expect_equal(j$correlation, cor(xx, yy))
})

test_that("menu: crosstab with ordinal column, two-variable correlation", {
  df4 <- data.frame(g = rep(c("Old", "Young"), each = 20), t = rep(c("Low", "High", "Med", "High"), 10),
                    u = 1:40, v = (1:40) * 2 + rep(c(-1, 1), 20))
  in_global(list(df4 = df4), {
    s <- scripted(c("1", "7", "1", "1", "2", "2 3 1", "1", "", "q"), sc())
    expect_true(any(grepl("desc_crosstab\\(x = df4\\$g, y = df4\\$t, order_y = c\\(\"Low\", \"Med\", \"High\"\\)\\)", s$out)))
    s2 <- scripted(c("1", "8", "1", "3", "4", "n", "q"), sc())
    expect_true(any(grepl("desc_cor\\(x = df4\\$u, y = df4\\$v\\)", s2$out)))
    expect_true(any(grepl("Correlation r_XY", s2$out)))
  })
})
