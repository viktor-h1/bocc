# Regression tests for the 0.7.2 problem: data frames / variables created
# during the exam were not recognised.

test_that("a data frame and a new column created after loading are listed and usable", {
  in_global(list(), {
    assign("raw", data.frame(a = c(5, 7, 6, 9, 8), b = c(4, 6, 6, 7, 7)), envir = .GlobalEnv)
    eval(quote(raw$d <- raw$a - raw$b), .GlobalEnv)
    lo <- statcram:::.live_objects()
    expect_true("raw$d" %in% lo$expr)
    r <- test_mean(get("raw", envir = .GlobalEnv)$d, mu0 = 0, alt = ">")
    expect_equal(r$p_value, t.test(c(1, 1, 0, 2, 1), mu = 0, alternative = "greater")$p.value)
    out <- capture.output(sc_data())
    expect_true(any(grepl("raw\\$d", out)))
  })
})

test_that("integer columns with few values are still usable as numbers", {
  visits <- c(1L, 2L, 2L, 3L, 5L, 4L, 1L, 3L)
  expect_equal(test_mean(visits, mu0 = 2)$p_value, t.test(visits, mu = 2)$p.value)
  in_global(list(dd = data.frame(visits = visits)), {
    lo <- statcram:::.live_objects()
    expect_equal(lo$kind[lo$expr == "dd$visits"], "numeric")
  })
})

test_that("0/1 dummies, logical expressions and factors all work for proportions", {
  vip <- c(1, 0, 1, 1, 0, 1, 0, 1, 1, 1)
  loyalty <- factor(c("High", "Low", "High", "High", "Low", "High", "Low", "High", "High", "High"))
  r1 <- ci_prop(vip); r2 <- ci_prop(loyalty == "High"); r3 <- ci_prop(loyalty, event = "High")
  expect_equal(r1$estimate, 0.7); expect_equal(r2$ci, r1$ci); expect_equal(r3$ci, r1$ci)
  expect_error(ci_prop(loyalty), "event = ")
  expect_error(ci_prop(loyalty, event = "Gold"), "not a category")
})

test_that("typed vectors, subsets and expressions are accepted; NAs are dropped with a note", {
  x <- c(12.1, 13.4, 9.8, NA, 11)
  r <- desc_summary(x)
  expect_equal(r$n, 4)
  expect_true(any(grepl("missing", r$notes)))
  df <- data.frame(g = c("A", "A", "B", "B", "B"), y = c(1, 2, 3, 4, 5))
  expect_equal(desc_summary(subset(df, g == "B")$y)$mean, 4)
})

test_that("non-syntactic column names work in regression and in the picker", {
  df <- data.frame(`monthly spend` = c(10, 12, 15, 11, 18, 20), age = c(20, 25, 30, 22, 35, 40), check.names = FALSE)
  fit <- reg_fit(`monthly spend` ~ age, data = df)
  expect_equal(fit$coefficients$estimate, unname(coef(lm(`monthly spend` ~ age, df))))
  in_global(list(df2 = df), {
    expect_true("df2$`monthly spend`" %in% statcram:::.live_objects()$expr)
  })
})

test_that("typed count tables (matrix / table / data frame) work for chi-square", {
  m <- matrix(c(20, 30, 25, 25), nrow = 2, dimnames = list(gender = c("F", "M"), buy = c("yes", "no")))
  ref <- unname(chisq.test(m, correct = FALSE)$statistic)
  expect_equal(chisq_indep(m)$statistic, ref)
  expect_equal(chisq_indep(as.table(m))$statistic, ref)
  tab <- data.frame(gender = c("F", "M"), yes = c(20, 30), no = c(25, 25))
  expect_equal(chisq_indep(tab)$statistic, ref)
})
