mk <- local({
  set.seed(3)
  df <- data.frame(spend = rnorm(60, 100, 15), loyalty = sample(c("High", "Medium", "Low"), 60, TRUE),
                   vip = sample(c(0, 1), 60, TRUE), before = rnorm(60, 10))
  df$after <- df$before + rnorm(60, 0.3)
  df
})

test_that("menu: one-mean test on a column created after loading, with re-run call", {
  in_global(list(mk = mk), {
    eval(quote(mk$d <- mk$after - mk$before), .GlobalEnv)
    s <- scripted(c("4", "1", "mk$d", "2", "0", "2", "0.05", "q"), sc())
    expect_length(s$left, 0)
    expect_true(any(grepl("One-mean test", s$out)))
    expect_true(any(grepl("^test_mean\\(x = mk\\$d, mu0 = 0, alt = \">\", alpha = 0.05, method = \"t\"\\)$", s$out)))
    expect_true(any(grepl("sc\\(4, 1\\)", s$out)))
  })
})

test_that("menu shortcuts, back / main navigation, and error recovery", {
  in_global(list(mk = mk), {
    s <- scripted(c("b", "m", "3", "1", "m", "0"), sc(4, 1))
    expect_length(s$left, 0)
    expect_true(any(grepl("Left statcram", s$out)))
    s2 <- scripted(c("abc", "1", "5", "2", "0", "3", "0.05", "q"), sc(4, 1))
    expect_true(any(grepl("R could not use that", s2$out)))
    expect_true(any(grepl("One-mean test", s2$out)))
    expect_error(sc(mk), "does not need the data")
  })
})

test_that("menu: two independent means by group, proportion of an expression, chi-square", {
  in_global(list(mk = mk), {
    s <- scripted(c("4", "4", "1", "1", "2", "1", "2", "1", "3", "0.05", "q"), sc())
    expect_true(any(grepl("test_2means\\(x = mk\\$spend, case = \"pooled\"", s$out)))
    s2 <- scripted(c("3", "2", "mk$loyalty == \"High\"", "90", "q"), sc())
    expect_true(any(grepl("ci_prop\\(x = mk\\$loyalty == \"High\", conf = 0.9\\)", s2$out)))
    s3 <- scripted(c("6", "2", "1", "2", "3", "0.05", "q"), sc())
    expect_true(any(grepl("Chi-square test of independence", s3$out)))
  })
})

test_that("menu: regression fit (saved as a model), prediction and table entry from the picker", {
  in_global(list(mk = mk), {
    s <- scripted(c("7", "1", "1", "1", "2 3", "n", "fit1", "0.05", "2", "1", "1", "1", "0.95", "q"), sc())
    expect_true(exists("fit1", envir = .GlobalEnv))
    expect_true(any(grepl("fit1 <- reg_fit\\(formula = spend ~ loyalty \\+ vip", s$out)))
    expect_true(any(grepl("PI for ONE new individual", s$out)))
    s2 <- scripted(c("6", "2", "2", "t", "2", "2", "2", "g", "F M", "buy", "yes no", "1", "20 30 25 25", "", "tab9", "n",
                     "1", "0.05", "q"), sc())
    expect_true(any(grepl("chisq_indep\\(x = tab9", s2$out)))
  })
})

test_that("menu: frequency table with ordinal order, classes from raw data and measured in classes", {
  gro <- data.frame(Satisf = rep(c("VLow", "Low", "Med", "High"), c(5, 10, 20, 15)), TotVisits = 3:52,
                    MonthExp = rep(c("[0,50)", "[50,100)", "[100,200]"), c(20, 20, 10)))
  in_global(list(gro = gro), {
    s1 <- scripted(c("1", "1", "1", "4 2 3 1", "3", "3", "q"), sc())
    expect_true(any(grepl("desc_freq\\(x = gro\\$Satisf, order = c\\(\"VLow\", \"Low\", \"Med\", \"High\"\\), at_least = \"Med\"\\)", s1$out)))
    expect_true(any(grepl("Freq\\(X >= Med\\) = .* = 0.7", s1$out)))
    s2 <- scripted(c("1", "2", "1", "2", "5", "2", "30", "q"), sc())
    expect_true(any(grepl("desc_classes\\(x = gro\\$TotVisits, breaks = 5, at_most = 30\\)", s2$out)))
    s3 <- scripted(c("1", "2", "2", "3", "3", "75", "q"), sc())
    expect_true(any(grepl("Freq\\(X >= 75\\) = 1 - Freq\\(X < 75\\) = 1 - 0.6 = 0.4", s3$out)))
    expect_true(any(grepl("Modal classes \\(tie\\)", s3$out)))
  })
})

test_that("guide, index, recipes and notes print", {
  expect_output(sc_guide(), "WHICH TEST")
  expect_output(sc_index(), "test_2means")
  expect_output(sc_recipes(), "subset")
  expect_output(sc_notes("inference"), "Welch")
  idx <- statcram:::.index
  expect_true(all(vapply(idx$fun, exists, logical(1), envir = asNamespace("statcram"))))
})
