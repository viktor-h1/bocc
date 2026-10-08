test_that("every definition has the four parts and a unique key", {
  d <- statcram:::.defs
  expect_gte(nrow(d), 50)
  expect_false(any(duplicated(d$key)))
  for (f in c("term", "what", "formula", "meaning", "property")) expect_true(all(nzchar(d[[f]])), info = f)
  expect_true(all(d$topic %in% statcram:::.def_topics))
  expect_true(all(statcram:::.def_topics %in% d$topic))
})

test_that("terms are found by key, name, abbreviation or a small misspelling", {
  find <- function(t) statcram:::.defs$key[statcram:::.def_find(t)]
  expect_equal(find("IQR"), "iqr")
  expect_equal(find("interquartile"), "iqr")
  expect_equal(find("Standard Error"), "standard-error")
  expect_equal(find("se"), "standard-error")
  expect_equal(find("CLT"), "clt")
  expect_equal(find("standart deviation"), "sd")
  expect_equal(find("Q1"), "quartiles")
  expect_equal(find("me"), "margin-of-error")
  expect_setequal(find("error"), c("standard-error", "efficiency", "margin-of-error"))
  expect_length(find("xyz"), 0)
})

test_that("sc_define prints the parts, lists matches and all terms", {
  out <- utils::capture.output(x <- sc_define("IQR"))
  expect_true(any(grepl("^== Interquartile range \\(IQR\\)", out)))
  expect_true(any(grepl("^Formula +IQR = Q3 - Q1", out)))
  expect_true(any(grepl("^Meaning +The width of the interval containing the central 50%", out)))
  expect_true(any(grepl("^Property +Robust to extreme values", out)))
  expect_equal(x, out)
  two <- utils::capture.output(sc_define(c("estimator", "estimate")))
  expect_true(any(grepl("^== Estimator ", two))); expect_true(any(grepl("^== Estimate ", two)))
  expect_true(any(grepl("matches several terms", utils::capture.output(sc_define("error")))))
  expect_true(any(grepl("No definition", utils::capture.output(sc_define("xyz")))))
  all <- utils::capture.output(sc_define())
  expect_true(all(statcram:::.def_topics %in% all))
  expect_true(any(grepl("^  iqr +Interquartile range", all)))
})

test_that("menu item 9,4 defines a typed term", {
  r <- scripted(c("CLT", "q"), sc(9, 4))
  expect_true(any(grepl("^== Central Limit Theorem \\(CLT\\)", r$out)))
})
