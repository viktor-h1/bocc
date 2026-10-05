# sc_app(): the panel's forms build the same calls as the console.

ac <- function(fn, ...) statcram:::.app_call(statcram:::.app_specs[[fn]], list(...))
dp <- function(cl) paste(deparse(cl, width.cutoff = 500L), collapse = " ")

test_that("every procedure in the panel is a statcram function with known arguments", {
  for (nm in names(statcram:::.app_specs)) {
    sp <- statcram:::.app_specs[[nm]]
    f <- get(sp$fn, envir = asNamespace("statcram"))
    args <- unlist(lapply(sp$fields, function(fl) if (fl$type == "pick") names(fl$choices) else fl$arg))
    args <- args[!is.na(args) & nzchar(args)]
    expect_true(all(args %in% c(names(formals(f)), "freq", "prop")), info = nm)
  }
})

test_that("forms build the calls of the Module 1 exercises", {
  expect_equal(dp(ac("desc_summary", x = "pizzerie$Sales", between = "15000 30000", population = FALSE)),
               "desc_summary(x = pizzerie$Sales, between = c(15000, 30000))")
  expect_equal(ac("desc_summary", x = ""), "Fill in: Numerical variable.")
  expect_equal(dp(ac("desc_freq", source = "raw", x = "DS$Age", order = c("Young", "Middle", "Senior"), at_most = "Middle", plot = "auto")),
               "desc_freq(x = DS$Age, order = c(\"Young\", \"Middle\", \"Senior\"), at_most = \"Middle\")")
  expect_match(dp(ac("desc_freq", source = "table", tvals = "1 2 3 4 5 6", tcounts = "5401 7340 8238 2561 0 700", plot = "auto")),
               "^desc_freq\\(x = c\\(`1` = 5401, `2` = 7340, `3` = 8238, `4` = 2561, `5` = 0, `6` = 700\\)\\)$")
  expect_equal(dp(ac("desc_classes", source = "table", breaks = "0 10 20 30 60 90 150", ftype = "freq", fvals = "122 420 294 176 571 217",
                     at_most = "5 30", split = "30", plot = "both")),
               "desc_classes(breaks = c(0, 10, 20, 30, 60, 90, 150), at_most = c(5, 30), split = 30, freq = c(122, 420, 294, 176, 571, 217))")
  expect_equal(dp(ac("desc_classes", source = "density", breaks = "0 20 40 60 80 120 160 300", density = "0.022 0.01 0.005 0.003 0.002 0.001 NA", plot = "both")),
               "desc_classes(breaks = c(0, 20, 40, 60, 80, 120, 160, 300), density = c(0.022, 0.01, 0.005, 0.003, 0.002, 0.001, NA))")
  expect_equal(dp(ac("desc_vars", data = "DS")), "desc_vars(data = DS)")
  expect_equal(dp(ac("desc_cv", x1 = "a", x2 = "b")), "desc_cv(a, b)")
  expect_equal(dp(ac("desc_cv", x1 = "Bidding$Bid", mean = "4.875", sd = "2.3243", names = "classes")),
               "desc_cv(Bidding$Bid, mean = 4.875, sd = 2.3243, names = \"classes\")")
  expect_equal(dp(ac("est_mean", source = "raw", x = "Bidding$PaidFare", group = "Bidding$Channel", levels = c("Aggregator", "Airline"))),
               "est_mean(x = Bidding$PaidFare, group = Bidding$Channel, levels = c(\"Aggregator\", \"Airline\"))")
})

test_that("forms for random variables and estimation", {
  expect_equal(dp(ac("rv_iid", mu = "12", sigma = "sqrt(380)", n = "80", stat = "mean", q = "above", q_value = "15")),
               "rv_iid(mu = 12, sigma = sqrt(380), n = 80, above = 15)")
  expect_equal(dp(ac("prob_normal", mean = "10", sd = "2", q = "between", q_value = "8 12")),
               "prob_normal(mean = 10, sd = 2, between = c(8, 12))")
  expect_equal(dp(ac("ci_mean", source = "summary", xbar = "275.3343", sigma = "500", n = "390", conf = "0.9", method = "t")),
               "ci_mean(xbar = 275.3343, sigma = 500, n = 390, conf = 0.9)")
  expect_equal(dp(ac("ci_prop", source = "summary", count = "52", n = "300", conf = "0.95")), "ci_prop(count = 52, n = 300)")
  expect_equal(dp(ac("n_prop", target = "width", target_value = "0.09", p = "0.5", conf = "0.99")), "n_prop(width = 0.09, conf = 0.99)")
  expect_equal(ac("n_mean", target = "margin", target_value = "", sigma = "2", conf = "0.95"), "Type the value for: Requirement.")
  expect_equal(statcram:::.app_nums("12%"), "0.12")
  expect_equal(statcram:::.app_nums("mean(x) / 2"), "mean(x) / 2")
})

test_that("the panel prints the console output and evaluates calls with workspace data", {
  in_global(list(pz = data.frame(s = c(10, 12, 15, 30, 18), d = c("A", "B", "A", "A", "C"))), {
    cl <- ac("desc_summary", x = "pz$s", at_most = "15")
    out <- statcram:::.app_print(cl)
    expect_match(out, "^> desc_summary\\(x = pz\\$s, at_most = 15\\)")
    expect_match(out, "Freq(X <= 15) = 3 / 5 = 0.6", fixed = TRUE)
    expect_equal(statcram:::.app_levels("pz$d"), c("A", "B", "C"))
    expect_match(statcram:::.app_print(ac("desc_summary", x = "pz$nope")), "^> desc_summary.*\nProblem:")
  })
})

test_that("server: choosing a procedure and filling the form gives the result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("miniUI")
  in_global(list(pz = data.frame(s = c(10, 12, 15, 30, 18))), {
    shiny::testServer(statcram:::.app_server, {
      session$setInputs(proc = "desc_summary", desc_summary__x = "pz$s", desc_summary__population = FALSE)
      session$elapse(500)
      expect_match(output$result, "Descriptive summary")
      expect_match(output$result, "Mean     = sum(x_i) / n = 85 / 5 = 17", fixed = TRUE)
    })
    expect_s3_class(statcram:::.app_ui(), "shiny.tag.list")
  })
})
