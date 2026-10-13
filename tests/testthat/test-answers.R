bank <- statcram:::.ab()
tpl <- bank$templates

test_that("all 59 templates load with unique ids and non-empty fields", {
  expect_equal(nrow(tpl), 59)
  expect_false(anyDuplicated(tpl$id) > 0)
  for (f in c("id", "section", "category", "title", "use_when", "avoid_when", "answer_template", "provenance", "scope"))
    expect_true(all(nzchar(tpl[[f]])), info = f)
  expect_true(all(tpl$provenance == "authored_exam_style_template_not_verbatim_official_answer"))
  expect_true(all(tpl$category %in% bank$sections$id))
  expect_equal(sum(bank$sections$count), 59)
  # wording kept exactly as in the JSON source (UTF-8 symbols included)
  expect_match(tpl$section[tpl$id == "C2.HISTOGRAM.UNEQUAL"], "5.5 — Interpreting", fixed = TRUE)
  src <- file.path(testthat::test_path("..", ".."), "data-raw", "answer_bank.json")
  if (file.exists(src) && requireNamespace("jsonlite", quietly = TRUE)) {
    j <- jsonlite::fromJSON(src, simplifyVector = FALSE)$templates
    expect_identical(vapply(j, `[[`, "", "id"), tpl$id)
    expect_identical(vapply(j, `[[`, "", "answer_template"), tpl$answer_template)
  }
})

test_that("categories, ids and the index", {
  expect_equal(nrow(sc_answers(category = "histogram", list = TRUE)), 6)
  expect_equal(nrow(sc_answers(category = "conditional", list = TRUE)), 4)
  expect_equal(nrow(sc_answers(category = 5, list = TRUE)), 6)
  expect_equal(nrow(sc_answers(chapter = 2, list = TRUE)), 59)
  expect_equal(nrow(sc_answers(scope = "core", list = TRUE)) + nrow(sc_answers(scope = "crossover", list = TRUE)), 59)
  expect_error(sc_answers(category = "nonsense"), "Unknown category")
  out <- capture.output(sc_answers())
  expect_true(any(grepl("histogram .* 6 cases", out)))
  expect_true(any(grepl("study use only", out, ignore.case = TRUE)))
})

test_that("search finds the right cards and handles misses", {
  d <- sc_answers("density", list = TRUE)$id
  expect_gte(length(d), 3)
  expect_true(all(c("C2.HISTOGRAM.UNEQUAL", "C2.MODAL.UNEQUAL") %in% d))
  a <- sc_answers("among those", list = TRUE)$id
  expect_true(length(a) >= 2 && all(grepl("^C2\\.CONDITIONAL\\.", a)))
  expect_equal(sc_answers("unequal intervals", list = TRUE)$id[1], "C2.HISTOGRAM.UNEQUAL")
  expect_true("C2.COMPARE.MEDIAN_SIMILAR" %in% sc_answers("same median", list = TRUE)$id)
  expect_true("C2.SHARE.WITHIN_CLASS" %in% sc_answers("uniformity", list = TRUE)$id)
  expect_true(all(c("C2.SHAPE.RIGHT", "C2.SHAPE.LEFT") %in% sc_answers("SKEW", list = TRUE)$id))
  expect_true("C2.MODAL.UNEQUAL" %in% sc_answers("modal class", list = TRUE)$id)
  expect_true("C2.HISTOGRAM.UNEQUAL" %in% sc_answers("histogramm", list = TRUE)$id)   # small typo
  expect_true("C2.GROUPED.VARIANCE" %in% sc_answers("variance", list = TRUE)$id)
  expect_true("C2.SHAPE.RIGHT" %in% sc_answers("rïght-skéwed", list = TRUE)$id)  # accents ignored
  expect_equal(nrow(sc_answers("xyzzy", list = TRUE)), 0)
  miss <- capture.output(r <- sc_answers("xyzzy"))
  expect_true(any(grepl("No case matches", miss))); expect_equal(nrow(r), 0)
})

test_that("full cards show conditions, answer, formulas and status", {
  u <- capture.output(sc_answers(id = "C2.HISTOGRAM.UNEQUAL"))
  txt <- paste(u, collapse = " ")
  for (k in c("USE WHEN", "DO NOT USE WHEN", "ANSWER TO STUDY", "FORMULAS", "SOURCE/STATUS",
              "densities", "area", "widths", "misleading", "c_k = p_k / w_k", "not a verbatim official answer", "STUDY USE ONLY"))
    expect_true(grepl(k, txt, fixed = TRUE), info = k)
  v <- paste(capture.output(sc_answers(id = "c2.grouped.variance")), collapse = " ")
  expect_match(v, "n/(n-1)", fixed = TRUE)
  expect_true(any(grepl("Closest ids", capture.output(sc_answers(id = "C2.HISTOGRAM.UNEQUL")))))
  one <- capture.output(r <- sc_answers("open-ended"))
  expect_equal(r$id, "C2.GROUPED.OPEN_END")
})

test_that("sc_answers_fill replaces blanks as plain text and flags the rest", {
  out <- capture.output(f <- sc_answers_fill("C2.CONDITIONAL.A_HIGH",
    values = list(outcome = "MediumTerm bookings", `grouping variable` = "Channel", A = "Aggregator", B = "Agency", x = "42")))
  expect_match(f, "Among observations belonging to group Aggregator, 42% have", fixed = TRUE)
  expect_match(f, "[y]%", fixed = TRUE)
  expect_equal(attr(f, "unfilled"), "y")
  expect_true(any(grepl("STILL TO FILL: \\[y\\]", out)))
  # interval blanks are filled part by part; interval notation is not a blank
  g <- sc_answers_fill("C2.CUMULATIVE.INSIDE", values = list(x = 26, a = 25, b = 30), print = FALSE)
  expect_match(g, "inside the interval [25, 30)", fixed = TRUE)
  expect_match(g, "the interval [25, 26]", fixed = TRUE)
  expect_length(attr(g, "unfilled"), 0)
  # nothing is evaluated: R code stays text
  h <- sc_answers_fill("C2.GRAPH.NOMINAL", values = list(X = "system('echo hi'); <b>x</b>"), print = FALSE)
  expect_match(h, "The variable system('echo hi'); <b>x</b> is", fixed = TRUE)
  expect_warning(sc_answers_fill("C2.GRAPH.NOMINAL", values = list(Z = "a"), print = FALSE), "Not a blank")
  expect_error(sc_answers_fill("C2.GRAPH.NOMINAL", values = list(X = quote(1 + 1))), "single number or piece of text")
  expect_error(sc_answers_fill("C2.GRAPH.NOMINAL", values = list("a")), "named")
  expect_error(sc_answers_fill("NOPE"), "No case")
  e <- sc_answers_fill("C2.SHARE.CLAIM", print = FALSE)
  expect_equal(as.character(e), tpl$answer_template[tpl$id == "C2.SHARE.CLAIM"])
  expect_length(attr(e, "unfilled"), 5)
})

test_that("menu 9,5 browses, shows and fills; old 9,x shortcuts still work", {
  r <- scripted(c("among those", "1", "y", "MediumTerm bookings", "Channel", "Aggregator", "51", "25", "Agency", "q"), sc(9, 5))
  txt <- gsub("\\s+", " ", paste(r$out, collapse = " "))
  expect_match(txt, "C2.CONDITIONAL.A_HIGH", fixed = TRUE)
  expect_match(txt, "Among observations belonging to group Aggregator, 51% have the specified outcome, compared with 25% in group Agency", fixed = TRUE)
  r2 <- scripted(c("5", "2", "n", "q"), sc(9, 5))
  expect_true(any(grepl("^== C2.HISTOGRAM.UNEQUAL", r2$out)))
  r3 <- scripted(c("b", "q"), sc(9, 5))
  expect_false(any(grepl("^== C2", r3$out)))
  expect_true(any(grepl("^== Central Limit Theorem", scripted(c("CLT", "q"), sc(9, 4))$out)))
  expect_output(sc_index(), "sc_answers")
})

test_that("the browser builds and escapes text (when shiny is installed)", {
  skip_if_not_installed("shiny"); skip_if_not_installed("miniUI")
  ui <- as.character(statcram:::.ab_app_ui())
  expect_match(ui, "Study use only", fixed = TRUE)
  expect_match(ui, "Copy answer", fixed = TRUE)
  expect_match(ui, "Recall mode", fixed = TRUE)
  shiny::testServer(statcram:::.ab_app_server, {
    session$setInputs(q = "unequal widths", scope = "all", cat = "all")
    expect_equal(tpl$id[visible()][1], "C2.HISTOGRAM.UNEQUAL")
    session$setInputs(case = "C2.MODAL.UNEQUAL", recall = FALSE)
    session$setInputs(ph_1 = "<b>[25, 30)</b>")
    expect_match(statcram:::.ab_fill_text(tpl$answer_template[current()], values()), "<b>[25, 30)</b>", fixed = TRUE)
    expect_match(output$todo$html, "All blanks filled", fixed = TRUE)
    session$setInputs(q = "among those", cat = "conditional")
    expect_true(all(tpl$category[visible()] == "conditional"))
  })
})
