bank <- statcram:::.ab()
tpl <- bank$templates
card <- function(id) gsub("\\s+", " ", paste(capture.output(sc_answer(id)), collapse = " "))
ids <- function(...) sc_answers(..., list = TRUE)$id

test_that("all 244 cards load: unique ids, counts by chapter, no blank fields, labelled authored", {
  expect_equal(nrow(tpl), 244)
  expect_false(anyDuplicated(tpl$id) > 0)
  expect_equal(as.integer(table(tpl$chapter)), c(12L, 59L, 36L, 33L, 57L, 47L))
  for (f in c("id", "category", "title", "use_when", "answer_template", "provenance", "chapter_title", "priority"))
    expect_true(all(nzchar(tpl[[f]])), info = f)
  expect_true(all(nzchar(tpl$avoid_when)))          # every card says when NOT to use it
  expect_true(all(grepl("^authored_", tpl$provenance)))
  expect_true(all(tpl$exam_scope == "2026_27_first_partial"))
  expect_true(all(vapply(tpl$source_refs, length, 1L) > 0))
  expect_true(all(unlist(tpl$source_refs) %in% bank$sources$key))
  expect_equal(sum(bank$categories$count), 244)
  expect_equal(nrow(bank$questions), 53); expect_equal(nrow(bank$quotes), 38)
  # the 59 Chapter 2 cards of the first bank are kept with the same wording
  ch2 <- tpl[tpl$chapter == 2, ]
  expect_equal(nrow(ch2), 59)
  expect_match(ch2$answer_template[ch2$id == "C2.HISTOGRAM.UNEQUAL"], "frequency densities must be represented", fixed = TRUE)
})

test_that("the bank matches its JSON source plus the documented corrections", {
  src <- file.path(testthat::test_path("..", ".."), "data-raw")
  skip_if_not(file.exists(file.path(src, "answer_bank.json")) && requireNamespace("jsonlite", quietly = TRUE))
  j <- jsonlite::fromJSON(file.path(src, "answer_bank.json"), simplifyVector = FALSE)$templates
  cr <- jsonlite::fromJSON(file.path(src, "answer_bank_corrections.json"), simplifyVector = FALSE)
  expect_identical(vapply(j, `[[`, "", "id"), tpl$id)
  touched <- unique(vapply(cr[vapply(cr, function(x) x$field == "answer_template", TRUE)], `[[`, "", "id"))
  same <- !tpl$id %in% touched
  expect_identical(vapply(j, `[[`, "", "answer_template")[same], tpl$answer_template[same])
  expect_equal(nrow(bank$corrections), length(cr))
  expect_true(all(nzchar(bank$corrections$reason)))
  expect_true(all(bank$corrections$kind %in% c("error", "misleading", "minor", "added_avoid_when", "placeholders_sync")))
})

test_that("verbatim questions and official quotes are kept apart from the authored cards", {
  q <- bank$questions
  expect_true(all(q$source %in% bank$sources$key[bank$sources$past_paper]))
  expect_true(all(vapply(q$cards, function(x) length(x) > 0 && all(x %in% tpl$id), TRUE)))
  out <- capture.output(r <- sc_answers_questions(source = "P25"))
  expect_equal(nrow(r), 10)
  txt <- paste(out, collapse = " ")
  expect_match(txt, "1st partial, 14 October 2025, line", fixed = TRUE)
  expect_match(txt, "verbatim", fixed = TRUE)
  expect_match(txt, "OFFICIAL SOLUTION EXCERPTS", fixed = TRUE)
  # an official excerpt is printed exactly (emphasis markers removed only)
  expect_match(txt, "Therefore, since 35 > 32.98 the considered value is not extreme", fixed = TRUE)
  # no authored card text is presented as an official excerpt
  expect_false(any(vapply(tpl$answer_template, function(a) any(grepl(a, bank$quotes$text, fixed = TRUE)), TRUE)))
  expect_equal(nrow(sc_answers_questions(topic = "ci", list = TRUE)), 3)
  expect_error(sc_answers_questions("NOPE"), "Unknown source")
  expect_output(sc_answers_questions(), "G26SEP")
})

test_that("required cards say the right thing", {
  expect_match(card("C5.EVENTS.CONDITIONAL"), "P(A|B)=P(A\u2229B)/P(B)", fixed = TRUE)
  expect_match(card("C5.EVENTS.CONDITIONAL"), "the denominator is P(B)", fixed = TRUE)
  v <- card("C5.TRANSFORM.VAR_LINEAR")
  expect_match(v, "[b]\u00b2 Var(X)", fixed = TRUE)
  expect_false(grepl("|b|", tpl$answer_template[tpl$id == "C5.TRANSFORM.VAR_LINEAR"], fixed = TRUE))
  se <- card("C6.ESTIMATOR.SE_MEANING")
  expect_match(se, "GENERIC estimate", fixed = TRUE)
  expect_match(se, "does not measure the actual error of the realised", fixed = TRUE)
  u <- card("C6.CI.UNKNOWN_NORMAL")
  expect_match(u, "Student's t", fixed = TRUE); expect_match(u, "population is Normal", fixed = TRUE)
  expect_match(u, "qt(1\u2212\u03b1/2,df=n\u22121)", fixed = TRUE)
  z <- card("C6.CI.UNKNOWN_LARGE_Z")
  expect_match(z, "CLT", fixed = TRUE); expect_match(z, "approximate", fixed = TRUE)
  # every card shows the study-only note, status and the other cases of its kind
  for (k in c("USE THIS WHEN", "READY-TO-PRACTISE ANSWER", "DON'T USE WHEN", "SOURCE STATUS", "STUDY USE ONLY",
              "not a verbatim official answer", "OTHER CASES OF THIS KIND"))
    expect_true(grepl(k, u, fixed = TRUE), info = k)
  # course traps from the notes: quantiles, histogram densities, z_alpha, CI meaning
  expect_match(tpl$answer_template[tpl$id == "C3.QUANTILES.GROUP_QUANTILE"], "within-class uniformity", fixed = TRUE)
  expect_match(card("C2.HISTOGRAM.UNEQUAL"), "c_k = p_k / w_k", fixed = TRUE)
  expect_match(card("C2.MODAL.UNEQUAL"), "comparing frequency densities", fixed = TRUE)
  expect_match(card("C6.CI.CI_FALSE_PROB"), "either contains it or does not", fixed = TRUE)
  expect_match(card("C3.OUTLIERS.LOW_TRUE"), "Q1\u22121.5IQR=[q1]\u22121.5\u00d7[iqr]=[lower]", fixed = TRUE)
})

test_that("search: required terms, ranking, short words, misses", {
  for (w in c("Bayes", "conditional", "qnorm", "outlier", "margin of error", "unbiased", "normal approximation",
              "SE", "generic estimate", "among those who", "Var(a+bX)", "right skewed", "confidence interval", "IQR",
              "histogram density"))
    expect_gt(length(ids(w)), 0)
  expect_true("C5.EVENTS.BAYES" %in% ids("Bayes")[1:3])
  expect_true(all(c("C5.TRANSFORM.VAR_LINEAR") %in% ids("Var(a+bX)")))
  expect_lt(length(ids("SE")), 60)                      # "se" is a whole word, not inside "use"
  expect_true("C6.ESTIMATOR.SE_MEANING" %in% ids("generic estimate"))
  expect_true(all(grepl("\\.CONDITIONAL\\.", ids("among those who")[1:5])))
  expect_true("C6.CI.UNKNOWN_NORMAL" %in% ids("\u03c3 unknown"))  # symbols match their ASCII names
  expect_true("C6.CI.UNKNOWN_NORMAL" %in% ids("sigma unknown"))
  expect_true("C2.HISTOGRAM.UNEQUAL" %in% ids("histogramm"))     # small typo
  expect_true("C2.SHAPE.RIGHT" %in% ids("r\u00efght-sk\u00e9wed"))      # accents ignored
  expect_equal(length(ids("xyzzy")), 0)
  expect_true(any(grepl("No card matches", capture.output(sc_answers("xyzzy")))))
  expect_equal(sc_answers("standard error", priority = "critical", list = TRUE)$id[1], "C6.ESTIMATOR.SE_MEANING")
})

test_that("filters: chapter, category, group, priority, scope", {
  expect_equal(length(ids(chapter = 5)), 57)
  expect_equal(length(ids(category = "ci", chapter = 6)), 23)
  expect_equal(length(ids(category = "histogram")), 6)
  expect_equal(length(ids(category = "conditional", chapter = 2)), 4)
  expect_equal(length(ids(category = "conditional")), 11)   # Ch. 2 and Ch. 4
  expect_equal(length(ids(category = "6.ci")), 23)
  expect_equal(length(ids(priority = "critical")), 32)
  expect_equal(length(ids(priority = "high")), 56)
  expect_true(all(c("C5.EVENTS.CONDITIONAL", "C6.ESTIMATOR.SE_MEANING") %in% ids(priority = "critical")))
  expect_equal(length(ids(group = "probability")), 17)
  expect_equal(length(ids(group = 3)), 22)
  expect_equal(sum(vapply(seq_len(nrow(bank$groups)), function(g) length(ids(group = g)), 1L)), 244)
  expect_equal(length(ids(scope = "core")) + length(ids(scope = "crossover")), 244)
  expect_error(sc_answers(category = "nonsense"), "Unknown category")
  expect_error(sc_answers(group = "nonsense"), "Unknown group")
  out <- paste(capture.output(sc_answers()), collapse = "\n")
  expect_match(out, "START HERE", fixed = TRUE); expect_match(out, "Ch. 6", fixed = TRUE)
  expect_match(paste(capture.output(sc_answers(chapter = 5)), collapse = "\n"), "Events, conditional probability, Bayes  (14)", fixed = TRUE)
})

test_that("no two-sample inference or hypothesis tests in the first-partial bank", {
  txt <- tolower(paste(tpl$title, tpl$use_when, tpl$answer_template))
  expect_false(any(grepl("two independent samples|mu_x ?- ?mu_y|difference between (two )?(means|proportions)|p-value|reject h0|chi-square test|chisq", txt)))
})

test_that("sc_answers_fill replaces blanks as plain text and flags the rest", {
  out <- capture.output(f <- sc_answers_fill("C2.CONDITIONAL.A_HIGH",
    values = list(outcome = "MediumTerm bookings", `grouping variable` = "Channel", A = "Aggregator", B = "Agency", x = "42")))
  expect_match(f, "Among observations belonging to group Aggregator, 42% have", fixed = TRUE)
  expect_equal(attr(f, "unfilled"), "y")
  expect_true(any(grepl("STILL TO FILL: \\[y\\]", out)))
  g <- sc_answers_fill("C2.CUMULATIVE.INSIDE", values = list(x = 26, a = 25, b = 30), print = FALSE)
  expect_match(g, "inside the interval [25, 30)", fixed = TRUE)
  ci <- sc_answers_fill("C6.CI.UNKNOWN_NORMAL", values = list(L = "51.2", U = "55.8"), print = FALSE)
  expect_match(ci, "=[51.2,55.8]", fixed = TRUE)                 # nested [[L],[U]]
  h <- sc_answers_fill("C2.GRAPH.NOMINAL", values = list(X = "system('echo hi'); <b>x</b>"), print = FALSE)
  expect_match(h, "The variable system('echo hi'); <b>x</b> is", fixed = TRUE)
  expect_warning(sc_answers_fill("C2.GRAPH.NOMINAL", values = list(Z = "a"), print = FALSE), "Not a blank")
  expect_error(sc_answers_fill("C2.GRAPH.NOMINAL", values = list(X = quote(1 + 1))), "single number or piece of text")
  expect_error(sc_answers_fill("NOPE"), "No card")
})

test_that("practice hides the answer until asked, records ratings, drills the review list", {
  assign("ratings", NULL, envir = statcram:::.ab_cache)
  r <- scripted(c("", "n", "", "y"), sc_answers_practice(chapter = 5, category = "events", n = 2, shuffle = FALSE))
  first <- tpl$answer_template[tpl$chapter == 5 & tpl$category == "events"][1]
  pos_sit <- grep("SITUATION", r$out)[1]; pos_ans <- grep("^ANSWER", r$out)[1]
  expect_true(pos_sit < pos_ans)
  expect_equal(r$res$rating, c("needs review", "remembered"))
  expect_output(sc_answers_progress(), "1 remembered, 1 need review")
  r2 <- scripted(c("", "y"), sc_answers_practice(review = TRUE, n = 5))
  expect_equal(nrow(r2$res), 1)
  r3 <- scripted("q", sc_answers_practice(priority = "critical", n = 3))
  expect_equal(nrow(r3$res), 0)
  assign("ratings", NULL, envir = statcram:::.ab_cache)
})

test_that("export writes study cards with the study-only note; corrections are listed", {
  f <- tempfile(fileext = ".md")
  capture.output(sc_answers_export(f, chapter = 6, priority = "critical"))
  md <- readLines(f, encoding = "UTF-8")
  expect_match(md[3], "STUDY USE ONLY", fixed = TRUE)
  expect_true(any(grepl("C6.ESTIMATOR.SE_MEANING", md, fixed = TRUE)))
  capture.output(sc_answers_export(f, group = "ci", answers = FALSE))
  expect_false(any(grepl("Answer to practise", readLines(f))))
  expect_output(sc_answers_corrections(), "C4.COMPARISON.BOX_SPREAD")
})

test_that("ASCII fallback for consoles without UTF-8", {
  a <- statcram:::.ab_ascii("x\u0304 \u00b1 t s/\u221an ; \u03c3\u00b2 \u2264 \u03bc ; p\u0302 ; \u2212")
  expect_equal(a, "xbar +- t s/sqrtn ; sigma^2 <= mu ; p-hat ; -")
  old <- options(statcram.ascii = TRUE)
  out <- capture.output(sc_answer("C6.CI.UNKNOWN_NORMAL"))
  options(old)
  expect_true(any(grepl("sigma unknown", out, fixed = TRUE)))
  expect_false(any(grepl("[^\\x01-\\x7f]", out, perl = TRUE)))
})

test_that("menu 9,5: start here, chapter, search, weak spots, questions, drill; old shortcuts work", {
  r <- scripted(c("among those who", "1", "y", "", "", "", "", "", "", "q"), sc(9, 5))
  txt <- paste(r$out, collapse = " ")
  expect_match(txt, "START HERE", fixed = TRUE)
  expect_match(txt, "USE THIS WHEN", fixed = TRUE)
  r2 <- scripted(c("c6", "1", "n", "q"), sc(9, 5))
  expect_true(any(grepl("^== C6\\.", r2$out)))
  r3 <- scripted(c("p", "P24", "q"), sc(9, 5))
  expect_true(any(grepl("22 October 2024", r3$out)))
  r4 <- scripted(c("b", "q"), sc(9, 5))
  expect_false(any(grepl("^== C", r4$out)))
  expect_true(any(grepl("^== Central Limit Theorem", scripted(c("CLT", "q"), sc(9, 4))$out)))
  expect_output(sc_index(), "sc_answers_questions")
})

test_that("the window builds and its server filters and practises (when shiny is installed)", {
  skip_if_not_installed("shiny"); skip_if_not_installed("miniUI")
  ui <- as.character(statcram:::.ab_app_ui())
  for (k in c("Study use only", "Copy answer", "Practice mode", "Past-paper questions", "Start here", "Weak spots only"))
    expect_true(grepl(k, ui, fixed = TRUE), info = k)
  assign("ratings", NULL, envir = statcram:::.ab_cache)
  shiny::testServer(statcram:::.ab_app_server, {
    session$setInputs(q = "", group = "ci", chapter = "", weak = FALSE, review = FALSE, cat = "all")
    expect_equal(length(visible()), 31)
    session$setInputs(chapter = "6", cat = "6.samplesize")
    expect_true(all(tpl$category[visible()] == "samplesize"))
    session$setInputs(group = "", chapter = "", cat = "all", q = "unequal widths")
    expect_equal(tpl$id[visible()][1], "C2.HISTOGRAM.UNEQUAL")
    session$setInputs(case = "C2.MODAL.UNEQUAL", recall = TRUE)
    expect_true(hidden())
    session$setInputs(showans = 1)
    expect_false(hidden())
    session$setInputs(rate_review = 1)
    expect_equal(unname(statcram:::.ab_ratings()["C2.MODAL.UNEQUAL"]), "needs review")
    session$setInputs(review = TRUE)
    expect_equal(tpl$id[visible()], "C2.MODAL.UNEQUAL")
  })
  assign("ratings", NULL, envir = statcram:::.ab_cache)
})
