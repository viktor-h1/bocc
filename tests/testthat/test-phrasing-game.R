# Phrasing game and answer scorer (R/phrasing-game.R).

score <- function(...) { utils::capture.output(r <- sc_phrasing_score(...)); r }

test_that("stemming and paraphrases make forms of the same word match", {
  st <- statcram:::.gm_stem
  expect_equal(unique(st(c("estimate", "estimates", "estimated", "estimating"))), "estimat")
  expect_equal(st("vary"), st("varies"))
  expect_equal(st("class"), st("classes"))
  expect_equal(st("imply"), st("implies"))
  expect_equal(st("approximately"), st("approximate"))
  w <- statcram:::.gm_words("Intervals built this way; the [placeholder words] are ignored")
  expect_true("constructed" %in% w$word)
  expect_false(any(c("placeholder", "words") %in% w$word))
})

test_that("every round scores 100 against itself and an empty answer scores low", {
  b <- statcram:::.pb()
  it <- statcram:::.gm_items(seq_len(nrow(b$cases)))
  expect_equal(nrow(it), 199)                       # 167 cases, labelled alternatives as separate rounds
  expect_equal(sum(nzchar(it$variant)), 60)
  self <- vapply(it$canon, function(x) statcram:::.gm_score(x, x)$score, 1)
  expect_true(all(self == 100))
  empty <- vapply(it$canon, function(x) statcram:::.gm_score(x, "")$score, 1)
  expect_true(all(empty <= 15))
})

test_that("missing crucial verbs, qualifiers and negations are flagged", {
  out <- capture.output(r <- sc_phrasing_score("6I.03", "95% of the intervals would have the true mean"))
  expect_true(all(c("contain", "vary") %in% r$missed_verbs))
  expect_true("approximately" %in% r$missed_qual)
  expect_true(any(grepl("^! MISSING VERB", out)))
  expect_true(any(grepl("^! QUALIFIER", out)))
  good <- score("6I.03", paste("Across repeated random samples of the same size, about 95% of intervals built",
                                           "with this procedure would contain the true population parameter; the parameter",
                                           "is fixed and the endpoints vary from sample to sample."))
  expect_gte(good$score, 85)
  expect_equal(length(good$missed_verbs) + length(good$missed_qual), 0)
  neg <- capture.output(n1 <- sc_phrasing_score("5N.09", "The mean and variance determine the distribution, so we calculate it"))
  expect_false(n1$neg_ok)
  expect_true(any(grepl("^! NEGATION", neg)))
  n2 <- score("5N.09", "Mean and variance don't determine the distribution: it cannot be calculated")
  expect_true(n2$neg_ok)
  expect_gt(n2$score, n1$score)
})

test_that("labelled alternatives: best match, or the one asked for", {
  r <- score("1.02", "X is ordinal: categories can be ordered but distances cannot be quantified")
  expect_equal(r$variant, "ORDINAL")
  expect_equal(score("1.02", "no order", variant = "nominal")$variant, "NOMINAL")
  expect_error(sc_phrasing_score("1.02", "x", variant = "binary"), "no variant")
  expect_error(sc_phrasing_score("9Z.99", "x"), "No case")
})

test_that("the game plays rounds with hint, skip and stop, and replays weak rounds", {
  assign("game", NULL, envir = statcram:::.pb_cache)
  r <- scripted(c("h", "Under this model alpha of units have X above c, c = mu + z sigma", "",
                  "", "",
                  "q"),
                sc_phrasing_game(id = c("5N.05", "5N.09", "6I.03")))
  txt <- paste(r$out, collapse = "\n")
  expect_match(txt, "ROUND 1/3", fixed = TRUE)
  expect_match(txt, "HINT", fixed = TRUE)
  expect_match(txt, "hint used: -10 points", fixed = TRUE)
  expect_match(txt, "Skipped.", fixed = TRUE)
  expect_match(txt, "CANON ANSWER", fixed = TRUE)
  expect_match(txt, "YOUR ERROR LOG", fixed = TRUE)
  expect_match(txt, "==== RESULT ====", fixed = TRUE)
  res <- r$res
  expect_equal(res$id, c("5N.05", "5N.09"))
  expect_true(res$hint[1]); expect_equal(res$score[2], 0)
  h <- statcram:::.gm_history()
  expect_equal(nrow(h), 2)
  r2 <- scripted(c("The mean and variance do not determine the distribution, so the probability cannot be calculated"),
                 sc_phrasing_game(review = TRUE))
  expect_equal(r2$res$id, "5N.09")
  assign("game", NULL, envir = statcram:::.pb_cache)
  expect_output(sc_phrasing_game(review = TRUE), "Nothing to review")
  r3 <- scripted(c("x", "", "y", "", "z"), sc_phrasing_game(n = 3, section = "6X"))
  expect_equal(nrow(r3$res), 3)
  expect_true(all(r3$res$id %in% c("6X.01", "6X.02", "6X.03")))
  expect_error(sc_phrasing_game(n = 0), "positive number")
})

test_that("menu 9,6: g starts the game", {
  r <- scripted(c("g", "2", "6X", "an answer", "", "another answer", "q"), sc(9, 6))
  expect_true(any(grepl("PHRASING GAME - 2 rounds", r$out, fixed = TRUE)))
  expect_true(any(grepl("==== RESULT ====", r$out, fixed = TRUE)))
  expect_output(sc_index(), "sc_phrasing_game")
})
