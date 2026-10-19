# Phrasing bank (inst/notes/phrasing-bank.md), parsed by R/phrasing.R.

pb <- statcram:::.pb()

test_that("the phrasing bank is shipped unchanged and parsed completely", {
  path <- statcram:::.notes_path("phrasing-bank.md")
  expect_true(nzchar(path))
  expect_equal(unname(tools::md5sum(path)), "64875f7989e924355230300a16361e87")
  cs <- pb$cases
  expect_equal(nrow(cs), 167)
  expect_equal(as.vector(table(factor(cs$section, levels = pb$sections$code))),
               c(8, 16, 25, 18, 16, 16, 9, 11, 14, 22, 9, 3))
  expect_equal(pb$sections$code, c("1", "2", "3", "4", "5P", "5R", "5N", "5S", "6E", "6I", "6N", "6X"))
  expect_false(anyDuplicated(cs$id) > 0)
  expect_true(all(grepl("^[0-9][0-9A-Z]*\\.[0-9]{2}$", cs$id)))
  expect_true(all(nzchar(cs$use_when) & nzchar(cs$write) & nzchar(cs$sources)))
  expect_true(all(!nzchar(cs$unparsed)))
  expect_equal(c(sum(nzchar(cs$formula)), sum(nzchar(cs$avoid)), sum(nzchar(cs$qualification))), c(78, 109, 53))
  expect_equal(nrow(pb$excerpts), 10); expect_equal(nrow(pb$mistakes), 19)
  expect_equal(nrow(pb$verbs), 6); expect_equal(nrow(pb$notation), 6); expect_equal(nrow(pb$sources), 13)
  # every quoted line of the file is kept word for word, as a template line or an excerpt
  L <- readLines(path, warn = FALSE, encoding = "UTF-8")
  quoted <- substring(L[startsWith(L, "> ")], 3)
  kept <- c(unlist(strsplit(cs$write, "\n", fixed = TRUE)), unlist(strsplit(pb$excerpts$quote, "\n", fixed = TRUE)))
  expect_equal(sort(kept), sort(quoted))
  # error-log "Revisit" ranges point at real cases
  ids <- unlist(strsplit(pb$mistakes$ids, " ", fixed = TRUE))
  expect_true(all(ids[nzchar(ids)] %in% cs$id))
  expect_equal(statcram:::.pb_revisit_ids("4.05\u201306; 5P.09\u201311"), c("4.05", "4.06", "5P.09", "5P.10", "5P.11"))
})

test_that("ids, sections and search find the right cases", {
  ids <- function(...) sc_phrasing(..., list = TRUE)$id
  expect_equal(ids(id = "5n.5"), "5N.05")
  expect_equal(ids("6I.13"), "6I.13")
  expect_error(sc_phrasing(id = "9Z.99"), "No case with id")
  expect_equal(length(ids(section = "6I")), 22)
  expect_equal(length(ids(section = 5)), 52)
  expect_true(all(ids(section = "normal probabilities") %in% ids(section = "5N")))
  expect_error(sc_phrasing(section = "nonsense"), "Unknown section")
  expect_true(all(c("5N.05", "5N.08") %in% ids("qnorm")))
  expect_equal(ids("t wider than z")[1], "6I.13")
  expect_equal(ids("whisker"), "3.21")
  expect_true("6E.07" %in% ids("SE"))
  expect_lt(length(ids("SE")), 60)
  expect_true(all(ids("qnorm", section = "5N") %in% ids(section = "5N")))
  expect_true("2.07" %in% ids("histogramm"))      # small typo
  expect_equal(length(ids("xyzzy")), 0)
  expect_true(any(grepl("No case matches", capture.output(sc_phrasing("xyzzy")))))
})

test_that("a case prints its parts, the study note and matching error-log entries", {
  out <- capture.output(sc_phrasing("5N.05"))
  txt <- paste(out, collapse = "\n")
  for (h in c("USE WHEN", "WRITE", "FORMULA / R", "AVOID", "YOUR ERROR LOG", "SOURCES", "STUDY USE ONLY"))
    expect_match(txt, h, fixed = TRUE)
  expect_match(txt, "qnorm(1-alpha, mean=mu, sd=sigma)", fixed = TRUE)   # backticks removed
  expect_false(grepl("`", txt, fixed = TRUE))
  expect_match(txt, "5N.06 Value exceeded with high probability >>", fixed = TRUE)
  start <- paste(capture.output(sc_phrasing()), collapse = "\n")
  for (k in pb$sections$code) expect_match(start, paste0("\n  ", k, " "), fixed = TRUE)
  lst <- capture.output(sc_phrasing("qnorm"))
  expect_true(any(grepl("cases match \"qnorm\"", lst, fixed = TRUE)))
})

test_that("verbs, notation, excerpts, mistakes, scope and sources print", {
  norm <- function(x) gsub("\\s+", " ", paste(x, collapse = " "))
  ex <- norm(capture.output(sc_phrasing(what = "excerpts")))
  for (q in pb$excerpts$quote) expect_true(grepl(gsub("\\s+", " ", q), ex, fixed = TRUE))
  expect_match(norm(capture.output(sc_phrasing(what = "mistakes"))), "Revisit: 5N.02\u201308", fixed = TRUE)
  expect_match(norm(capture.output(sc_phrasing(what = "verbs"))), "Which tool / measure / graph?", fixed = TRUE)
  expect_match(norm(capture.output(sc_phrasing(what = "notation"))), "z_\u03b1 and t_(\u03bd,\u03b1)", fixed = TRUE)
  expect_match(norm(capture.output(sc_phrasing(what = "scope"))), "SOURCE FIDELITY AND LIMITS", fixed = TRUE)
  expect_match(norm(capture.output(sc_phrasing(what = "sources"))), "PJ26", fixed = TRUE)
  expect_equal(capture.output(sc_phrasing(what = "verbs", list = TRUE)), character(0))
  expect_match(capture.output(sc_notes("bank"))[1], "Phrasing Bank", fixed = TRUE)
})

test_that("ASCII fallback works for phrasing-bank cases", {
  old <- options(statcram.ascii = TRUE)
  out <- capture.output(sc_phrasing("5N.05"))
  options(old)
  expect_false(any(grepl("[^\\x01-\\x7f]", out, perl = TRUE)))
  expect_true(any(grepl("alpha", out, fixed = TRUE)))
})

test_that("menu 9,6: case id, search, section and error log", {
  r <- scripted(c("5N.05", "q"), sc(9, 6))
  expect_true(any(grepl("^USE WHEN", r$out)))
  r2 <- scripted(c("qnorm", "1", "q"), sc(9, 6))
  expect_true(any(grepl("^5N\\.0[0-9]  ", r2$out)))
  r3 <- scripted(c("6X", "2", "q"), sc(9, 6))
  expect_true(any(grepl("^6X\\.02  ", r3$out)))
  r4 <- scripted(c("e", "q"), sc(9, 6))
  expect_true(any(grepl("YOUR PERSONAL ERROR LOG", r4$out, fixed = TRUE)))
  expect_output(sc_index(), "sc_phrasing")
})
