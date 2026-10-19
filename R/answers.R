# ------------------------------------------------------------
# Study answer bank: authored exam-style answer templates for the 2026/27
# first partial (Chapters 1-6), chosen by the situation in the question.
# Study use only: the course rules forbid prepared R scripts and AI tools
# at the exam.
#
# Data: data-raw/answer_bank.json (source) + data-raw/answer_bank_corrections.json
# (documented corrections), compiled by data-raw/build_answer_bank.R into
# inst/extdata/answer_bank.rds and read here with base R only.
# ------------------------------------------------------------

.ab_cache <- new.env(parent = emptyenv())

.ab <- function() {
  if (is.null(.ab_cache$bank)) {
    p <- system.file("extdata", "answer_bank.rds", package = "statcram")
    if (!nzchar(p)) p <- file.path("inst", "extdata", "answer_bank.rds")
    if (!file.exists(p)) stop("The answer bank file is missing; reinstall statcram.", call. = FALSE)
    .ab_cache$bank <- readRDS(p)
  }
  .ab_cache$bank
}

.ab_study_note <- "STUDY USE ONLY - authored exam-style templates, not official solutions. The 2026/27 rules forbid prepared R scripts and AI tools at the exam: study the wording beforehand and write your own answer."
.ab_provenance <- c(
  authored_exam_style_template_not_verbatim_official_answer =
    "Authored study template in the style of the official solutions (not a verbatim official answer).",
  authored_study_template_based_on_framework_and_or_official_solutions =
    "Authored study template based on the course framework and/or official solutions (not a verbatim official answer).")
.ab_support <- c(official_exam_pattern_or_framework = "Pattern seen in past papers or stated in the framework.",
                 framework_extrapolation = "Variation extrapolated from the framework (no past-paper question of exactly this kind).")

.ab_status <- function(prov) { s <- .ab_provenance[prov]; if (is.na(s)) prov else unname(s) }
.ab_src_label <- function(k) { s <- .ab()$sources; l <- s$label[match(k, s$key)]; ifelse(is.na(l), k, l) }

# ---------- text helpers ----------

# Plain-ASCII versions of the symbols used in the cards (Windows consoles
# without UTF-8; also used to normalise search text).
.ab_ascii_map <- c("\u2264" = "<=", "\u2265" = ">=", "\u2260" = "!=", "\u2248" = "~", "\u2212" = "-",
                   "\u2013" = "-", "\u2014" = "-", "\u00d7" = "x", "\u221a" = "sqrt", "\u00b2" = "^2",
                   "\u00b3" = "^3", "\u2211" = "sum", "\u03a3" = "sum", "\u2229" = " and ", "\u222a" = " or ",
                   "\u2192" = "->", "\u03bc" = "mu", "\u03c3" = "sigma", "\u03b1" = "alpha", "\u03b2" = "beta",
                   "\u03b8" = "theta", "\u03c1" = "rho", "\u03bb" = "lambda", "\u03c7" = "chi", "\u03b5" = "eps",
                   "\u00b1" = "+-", "\u2018" = "'", "\u2019" = "'", "\u201c" = "\"", "\u201d" = "\"", "\u2026" = "...", "\u00b7" = "*",
                   "\u2081" = "1", "\u2082" = "2", "\u2080" = "0")
.ab_ascii <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("([A-Za-z])\u0304", "\\1bar", x, perl = TRUE)   # x-bar
  x <- gsub("([A-Za-z])\u0302", "\\1-hat", x, perl = TRUE)  # p-hat, theta-hat
  for (k in names(.ab_ascii_map)) x <- gsub(k, .ab_ascii_map[[k]], x, fixed = TRUE)
  a <- suppressWarnings(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "?"))
  ifelse(is.na(a), x, a)
}

# Print lines; transliterate when the console is not UTF-8 or when
# options(statcram.ascii = TRUE).
.ab_cat <- function(...) {
  x <- c(...)
  if (isTRUE(getOption("statcram.ascii")) || !isTRUE(l10n_info()[["UTF-8"]])) x <- .ab_ascii(x)
  cat(x, sep = "\n")
}

.ab_wrap <- function(txt, label = "", indent = 17, width = getOption("width", 80)) {
  w <- max(50, min(width, 100))
  strwrap(txt, width = w, initial = formatC(label, width = -indent), exdent = indent)
}

# Remove Markdown emphasis for console display (the stored quotes are untouched).
.ab_plain <- function(x) gsub("\\*{1,3}([^*]+)\\*{1,3}", "\\1", x)

# ---------- search ----------

.ab_norm <- function(s) {
  s <- tolower(.ab_ascii(s))
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9%<>=/.+()|^-]+", " ", s)))
}

.ab_stem <- function(w) {
  for (suf in c("ness", "ity", "ies", "ing", "ed", "ly", "es", "s"))
    if (nchar(w) - nchar(suf) >= 4 && endsWith(w, suf)) return(substr(w, 1, nchar(w) - nchar(suf)))
  w
}

.ab_syn <- list(same = c("similar", "aligned", "equal"), similar = c("same", "aligned"),
                spread = c("variab", "dispers", "iqr"), dispersion = c("variab", "spread"),
                variability = c("dispers", "spread", "iqr"), centre = c("center", "central"),
                center = c("centre", "central"), average = "mean", percent = c("percentage", "proportion", "share"),
                percentage = c("percent", "proportion", "share"), proportion = c("percent", "share"),
                share = c("percent", "proportion"), bin = c("class", "interval"), bins = c("class", "interval"),
                peak = c("bimodal", "concentration"), peaks = c("bimodal", "concentration"),
                skew = "asymmetr", width = c("wide", "width"), among = c("conditional", "among"),
                denominator = c("conditional", "denominator"), se = "standard error", sd = "standard deviation",
                ci = "confidence interval", clt = "central limit", me = "margin of error", iid = "i.i.d",
                bayes = c("bayes", "reverse"), sigma = "sigma", mu = "mu")
.ab_stop <- c("the", "of", "a", "an", "and", "or", "in", "to", "is", "for", "with", "what", "how", "does")

# Searchable text of each card, by field, normalised once.
.ab_docs <- function() {
  if (is.null(.ab_cache$docs)) {
    b <- .ab(); t <- b$templates
    f <- function(x) vapply(x, function(v) .ab_norm(paste(v, collapse = " ")), "")
    clab <- b$categories$label[match(paste(t$chapter, t$category, sep = "."), b$categories$key)]
    .ab_cache$docs <- list(title = .ab_norm(t$title), triggers = f(t$triggers), id = .ab_norm(t$id),
                           section = .ab_norm(paste(t$category, clab, t$section, t$chapter_title)),
                           use = .ab_norm(t$use_when), avoid = .ab_norm(t$avoid_when), answer = .ab_norm(t$answer_template),
                           formulas = f(t$formulas), sources = f(lapply(t$source_refs, .ab_src_label)),
                           priority = .ab_norm(ifelse(t$priority == "normal", "", paste(t$priority, "weak spot"))))
  }
  .ab_cache$docs
}
.ab_weights <- c(title = 5, triggers = 5, id = 4, section = 3, use = 3, avoid = 1, answer = 1, formulas = 2,
                 sources = 1, priority = 1)

# Does word w (or its stem / synonyms) occur in each document? Words of up to
# three letters must match whole words ("se" does not match "use").
.ab_hit <- function(w, docs) {
  keys <- unique(c(.ab_stem(w), .ab_syn[[w]]))
  Reduce(`|`, lapply(keys, function(k) {
    if (nchar(k) <= 3) grepl(paste0("(^|[^a-z0-9])", gsub("([.+()|^])", "\\\\\\1", k), "($|[^a-z0-9])"), docs, perl = TRUE)
    else grepl(k, docs, fixed = TRUE)
  }))
}

# Rank every document: phrase hits first, then documents with all the words,
# then (if none) those with most words, then small misspellings. docs is a
# list of normalised fields, weights their weights, boost a per-document bonus.
.ab_rank <- function(query, docs, weights, boost = 0) {
  q <- .ab_norm(query)
  n <- length(docs[[1]])
  if (!nzchar(q)) return(list(rows = seq_len(n), how = "all"))
  fields <- names(weights)
  # the phrase must start at a word boundary (and end at one, if short)
  pat <- paste0("(^|[^a-z0-9])", gsub("([.+()|^\\[\\]])", "\\\\\\1", q), if (nchar(q) <= 3) "($|[^a-z0-9])" else "")
  phrase <- rowSums(vapply(fields, function(f) weights[[f]] * 3 * grepl(pat, docs[[f]], perl = TRUE), numeric(n)))
  words <- unique(strsplit(q, " ", fixed = TRUE)[[1]])
  words <- words[!words %in% .ab_stop & nzchar(words)]
  if (!length(words)) words <- strsplit(q, " ", fixed = TRUE)[[1]]
  hits <- lapply(fields, function(f) matrix(vapply(words, .ab_hit, logical(n), docs = docs[[f]]), nrow = n))
  names(hits) <- fields
  wmat <- Reduce(`|`, hits)
  wscore <- rowSums(vapply(fields, function(f) weights[[f]] * rowSums(hits[[f]]), numeric(n)))
  nmatch <- rowSums(wmat)
  how <- if (any(phrase > 0)) "phrase" else "words"
  keep <- phrase > 0 | nmatch == length(words)
  if (!any(keep) && max(nmatch) > 0) { keep <- nmatch == max(nmatch); how <- "partial" }
  if (!any(keep)) {
    dw <- unique(unlist(strsplit(unlist(docs[fields]), " ", fixed = TRUE)))
    fz <- vapply(words, function(w) if (nchar(w) < 5) w else {
      d <- utils::adist(w, dw); if (min(d) <= max(1, floor(nchar(w) / 5))) dw[which.min(d)] else w }, "")
    if (!identical(unname(fz), words)) {
      r <- .ab_rank(paste(fz, collapse = " "), docs, weights, boost)
      if (length(r$rows)) r$how <- paste0("spelling:", paste(fz, collapse = " "))
      return(r)
    }
    return(list(rows = integer(0), how = "none"))
  }
  score <- phrase + wscore + boost
  rows <- which(keep)
  list(rows = rows[order(-score[rows], rows)], how = how)
}

.ab_search <- function(query)
  .ab_rank(query, .ab_docs(), .ab_weights, c(critical = 0.5, high = 0.25, normal = 0)[.ab()$templates$priority])

# ---------- filters ----------

.ab_cat_keys <- function(category, chapter = NULL) {
  cats <- .ab()$categories
  if (!is.null(chapter)) cats <- cats[cats$chapter %in% chapter, ]
  if (is.numeric(category)) {
    if (any(category < 1 | category > nrow(cats)))
      stop(sprintf("Category number must be 1 to %d%s.", nrow(cats), if (is.null(chapter)) "" else " for this chapter"), call. = FALSE)
    return(cats$key[category])
  }
  k <- .ab_norm(category)
  hit <- cats$key[.ab_norm(cats$key) == k | .ab_norm(cats$category) == k]
  if (!length(hit)) hit <- cats$key[grepl(k, .ab_norm(paste(cats$category, cats$label)), fixed = TRUE)]
  if (!length(hit)) stop(sprintf("Unknown category \"%s\". sc_answers() lists them (e.g. \"ci\", \"events\", \"histogram\", \"6.ci\").", category), call. = FALSE)
  hit
}

.ab_group_keys <- function(group) {
  g <- .ab()$groups
  i <- if (is.numeric(group)) group else match(.ab_norm(group), .ab_norm(g$id))
  if (is.na(i) || i < 1 || i > nrow(g)) i <- which(grepl(.ab_norm(group), .ab_norm(g$label), fixed = TRUE))[1]
  if (is.na(i)) stop(sprintf("Unknown group \"%s\". Groups: %s.", group, paste(g$id, collapse = ", ")), call. = FALSE)
  list(keys = g$keys[[i]], label = g$label[i])
}

.ab_priority_rows <- function(priority) {
  t <- .ab()$templates
  if (is.null(priority)) return(rep(TRUE, nrow(t)))
  priority <- match.arg(priority, c("critical", "high", "normal"))
  switch(priority, critical = t$priority == "critical", high = t$priority %in% c("critical", "high"),
         normal = rep(TRUE, nrow(t)))
}

.ab_scope_rows <- function(scope) {
  t <- .ab()$templates
  if (is.null(scope)) return(rep(TRUE, nrow(t)))
  scope <- match.arg(scope, c("all", "core", "crossover"))
  cross <- vapply(seq_len(nrow(t)), function(i) any(t$related_chapters[[i]] != t$chapter[i]), TRUE)
  switch(scope, all = rep(TRUE, nrow(t)), core = !cross, crossover = cross)
}

.ab_keep <- function(chapter = NULL, category = NULL, group = NULL, priority = NULL, scope = NULL) {
  t <- .ab()$templates
  key <- paste(t$chapter, t$category, sep = ".")
  keep <- .ab_scope_rows(scope) & .ab_priority_rows(priority)
  if (!is.null(chapter)) keep <- keep & t$chapter %in% chapter
  if (!is.null(category)) keep <- keep & key %in% .ab_cat_keys(category, chapter)
  if (!is.null(group)) keep <- keep & key %in% .ab_group_keys(group)$keys
  keep
}

# ---------- printing ----------

.ab_pri_label <- function(p) switch(p, critical = "WEAK SPOT (critical)", high = "weak spot (high priority)", "")

.ab_find_id <- function(id) which(toupper(.ab()$templates$id) == toupper(trimws(id)))

.ab_card_lines <- function(i, answer = TRUE, siblings = TRUE, questions = TRUE) {
  b <- .ab(); t <- b$templates[i, ]
  w <- max(50, min(getOption("width", 80), 100))
  head <- sprintf("== %s  %s ", t$id, t$title)
  ph <- t$placeholders[[1]]; fm <- t$formulas[[1]]; src <- t$source_refs[[1]]
  key <- paste(t$chapter, t$category, sep = ".")
  clab <- b$categories$label[b$categories$key == key]
  rel <- t$related_chapters[[1]]
  status <- paste(.ab_status(t$provenance), if (t$source_support %in% names(.ab_support)) .ab_support[[t$source_support]],
                  if (t$corrected) "Wording corrected in statcram (see sc_answers_corrections()).",
                  if (isTRUE(t$avoid_added)) "\"Don't use when\" added by statcram.")
  fill_eg <- if (length(ph)) sprintf("sc_answers_fill(\"%s\", values = list(%s))", t$id,
                                     paste(sprintf("`%s` = \"...\"", ph), collapse = ", "))
  out <- c(paste0(head, strrep("=", max(3, w - nchar(head)))),
           .ab_wrap(sprintf("Ch. %d %s  |  %s%s%s", t$chapter, t$chapter_title, clab,
                            if (length(rel) > 1 || any(rel != t$chapter)) paste0("  |  related chapters: ", paste(rel, collapse = ", ")) else "",
                            if (t$priority != "normal") paste0("  |  ", .ab_pri_label(t$priority)) else ""), "", indent = 0),
           .ab_wrap(.ab_study_note, "", indent = 0), "",
           .ab_wrap(t$use_when, "USE THIS WHEN"), "")
  out <- c(out, "READY-TO-PRACTISE ANSWER",
           if (answer) .ab_wrap(t$answer_template, "", indent = 2) else "  (hidden: practise first, then reveal)", "")
  if (length(fm)) out <- c(out, .ab_wrap(paste(fm, collapse = " ;  "), "FORMULAS"))
  if (nzchar(t$avoid_when)) out <- c(out, .ab_wrap(t$avoid_when, "DON'T USE WHEN"))
  if (length(ph)) out <- c(out, .ab_wrap(paste(sprintf("[%s]", ph), collapse = "  "), "BLANKS TO FILL"), .ab_wrap(fill_eg, ""))
  out <- c(out, .ab_wrap(status, "SOURCE STATUS"))
  if (length(src)) out <- c(out, .ab_wrap(paste(.ab_src_label(src), collapse = "; "), "SOURCES"))
  if (questions) {
    q <- b$questions[vapply(b$questions$cards, function(cc) t$id %in% cc, TRUE), ]
    q <- q[order(!q$source %in% src), ]
    if (nrow(q)) out <- c(out, "", sprintf("PAST-PAPER QUESTIONS OF THIS KIND (verbatim excerpts; %d)", nrow(q)),
                          unlist(lapply(seq_len(min(3, nrow(q))), function(k)
                            .ab_wrap(sprintf("%s, line %d: \"%s\"", .ab_src_label(q$source[k]), q$line[k], .ab_plain(q$text[k])), "", indent = 2))),
                          if (nrow(q) > 3) sprintf("  ... sc_answers_questions(topic = \"%s\") lists all %d", t$category, nrow(q)))
  }
  if (siblings) {
    sib <- which(b$templates$chapter == t$chapter & b$templates$category == t$category & b$templates$id != t$id)
    if (length(sib)) {
      more <- length(sib) - 8; sib <- utils::head(sib, 8)
      out <- c(out, "", "OTHER CASES OF THIS KIND - check you have the right one:",
               unlist(lapply(sib, function(s) .ab_wrap(sprintf("%s - use when: %s  (%s)", b$templates$title[s],
                                                               b$templates$use_when[s], b$templates$id[s]), "  -", indent = 4))),
               if (more > 0) sprintf("  ... and %d more: sc_answers(chapter = %d, category = \"%s\")", more, t$chapter, t$category))
    }
  }
  out
}

.ab_list_lines <- function(rows, header) {
  t <- .ab()$templates
  if (!length(rows)) return(header)
  c(header, unlist(lapply(seq_along(rows), function(k) {
    i <- rows[k]
    c(sprintf(" %2d  %-31s %s%s", k, t$id[i], t$title[i], if (t$priority[i] == "critical") "  [weak spot]" else ""),
      .ab_wrap(paste("use when:", t$use_when[i]), "", indent = 9))
  })))
}

.ab_start_lines <- function() {
  b <- .ab(); g <- b$groups; ch <- b$chapters
  c("STUDY ANSWER BANK - 2026/27 first partial  (authored templates; study use only)", "",
    "START HERE - what does the question ask?",
    sprintf(" %2d  %-48s %3d cases%s", seq_len(nrow(g)), g$label, g$count, ifelse(g$weak, "   (weak spot)", "")), "",
    "BY CHAPTER",
    sprintf("  Ch. %d  %-55s %3d", ch$number, ch$title, ch$count), "",
    "  sc_answers(group = 3)               a START HERE group      sc_answers(chapter = 5)   one chapter",
    "  sc_answers(\"among those who\")       search                 sc_answer(\"C6.CI.UNKNOWN_NORMAL\")  one card",
    "  sc_answers(priority = \"critical\")   weak spots             sc_answers_questions(\"P25\")  past-paper questions",
    "  sc_answers_practice(chapter = 6)    drill: answer hidden    sc_answers_app()          window")
}

.ab_grouped_lines <- function(rows, header) {
  b <- .ab(); t <- b$templates
  key <- paste(t$chapter, t$category, sep = ".")[rows]
  out <- header
  for (k in unique(key)) {
    lab <- b$categories$label[b$categories$key == k]
    rr <- rows[key == k]
    out <- c(out, "", sprintf("%s  (%d)", lab, length(rr)),
             sprintf("   %-34s %s%s", t$id[rr], t$title[rr], ifelse(t$priority[rr] == "critical", "  [weak spot]", "")))
  }
  c(out, "", "One card in full: sc_answer(\"<id>\")")
}

#' Study answer bank: exam-style answer templates by question type
#'
#' A searchable bank of 244 authored answer templates for the 2026/27 first
#' partial (Chapters 1-6: data, descriptive statistics, two variables,
#' probability and the CLT, estimation of one mean and one proportion). Each
#' card shows **when to use it**, the **answer to practise** with `[blanks]`,
#' the formulas, **when not to use it**, its source status, past-paper
#' questions of the same kind (verbatim excerpts) and the other cases of the
#' same kind, so you can see why they would be wrong. The templates are
#' written in the style of the official solutions but are **not** verbatim
#' official answers, and they are for **study only**: the 2026/27 rules forbid
#' prepared R scripts and AI tools at the exam. Nothing is computed: pick a
#' card only if its conditions match your data.
#'
#' * `sc_answers()` shows START HERE (groups by the wording of the question)
#'   and the chapters.
#' * `sc_answers("words")` searches (case-, accent- and punctuation-
#'   insensitive, small typos tolerated); one hit is shown in full.
#' * `sc_answers(chapter = 5)`, `sc_answers(category = "ci")`,
#'   `sc_answers(group = 3)`, `sc_answers(priority = "critical")` list cards.
#' * `sc_answer("C6.CI.UNKNOWN_NORMAL")` (or `sc_answers(id = ...)`) shows
#'   one card.
#' * `list = TRUE` returns the matching cards as a data frame.
#'
#' Symbols print as ASCII (`mu`, `sigma`, `<=`, `xbar`) on consoles that are
#' not UTF-8, or always with `options(statcram.ascii = TRUE)`.
#'
#' @param query Words to search for.
#' @param category A category (e.g. `"ci"`, `"events"`, `"histogram"`,
#'   `"6.ci"`), its number in the chapter's list, or part of its label.
#' @param id A card id, e.g. `"C2.GROUPED.VARIANCE"`.
#' @param chapter One or more chapters (1 to 6).
#' @param priority `"critical"` (the weak spots) or `"high"` (critical and
#'   high).
#' @param group A START HERE group: its number or id (`"graph"`, `"compare"`,
#'   `"percent"`, `"quantile"`, `"grouped"`, `"relation"`, `"probability"`,
#'   `"rv"`, `"clt"`, `"estimator"`, `"ci"`).
#' @param scope `"all"`, `"core"` (cards about their own chapter only) or
#'   `"crossover"` (cards tagged with other chapters too).
#' @param list If `TRUE`, return the matching cards as a data frame instead of
#'   printing them.
#' @return Invisibly: the card shown (a one-row data frame) or the data frame
#'   of matching cards; with `list = TRUE`, that data frame visibly.
#' @seealso [sc_answer()], [sc_answers_fill()], [sc_answers_questions()],
#'   [sc_answers_practice()], [sc_answers_export()], [sc_answers_app()],
#'   `sc_notes("phrasing")`, [sc_define()].
#' @examples
#' sc_answers()
#' sc_answers("among those who")
#' sc_answers(chapter = 6, category = "ci")
#' sc_answers(priority = "critical")
#' sc_answer("C5.TRANSFORM.VAR_LINEAR")
#' head(sc_answers(chapter = 2, list = TRUE))
#' @export
sc_answers <- function(query = NULL, category = NULL, id = NULL, chapter = NULL, priority = NULL,
                       group = NULL, scope = NULL, list = FALSE) {
  t <- .ab()$templates
  if (!is.null(id)) {
    i <- .ab_find_id(id)
    if (!length(i)) {
      near <- t$id[order(utils::adist(toupper(id), toupper(t$id)))[1:3]]
      cat(sprintf("No card with id \"%s\". Closest ids: %s.\n", id, paste(near, collapse = ", ")))
      return(invisible(t[0, ]))
    }
    if (list) return(.ab_index(i))
    .ab_cat(.ab_card_lines(i))
    return(invisible(t[i, ]))
  }
  keep <- .ab_keep(chapter, category, group, priority, scope)
  filters <- !is.null(chapter) || !is.null(category) || !is.null(group) || !is.null(priority) || !is.null(scope)
  if (is.null(query)) {
    rows <- which(keep)
    if (list) return(.ab_index(rows))
    if (!filters) { .ab_cat(.ab_start_lines()); return(invisible(.ab_index(rows))) }
    head <- c(sprintf("%d cards%s  (study use only - authored templates)", length(rows),
                      paste0(c(if (!is.null(group)) paste0(": ", .ab_group_keys(group)$label),
                               if (!is.null(chapter)) paste0(" - chapter ", paste(chapter, collapse = ", ")),
                               if (!is.null(priority)) paste0(" - priority ", priority)), collapse = "")))
    .ab_cat(.ab_grouped_lines(rows, head))
    return(invisible(.ab_index(rows)))
  }
  r <- .ab_search(query)
  rows <- r$rows[keep[r$rows]]
  if (list) return(.ab_index(rows))
  if (!length(rows)) {
    cat(sprintf("No card matches \"%s\".\n", query),
        "Try a shorter word or phrase (\"skew\", \"density\", \"among those\", \"Bayes\", \"standard error\", \"qnorm\"),",
        "sc_answers() for the START HERE list, or sc_define() for the definition of a term.\n", sep = "\n")
    return(invisible(.ab_index(rows)))
  }
  if (length(rows) == 1) {
    .ab_cat(.ab_card_lines(rows))
    return(invisible(t[rows, ]))
  }
  note <- switch(sub(":.*", "", r$how), partial = " (no card has every word; showing those with most of them)",
                 spelling = sprintf(" (searched for \"%s\")", sub("^spelling:", "", r$how)), "")
  .ab_cat(.ab_list_lines(rows, c(sprintf("Search \"%s\": %d cards%s, best first.  Study use only - authored templates.",
                                         query, length(rows), note), "")),
          "", "One card in full: sc_answer(\"<id>\")")
  invisible(.ab_index(rows))
}

#' @rdname sc_answers
#' @export
sc_answer <- function(id) sc_answers(id = id)

.ab_index <- function(rows) {
  t <- .ab()$templates[rows, c("id", "chapter", "category", "priority", "title", "use_when", "avoid_when")]
  rownames(t) <- NULL
  t
}

# ---------- filling the blanks ----------

# Replace bracketed blanks with the values supplied. Values are used as
# plain text (never evaluated). A blank like [a,x] or the interval [a,b)
# is filled part by part; [[L],[U]] becomes [L-value, U-value].
.ab_fill_text <- function(txt, values) {
  m <- gregexpr("\\[([^][]+)([])])", txt, perl = TRUE)
  tok <- regmatches(txt, m)[[1]]
  if (!length(tok)) return(txt)
  rep <- vapply(tok, function(k) {
    close <- substr(k, nchar(k), nchar(k)); inner <- substr(k, 2, nchar(k) - 1)
    if (close == "]" && inner %in% names(values)) return(values[[inner]])
    parts <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
    if (length(parts) > 1 && any(parts %in% names(values))) {
      parts <- vapply(parts, function(p) if (p %in% names(values)) values[[p]] else p, "")
      return(paste0("[", paste(parts, collapse = ", "), close))
    }
    k
  }, "")
  regmatches(txt, m) <- list(unname(rep))
  txt
}

.ab_names_in <- function(txt) {
  tok <- regmatches(txt, gregexpr("\\[([^][]+)([])])", txt, perl = TRUE))[[1]]
  inner <- substr(tok, 2, nchar(tok) - 1)
  unique(c(inner, trimws(unlist(strsplit(inner, ",", fixed = TRUE)))))
}

.ab_values <- function(values) {
  if (is.null(values)) return(list())
  if (!is.list(values)) values <- as.list(values)
  if (length(values) && (is.null(names(values)) || any(!nzchar(names(values)))))
    stop("values must be named, e.g. list(X = \"Bid\", A = \"Agency\").", call. = FALSE)
  lapply(values, function(v) {
    if (!is.atomic(v) || length(v) != 1) stop("Each value must be a single number or piece of text.", call. = FALSE)
    if (is.na(v)) "" else as.character(v)
  })
}

.ab_unfilled <- function(i, vals) {
  filled <- names(vals)[nzchar(trimws(unlist(vals)))]
  Filter(function(p) !all(trimws(strsplit(p, ",", fixed = TRUE)[[1]]) %in% filled), .ab()$templates$placeholders[[i]])
}

#' Fill the blanks of a study answer template
#'
#' Replaces the `[blanks]` of a card of the study answer bank with the text you
#' supply and prints the result, listing any blank still to fill. Values are
#' inserted as plain text: nothing is evaluated or computed, so check that the
#' card you chose matches your data. Study use only.
#'
#' @param id A card id, e.g. `"C2.CONDITIONAL.A_HIGH"` (see [sc_answers()]).
#' @param values A named list (or named vector) of texts, one per blank, e.g.
#'   `list(X = "Bid", A = "Agency")`. Blanks with spaces need backticks:
#'   `` list(`grouping variable` = "Channel") ``. Interval blanks such as
#'   `[a,x]` are filled from `a` and `x`.
#' @param print Print the filled answer (default `TRUE`).
#' @return Invisibly, the filled text, with attribute `unfilled` (the blanks
#'   still empty) and `id`.
#' @examples
#' sc_answers_fill("C2.CONDITIONAL.A_HIGH",
#'                 values = list(outcome = "MediumTerm bookings", `grouping variable` = "Channel",
#'                               A = "Aggregator", B = "Agency", x = "51", y = "25"))
#' sc_answers_fill("C2.MODAL.UNEQUAL", values = list(interval = "[25, 30)"))
#' @export
sc_answers_fill <- function(id, values = list(), print = TRUE) {
  t <- .ab()$templates
  i <- .ab_find_id(id)
  if (length(i) != 1) stop(sprintf("No card with id \"%s\"; sc_answers() lists them.", id), call. = FALSE)
  vals <- .ab_values(values)
  tmpl <- t$answer_template[i]
  unknown <- setdiff(names(vals), .ab_names_in(tmpl))
  if (length(unknown))
    warning(sprintf("Not a blank of %s (ignored): %s. Its blanks: %s.", t$id[i], paste(unknown, collapse = ", "),
                    paste(sprintf("[%s]", t$placeholders[[i]]), collapse = " ")), call. = FALSE)
  out <- .ab_fill_text(tmpl, vals)
  unf <- .ab_unfilled(i, vals)
  attr(out, "unfilled") <- unf; attr(out, "id") <- t$id[i]
  if (print) {
    w <- max(50, min(getOption("width", 80), 100))
    .ab_cat(sprintf("%s  %s  (study use only - authored template, not an official solution)", t$id[i], t$title[i]), "",
            strwrap(out, width = w), "")
    if (length(unf)) cat(sprintf("STILL TO FILL: %s\n", paste(sprintf("[%s]", unf), collapse = "  ")))
    else .ab_cat("All blanks filled. Check that the card's conditions hold for your data:",
                 .ab_wrap(t$use_when[i], "  use when:", indent = 13))
  }
  invisible(out)
}

# ---------- past-paper questions ----------

#' Past-paper questions behind the answer bank (verbatim excerpts)
#'
#' Lists the exam questions of the first-partial kind found in your
#' transcription of the past papers, word for word, each with its source and
#' line and the answer-bank cards to practise it with; then the official
#' solution excerpts of the same exam, labelled separately. Questions are
#' quoted exactly (Markdown emphasis is removed only for display); the cards
#' are authored templates, never official answers. Some questions need the
#' preceding context, a graph or the exam's data file.
#'
#' @param source A source key: `"M1"` (Module 1 exercises), `"P24"` (1st
#'   partial 22 Oct 2024), `"P25"` (1st partial 14 Oct 2025), `"S26JAN"`,
#'   `"G26JAN"`, `"G26JUL"`, `"G26SEP"`. `NULL` lists the sources.
#' @param query Words to look for in the questions.
#' @param topic A card category, e.g. `"ci"` or `"estimator"`.
#' @param quotes Also show the official solution excerpts (default `TRUE`).
#' @param list If `TRUE`, return the questions as a data frame.
#' @return Invisibly (visibly with `list = TRUE`), the data frame of
#'   questions shown.
#' @examples
#' sc_answers_questions()
#' sc_answers_questions("P25")
#' sc_answers_questions(topic = "estimator", quotes = FALSE)
#' @export
sc_answers_questions <- function(source = NULL, query = NULL, topic = NULL, quotes = TRUE, list = FALSE) {
  b <- .ab(); q <- b$questions; s <- b$sources
  if (is.null(source) && is.null(query) && is.null(topic)) {
    if (list) return(q)
    s <- s[s$past_paper, ]
    .ab_cat("PAST-PAPER QUESTIONS (verbatim excerpts from your transcription of the past papers)", "",
            sprintf("  %-7s %-44s %2d questions, %2d official excerpts", s$key, substr(s$label, 1, 44),
                    vapply(s$key, function(k) sum(q$source == k), 1L), vapply(s$key, function(k) sum(b$quotes$source == k), 1L)),
            "", "  sc_answers_questions(\"P25\")    one source", "  sc_answers_questions(topic = \"ci\")    one kind of question")
    return(invisible(q))
  }
  keep <- rep(TRUE, nrow(q))
  if (!is.null(source)) {
    src <- toupper(source)
    if (!all(src %in% s$key)) stop(sprintf("Unknown source \"%s\". Sources: %s.", source, paste(s$key[s$past_paper], collapse = ", ")), call. = FALSE)
    keep <- keep & q$source %in% src
  }
  if (!is.null(topic)) keep <- keep & .ab_norm(q$topic) %in% .ab_norm(topic)
  if (!is.null(query)) keep <- keep & Reduce(`&`, lapply(strsplit(.ab_norm(query), " ")[[1]], function(w) grepl(w, .ab_norm(q$text), fixed = TRUE)))
  qq <- q[keep, ]
  if (list) return(qq)
  out <- sprintf("PAST-PAPER QUESTIONS - %d (verbatim excerpts; may need context, a graph or the exam data)", nrow(qq))
  for (k in seq_len(nrow(qq)))
    out <- c(out, "", .ab_wrap(sprintf("%s  [%s, line %d; %s]", qq$id[k], .ab_src_label(qq$source[k]), qq$line[k], qq$pattern[k]), "", indent = 0),
             .ab_wrap(paste0("\"", .ab_plain(qq$text[k]), "\""), "", indent = 4),
             .ab_wrap(paste(qq$cards[[k]], collapse = "  "), "    practise with: ", indent = 19))
  if (quotes && !is.null(source)) {
    oq <- b$quotes[b$quotes$source %in% toupper(source), ]
    if (nrow(oq)) {
      out <- c(out, "", sprintf("OFFICIAL SOLUTION EXCERPTS - %d (verbatim, from the same transcription)", nrow(oq)))
      for (k in seq_len(nrow(oq)))
        out <- c(out, .ab_wrap(sprintf("line %d: \"%s\"", oq$line[k], .ab_plain(oq$text[k])), "", indent = 4))
    }
  }
  if (!nrow(qq)) out <- c(out, "", "No question matches; sc_answers_questions() lists the sources.")
  .ab_cat(out)
  invisible(qq)
}

# ---------- practice ----------

#' Practise with the answer bank: situation first, answer on request
#'
#' A console drill: for each card it shows the situation (when the card
#' applies) and hides the answer until you press Enter; then you rate it:
#' `y` remembered, `n` needs review. Ratings are kept for this R session;
#' `sc_answers_practice(review = TRUE)` drills only the cards marked "needs
#' review", and [sc_answers_progress()] lists them. Study use only.
#'
#' @inheritParams sc_answers
#' @param n Number of cards to drill (default 5).
#' @param review If `TRUE`, drill only cards marked "needs review" in this
#'   session.
#' @param shuffle Random order (default `TRUE`).
#' @return Invisibly, a data frame of the ratings given in this drill.
#' @examples
#' if (interactive()) sc_answers_practice(chapter = 6, priority = "high")
#' @export
sc_answers_practice <- function(query = NULL, chapter = NULL, category = NULL, group = NULL, priority = NULL,
                                n = 5, review = FALSE, shuffle = TRUE) {
  t <- .ab()$templates
  rows <- if (!is.null(query)) { r <- .ab_search(query)$rows; r[.ab_keep(chapter, category, group, priority)[r]] }
          else which(.ab_keep(chapter, category, group, priority))
  if (review) rows <- rows[t$id[rows] %in% names(.ab_ratings())[.ab_ratings() == "needs review"]]
  if (!length(rows)) { cat(if (review) "No card is marked \"needs review\" in this session.\n" else "No card matches.\n"); return(invisible(NULL)) }
  if (shuffle && length(rows) > 1) rows <- sample(rows)
  rows <- rows[seq_len(min(n, length(rows)))]
  res <- data.frame(id = character(0), rating = character(0), stringsAsFactors = FALSE)
  .ab_cat(sprintf("PRACTICE - %d cards. For each: write or say your answer, then press Enter to compare.", length(rows)),
          "At any prompt: q = stop.  (Study use only.)")
  for (k in seq_along(rows)) {
    i <- rows[k]
    .ab_cat("", sprintf("---- %d/%d  %s  (%s) ----", k, length(rows), t$title[i], t$id[i]),
            .ab_wrap(t$use_when[i], "SITUATION"),
            if (t$priority[i] != "normal") paste("  ", .ab_pri_label(t$priority[i])))
    a <- tolower(trimws(.read_line("Press Enter to reveal the answer (q = stop): ")))
    if (a == "q") break
    .ab_cat("", .ab_wrap(t$answer_template[i], "ANSWER", indent = 8),
            if (nzchar(t$avoid_when[i])) .ab_wrap(t$avoid_when[i], "TRAP", indent = 8))
    r <- tolower(trimws(.read_line("Remembered? y = yes, n = needs review, Enter = skip, q = stop: ")))
    if (r == "q") break
    rating <- switch(r, y = , yes = "remembered", n = , no = "needs review", NA_character_)
    if (!is.na(rating)) { .ab_rate(t$id[i], rating); res[nrow(res) + 1, ] <- c(t$id[i], rating) }
  }
  rv <- sum(.ab_ratings() == "needs review")
  cat(sprintf("\nDone: %d rated in this drill. Cards marked \"needs review\" this session: %d%s\n", nrow(res), rv,
              if (rv) " - sc_answers_practice(review = TRUE) drills them." else "."))
  invisible(res)
}

.ab_ratings <- function() { r <- .ab_cache$ratings; if (is.null(r)) stats::setNames(character(0), character(0)) else r }
.ab_rate <- function(id, rating) { r <- .ab_ratings(); r[id] <- rating; .ab_cache$ratings <- r; invisible(r) }

#' @rdname sc_answers_practice
#' @export
sc_answers_progress <- function() {
  r <- .ab_ratings(); t <- .ab()$templates
  if (!length(r)) { cat("No card rated yet in this session: sc_answers_practice() starts a drill.\n"); return(invisible(data.frame(id = character(0), title = character(0), rating = character(0)))) }
  d <- data.frame(id = names(r), title = t$title[match(names(r), t$id)], rating = unname(r), stringsAsFactors = FALSE)
  d <- d[order(d$rating != "needs review", d$id), ]; rownames(d) <- NULL
  .ab_cat(sprintf("This session: %d remembered, %d need review.", sum(d$rating == "remembered"), sum(d$rating == "needs review")), "",
          sprintf("  %-13s %-32s %s", d$rating, d$id, d$title))
  invisible(d)
}

# ---------- export and corrections ----------

#' Export study cards to a Markdown file for revision
#'
#' Writes the selected cards (all fields, with the study-only notice) to a
#' Markdown file you can print or read away from R. Study use only: do not
#' bring it to the exam.
#'
#' @inheritParams sc_answers
#' @param file Path of the Markdown file to write.
#' @param answers Include the answers (default `TRUE`); `FALSE` writes a
#'   self-test sheet with the situations only.
#' @return Invisibly, the path written.
#' @examples
#' f <- tempfile(fileext = ".md")
#' sc_answers_export(f, chapter = 6, priority = "high")
#' @export
sc_answers_export <- function(file, query = NULL, chapter = NULL, category = NULL, group = NULL,
                              priority = NULL, answers = TRUE) {
  t <- .ab()$templates
  rows <- if (!is.null(query)) { r <- .ab_search(query)$rows; r[.ab_keep(chapter, category, group, priority)[r]] }
          else which(.ab_keep(chapter, category, group, priority))
  if (!length(rows)) stop("No card matches; nothing written.", call. = FALSE)
  md <- c("# statcram study cards", "", paste0("> ", .ab_study_note), "",
          sprintf("%d cards. Generated by statcram %s.", length(rows), .ab_version()), "")
  for (i in rows) {
    md <- c(md, sprintf("## %s  `%s`", t$title[i], t$id[i]), "",
            sprintf("*Ch. %d %s - %s%s*", t$chapter[i], t$chapter_title[i], t$category[i],
                    if (t$priority[i] != "normal") paste0(" - ", .ab_pri_label(t$priority[i])) else ""), "",
            paste0("**Use this when:** ", t$use_when[i]), "",
            if (answers) c(paste0("**Answer to practise:** ", t$answer_template[i]), "") else c("**Answer:** _write it yourself_", ""),
            if (answers && length(t$formulas[[i]])) c(paste0("**Formulas:** ", paste(t$formulas[[i]], collapse = " ; ")), ""),
            if (nzchar(t$avoid_when[i])) c(paste0("**Don't use when:** ", t$avoid_when[i]), ""),
            paste0("*", .ab_status(t$provenance[i]), "*"), "")
  }
  writeLines(enc2utf8(md), file, useBytes = TRUE)
  cat(sprintf("Wrote %d cards to %s (study use only).\n", length(rows), file))
  invisible(file)
}

.ab_version <- function() tryCatch(as.character(utils::packageVersion("statcram")), error = function(e) "dev")

#' Corrections made to the supplied answer bank
#'
#' Lists every change statcram made to the wording of the supplied answer
#' bank (`data-raw/answer_bank.json` in the source package), with the reason:
#' statistical corrections and the "don't use when" sentences added where the
#' source left them empty.
#'
#' @return Invisibly, a data frame of corrections (id, field, kind, reason).
#' @examples
#' sc_answers_corrections()
#' @export
sc_answers_corrections <- function() {
  cr <- .ab()$corrections
  if (!nrow(cr)) { cat("No corrections.\n"); return(invisible(cr)) }
  tab <- table(cr$kind)
  .ab_cat(sprintf("Changes to the supplied answer bank: %d (%s)", nrow(cr), paste(sprintf("%s %d", names(tab), tab), collapse = ", ")), "")
  fix <- cr[cr$kind != "added_avoid_when", ]
  if (nrow(fix)) .ab_cat(unlist(lapply(seq_len(nrow(fix)), function(k)
    .ab_wrap(sprintf("%s [%s]: %s", fix$field[k], fix$kind[k], fix$reason[k]), sprintf("  %s", fix$id[k]), indent = 32))))
  if (any(cr$kind == "added_avoid_when"))
    cat(sprintf("\n  plus %d \"don't use when\" sentences added where the source had none.\n", sum(cr$kind == "added_avoid_when")))
  invisible(cr)
}
