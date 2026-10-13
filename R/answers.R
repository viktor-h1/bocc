# ------------------------------------------------------------
# Study answer bank: authored exam-style answer templates, searchable by
# question type, situation and keyword. Study use only: the course rules
# forbid prepared scripts and AI tools at the exam.
#
# The data live in data-raw/answer_bank.json (editable source) and are
# compiled by data-raw/build_answer_bank.R into inst/extdata/answer_bank.rds,
# read here with base R. Nothing below is tied to Chapter 2: new chapters
# are new records with new ids.
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
.ab_provenance <- c(authored_exam_style_template_not_verbatim_official_answer =
                      "Authored study template in the style of the official solutions (not a verbatim official answer).")

.ab_status <- function(prov) { s <- .ab_provenance[prov]; if (is.na(s)) prov else unname(s) }

# ---------- search ----------

.ab_norm <- function(s) {
  s <- enc2utf8(as.character(s))
  s <- gsub("\u2264", "<=", s); s <- gsub("\u2265", ">=", s)
  a <- suppressWarnings(iconv(s, "UTF-8", "ASCII//TRANSLIT", sub = " "))
  s <- ifelse(is.na(a), s, a)
  s <- tolower(s)
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9%<>=/.+-]+", " ", s)))
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
                skew = "asymmetr", tail = c("skew", "tail"), width = c("wide", "width"),
                among = c("conditional", "among"), denominator = c("conditional", "denominator"))

# Searchable text of each record, by field, normalised once.
.ab_docs <- function() {
  if (is.null(.ab_cache$docs)) {
    t <- .ab()$templates
    f <- function(x) vapply(x, function(v) .ab_norm(paste(v, collapse = " ")), "")
    .ab_cache$docs <- list(title = .ab_norm(t$title), triggers = f(t$triggers), id = .ab_norm(t$id),
                           section = .ab_norm(paste(t$category, t$section)), use = .ab_norm(t$use_when),
                           avoid = .ab_norm(t$avoid_when), answer = .ab_norm(t$answer_template), formulas = f(t$formulas))
  }
  .ab_cache$docs
}
.ab_weights <- c(title = 5, triggers = 5, id = 4, section = 3, use = 2, avoid = 1, answer = 1, formulas = 1)

# Score every record against a query: phrase hits first, then all words, then
# the records matching most words. Returns the row order and how it matched.
.ab_search <- function(query) {
  q <- .ab_norm(query)
  docs <- .ab_docs(); n <- length(docs$title)
  if (!nzchar(q)) return(list(rows = seq_len(n), how = "all"))
  phrase <- rowSums(vapply(names(.ab_weights), function(f) .ab_weights[[f]] * 3 * grepl(q, docs[[f]], fixed = TRUE), numeric(n)))
  words <- unique(strsplit(q, " ", fixed = TRUE)[[1]])
  words <- words[nchar(words) >= 2 & !words %in% c("the", "of", "a", "an", "and", "or", "in", "to", "is", "for", "with")]
  if (!length(words)) words <- strsplit(q, " ", fixed = TRUE)[[1]]
  hit_word <- function(w, f) {
    keys <- unique(c(.ab_stem(w), .ab_syn[[w]]))
    Reduce(`|`, lapply(keys, function(k) grepl(k, docs[[f]], fixed = TRUE)))
  }
  wmat <- vapply(words, function(w) Reduce(`|`, lapply(names(.ab_weights), function(f) hit_word(w, f))), logical(n))
  wmat <- matrix(wmat, nrow = n)
  wscore <- rowSums(vapply(names(.ab_weights), function(f)
    .ab_weights[[f]] * rowSums(matrix(vapply(words, hit_word, logical(n), f = f), nrow = n)), numeric(n)))
  nmatch <- rowSums(wmat)
  how <- "words"
  keep <- phrase > 0 | nmatch == length(words)
  if (!any(keep) && max(nmatch) > 0) { keep <- nmatch == max(nmatch); how <- "partial" }
  if (!any(keep)) {                        # small misspellings ("histogramm", "uniformty")
    dw <- unique(unlist(strsplit(unlist(docs), " ", fixed = TRUE)))
    fz <- vapply(words, function(w) if (nchar(w) < 5) w else {
      d <- utils::adist(w, dw); if (min(d) <= max(1, floor(nchar(w) / 5))) dw[which.min(d)] else w }, "")
    if (!identical(unname(fz), words)) {
      r <- .ab_search(paste(fz, collapse = " "))
      if (length(r$rows)) r$how <- paste0("spelling:", paste(fz, collapse = " "))
      return(r)
    }
    return(list(rows = integer(0), how = "none"))
  }
  score <- phrase + wscore
  rows <- which(keep)
  if (any(phrase > 0)) how <- "phrase"
  list(rows = rows[order(-score[rows], rows)], how = how)
}

.ab_category <- function(category) {
  s <- .ab()$sections
  if (is.numeric(category)) {
    if (category < 1 || category > nrow(s)) stop(sprintf("Category number must be 1 to %d.", nrow(s)), call. = FALSE)
    return(s$id[category])
  }
  k <- .ab_norm(category)
  hit <- s$id[.ab_norm(s$id) == k]
  if (!length(hit)) hit <- s$id[grepl(k, .ab_norm(paste(s$id, s$title)), fixed = TRUE)]
  if (length(hit) != 1) stop(sprintf("Unknown category \"%s\". Categories: %s.", category, paste(s$id, collapse = ", ")), call. = FALSE)
  hit
}

.ab_scope_rows <- function(scope) {
  t <- .ab()$templates
  if (is.null(scope)) return(rep(TRUE, nrow(t)))
  scope <- match.arg(scope, c("all", "core", "crossover"))
  switch(scope, all = rep(TRUE, nrow(t)), core = t$scope == "Ch. 2 core", crossover = t$scope != "Ch. 2 core")
}

# ---------- printing ----------

# Print text; on a console that is not UTF-8, transliterate (em dash -> "-").
.ab_cat <- function(...) {
  x <- c(...)
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    x <- gsub("\u2014|\u2013", "-", x)
    a <- suppressWarnings(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "?")); x <- ifelse(is.na(a), x, a)
  }
  cat(x, sep = "\n")
}

.ab_wrap <- function(txt, label = "", indent = 17, width = getOption("width", 80)) {
  w <- max(50, min(width, 100))
  strwrap(txt, width = w, initial = formatC(label, width = -indent), exdent = indent)
}

.ab_card_lines <- function(i) {
  t <- .ab()$templates[i, ]
  w <- max(50, min(getOption("width", 80), 100))
  head <- sprintf("== %s  %s ", t$id, t$title)
  ph <- t$placeholders[[1]]; fm <- t$formulas[[1]]; src <- t$source_refs[[1]]
  status <- .ab_status(t$provenance)
  fill_eg <- if (length(ph)) sprintf("sc_answers_fill(\"%s\", values = list(%s))", t$id,
                                     paste(sprintf("`%s` = \"...\"", ph), collapse = ", "))
  c(paste0(head, strrep("=", max(3, w - nchar(head)))),
    .ab_wrap(sprintf("%s  |  %s", .ab()$sections$title[match(t$category, .ab()$sections$id)], t$scope), "", indent = 0),
    .ab_wrap(.ab_study_note, "", indent = 0), "",
    .ab_wrap(t$use_when, "USE WHEN"),
    .ab_wrap(t$avoid_when, "DO NOT USE WHEN"),
    "", "ANSWER TO STUDY", .ab_wrap(t$answer_template, "", indent = 2), "",
    if (length(fm)) .ab_wrap(paste(fm, collapse = " ;  "), "FORMULAS"),
    if (length(ph)) .ab_wrap(paste(sprintf("[%s]", ph), collapse = "  "), "BLANKS TO FILL"),
    if (length(ph)) .ab_wrap(fill_eg, ""),
    .ab_wrap(paste0(status, if (length(src)) paste0(" Related exam questions: ", paste(src, collapse = "; "), ".")), "SOURCE/STATUS"))
}

.ab_list_lines <- function(rows, header) {
  t <- .ab()$templates
  if (!length(rows)) return(header)
  c(header, unlist(lapply(seq_along(rows), function(k) {
    i <- rows[k]
    c(sprintf(" %2d  %-31s %s", k, t$id[i], t$title[i]),
      .ab_wrap(paste("use when:", t$use_when[i]), "", indent = 9))
  })))
}

.ab_categories_lines <- function(rows = NULL) {
  s <- .ab()$sections; t <- .ab()$templates
  cnt <- if (is.null(rows)) s$count else as.integer(table(factor(t$category[rows], levels = s$id)))
  c("STUDY ANSWER BANK  (authored exam-style templates; study use only)", "",
    sprintf(" %2d  %-12s %-52s %2d cases", seq_len(nrow(s)), s$id, s$title, cnt), "",
    "  sc_answers(\"words\")            search (e.g. \"unequal widths\", \"among those\", \"skew\")",
    "  sc_answers(category = \"histogram\")  or  sc_answers(category = 5)",
    "  sc_answers(id = \"C2.HISTOGRAM.UNEQUAL\")   one case in full",
    "  sc_answers_app()                window to browse, search and fill in")
}

#' Study answer bank: exam-style answer templates by question type
#'
#' A searchable bank of authored answer templates for the exam questions of
#' the first partial (Chapter 2 and the Chapter 3/4 topics that the same
#' exercises mix in). Each case shows **when it applies**, **when it does
#' not**, the **answer to study** with `[blanks]`, the formulas and the
#' related past-paper questions. The templates are written in the style of
#' the official solutions but are **not** verbatim official answers, and
#' they are for **study only**: the course rules forbid prepared R scripts
#' and AI tools at the exam. Nothing is computed: the bank never asserts a
#' conclusion about your data; pick a case only if its conditions match.
#'
#' * `sc_answers()` lists the categories with their counts.
#' * `sc_answers("words")` searches titles, triggers, conditions and
#'   templates (case- and accent-insensitive, small typos tolerated); one hit
#'   is shown in full, several are listed.
#' * `sc_answers(category = "histogram")` (or a number) lists a category.
#' * `sc_answers(id = "C2.HISTOGRAM.UNEQUAL")` shows one case in full.
#' * `sc_answers(list = TRUE)` returns the index as a data frame.
#'
#' @param query Words to search for.
#' @param category A category id (e.g. `"histogram"`), its number, or part of
#'   its title.
#' @param id A case id, e.g. `"C2.GROUPED.VARIANCE"`.
#' @param chapter Keep only cases of this chapter (e.g. `2`).
#' @param scope `"all"` (default), `"core"` (Chapter 2 only) or
#'   `"crossover"` (cases that also use Chapter 3/4 tools).
#' @param list If `TRUE`, return the matching cases as a data frame index
#'   instead of printing them.
#' @return Invisibly: the record shown (a one-row data frame) or the data
#'   frame of matching cases; with `list = TRUE`, that data frame visibly.
#' @seealso [sc_answers_fill()], [sc_answers_app()], [sc_notes()] with
#'   `"phrasing"` for the seven-move framework, [sc_define()] for definitions.
#' @examples
#' sc_answers()
#' sc_answers("unequal widths")
#' sc_answers(category = "conditional")
#' sc_answers(id = "C2.GROUPED.VARIANCE")
#' head(sc_answers(chapter = 2, list = TRUE))
#' @export
sc_answers <- function(query = NULL, category = NULL, id = NULL, chapter = NULL, scope = NULL, list = FALSE) {
  t <- .ab()$templates
  if (!is.null(id)) {
    i <- which(toupper(t$id) == toupper(trimws(id)))
    if (!length(i)) {
      near <- t$id[order(utils::adist(toupper(id), toupper(t$id)))[1:3]]
      cat(sprintf("No case with id \"%s\". Closest ids: %s.\n", id, paste(near, collapse = ", ")))
      return(invisible(t[0, ]))
    }
    if (list) return(.ab_index(i))
    .ab_cat(.ab_card_lines(i))
    return(invisible(t[i, ]))
  }
  keep <- .ab_scope_rows(scope)
  if (!is.null(chapter)) keep <- keep & t$chapter %in% chapter
  if (!is.null(category)) keep <- keep & t$category == .ab_category(category)
  if (is.null(query)) {
    rows <- which(keep)
    if (list) return(.ab_index(rows))
    if (is.null(category)) { .ab_cat(.ab_categories_lines(rows)); return(invisible(.ab_index(rows))) }
    s <- .ab()$sections[.ab()$sections$id == .ab_category(category), ]
    .ab_cat(.ab_list_lines(rows, c(sprintf("%s  (%d cases; study use only - authored templates)", s$title, length(rows)), "")),
            "", "Show one in full: sc_answers(id = \"<id>\")")
    return(invisible(.ab_index(rows)))
  }
  r <- .ab_search(query)
  rows <- r$rows[keep[r$rows]]
  if (list) return(.ab_index(rows))
  if (!length(rows)) {
    cat(sprintf("No case matches \"%s\".\n", query),
        "Try a shorter word or phrase (\"skew\", \"density\", \"modal class\", \"among those\", \"variance\"),",
        "sc_answers() for the categories, or sc_define() for the definition of a term.\n", sep = "\n")
    return(invisible(.ab_index(rows)))
  }
  if (length(rows) == 1) {
    .ab_cat(.ab_card_lines(rows))
    return(invisible(t[rows, ]))
  }
  note <- switch(sub(":.*", "", r$how), partial = " (no case has every word; showing those with most of them)",
                 spelling = sprintf(" (searched for \"%s\")", sub("^spelling:", "", r$how)), "")
  .ab_cat(.ab_list_lines(rows, c(sprintf("Search \"%s\": %d cases%s, best first.  Study use only - authored templates.",
                                         query, length(rows), note), "")),
          "", "Show one in full: sc_answers(id = \"<id>\")")
  invisible(.ab_index(rows))
}

.ab_index <- function(rows) {
  t <- .ab()$templates[rows, c("id", "chapter", "category", "section", "title", "use_when", "avoid_when", "scope")]
  rownames(t) <- NULL
  t
}

# ---------- filling the blanks ----------

# Replace bracketed blanks with the values supplied. Values are used as
# plain text (never evaluated). A blank like [a,x] or the interval [a,b)
# is filled part by part. Text in brackets with no value is left as it is.
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

#' Fill the blanks of a study answer template
#'
#' Replaces the `[blanks]` of a case of the study answer bank with the text
#' you supply and prints the result, listing any blank still to fill. Values
#' are inserted as plain text: nothing is evaluated or computed, so check
#' that the case you chose matches your data. Study use only.
#'
#' @param id A case id, e.g. `"C2.CONDITIONAL.A_HIGH"` (see [sc_answers()]).
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
  i <- which(toupper(t$id) == toupper(trimws(id)))
  if (length(i) != 1) stop(sprintf("No case with id \"%s\"; sc_answers() lists them.", id), call. = FALSE)
  vals <- .ab_values(values)
  tmpl <- t$answer_template[i]
  unknown <- setdiff(names(vals), .ab_names_in(tmpl))
  if (length(unknown))
    warning(sprintf("Not a blank of %s (ignored): %s. Its blanks: %s.", t$id[i], paste(unknown, collapse = ", "),
                    paste(sprintf("[%s]", t$placeholders[[i]]), collapse = " ")), call. = FALSE)
  out <- .ab_fill_text(tmpl, vals)
  filled <- names(vals)[nzchar(unlist(vals))]
  unf <- Filter(function(p) !all(trimws(strsplit(p, ",", fixed = TRUE)[[1]]) %in% filled), t$placeholders[[i]])
  attr(out, "unfilled") <- unf; attr(out, "id") <- t$id[i]
  if (print) {
    w <- max(50, min(getOption("width", 80), 100))
    .ab_cat(sprintf("%s  %s  (study use only - authored template, not an official solution)", t$id[i], t$title[i]), "",
            strwrap(out, width = w), "")
    if (length(unf)) cat(sprintf("STILL TO FILL: %s\n", paste(sprintf("[%s]", unf), collapse = "  ")))
    else cat("All blanks filled. Check that the case's conditions hold for your data:",
             .ab_wrap(t$use_when[i], "  use when:", indent = 13), sep = "\n")
  }
  invisible(out)
}
