# ------------------------------------------------------------
# Every procedure returns an "sc_result": a list with the numbers
# (statistic, p_value, ci, ...) plus the text blocks that are printed:
# title, step lines, notes, exam wording and the re-run call.
# ------------------------------------------------------------

# Formals are dot-prefixed so result fields passed in `...` (e.g. n = 40)
# can never partially match them.
.result <- function(.title, .lines, .wording = NULL, .notes = NULL, .call = NULL, ...) {
  structure(c(list(title = .title, lines = .lines, wording = .wording, notes = .notes,
                   call = .call_text(.call)), list(...)),
            class = "sc_result")
}

# Capture a printed table (data.frame or matrix) as indented lines.
.table_lines <- function(tab, row.names = FALSE, indent = "  ") {
  old <- options(width = max(getOption("width", 80), 120)); on.exit(options(old))
  out <- utils::capture.output(print(tab, row.names = row.names))
  paste0(indent, out)
}

# Round numeric columns of a data.frame for display.
.round_df <- function(df, d = .digits()) {
  for (nm in names(df)) if (is.numeric(df[[nm]])) df[[nm]] <- round(df[[nm]], d)
  df
}

# Exam wording tagged by "move" (see sc_notes("phrasing")): names of the
# wording vector. Printed in this order, each move labelled once.
.moves <- c(frame = "Frame", tool = "Tool", result = "Result", meaning = "Meaning",
            conclusion = "Conclusion", caveat = "Caveat", note = "Note")

# Tag sentences with a move: .w("tool", "sentence", NULL, ...) -> named vector.
.w <- function(move, ...) {
  s <- unlist(list(...), use.names = FALSE)
  s <- s[!is.na(s) & nzchar(s)]
  if (!length(s)) return(NULL)
  stats::setNames(s, rep(move, length(s)))
}

# Order tagged wording by move; untagged sentences count as "meaning".
.order_wording <- function(wd) {
  nm <- names(wd)
  if (is.null(nm) || all(!nzchar(nm))) return(list(text = unname(wd), label = rep("", length(wd))))
  nm[!nzchar(nm) | !nm %in% names(.moves)] <- "meaning"
  o <- order(match(nm, names(.moves)))
  nm <- nm[o]; txt <- unname(wd)[o]
  lab <- ifelse(!duplicated(nm), unname(.moves[nm]), "")
  list(text = txt, label = lab)
}

#' Print a statcram result
#'
#' Results print as: the hypotheses / inputs, each formula with the numbers
#' plugged in, the decision, an exam-wording paragraph and the one-line call
#' that re-runs the procedure. The exam wording follows the moves of the
#' official answers (Frame, Tool, Result, Meaning, Conclusion, Caveat; see
#' `sc_notes("phrasing")`); `options(statcram.labels = FALSE)` hides the labels. Descriptive results, intervals and tests also
#' show the matching call of the course package UBStats (chi-square tests:
#' base R `chisq.test()`). Turn parts off with
#' `options(statcram.wording = FALSE)`, `options(statcram.plot = FALSE)` or
#' `options(statcram.ubstats = FALSE)`;
#' change decimals with `options(statcram.digits = 6)`.
#'
#' @param x An `sc_result`.
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.sc_result <- function(x, ...) {
  w <- .width()
  cat("\n", .rule(paste0("== ", x$title, " "), w, "="), "\n", sep = "")
  if (length(x$lines)) cat(paste(x$lines, collapse = "\n"), "\n", sep = "")
  if (length(x$notes)) cat("\n", paste0("NOTE: ", x$notes, collapse = "\n"), "\n", sep = "")
  if (length(x$wording) && isTRUE(.opt("wording", TRUE))) {
    cat("\n", .rule("-- Exam wording ", w), "\n", sep = "")
    ow <- .order_wording(x$wording)
    labels <- isTRUE(.opt("labels", TRUE))
    for (i in seq_along(ow$text)) {
      if (i > 1) cat("\n")
      txt <- if (labels && nzchar(ow$label[i])) paste0(ow$label[i], ": ", ow$text[i]) else ow$text[i]
      cat(strwrap(txt, width = w), sep = "\n")
    }
  }
  if (length(x$ubstats) && isTRUE(.opt("ubstats", TRUE))) {
    cat(.rule("-- UBStats (course package): the call to practise ", w), "\n", sep = "")
    cat(x$ubstats, sep = "\n")
  }
  if (length(x$rbase) && isTRUE(.opt("ubstats", TRUE))) {
    cat(.rule("-- Base R (as in the book) ", w), "\n", sep = "")
    cat(x$rbase, sep = "\n")
  }
  if (!is.null(x$call)) {
    cat(.rule("-- Re-run ", w), "\n", sep = "")
    cat(x$call, "\n", sep = "")
  }
  invisible(x)
}
