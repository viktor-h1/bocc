# ------------------------------------------------------------
# Phrasing bank: the student's own "Stats30001 Exact Phrasing Bank"
# (167 source-adapted answer cases, 10 verbatim solution excerpts, a
# personal error log), shipped unchanged as inst/notes/phrasing-bank.md
# and parsed here on first use. Study use only: the course rules forbid
# prepared R scripts and AI tools at the exam.
# ------------------------------------------------------------

.pb_cache <- new.env(parent = emptyenv())

.pb_study_note <- "STUDY USE ONLY - source-adapted templates from your phrasing bank, not official solutions (only the Appendix B excerpts are verbatim). The 2026/27 rules forbid prepared R scripts and AI tools at the exam: learn the wording beforehand."

.pb <- function() {
  if (is.null(.pb_cache$bank)) {
    p <- .notes_path("phrasing-bank.md")
    if (!nzchar(p)) stop("The phrasing bank file is missing; reinstall statcram.", call. = FALSE)
    .pb_cache$bank <- .pb_parse(readLines(p, warn = FALSE, encoding = "UTF-8"))
  }
  .pb_cache$bank
}

# ---------- parser ----------

# Text after "**Label:** " on the lines that start with it.
.pb_field <- function(x, label) {
  pre <- paste0("**", label, ":** ")
  v <- x[startsWith(x, pre)]
  if (!length(v)) "" else paste(substring(v, nchar(pre) + 1), collapse = " ")
}

# "**Key:** text" lines -> data frame.
.pb_pairs <- function(x) {
  m <- regmatches(x, regexec("^\\*\\*(.+?):\\*\\* (.*)$", x, perl = TRUE))
  m <- m[lengths(m) == 3]
  data.frame(key = vapply(m, `[`, "", 2), text = vapply(m, `[`, "", 3), stringsAsFactors = FALSE)
}

.pb_bullets <- function(x) substring(x[startsWith(x, "- ")], 3)

# Split lines into blocks starting at lines that begin with `prefix`.
.pb_blocks <- function(x, prefix) {
  h <- which(startsWith(x, prefix))
  if (!length(h)) return(list())
  lapply(seq_along(h), function(k) x[h[k]:(if (k < length(h)) h[k + 1] - 1 else length(x))])
}

.pb_is_text <- function(x) nzchar(trimws(x)) & !startsWith(x, "<a id=")

.pb_case <- function(x, sec) {
  m <- regmatches(x[1], regexec("^### ([0-9][0-9A-Z]*\\.[0-9]+) \u2014 (.*)$", x[1]))[[1]]
  if (length(m) != 3) stop("Unexpected case heading: ", x[1], call. = FALSE)
  body <- x[-1]
  known <- startsWith(body, "**Use when:** ") | startsWith(body, "**Formula / R:** ") | startsWith(body, "**Avoid:** ") |
           startsWith(body, "**Qualification:** ") | body == "**Write:**" | startsWith(body, "> ") |
           grepl("^\\*Sources: .*\\*$", body) | !.pb_is_text(body)
  src <- body[grepl("^\\*Sources: .*\\*$", body)]
  data.frame(id = m[2], section = sec, title = m[3],
             use_when = .pb_field(body, "Use when"),
             write = paste(substring(body[startsWith(body, "> ")], 3), collapse = "\n"),
             formula = .pb_field(body, "Formula / R"),
             avoid = .pb_field(body, "Avoid"),
             qualification = .pb_field(body, "Qualification"),
             sources = if (length(src)) sub("^\\*Sources: (.*)\\*$", "\\1", src[1]) else "",
             unparsed = paste(body[!known], collapse = "\n"),
             stringsAsFactors = FALSE)
}

# Expand "6E.01-03; 5P.09-11" (en dash in the file) into case ids.
.pb_revisit_ids <- function(r) {
  parts <- trimws(unlist(strsplit(r, "[;,]")))
  unlist(lapply(parts, function(p) {
    m <- regmatches(p, regexec("^([0-9][0-9A-Z]*)\\.([0-9]+)(?:[\u2013-]([0-9]+))?$", p, perl = TRUE))[[1]]
    if (length(m) != 4) return(character(0))
    to <- if (nzchar(m[4])) as.integer(m[4]) else as.integer(m[3])
    sprintf("%s.%02d", m[2], seq(as.integer(m[3]), to))
  }))
}

.pb_parse <- function(L) {
  L <- enc2utf8(L)
  out <- list(title = sub("^# ", "", L[1]), counts = .ab_plain(L[3]), disclaimer = L[5])
  secs <- list(); cases <- list()
  for (blk in .pb_blocks(L, "## ")) {
    name <- sub("^## ", "", blk[1]); body <- blk[-1]
    if (name == "Scope and source rules") out$scope <- .pb_bullets(body)
    else if (name == "Read the verb first") out$verbs <- .pb_pairs(body)
    else if (name == "Notation you must not mix up") out$notation <- .pb_pairs(body)
    else if (name == "Sources") out$sources <- .pb_pairs(body)
    else if (name == "Source fidelity and limits") out$fidelity <- .pb_bullets(body)
    else if (startsWith(name, "Appendix B")) {
      first <- which(startsWith(body, "### "))[1]
      out$excerpts_intro <- paste(body[seq_len(first - 1)][.pb_is_text(body[seq_len(first - 1)])], collapse = " ")
      out$excerpts <- do.call(rbind, lapply(.pb_blocks(body, "### "), function(b) {
        m <- regmatches(b[1], regexec("^### (Q[0-9]+) \\| (.*)$", b[1]))[[1]]
        data.frame(id = m[2], title = m[3], source = .pb_field(b, "Source"),
                   quote = paste(substring(b[startsWith(b, "> ")], 3), collapse = "\n"), stringsAsFactors = FALSE)
      }))
    } else if (startsWith(name, "Appendix C")) {
      out$mistakes <- do.call(rbind, lapply(.pb_blocks(body, "### "), function(b) {
        txt <- b[-1][.pb_is_text(b[-1]) & !startsWith(b[-1], "**Revisit:** ")]
        rv <- .pb_field(b, "Revisit")
        data.frame(title = sub("^### ", "", b[1]), text = paste(txt, collapse = " "), revisit = rv,
                   ids = paste(.pb_revisit_ids(rv), collapse = " "), stringsAsFactors = FALSE)
      }))
    } else if (grepl("^(Chapter|Appendix A) ", name) && grepl("|", name, fixed = TRUE)) {
      cb <- .pb_blocks(body, "### ")
      first <- which(startsWith(body, "### "))[1]
      intro <- body[seq_len(first - 1)]
      cs <- do.call(rbind, lapply(cb, .pb_case, sec = ""))
      code <- unique(sub("\\..*$", "", cs$id))
      if (length(code) != 1) stop("A section mixes case prefixes: ", name, call. = FALSE)
      cs$section <- code
      cases[[length(cases) + 1]] <- cs
      secs[[length(secs) + 1]] <- data.frame(
        code = code, label = trimws(sub("\\|.*$", "", name)), title = trimws(sub("^[^|]*\\|", "", name)),
        intro = paste(intro[.pb_is_text(intro) & !startsWith(intro, "**Basis:** ")], collapse = " "),
        basis = .pb_field(intro, "Basis"), stringsAsFactors = FALSE)
    }
  }
  out$sections <- do.call(rbind, secs)
  out$cases <- do.call(rbind, cases)
  out
}

# ---------- display ----------

.pb_md <- function(x) gsub("`", "", .ab_plain(x), fixed = TRUE)

.pb_find <- function(id) {
  id <- toupper(trimws(id))
  m <- regmatches(id, regexec("^([0-9][0-9A-Z]*)\\.([0-9]{1,2})$", id))[[1]]
  if (length(m) == 3) id <- sprintf("%s.%02d", m[2], as.integer(m[3]))
  which(.pb()$cases$id == id)
}

.pb_section_keys <- function(section) {
  secs <- .pb()$sections
  s <- toupper(trimws(as.character(section)))
  if (s %in% secs$code) return(s)
  if (grepl("^[1-6]$", s)) return(secs$code[substr(secs$code, 1, 1) == s])
  hit <- grepl(.ab_norm(section), .ab_norm(paste(secs$label, secs$title)), fixed = TRUE)
  if (!any(hit)) stop(sprintf("Unknown section \"%s\". Sections: %s.", section, paste(secs$code, collapse = ", ")), call. = FALSE)
  secs$code[hit]
}

.pb_docs <- function() {
  if (is.null(.pb_cache$docs)) {
    b <- .pb(); cs <- b$cases
    sl <- b$sections[match(cs$section, b$sections$code), ]
    .pb_cache$docs <- list(title = .ab_norm(cs$title), id = .ab_norm(cs$id), use = .ab_norm(cs$use_when),
                           section = .ab_norm(paste(sl$label, sl$title)), write = .ab_norm(cs$write),
                           formula = .ab_norm(cs$formula), avoid = .ab_norm(cs$avoid),
                           qualification = .ab_norm(cs$qualification), sources = .ab_norm(cs$sources))
  }
  .pb_cache$docs
}
.pb_weights <- c(title = 5, id = 4, use = 3, section = 3, write = 1, formula = 2, avoid = 1, qualification = 1, sources = 1)
.pb_search <- function(query) .ab_rank(query, .pb_docs(), .pb_weights)

.pb_case_lines <- function(i) {
  b <- .pb(); cs <- b$cases; s <- b$sections[b$sections$code == cs$section[i], ]
  mk <- b$mistakes[vapply(strsplit(b$mistakes$ids, " ", fixed = TRUE), function(v) cs$id[i] %in% v, logical(1)), ]
  same <- which(cs$section == cs$section[i]); k <- match(i, same)
  nb <- c(if (k > 1) sprintf("<< %s %s", cs$id[same[k - 1]], cs$title[same[k - 1]]),
          if (k < length(same)) sprintf("%s %s >>", cs$id[same[k + 1]], cs$title[same[k + 1]]))
  c(sprintf("%s  %s", cs$id[i], cs$title[i]),
    sprintf("%s | %s", s$label, s$title),
    .ab_wrap(.pb_study_note, "", indent = 0), "",
    .ab_wrap(.pb_md(cs$use_when[i]), "USE WHEN"), "",
    "WRITE",
    unlist(lapply(strsplit(cs$write[i], "\n", fixed = TRUE)[[1]], function(w) .ab_wrap(.pb_md(w), "", indent = 2))),
    "",
    if (nzchar(cs$formula[i])) .ab_wrap(.pb_md(cs$formula[i]), "FORMULA / R"),
    if (nzchar(cs$avoid[i])) .ab_wrap(.pb_md(cs$avoid[i]), "AVOID"),
    if (nzchar(cs$qualification[i])) .ab_wrap(.pb_md(cs$qualification[i]), "QUALIFICATION"),
    if (nrow(mk)) unlist(lapply(seq_len(nrow(mk)), function(r)
      .ab_wrap(paste0(.pb_md(mk$title[r]), " ", .pb_md(mk$text[r])), "YOUR ERROR LOG"))),
    .ab_wrap(cs$sources[i], "SOURCES"),
    if (length(nb)) c("", paste(nb, collapse = "    ")))
}

.pb_list_lines <- function(rows, header) {
  cs <- .pb()$cases
  c(header, unlist(lapply(seq_along(rows), function(k) {
    i <- rows[k]
    c(sprintf(" %3d  %-6s %s", k, cs$id[i], cs$title[i]),
      .ab_wrap(paste("use when:", .pb_md(cs$use_when[i])), "", indent = 12))
  })))
}

.pb_start_lines <- function() {
  b <- .pb(); s <- b$sections
  n <- vapply(s$code, function(k) sum(b$cases$section == k), 1L)
  c(sprintf("PHRASING BANK  (%s)", .pb_md(b$counts)),
    .ab_wrap(.pb_study_note, "", indent = 0), "",
    "SECTIONS                                                         cases",
    sprintf("  %-4s %-58s %3d", s$code, paste(s$label, "|", s$title), n), "",
    "MORE",
    sprintf("  %-34s %s", c("sc_phrasing(what = \"verbs\")", "sc_phrasing(what = \"notation\")",
                            "sc_phrasing(what = \"excerpts\")", "sc_phrasing(what = \"mistakes\")",
                            "sc_phrasing(what = \"scope\")", "sc_phrasing(what = \"sources\")", "sc_notes(\"bank\")"),
            c(sprintf("read the verb first (%d question verbs)", nrow(b$verbs)), "notation you must not mix up",
              sprintf("%d verbatim official-solution excerpts", nrow(b$excerpts)),
              sprintf("your personal error log (%d)", nrow(b$mistakes)), "scope rules and source-fidelity notes",
              "what the source keys (F, B1-B6, M1, P24, ...) mean", "the whole file as written")), "",
    "PLAY",
    "  sc_phrasing_game()                 type your answer; score, missing crucial verbs flagged",
    "  sc_phrasing_score(\"6I.03\", \"...\")   score one answer you wrote",
    "",
    "OPEN A CASE",
    "  sc_phrasing(\"qnorm\")   sc_phrasing(section = \"5N\")   sc_phrasing(\"5N.05\")")
}

.pb_pair_lines <- function(d) unlist(lapply(seq_len(nrow(d)), function(r)
  c(.pb_md(d$key[r]), .ab_wrap(.pb_md(d$text[r]), "", indent = 4), "")))

.pb_other_lines <- function(what) {
  b <- .pb()
  switch(what,
    verbs = c("READ THE VERB FIRST", "", .pb_pair_lines(b$verbs)),
    notation = c("NOTATION YOU MUST NOT MIX UP", "", .pb_pair_lines(b$notation)),
    sources = c("SOURCE KEYS", "", .pb_pair_lines(b$sources)),
    scope = c("SCOPE AND SOURCE RULES", unlist(lapply(b$scope, function(x) .ab_wrap(.pb_md(x), "-", indent = 2))), "",
              "SOURCE FIDELITY AND LIMITS", unlist(lapply(b$fidelity, function(x) .ab_wrap(.pb_md(x), "-", indent = 2)))),
    excerpts = c("VERBATIM SOLUTION EXCERPTS (Appendix B of your bank; wording unchanged, lines re-wrapped)",
                 .ab_wrap(.pb_md(b$excerpts_intro), "", indent = 0), "",
                 unlist(lapply(seq_len(nrow(b$excerpts)), function(r) {
                   e <- b$excerpts[r, ]
                   c(sprintf("%s | %s   [%s]", e$id, e$title, e$source),
                     unlist(lapply(strsplit(e$quote, "\n", fixed = TRUE)[[1]], function(q) .ab_wrap(q, "", indent = 4))), "")
                 }))),
    mistakes = c("YOUR PERSONAL ERROR LOG", "",
                 unlist(lapply(seq_len(nrow(b$mistakes)), function(r) {
                   m <- b$mistakes[r, ]
                   c(.pb_md(m$title), .ab_wrap(.pb_md(m$text), "", indent = 4), sprintf("    Revisit: %s", m$revisit), "")
                 }))))
}

#' Your phrasing bank: exam answer wording by question type (study use only)
#'
#' Search and read the "Stats30001 Exact Phrasing Bank": 167
#' source-adapted answer cases for the first partial (Chapters 1-6, with
#' Chapter 5 split into probability, random variables, Normal and sampling
#' sections), 10 verbatim official-solution excerpts and a personal error
#' log. Each case shows when it applies, the sentence to write, the formula
#' or R call, what to avoid, qualifications, matching entries from the error
#' log and its sources. The file is shipped unchanged; `sc_notes("bank")`
#' prints it as written.
#'
#' **Study use only.** The templates are source-adapted, not official
#' solutions (only the Appendix B excerpts are verbatim). The 2026/27 course
#' rules forbid R scripts prepared before the exam and AI tools during it:
#' use statcram to learn the wording beforehand, not in the exam. Being able
#' to install statcram in RStudio does not make it permitted there.
#'
#' @param query Words to search for (e.g. `"qnorm"`, `"t wider than z"`), or
#'   a case id such as `"5N.05"`.
#' @param section A section code (`"1"`, `"2"`, `"3"`, `"4"`, `"5P"`, `"5R"`,
#'   `"5N"`, `"5S"`, `"6E"`, `"6I"`, `"6N"`, `"6X"`), a chapter number (5
#'   gives all four Chapter 5 sections) or words from a section title.
#' @param id A case id, e.g. `"6I.13"`.
#' @param what `"cases"` (default), or one of the other parts of the bank:
#'   `"verbs"`, `"notation"`, `"excerpts"`, `"mistakes"`, `"scope"`,
#'   `"sources"`.
#' @param list If `TRUE`, return the matching cases as a data frame without
#'   printing.
#' @return Invisibly, a data frame of the matching cases (id, section, title,
#'   use_when); with `list = TRUE`, that data frame visibly.
#' @examples
#' sc_phrasing()                    # sections and commands
#' sc_phrasing("qnorm")             # search
#' sc_phrasing("5N.05")             # one case in full
#' sc_phrasing(section = "6I")      # all cases of a section
#' sc_phrasing(what = "mistakes")   # your error log
#' @export
sc_phrasing <- function(query = NULL, section = NULL, id = NULL,
                        what = c("cases", "verbs", "notation", "excerpts", "mistakes", "scope", "sources"),
                        list = FALSE) {
  what <- match.arg(what)
  if (what != "cases") {
    lines <- .pb_other_lines(what)
    if (!list) .ab_cat(lines)
    return(invisible(lines))
  }
  cs <- .pb()$cases
  if (is.null(id) && !is.null(query) && grepl("^\\s*[0-9][0-9A-Za-z]*\\.[0-9]{1,2}\\s*$", query)) { id <- query; query <- NULL }
  how <- "all"
  if (!is.null(id)) {
    rows <- unlist(lapply(id, .pb_find))
    if (!length(rows)) stop(sprintf("No case with id \"%s\". Use sc_phrasing() to see the sections.", paste(id, collapse = ", ")), call. = FALSE)
  } else {
    rows <- seq_len(nrow(cs))
    if (!is.null(section)) rows <- rows[cs$section[rows] %in% .pb_section_keys(section)]
    if (!is.null(query) && nzchar(trimws(query))) {
      r <- .pb_search(query); how <- r$how
      rows <- r$rows[r$rows %in% rows]
    }
  }
  out <- cs[rows, c("id", "section", "title", "use_when"), drop = FALSE]
  rownames(out) <- NULL
  if (list) return(out)
  if (is.null(query) && is.null(section) && is.null(id)) { .ab_cat(.pb_start_lines()); return(invisible(out)) }
  if (!length(rows)) {
    cat(sprintf("No case matches \"%s\". Try a shorter word, or sc_phrasing() for the sections.\n", query %||% section))
    return(invisible(out))
  }
  if (length(rows) == 1 || !is.null(id)) {
    for (i in rows) .ab_cat(.pb_case_lines(i), "")
    return(invisible(out))
  }
  header <- c(sprintf("%d cases%s (study use only - source-adapted templates):", length(rows),
                      if (!is.null(query)) sprintf(" match \"%s\"", query) else ""),
              if (startsWith(how, "spelling:")) sprintf("(no exact match - showing results for \"%s\")", sub("^spelling:", "", how)),
              "")
  .ab_cat(.pb_list_lines(rows, header), "", sprintf("One case in full: sc_phrasing(\"%s\")", cs$id[rows[1]]))
  invisible(out)
}
