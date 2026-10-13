# Builds inst/extdata/answer_bank.rds from
#   data-raw/answer_bank.json              the answer bank (schema 2.x, 244 cards)
#   data-raw/answer_bank_corrections.json  documented corrections applied on top
# Run from the package root after editing either file:
#   Rscript data-raw/build_answer_bank.R
# jsonlite is needed only here, not when the package runs.
# Adding cards: append templates with new unique ids; chapters, categories and
# question-wording groups are taken from the data and the tables below.

j <- jsonlite::fromJSON("data-raw/answer_bank.json", simplifyVector = FALSE)
corr <- if (file.exists("data-raw/answer_bank_corrections.json"))
  jsonlite::fromJSON("data-raw/answer_bank_corrections.json", simplifyVector = FALSE) else list()

req <- c("id", "chapter", "category", "title", "use_when", "answer_template", "avoid_when", "triggers",
         "formulas", "source_refs", "provenance", "placeholders", "priority", "related_chapters", "exam_scope")
ids <- vapply(j$templates, `[[`, "", "id")
stopifnot(!anyDuplicated(ids))
for (t in j$templates) {
  miss <- setdiff(req, names(t)); if (length(miss)) stop(t$id, ": missing ", paste(miss, collapse = ", "))
}

# ---- corrections: each one names the id, the field, the text it replaces and why ----
log <- list()
for (k in seq_along(corr)) {
  cr <- corr[[k]]; i <- match(cr$id, ids)
  if (is.na(i)) stop("correction for unknown id ", cr$id)
  old <- j$templates[[i]][[cr$field]]
  if (!identical(unlist(old), unlist(cr$from))) stop("correction ", cr$id, "/", cr$field, ": 'from' does not match the source text")
  j$templates[[i]][[cr$field]] <- if (cr$field %in% c("formulas", "triggers", "placeholders")) as.list(unlist(cr$to)) else cr$to
  log[[k]] <- data.frame(id = cr$id, field = cr$field, kind = cr$kind, reason = cr$reason, stringsAsFactors = FALSE)
}
corrections <- if (length(log)) do.call(rbind, log) else
  data.frame(id = character(0), field = character(0), kind = character(0), reason = character(0))
for (t in j$templates)
  for (f in c("id", "category", "title", "use_when", "answer_template", "provenance"))
    if (!nzchar(t[[f]])) stop(t$id, ": empty ", f)

chr <- function(f) vapply(j$templates, function(t) if (is.null(t[[f]])) "" else as.character(t[[f]]), "")
lst <- function(f) lapply(j$templates, function(t) as.character(unlist(t[[f]])))
templates <- data.frame(id = ids, chapter = vapply(j$templates, function(t) as.integer(t$chapter), 1L),
                        chapter_title = chr("chapter_title"), category = chr("category"), section = chr("section"),
                        title = chr("title"), use_when = chr("use_when"), avoid_when = chr("avoid_when"),
                        answer_template = chr("answer_template"), provenance = chr("provenance"),
                        priority = chr("priority"), source_support = chr("source_support"), exam_scope = chr("exam_scope"),
                        stringsAsFactors = FALSE)
templates$triggers <- lst("triggers"); templates$formulas <- lst("formulas")
templates$source_refs <- lapply(lst("source_refs"), function(s) unique(sub("^syllabus.*", "SYL", s)))
templates$placeholders <- lapply(lst("placeholders"), unique)
templates$related_chapters <- lapply(j$templates, function(t) as.integer(unlist(t$related_chapters)))
templates$corrected <- templates$id %in% corrections$id[corrections$kind %in% c("error", "misleading", "minor")]
templates$avoid_added <- templates$id %in% corrections$id[corrections$kind == "added_avoid_when"]
stopifnot(all(templates$exam_scope == "2026_27_first_partial"), all(templates$priority %in% c("critical", "high", "normal")))

# ---- chapters and categories (a category is unique within a chapter) ----
chapters <- data.frame(number = vapply(j$chapters, function(x) as.integer(x$number), 1L),
                       title = vapply(j$chapters, `[[`, "", "title"), stringsAsFactors = FALSE)
chapters$count <- as.integer(table(factor(templates$chapter, levels = chapters$number)))
cat_labels <- c(
  "1.data" = "Variables, data and sampling",
  "3.location" = "Location: mean, median, mode", "3.spread" = "Dispersion: range, IQR, variance, SD, CV",
  "3.quantiles" = "Quartiles and percentiles", "3.outliers" = "Boxplots and outliers",
  "4.joint" = "Joint distributions", "4.conditional" = "Conditional distributions and percentages",
  "4.independence" = "Independence (descriptive)", "4.comparison" = "A numerical variable across groups",
  "4.relationship" = "Scatterplots, covariance, correlation",
  "5.events" = "Events, conditional probability, Bayes", "5.rv" = "Random variables: distribution, E, Var",
  "5.transform" = "Transformations and linear combinations", "5.normal" = "Normal distribution",
  "5.sampling" = "Sampling distributions and CLT",
  "6.estimator" = "Estimators, unbiasedness, standard errors", "6.ci" = "Confidence intervals (one mean, one proportion)",
  "6.samplesize" = "Margin of error and sample size")
key <- paste(templates$chapter, templates$category, sep = ".")
ukey <- unique(key)
categories <- data.frame(key = ukey, chapter = as.integer(sub("\\..*", "", ukey)), category = sub("^[0-9]+\\.", "", ukey),
                         stringsAsFactors = FALSE)
categories$label <- vapply(seq_along(ukey), function(i) {
  if (!is.na(cat_labels[ukey[i]])) return(unname(cat_labels[ukey[i]]))
  s <- templates$section[match(ukey[i], key)]
  if (nzchar(s)) sub("^[0-9.]+ [^ ]+ ", "", s, useBytes = TRUE) else categories$category[i]
}, "")
categories$count <- as.integer(table(factor(key, levels = ukey)))
categories <- categories[order(categories$chapter, match(categories$key, ukey)), ]
rownames(categories) <- NULL

# ---- "start here": entry points by the wording of the exam question ----
groups <- list(
  list(id = "graph", label = "Which graph / describe / sketch", keys = c("1.data", "2.graph", "2.shape")),
  list(id = "compare", label = "Compare locations, shapes and dispersions", keys = c("2.compare", "2.center", "2.conclusion", "3.spread", "4.comparison")),
  list(id = "percent", label = "Calculate a percentage / \"among those who\"", keys = c("2.share", "2.cumulative", "2.conditional", "4.conditional", "4.joint")),
  list(id = "quantile", label = "Find a median, percentile or outlier", keys = c("3.location", "3.quantiles", "3.outliers")),
  list(id = "grouped", label = "Grouped data and histograms", keys = c("2.histogram", "2.modal", "2.grouped")),
  list(id = "relation", label = "Relationship between two numerical variables", keys = c("4.relationship")),
  list(id = "probability", label = "Probability / Bayes / independence", keys = c("5.events", "4.independence"), weak = TRUE),
  list(id = "rv", label = "Random variables: E, Var, transformations", keys = c("5.rv", "5.transform")),
  list(id = "clt", label = "Normal / sampling distribution / CLT", keys = c("5.normal", "5.sampling")),
  list(id = "estimator", label = "Estimator / standard error / unbiasedness", keys = c("6.estimator"), weak = TRUE),
  list(id = "ci", label = "Confidence intervals / sample size", keys = c("6.ci", "6.samplesize")))
stopifnot(setequal(unlist(lapply(groups, `[[`, "keys")), ukey), !anyDuplicated(unlist(lapply(groups, `[[`, "keys"))))
groups <- data.frame(id = vapply(groups, `[[`, "", "id"), label = vapply(groups, `[[`, "", "label"),
                     weak = vapply(groups, function(g) isTRUE(g$weak), TRUE), stringsAsFactors = FALSE,
                     keys = I(lapply(groups, `[[`, "keys")))
groups$count <- vapply(groups$keys, function(k) sum(key %in% k), 1L)

# ---- sources ----
sources <- data.frame(
  key = c("M1", "P24", "P25", "S26JAN", "G26JAN", "G26JUL", "G26SEP", "FR", "AG", "SYL"),
  label = c("Module 1 exercises (official solutions)", "1st partial, 22 October 2024", "1st partial, 14 October 2025",
            "2nd partial, 9 January 2026 (its one-mean / one-proportion CI parts are 1st-partial material in 2026/27)",
            "General exam, 30 January 2026", "General exam, 3 July 2026", "General exam, 1 September 2026",
            "Course framework (book notation and procedures)", "Answer-phrasing guide (official answer style)",
            "Syllabus 2026/27"),
  past_paper = c(rep(TRUE, 7), FALSE, FALSE, FALSE), stringsAsFactors = FALSE)
stopifnot(all(unlist(templates$source_refs) %in% sources$key))

# ---- verbatim questions and official solution quotes (exact text, untouched) ----
vq <- j$verbatim_question_index
questions <- data.frame(id = vapply(vq, `[[`, "", "id"), source = vapply(vq, `[[`, "", "source_key"),
                        line = vapply(vq, function(q) as.integer(q$transcription_line), 1L),
                        topic = vapply(vq, `[[`, "", "topic"), pattern = vapply(vq, `[[`, "", "pattern"),
                        text = vapply(vq, `[[`, "", "verbatim_question_markdown"),
                        completeness = vapply(vq, `[[`, "", "quote_completeness"), stringsAsFactors = FALSE)
questions$cards <- lapply(seq_len(nrow(questions)), function(i) {
  same <- templates$category == questions$topic[i]
  both <- same & vapply(templates$source_refs, function(s) questions$source[i] %in% s, TRUE)
  templates$id[if (any(both)) both else same]
})
# line ranges of the exams in Stats_past_papers_verbatim.md (headings), to label each official quote
exam_lines <- data.frame(from = c(14, 781, 902, 1004, 1994, 2135, 2275, 2408, 2578, 2725),
                         key = c("M1", "P25", "P24", "OLD", "S26JAN", "OLD", "OLD", "G26JAN", "G26JUL", "G26SEP"))
oq <- j$official_solution_quote_index
quotes <- data.frame(line = vapply(oq, function(q) as.integer(q$line), 1L),
                     text = vapply(oq, `[[`, "", "quote_markdown_verbatim"), stringsAsFactors = FALSE)
quotes$source <- vapply(quotes$line, function(l) exam_lines$key[max(which(exam_lines$from <= l))], "")
stopifnot(!any(quotes$source == "OLD"), all(questions$source %in% sources$key))

bank <- list(schema_version = j$schema_version, name = j$name, purpose = j$purpose,
             integrity = j$academic_integrity_rule, provenance_notice = j$template_provenance_notice,
             exam_scope = j$exam_scope, seven_moves = unlist(j$seven_moves),
             transcription = "Stats_past_papers_verbatim.md (the student's transcription of the past papers)",
             chapters = chapters, categories = categories, groups = groups, sources = sources,
             templates = templates, questions = questions, quotes = quotes, corrections = corrections)
saveRDS(bank, "inst/extdata/answer_bank.rds", version = 2)
cat(sprintf("Saved %d cards (%s), %d categories, %d questions, %d quotes; %d corrections applied\n",
            nrow(templates), paste(chapters$count, collapse = "/"), nrow(categories), nrow(questions), nrow(quotes), nrow(corrections)))
