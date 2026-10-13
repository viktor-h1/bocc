# Builds inst/extdata/answer_bank.rds from data-raw/answer_bank.json.
# Run from the package root after editing the JSON:
#   Rscript data-raw/build_answer_bank.R
# jsonlite is needed only here, not when the package runs.
# Adding a chapter: append templates with new unique ids (e.g. "C3....")
# and, if needed, sections; nothing in R/answers.R is tied to Chapter 2.

j <- jsonlite::fromJSON("data-raw/answer_bank.json", simplifyVector = FALSE)

req <- c("id", "chapter", "section", "category", "title", "use_when", "answer_template",
         "avoid_when", "triggers", "formulas", "source_refs", "provenance", "placeholders")
ids <- vapply(j$templates, `[[`, "", "id")
stopifnot(!anyDuplicated(ids))
for (t in j$templates) {
  miss <- setdiff(req, names(t))
  if (length(miss)) stop(t$id, ": missing ", paste(miss, collapse = ", "))
  for (f in c("id", "section", "category", "title", "use_when", "answer_template", "avoid_when", "provenance"))
    if (!nzchar(t[[f]])) stop(t$id, ": empty ", f)
}
sec_ids <- vapply(j$sections, `[[`, "", "id")
stopifnot(all(vapply(j$templates, `[[`, "", "category") %in% sec_ids))

# Scope label (derived here, not in the JSON): which book chapters a case draws on.
# Chapter 2 = tables and graphs; Chapter 3 = summary measures; Chapter 4 = two variables.
scope_of <- function(t) {
  cat <- t$category; id <- t$id
  if (cat %in% c("center", "grouped")) return("Ch. 2 + Ch. 3 (summary measures)")
  if (cat == "compare" || id == "C2.GRAPH.GROUP_BOXPLOTS") return("Ch. 2 + Ch. 3/4 (boxplots, groups)")
  if (cat == "conditional" || id == "C2.CONCLUSION.ASSOCIATION") return("Ch. 2 + Ch. 4 (conditional distributions)")
  if (id == "C2.SHAPE.OUTLIER_SKEW") return("Ch. 2 + Ch. 3 (outliers)")
  if (cat == "conclusion") return("Ch. 2 + Ch. 3/4 (conclusions)")
  "Ch. 2 core"
}
chr <- function(x) vapply(x, function(v) if (is.null(v)) "" else as.character(v), "")
lst <- function(t, f) as.character(unlist(t[[f]]))
templates <- data.frame(
  id = ids,
  chapter = vapply(j$templates, function(t) as.integer(t$chapter), 1L),
  section = chr(lapply(j$templates, `[[`, "section")),
  category = chr(lapply(j$templates, `[[`, "category")),
  title = chr(lapply(j$templates, `[[`, "title")),
  use_when = chr(lapply(j$templates, `[[`, "use_when")),
  avoid_when = chr(lapply(j$templates, `[[`, "avoid_when")),
  answer_template = chr(lapply(j$templates, `[[`, "answer_template")),
  provenance = chr(lapply(j$templates, `[[`, "provenance")),
  scope = vapply(j$templates, scope_of, ""),
  stringsAsFactors = FALSE)
templates$triggers <- lapply(j$templates, lst, "triggers")
templates$formulas <- lapply(j$templates, lst, "formulas")
templates$source_refs <- lapply(j$templates, lst, "source_refs")
templates$placeholders <- lapply(j$templates, function(t) unique(lst(t, "placeholders")))

sections <- data.frame(id = sec_ids, title = vapply(j$sections, `[[`, "", "title"), stringsAsFactors = FALSE)
sections$count <- as.integer(table(factor(templates$category, levels = sections$id)))
patterns <- lapply(j$exam_patterns, function(p) list(question = p$verbatim_question_excerpt, source = p$source,
                                                      categories = as.character(unlist(p$suggested_categories))))
bank <- list(schema_version = j$schema_version, purpose = j$purpose, warnings = as.character(unlist(j$warnings)),
             sections = sections, templates = templates, exam_patterns = patterns)
saveRDS(bank, "inst/extdata/answer_bank.rds", version = 2)
cat(sprintf("Saved %d templates in %d sections to inst/extdata/answer_bank.rds\n", nrow(templates), nrow(sections)))
