# statcram 0.10.0

## New: study answer bank (study use only)

* `sc_answers()`: 59 authored exam-style answer templates for the first-partial question
  types, in 11 categories. Search by words (case- and accent-insensitive, small typos
  tolerated, a few synonyms such as "same" ~ "similar"), browse by category or scope
  (`"core"` = Chapter 2, `"crossover"` = also Chapter 3/4), or show one case by id. Each card
  shows USE WHEN, DO NOT USE WHEN, the answer to study, formulas, related past-paper questions
  and its status (authored template, not a verbatim official answer). `list = TRUE` returns
  the index as a data frame. Works without shiny and offline.
* `sc_answers_fill()`: fills the `[blanks]` of a template with plain text (never evaluated)
  and lists the blanks still empty. Interval notation such as `[a,b)` is filled part by part
  and is not mistaken for a blank.
* `sc_answers_app()`: a window (and RStudio addin "Answer template browser") with search,
  categories with counts, case list, card, fill form, Copy / Reset / Print to console and a
  recall mode that hides the answer until asked.
* Menu item `sc(9, 5)`; listed in `sc_index()`, `sc_guide()`, `sc_notes("phrasing")` and the
  README. Earlier `sc(9, 1:4)` shortcuts are unchanged.
* Data: `data-raw/answer_bank.json` (source, not installed) compiled by
  `data-raw/build_answer_bank.R` into `inst/extdata/answer_bank.rds`, read with base R.
  The template wording is unchanged from the source JSON. A derived `scope` field labels the
  cases that also use Chapter 3/4 tools.
* No statistical function changed.

# statcram 0.9.3

* "Fix a table you entered" (`sc(8, 2)`) lists data frames instead of their columns.

# statcram 0.9.2

* `sc_define()`: exam-ready definitions of about 55 first-partial terms (menu `sc(9, 4)`).
