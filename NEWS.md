# statcram 0.13.1

* `desc_compare()` (menu `sc(1, 4)`) with a categorical variable in the "numerical variable" slot,
  e.g. a rating scale such as `WouldRepeat` (VeryUnlikely ... VeryLikely), stopped with
  "missing value where TRUE/FALSE needed". It now says the variable is categorical and gives the
  call for its conditional distributions, e.g. `desc_crosstab(Bidding$Type, Bidding$WouldRepeat)`
  (menu `sc(1, 7)`). Numbers stored as a factor still work.

# statcram 0.13.0

## Phrasing game (study use only)

* New `sc_phrasing_game()`: a quiz on your phrasing bank. Each round shows a case's situation;
  you type the answer you would write; statcram scores it against the bank's sentence (0-100),
  lists the key terms you missed and **flags every crucial verb you left out** (`! MISSING VERB`,
  e.g. *contain*, *vary*, *determine*), a missing qualifier such as *approximately*
  (`! QUALIFIER`) and a missing negation (`! NEGATION`: the canon says what does *not* hold).
  Then it shows the canon sentence, what to avoid and your error-log entry for the case.
* Points, hint (`h`, -10), skip, +10 for a clean answer, streaks, a final rank, and
  `review = TRUE` to replay the rounds you scored below 70 this session. Cases with labelled
  alternatives (e.g. NOMINAL / ORDINAL) are separate rounds: 199 rounds from 167 cases.
* `sc_phrasing_score(id, answer)` scores one answer, e.g. a longer one written in the editor.
* Scoring is word matching, offline: 55% key terms (80% of them earns the full share), 35%
  crucial verbs, 10% negation. Tense, plural, British/American spelling and a few paraphrases
  ("built" / "constructed", "about" / "approximately") do not matter. It does not understand
  meaning: read the canon sentence after each round.
* Menu `sc(9, 6)`: `g` starts the game.

## Fixes: frequency lists entered with `sc_table()`

* `desc_freq(table, n = 220)` on a table of proportions now rebuilds the counts
  (f_k = p_k x n) and shows n = 220; before, the header said "n = unknown". A clear note is given
  if p_k x n is not a whole number (rounded proportions). `n` must be a positive whole number.
* `desc_freq()` on a typed frequency table names the variable (e.g. `products`), not the table.
* The one-row-per-unit table (`<name>_long`) keeps numeric labels as numbers, so a count variable
  such as "number of products: 1 2 4 5 8" is quantitative discrete, not nominal. Text labels
  are still a factor in the order typed.
* For a table of proportions, the "Next steps" line suggests `desc_freq(<name>, n = <sample size>)`.

# statcram 0.12.0

## Your phrasing bank (study use only)

* New `sc_phrasing()`: your "Stats30001 Exact Phrasing Bank" inside statcram. 167
  source-adapted answer cases in 12 sections (Chapters 1-4, Chapter 5 split into probability,
  random variables, Normal and sampling/CLT, Chapter 6 split into point estimation, confidence
  intervals and sample size, plus the estimator-definitions appendix).
* Each case shows use when, the sentence to write, formula / R, avoid, qualification, the
  matching entries of your personal error log, sources, and the previous / next case.
* `sc_phrasing("qnorm")` searches, `sc_phrasing("5N.05")` opens a case, `section = "6I"` or
  `section = 5` lists a section or chapter. `what = "verbs"`, `"notation"`, `"excerpts"` (the 10
  verbatim official-solution excerpts), `"mistakes"` (your error log), `"scope"`, `"sources"`.
* The file is shipped unchanged (`inst/notes/phrasing-bank.md`); `sc_notes("bank")` prints it
  as written. Menu `sc(9, 6)`; listed in `sc_guide()` and `sc_index()`.
* Internal: the answer-bank search ranker is shared with the phrasing bank (no behaviour change).

# statcram 0.11.0

## Study answer bank: full 2026/27 first partial (study use only)

* The bank now holds **244 cards** for Chapters 1-6 (12 / 59 / 36 / 33 / 57 / 47), from
  `Statcram_FirstPartial_Answer_Bank_v2.json`. The 59 Chapter 2 cards are unchanged.
* New: `sc_answer(id)`, `sc_answers_questions()` (53 verbatim past-paper questions and 38
  official solution excerpts, with source and line, linked to cards; shown separately from the
  authored cards), `sc_answers_practice()` / `sc_answers_progress()` (drill with the answer
  hidden, ratings "remembered" / "needs review" kept for the session), `sc_answers_export()`
  (Markdown study cards), `sc_answers_corrections()`.
* `sc_answers()` gains `chapter`, `priority` and `group` (START HERE groups by the wording of the
  question; probability and estimators flagged as weak spots). Cards now show the other cases of
  the same kind and the past-paper questions of that kind.
* Search: symbols and their names match (`sigma` / `σ`, `Var(a+bX)`), short words match whole
  words only (`SE` no longer matches "use"), weak spots rank first on ties.
* ASCII fallback for symbols on non-UTF-8 consoles, or `options(statcram.ascii = TRUE)`.
* Window: START HERE, chapter and weak-spot filters, practice ratings, "needs review" filter,
  Next card, and a Past-paper questions tab. `sc_app()` has a link to it.
* Menu `sc(9, 5)`: START HERE number, `c1`..`c6`, `w` weak spots, `p` past-paper questions,
  `d` drill, or words to search.
* Content review: all 185 new cards checked against the course rules. 225 documented changes
  in `data-raw/answer_bank_corrections.json`: 5 misleading and 33 minor wording fixes (e.g. box
  LENGTH not width for the IQR; `qnorm` takes a proportion; random assignment in experiments;
  approximate confidence level for large-sample intervals), 172 "don't use when" sentences
  where the source had none, 15 blank-list updates. No statistical function changed.
* Changed behaviour: `sc_answers(category = "conditional")` now returns the Chapter 2 and
  Chapter 4 cards (11); use `chapter = 2` for the previous 4. `scope` now uses the cards'
  related-chapter tags.

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
