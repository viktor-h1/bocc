# statcram verification report

**Study use only.** statcram is a tool for revising, drilling and memorising.
Do not open or use it during the exam. The 30001 syllabus forbids R scripts
prepared in advance and AI tools in the exam. Being installable in RStudio
does not make it permitted there.

All checks below ran on 2026-10-13 in a Linux container
(R, testthat edition 3, `LANG=C.UTF-8`). Anything not listed as run was not run.

## 0.13.0 — phrasing game and frequency-list fixes, checked 2026-10-19

| Check | Result |
|---|---|
| Full suite from source | 173 tests, 1262 expectations, 0 failed, 0 skipped |
| `R CMD check --no-manual statcram_0.13.0.tar.gz` | **Status: OK**; tests `[ FAIL 0 \| WARN 0 \| SKIP 1 \| PASS 1257 ]` (the same data-raw skip) |
| Scorer sanity | all 199 rounds score 100 against their own canon text; an empty answer scores at most 15 |
| Flags | missing *contain* / *vary* (6I.03), *approximately* (qualifier), missing negation (5N.09) are flagged; a full paraphrase of 6I.03 scores >= 85 with no flag |
| Variants | 1.02 picks ORDINAL for an ordinal answer; `variant =` honoured; unknown variant / id errors |
| Game | scripted play: hint (-10), skip, stop, result, error-log entry, history, `review = TRUE`, section rounds, menu `g` |
| Frequency lists | proportions + `n = 220` rebuild counts 66/55/55/33/11 and show n = 220; variable named `products`; `_long` table numeric; text labels stay ordered factors |

Not verified: how well the word-matching score agrees with a human marker. It is a check on
wording, not a grade.

## 0.12.0 — phrasing bank (`sc_phrasing()`), checked 2026-10-19

| Check | Result |
|---|---|
| `R CMD check --no-manual statcram_0.12.0.tar.gz` | **Status: OK**; tests `[ FAIL 0 \| WARN 0 \| SKIP 1 \| PASS 1201 ]` (same data-raw skip as below) |
| Full suite from source | 166 tests, 1206 expectations, 0 failed, 0 skipped |
| File shipped unchanged | `inst/notes/phrasing-bank.md` has the uploaded file's MD5 (`64875f79...`); tested in the installed package |
| Parse completeness | 167 cases (8/16/25/18/16/16/9/11/14/22/9/3 by section), 10 excerpts, 19 error-log entries, 6 verbs, 6 notation rows, 13 source keys; 0 unparsed lines; every case has use-when, write and sources |
| Wording kept | all 217 quoted (`> `) lines of the file appear word for word as template lines (207) or excerpts (10) |
| Error-log links | every "Revisit" range expands to existing case ids |
| Search / lookup | `"qnorm"`, `"t wider than z"` (6I.13 first), `"whisker"`, `"SE"` (whole word), typo `"histogramm"`, ids like `"5n.5"`, sections `"6I"`, `5`, title words |
| Display | case layout, study note, backticks removed, ASCII fallback, all `what =` parts, `sc_notes("bank")` |
| Menu | `sc(9, 6)`: case id, search + pick, section code, `e` error log |

Not verified: a real Windows console (forced-ASCII path only). The bank's statistical content
was not reviewed for this release; it is shipped as you wrote it.

## 1. R CMD check

| Step | Command | Result |
|---|---|---|
| Build | `R CMD build .` | `statcram_0.11.0.tar.gz` built |
| Check | `LANG=C.UTF-8 LC_ALL=C.UTF-8 R CMD check --no-manual statcram_0.11.0.tar.gz` | **Status: OK** (0 errors, 0 warnings, 0 notes) |
| Tests inside the check | testthat | `[ FAIL 0 \| WARN 0 \| SKIP 1 \| PASS 1120 ]` |
| Full suite from source | `devtools::test()` | **0 failed, 0 skipped**, 1125 expectations |

The one skip in the check is expected. The test that rebuilds the bank from
`data-raw/answer_bank.json` plus `data-raw/answer_bank_corrections.json` needs
`data-raw/`, which `.Rbuildignore` leaves out of the tarball. That test passes
when run from source (the developer ZIP).

`--no-manual` was used because there is no LaTeX in the container, so the
PDF manual was not built.

## 2. Answer-bank contents

| Item | Expected (brief / JSON) | Shipped | Checked by |
|---|---|---|---|
| Cards, total | 244 | 244 | test-answers.R |
| Chapters 1–6 | 12 / 59 / 36 / 33 / 57 / 47 | 12 / 59 / 36 / 33 / 57 / 47 | test-answers.R |
| Card IDs | unchanged from JSON | identical set, identical order | test-answers.R (from source) |
| Sources / provenance per card | kept | `source_refs`, `provenance`, `support` shown on every card | test-answers.R |
| Kinds of question (category keys) | — | 29 | test-answers.R |
| START HERE groups | — | 11, covering all 244 cards exactly once | test-answers.R |
| Priority | — | 32 critical, 24 high, 188 normal | test-answers.R |
| Verbatim past-paper questions | kept separate | 53, in `questions`, never mixed into card text | test-answers.R |
| Official solution excerpts | kept separate | 38, in `quotes` | test-answers.R |
| Scope | first partial only | no two-sample, hypothesis-test or regression cards (pattern scan of every card) | test-answers.R |

The 53 questions and 38 excerpts were compared one by one with the verbatim
transcription. Each matched its source lines exactly. statcram did not alter
any official wording.

### Corrections applied to authored templates (225)

All corrections live in `data-raw/answer_bank_corrections.json`. Each has an
ID, field, reason, original text and new text. The build refuses to apply a
correction whose original text no longer matches the JSON. You can list them
with `sc_answers_corrections()`.

| Kind | Count | What it is |
|---|---|---|
| `misleading` | 5 | Wording that could lead to a wrong exam answer. Examples: box *length* vs *width* in the box-plot comparison card; a quantile card that read q as a proportion; individual-vs-mean card wording |
| `minor` | 33 | Clarity or consistency fixes that do not change the statistics |
| `added_avoid_when` | 172 | A "Don't use when" line added where the JSON had none, so every card says why a neighbouring card would be wrong |
| `placeholders_sync` | 15 | The placeholder list made to match the blanks that actually appear in the answer text |

Four independent read-throughs of the 185 cards new in v2 found **no
statistical errors**. Cards show "Wording corrected in statcram" only for the
`misleading`/`minor` fixes, and "\"Don't use when\" added by statcram" for the
added lines. The brief's verbatim questions and official quotes were not
touched by any correction.

## 3. Search and navigation

| Query or call | Result | Test |
|---|---|---|
| `sc_answers()` | START HERE screen: 11 groups, 6 chapters, commands | yes |
| `sc_answers(group = "probability")` / `group = 3` | 17 / 22 cards; all groups together = 244 | yes |
| `sc_answers(chapter = 5)` | 57 cards | yes |
| `sc_answers(category = "ci", chapter = 6)` | 23 cards | yes |
| `sc_answers(category = "conditional")` | 11 cards (*changed:* key match, not label substring) | yes |
| `sc_answers(priority = "critical")` / `"high"` | 32 / 56 (high includes critical) | yes |
| `sc_answer("C6.CI.KNOWN_CLT")` | full card | yes |
| `sc_answers_questions(source = "P25")` | P25 questions, verbatim | yes |
| 15 required search terms (Bayes, conditional, qnorm, outlier, margin of error, unbiased, normal approximation, SE, generic estimate, among those who, Var(a+bX), right skewed, confidence interval, IQR, histogram density) | each returns at least one card | yes |
| search "Bayes" | C5.EVENTS.BAYES in the top 3 | yes |
| search "among those who" | top 5 are all conditional cards | yes |
| search "SE" | 26 hits (whole-word match, so "use" no longer matches) | yes (< 60) |
| search "standard error", critical only | C6.ESTIMATOR.SE_MEANING first | yes |
| search "conditional" / "CLT" / "fences" / "box plot" / "sample size" | 55 / 63 / 9 / 6 / 18 hits | counts noted by hand, not asserted |
| symbols `σ`, `≤`, `x̄`, `p̂` in a query | normalised to sigma, <=, xbar, p-hat | yes |
| accents ("rïght-skéwed") | ignored | yes |
| typo "histogramm" | still finds C2.HISTOGRAM.UNEQUAL | yes |
| nonsense query | "No card matches" message, 0 cards | yes |
| menu `sc()` → 9 → 5 | answer-bank menu, START HERE, chapter, weak spots, past papers, drill, search | yes (scripted input) |
| `sc_app()` link | "Open the study answer bank" opens `sc_answers_app()` | yes (code path) |

"Chebyshev" returns 0 cards and "binomial" returns 1. That reflects the
content of the v2 JSON, not a search bug.

## 4. Other features

| Feature | How it was checked | Result |
|---|---|---|
| Card layout: when to use, answer, formulas, when not to use, blanks, source status, sources, past-paper questions, other cases | test-answers.R on several cards | pass |
| Required content (conditional denominator, b² for variance of a+bX, generic SE, t vs Normal population, CLT "approximately", fences substitution) | test-answers.R | pass |
| `sc_answers_fill()`: plain blanks, interval blanks, nested `[[L],[U]]` | test-answers.R (`[51.2,55.8]`) | pass |
| Fill values are never evaluated as R | test-answers.R | pass |
| `sc_answers_practice()`: situation, Enter reveals the answer and trap, y/n/q rating | scripted input | pass |
| `sc_answers_progress()` | after scripted ratings | pass |
| `sc_answers_export()` to Markdown with the study note | temp file | pass |
| ASCII fallback: `options(statcram.ascii = TRUE)` or a non-UTF-8 console | `xbar +- t s/sqrtn ; sigma^2 <= mu ; p-hat ; -` | pass |
| R source is ASCII-only | R CMD check (no non-ASCII warning) | pass |
| `sc_answers_app()` server logic | `shiny::testServer` | pass |
| `sc_answers_app()` in a browser | Playwright + Chromium against `runApp` | no console errors; group filter, card, practice mode, Show answer, rating, "needs review" filter and Past-paper tab all worked |

## 5. Test matrix

| Area | Unit tests | R CMD check | Browser | Manual |
|---|---|---|---|---|
| Data counts and fields | ✔ | ✔ | — | — |
| JSON + corrections rebuild | ✔ (source only) | skipped (data-raw excluded) | — | — |
| Verbatim questions / quotes | ✔ | ✔ | ✔ tab view | ✔ line-by-line |
| Search and filters | ✔ | ✔ | ✔ | ✔ |
| Card printing | ✔ | ✔ | ✔ | ✔ |
| Fill blanks | ✔ | ✔ | ✔ form | — |
| Practice and ratings | ✔ | ✔ | ✔ | — |
| Export | ✔ | ✔ | — | — |
| ASCII fallback | ✔ (forced) | ✔ | — | not on real Windows |
| Menu entry 9,5 | ✔ | ✔ | — | — |
| `sc_app()` link | code path | ✔ | not clicked | — |
| Copy-to-clipboard button | — | — | Chromium only | not in RStudio viewer |

## 6. Not verified

- **A real Windows console** (CP1252). Only the forced-ASCII path was tested.
- **The Copy button inside the RStudio viewer pane.** It was tested only in
  Chromium. The button falls back to selecting the text when the clipboard API
  is blocked.
- **Installing without shiny/miniUI.** The console functions do not need them.
  `sc_answers_app()` stops with a message if they are missing. This was not
  tested on a library without shiny.
- **The PDF manual** (no LaTeX here).

## 7. Known limitations

- **Self-ratings last for the current R session only.** They are not written
  to disk.
- **Past-paper questions are linked to cards by kind of question (category).**
  Questions from the same source are preferred, but there is no hand-made
  question-to-card map. A linked question is the same *kind*, not
  necessarily the same card.
- **There is no separate "next variation" command.** Use the app's *Next card*
  button, or the "Other cases of this kind" list printed under each card.
- **Search is keyword based** (stemming, synonyms, fuzzy fallback). It does
  not understand a pasted exam question. Use START HERE or the kind-of-question
  filter when a search returns too much.
