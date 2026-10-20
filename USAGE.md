# statcram study answer bank and phrasing bank: usage

> **Study use only.** The 2026/27 syllabus forbids R scripts prepared before the exam and AI tools
> during it. Use the answer bank to revise and memorise beforehand; do not open statcram or exported
> cards at the exam. At the exam use only what the rules allow (R, RStudio, UBStats, the R manual).

Install: `install.packages("statcram_0.13.2.tar.gz", repos = NULL, type = "source")`, then
`library(statcram)`. Everything below works offline and without shiny, except `sc_answers_app()`.

| Command | What it does |
|---|---|
| `sc_answers()` | START HERE: 11 groups by the wording of the question, then the 6 chapters |
| `sc_answers(group = "probability")` | one START HERE group (number or id) |
| `sc_answers(chapter = 5)` | all Chapter 5 cards, grouped by kind of question |
| `sc_answers(query = "conditional probability")` | search titles, conditions, answers, formulas, sources |
| `sc_answers(query = "standard error", priority = "critical")` | search the weak spots only |
| `sc_answers(category = "ci", chapter = 6)` | one kind of question |
| `sc_answer("C5.EVENTS.CONDITIONAL")` | one card: use when, answer, formulas, don't use when, status, past papers, other cases |
| `sc_answer("C6.CI.UNKNOWN_NORMAL")` | assumptions + template + formula + R quantile command (for practice) |
| `sc_answers_fill("C6.CI.UNKNOWN_NORMAL", values = list(L = "51.2", U = "55.8"))` | fill blanks (plain text, never evaluated); lists blanks still empty |
| `sc_answers_questions(source = "P25")` | verbatim questions of the 14 Oct 2025 partial, official excerpts, linked cards |
| `sc_answers_questions(topic = "estimator")` | verbatim questions of one kind, all sources |
| `sc_answers_practice(chapter = 6, n = 5)` | drill: situation first, Enter reveals, rate y / n |
| `sc_answers_practice(review = TRUE)` / `sc_answers_progress()` | drill / list the cards marked "needs review" (this session) |
| `sc_answers_export("cards.md", group = "ci", answers = FALSE)` | Markdown cards for home revision (self-test sheet without answers) |
| `sc_answers_corrections()` | what statcram changed in the supplied bank, and why |
| `sc_answers_app()` | window: search, START HERE, chapter, weak spots, cards, practice, fill, copy, past papers |
| `sc(9, 5)` | the same from the menu (`b` back, `m` main menu, `q` quit) |

Your phrasing bank (167 source-adapted cases, 10 verbatim excerpts, your error log):

| Command | What it does |
|---|---|
| `sc_phrasing()` | the 12 sections with case counts, and the commands below |
| `sc_phrasing("qnorm")` | search titles, use-when, template text, formulas, sources |
| `sc_phrasing("5N.05")` or `sc_phrasing(id = "5N.05")` | one case: use when, write, formula / R, avoid, qualification, your error-log entries, sources |
| `sc_phrasing(section = "6I")` / `sc_phrasing(section = 5)` | one section / all four Chapter 5 sections |
| `sc_phrasing("SE", section = "6E")` | search inside one section |
| `sc_phrasing(what = "excerpts")` | the 10 verbatim official-solution excerpts |
| `sc_phrasing(what = "mistakes")` | your personal error log, with the cases to revisit |
| `sc_phrasing(what = "verbs")`, `"notation"`, `"scope"`, `"sources"` | the other parts of the file |
| `sc_phrasing("qnorm", list = TRUE)` | the matches as a data frame (no printing) |
| `sc_notes("bank")` | the whole file as written |
| `sc(9, 6)` | the same from the menu: section code, case id, `g` game, `v` verbs, `n` notation, `x` excerpts, `e` error log, or words |
| `sc_phrasing_game()` | game, 10 random rounds: type your answer, get a score 0-100, missing key terms, `! MISSING VERB`, `! QUALIFIER`, `! NEGATION`, then the canon sentence |
| `sc_phrasing_game(n = 5, section = "6I")` / `query = "qnorm"` / `id = c("5N.05", "6I.13")` | choose the rounds |
| `sc_phrasing_game(review = TRUE)` | replay the rounds you scored below 70 this session |
| `sc_phrasing_score("6I.03", "your answer")` | score one answer without the game (e.g. a longer one from the editor) |

In the game: type the answer and press Enter; `h` = hint (-10 points), Enter alone = skip, `q` = stop.
Scoring is word matching (55% key terms, 35% crucial verbs, 10% negation), not meaning.

Sources: `M1` Module 1 exercises, `P24` 1st partial 22 Oct 2024, `P25` 1st partial 14 Oct 2025,
`S26JAN` 2nd partial 9 Jan 2026 (one-sample CI parts only), `G26JAN` general exam 30 Jan 2026,
`G26JUL` general exam 3 Jul 2026, `G26SEP` general exam 1 Sep 2026, `FR` course framework,
`AG` answer-phrasing guide.

## Example: a question you have not seen

> "Explain what the standard error of the sample mean tells us about our estimate of 54.2."

1. `sc_answers(group = "estimator")` or `sc_answers("standard error")`: the first card is
   *Standard error interpretation* (`C6.ESTIMATOR.SE_MEANING`, a weak spot).
2. `sc_answer("C6.ESTIMATOR.SE_MEANING")`: use when "asked to interpret SE or identify a
   misconception"; the answer stresses GENERIC estimate vs realised estimate; "other cases of this
   kind" shows the SE formula cards you would need if the question also asked for the value.
3. Write your own answer, then `sc_answers_practice(group = "estimator")` a few days later.

## Rebuilding the data

Edit `data-raw/answer_bank.json` or `data-raw/answer_bank_corrections.json`, then from the
package root `Rscript data-raw/build_answer_bank.R` (needs jsonlite and a UTF-8 locale) and
rebuild the package. Corrections name the id, the field, the exact text they replace and a reason;
the build stops if the source text no longer matches.
