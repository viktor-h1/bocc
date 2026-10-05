# statcram 0.8.7

An offline statistics exam toolkit for R / RStudio. One numbered menu, plus plain functions
with consistent names. Every procedure works with **raw data** (including data frames and
variables you create during the exam) **or the summary numbers printed in the question**, and
prints the formula with your numbers plugged in, the decision, exam wording and the one-line
call to re-run it.

## Install

Pure R, no compilation, no internet needed after installing.

```r
# from the built file
install.packages("statcram_0.8.7.tar.gz", repos = NULL, type = "source")
# or from GitHub
remotes::install_github("viktor-h1/bocc", ref = "claude/quirky-gates-9i7hvr")

library(statcram)
sc_selftest()        # checks every procedure against base R
```

## Exam quick card

```r
load("EXAM.RData")
library(statcram)

sc()             # the menu                         sc(4, 1) = straight to "one-mean test"
sc_table()       # rebuild a table printed on paper  (numbered placeholders)
sc_data()        # what is loaded right now
sc_guide()       # which test do I need?
sc_index()       # every function + its menu shortcut
sc_recipes()     # R one-liners: new columns, subsets, factors, typed tables
sc_notes("inference")   # course rules (also: rules, descriptive, estimation, random, chisq, regression, patterns)
```

At every menu prompt: **b** = back, **m** = main menu, **q** = quit.
When the menu asks for data it lists every data frame column, vector and table loaded *right
now*. Type a number, or **any R expression** (`df$after - df$before`, `df$loyalty == "High"`,
`subset(df, region == "North")$spend`), or **s** to enter summary numbers, or **t** to type a
table from paper first.

## The menu

| # | Topic | Functions |
|---|---|---|
| 1 | Describe data | `desc_freq` `desc_classes` `desc_summary` `desc_compare` `desc_cv` `desc_prop` `desc_crosstab` `desc_cor` |
| 2 | Probability & random variables | `prob_events` `prob_bayes` `rv_discrete` `prob_binom` `prob_unif` `prob_normal` `prob_t` `prob_chisq` `rv_lincomb` `rv_joint` `rv_iid` `rv_prop` |
| 3 | Estimation and confidence intervals | `ci_mean` `ci_prop` `ci_paired` `ci_2means` `ci_2props` `est_mean` |
| 4 | Hypothesis tests | `test_mean` `test_prop` `test_paired` `test_2means` `test_2props` `test_levene` |
| 5 | Power / Type II error / sample size | `power_mean` `power_prop` `n_mean` `n_prop` `n_2props` `power_2means` |
| 6 | Chi-square | `chisq_gof` `chisq_indep` |
| 7 | Regression | `reg_fit` `reg_predict` `reg_test` `reg_check` `reg_compare` |
| 8 | Enter a table from paper / data tools | `sc_table` `sc_data` `sc_recipes` |
| 9 | Which test? / course notes | `sc_guide` `sc_index` `sc_notes` |

Type a prefix and press Tab in RStudio (`test_`, `ci_`, `desc_` ...) to see the family;
`?test_mean` opens one help page covering all tests (same for `?ci_mean`, `?reg_fit`, ...).

## Functions: raw data or summary numbers

```r
test_mean(df$spend, mu0 = 50, alt = ">")                      # raw data (t, sigma unknown)
test_mean(xbar = 52.3, s = 6.1, n = 40, mu0 = 50, alt = ">")   # numbers from the question
test_mean(xbar = 2400, sigma = 850, n = 100, mu0 = 2500, alt = "<")   # sigma known -> Z
test_paired(df$after, df$before, alt = ">")                    # D = after - before
test_2means(df$spend, group = df$loyalty, levels = c("High", "Medium"), case = "welch")
ci_prop(df$loyalty == "High", conf = 90)                       # any TRUE/FALSE expression
test_prop(count = 120, n = 400, p0 = 0.25, alt = ">")
chisq_indep(df$premium, df$channel)
chisq_gof(c(A = 151, B = 117, C = 140, D = 162))               # equal shares if p omitted
fit <- reg_fit(spend ~ age + premium, data = df)
reg_predict(fit, age = 40, premium = "Yes")
```

Frequency distributions follow the course book (Applied Statistical Methods, ch. 2):

```r
desc_freq(df$Satisf, order = c("VLow", "Low", "QLow", "Med", "QHigh", "High", "VHigh"))  # f_k, p_k, F_k
desc_freq(df$NMonths, at_least = 10)                    # Freq(X >= 10), exact
desc_classes(df$TotVisits, breaks = 10)                 # 10 equal-width classes, w = (Max - Min) / 10
desc_classes(df$TotVisits, breaks = c(0, 5, 10, 15, 25, 45, 60, 100, 150, 220))
desc_classes(df$MonthExp, at_most = 250)                # variable measured in classes "[0,50)", ...
```

Summaries follow chapter 3: `desc_summary()` gives the mode(s), median, mean, five-number summary
(R's quartiles and the book's hand rule "smallest value with F >= 0.25"), percentiles, range, IQR,
s^2 with n - 1, SD, CV, Tukey whiskers and extreme values, and the shape read from the box.
`desc_freq()` adds mode / median / quartiles (cumulative rule, also for ordinal data) and
mean / variance from the frequencies. `desc_classes()` handles open-ended classes
(`breaks = c(0, 300, 500, 1000, Inf)`, labels such as "1000 or more"), gives the median and
quartiles as l_k + (p - F_(k-1)) / c_k, and the grouped variance with the n / (n - 1) correction.

Two variables follow chapter 4: `desc_crosstab()` (joint and conditional distributions,
conditional modes / quartiles, expected counts, chi-square and Cramer's V, stacked or side-by-side
bars), `desc_compare()` (conditional summaries and side-by-side boxplots, optional second grouping
variable) and `desc_cor()` (scatterplot, covariance, correlation, regression line b0 + b1 x;
matrices; covariance from a joint frequency table).

Probability follows chapter 5: `prob_events()` (union, conditional, independence),
`prob_bayes()` (total probability and Bayes), `rv_discrete()`, `prob_binom()` (with the R call:
`P(X > 7) = 1 - pbinom(7, 15, 0.65)`), `prob_unif()`, `prob_normal()` (probabilities, quantiles
q_alpha = x_(1-alpha), central intervals; `var =` accepted), `rv_lincomb()` (any number of variables
with correlations), `rv_joint()` (marginals, conditionals, covariance, independence) and `rv_iid()`
(sums and means, CLT, quantiles).

Estimation follows chapter 6. `est_mean()` gives xbar, its standard error (sigma / sqrt(n) or
s / sqrt(n)) and the n needed for a target SE. With sigma unknown, `ci_mean()` and `ci_paired()`
print both rows of the UBStats output (Normal.Approx and Student-t). Without `case`, `ci_2means()`
prints all four intervals: variances assumed equal (s^2_pool, n_x + n_y - 2 df) or different
(Welch-Satterthwaite df), each with z and t. `ci_mean(sum_x =, sum_x2 =, n =)` works from the sums
in the question, and `ci_paired(sigma_d =)` from a known sigma_D. `n_mean()` / `n_prop()` take
`width =` (= 2 x margin) and, for sigma unknown, `s =` (approximate). `n_2props()` gives the n per
group for p_x - p_y. Each interval also prints the matching UBStats call, e.g.
`CI.diffmean(x = df$spend, by = df$loyalty, conf.level = 0.95)`.

Tests follow chapter 7. A one-sided H0 is written with its equality ("H0: mu <= 15 (or mu = 15)")
and tested at the boundary value, and every test reads the p-value as "the smallest alpha that
rejects". As in UBStats `TEST.mean` / `TEST.diffmean`, a variance that is unknown gives both
Normal.Approx and Student-t rows. Without `case`, `test_2means()` shows all four tests, and with
raw data `ci_2means()` / `test_2means()` add Levene's test for equal variances (deviations from
the medians, as UBStats `var.test = TRUE`; also `test_levene()`). `test_paired(sigma_d =)` covers
a known sigma_D. `test_2props(d0 = 0.05)` tests "more than 5 points higher" with the unpooled se,
while d0 = 0 uses the pooled p. `power_2means()` gives beta for two means with known sigmas. A
true value inside H0 is reported as P(reject) = alpha(mu1), not as beta. Chi-square tests add
the residuals (O - E) / sqrt(E), Cramer's V, the E >= 5 rule and the base R call (2 x 2:
`chisq.test(..., correct = FALSE)`).

Descriptive results also print the matching UBStats call (book sections 2.6, 3.5 and 4.5):
`distr.table.x()` / `distr.plot.x()` for frequency tables, classes (`breaks =`, `interval = TRUE`)
and plots; `distr.summary.x()` for summaries (with `by1`, `by2` for groups); `distr.table.xy()` /
`distr.plot.xy()` for two variables. An ordinal text variable is given as `factor(x, levels = ...)`,
because UBStats otherwise sorts its levels alphabetically.

Classes are `[a, b)` with the last one closed, densities are c_k = p_k / w_k, histograms use
densities, and proportions inside a class are approximated assuming values are spread uniformly
(Freq(X <= 250) = Freq(X < 200) + c_k x 50). Columns holding interval labels such as `"[0,50)"` are
recognised and sorted by their limits, not alphabetically.

`alt` takes `"<"`, `">"`, `"!="` (or less / greater / two.sided). `alpha` and `conf` take
`0.05` or `5`, `0.95` or `95`. For two means, `case =` (`"pooled"`, `"welch"`, `"large"` or
`"known"`) picks the procedure that is worked step by step. The package never guesses an
assumption the question did not state: without `case` it shows all four intervals or tests.
Turn parts of the output off with `options(statcram.plot = FALSE)`,
`options(statcram.wording = FALSE)`, `options(statcram.ubstats = FALSE)`; change decimals with
`options(statcram.digits = 6)`.

## Tables printed on paper: `sc_table()`

```
> sc_table()
 1  Data table      - each row is one unit, columns are variables
 2  Count table     - row categories x column categories (cross-tab)
 3  Class table     - classes like 10-25, 25-30 with frequencies or %
 4  Frequency list  - categories with counts
What kind of table?: 2
How many rows ...: 3        How many columns ...: 2
Row variable name [row]: region     Row labels ...: north south east
Column variable name [col]: loyalty Column labels ...: low high

  region   low   high
  north    [1]    [2]
  south    [3]    [4]
  east     [5]    [6]

[1/6] north / low = 20
[2/6] north / high = 30
[3/6] south / low = 25 25 30 20        <- several values fill the next placeholders
Enter = done,  4 or 4=27 = fix a cell,  n = rename labels,  q = cancel: 4=27
Save as (object name) [tab1]: regions
Saved 'regions' (count table 3x2) and 'regions_long' (one row per unit)
Next: chisq_indep(regions)  desc_crosstab(regions)
```

* `b` goes back one cell; `3,5` is read as 3.5; `12%` as 12; `NA` or `-` as missing.
* Type `c` at any label prompt to build class labels from boundaries (`10 25 30 35 ...`).
* The R code that recreates the table is printed. `sc_table(edit = regions)` reopens it.

## What changed from 0.7.2

* **Removed:** the local AI layer (Ollama/Qwen, `httr2`, `jsonlite`) and the "paste the
  question" parser. You pick the procedure from the menu, which is faster and predictable. (The
  old parser could not read alpha or confidence levels: its text cleaning removed `.` and `%`.)
* **Fixed: custom data frames and new variables were not recognised.** 0.7.2 read the data once
  when `statcram()` started, matched variables by guessing from the question text, hid integer
  columns with few values or 0/1 dummies from the picker, and ignored vectors and typed tables.
  Now nothing is cached, nothing is hidden by type, and any R expression is accepted.
* **New:** summary-number entry everywhere, `sc_table()`, critical values and rejection regions,
  step-by-step formulas, plots, `reg_test()`, `reg_compare()`, VIFs, `sc_selftest()`.
* **Renamed:** helpers now use prefixed names (`grouped_stats` -> `desc_classes`,
  `rv_combine` -> `rv_linear`, `ci_two_means` -> `ci_2means`, `sample_size` -> `n_mean` /
  `n_prop`, `regression_toolkit` -> `reg_fit` / `reg_predict` / `reg_check`, ...).
  `statcram()` still works and opens the same menu as `sc()`.
