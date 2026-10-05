# ------------------------------------------------------------
# Navigation aids: decision tree, function index, R recipes,
# course notes, and a self-test to run after installing.
# ------------------------------------------------------------

#' Which test do I need? Function index, R recipes and course notes
#'
#' * `sc_guide()`   decision tree from "what does the question give me" to
#'   the right function / menu number.
#' * `sc_index()`   every function by topic, with its menu shortcut.
#' * `sc_recipes()` R one-liners for making variables, subsets, factors,
#'   classes and tables.
#' * `sc_notes()`   course rules (estimation, CIs, tests, chi-square,
#'   regression, past-paper wording). Call without a topic to list topics.
#' * `sc_selftest()` checks the installed package against base R.
#'
#' @param topic Notes topic, e.g. `"inference"`; `NULL` lists the topics.
#' @param verbose Print each check.
#' @return Invisibly: the text shown (`sc_guide`, `sc_recipes`, `sc_notes`),
#'   the index data frame (`sc_index`), or `TRUE`/`FALSE` (`sc_selftest`).
#' @examples
#' sc_guide()
#' sc_index()
#' sc_notes("inference")
#' @name guide
NULL

#' @rdname guide
#' @export
sc_guide <- function() {
  txt <- c(
    "WHICH TEST / PROCEDURE?                                   function        menu",
    "",
    "Setting up a test (chapter 7)",
    "  H0 = status quo / no effect, ALWAYS with the equality: =, <= or >=.  H1 = what must be PROVEN.",
    "  Ask: which wrong conclusion is worse? That conclusion goes in H1 (rejecting a true H0 = Type I",
    "  error, probability alpha, fixed in advance). 'is it effective / higher / exceeds' -> H1: > ;",
    "  'below target / reduces' -> H1: < ; 'differs / out of control / changed' -> H1: != .",
    "  One-sided H0 (mu <= 15) is tested at its boundary value (mu = 15).",
    "  p-value < alpha -> reject H0; otherwise 'fail to reject' (never 'accept'). The p-value is the",
    "  smallest alpha that rejects. A two-sided test at alpha rejects <-> mu0 is outside the 100(1-alpha)% CI.",
    "  Very large n: tiny, practically irrelevant differences become significant.",
    "",
    "Describe data (no population claim)",
    "  frequency table / cumulative / Freq(X <= x), few values .. desc_freq()      sc(1,1)",
    "  numbers grouped into intervals, or measured in classes ... desc_classes()   sc(1,2)",
    "     ([0,50), 10-20 ...): densities, histogram, ogive, approx. Freq(X <= x)",
    "  one numeric variable: centre, spread, quartiles .......... desc_summary()   sc(1,3)",
    "  numerical variable across groups: summaries | boxplots .... desc_compare()   sc(1,4)",
    "  dispersion of variables in different units ............... desc_cv()        sc(1,5)",
    "  'report the proportion ... and how its SE is estimated' .. desc_prop()      sc(1,6)",
    "  two variables with few values: joint / conditional ........ desc_crosstab()  sc(1,7)",
    "     distributions, conditional summaries, chi-square, Cramer's V",
    "  two numerical variables: scatter, cov, r, regression line .. desc_cor()       sc(1,8)",
    "  type of each variable in a data frame (qualitative / quantitative) .. desc_vars()  sc(1,9)",
    "",
    "Estimation (chapter 6): point estimate + SE ..... est_mean() / desc_prop()   sc(3,6)",
    "  sample size for a margin / width / SE .......... n_mean() n_prop() n_2props()   sc(5,3..5)",
    "",
    "NUMERIC outcome (means)",
    "  one sample ................ CI: ci_mean()     test: test_mean()     sc(3,1) / sc(4,1)",
    "     sigma known -> Z | sigma unknown + normal -> t | not normal, n large -> Z (CLT)",
    "     (with sigma unknown the CI shows both z and t, as UBStats)",
    "  same units measured twice (before/after, matched) ... ci_paired() / test_paired()   sc(3,3) / sc(4,3)",
    "  two independent groups ..... ci_2means() / test_2means()   sc(3,4) / sc(4,4)",
    "     variances assumed equal -> pooled t | not assumed equal -> Welch t",
    "     not normal but both n large -> Z | sigmas known -> Z",
    "     (no case given: all four intervals / tests, as UBStats; raw data: + Levene test)",
    "  equal variances? (Levene, raw data) ................... test_levene()         sc(4,6)",
    "",
    "CATEGORICAL outcome (an event: 'customers with High loyalty')",
    "  one sample ................ CI: ci_prop()     test: test_prop()     sc(3,2) / sc(4,2)",
    "  two independent groups .... ci_2props() / test_2props()            sc(3,5) / sc(4,5)",
    "     test of p1 - p2 = 0: pooled p under H0 | p1 - p2 = d0 (e.g. 'more than 5 points') -> d0 = 0.05, no pooling",
    "  one variable vs stated shares (uniform, 30/50/20 ...) ... chisq_gof()     sc(6,1)",
    "  two categorical variables associated? ................... chisq_indep()   sc(6,2)",
    "",
    "Type II error / power ......... power_mean() / power_prop()          sc(5,1) / sc(5,2)",
    "  two means, sigmas known ...... power_2means()                       sc(5,6)",
    "  (true value inside H0 -> P(reject) = alpha(true value) <= alpha, not beta)",
    "Required sample size .......... n_mean() / n_prop() / n_2props()     sc(5,3) / sc(5,4) / sc(5,5)",
    "",
    "Probability (chapter 5)",
    "  P(A or B), P(A | B), independence of events ................ prob_events()  sc(2,1)",
    "  total probability, Bayes (test positive -> disease?) ...... prob_bayes()   sc(2,2)",
    "  discrete r.v.: E, Var, median from p(x) ................... rv_discrete()  sc(2,3)",
    "  number of successes in m independent trials ............... prob_binom()   sc(2,4)   (not in the 2026/27 syllabus)",
    "  Uniform / Normal / t / chi-square probabilities, quantiles .. prob_unif() prob_normal() prob_t() prob_chisq()   sc(2,5..8)",
    "  a + bX, aX + bY + c, portfolios, sums of variables ........ rv_lincomb()   sc(2,9)",
    "  joint table of two r.v.s: covariance, independence ....... rv_joint()     sc(2,10)",
    "  sum / mean of n iid variables, CLT ........................ rv_iid()       sc(2,11)",
    "",
    "Explain / predict a numeric Y from X variables (regression)",
    "  fit, t tests, F test, R2 ......... reg_fit()       sc(7,1)",
    "  predict mean (CI) / individual (PI) ... reg_predict()   sc(7,2)",
    "  H0: beta = some value ............ reg_test()      sc(7,3)",
    "  assumptions, outliers, multicollinearity ... reg_check()   sc(7,4)",
    "  compare models / adjusted R2 ..... reg_compare()   sc(7,5)",
    "  CI for a coefficient, effect of +10 units ... reg_effect()   sc(7,6)",
    "",
    "Table printed on paper? sc_table() (menu 8,1) rebuilds it as a data frame first.",
    "Keywords: 'same' units twice -> paired; 'assume equal variances' -> pooled;",
    "'is X associated with Y' (both categorical) -> chisq_indep; 'follows / agrees with' shares -> chisq_gof.")
  cat(txt, sep = "\n")
  invisible(txt)
}

.index <- data.frame(
  topic = c(rep("1 Describe", 9), rep("2 Probability", 12), rep("3 Estimation / CIs", 6),
            rep("4 Hypothesis tests", 6), rep("5 Power / sample size", 6), rep("6 Chi-square", 2),
            rep("7 Regression", 6), rep("8 Data tools", 3), rep("9 Guide", 5)),
  menu = c(paste0("sc(1,", 1:9, ")"), paste0("sc(2,", 1:12, ")"), paste0("sc(3,", 1:6, ")"), paste0("sc(4,", 1:6, ")"),
           paste0("sc(5,", 1:6, ")"), paste0("sc(6,", 1:2, ")"), paste0("sc(7,", 1:6, ")"),
           "sc(8,1)", "sc(8,3)", "sc(8,6)", "sc(9,1)", "sc(9,2)", "sc(9,3)", "", ""),
  fun = c("desc_freq", "desc_classes", "desc_summary", "desc_compare", "desc_cv", "desc_prop", "desc_crosstab", "desc_cor", "desc_vars",
          "prob_events", "prob_bayes", "rv_discrete", "prob_binom", "prob_unif", "prob_normal", "prob_t", "prob_chisq",
          "rv_lincomb", "rv_joint", "rv_iid", "rv_prop",
          "ci_mean", "ci_prop", "ci_paired", "ci_2means", "ci_2props", "est_mean",
          "test_mean", "test_prop", "test_paired", "test_2means", "test_2props", "test_levene",
          "power_mean", "power_prop", "n_mean", "n_prop", "n_2props", "power_2means",
          "chisq_gof", "chisq_indep",
          "reg_fit", "reg_predict", "reg_test", "reg_check", "reg_compare", "reg_effect",
          "sc_table", "sc_data", "sc_recipes",
          "sc_guide", "sc_index", "sc_notes", "sc_selftest", "sc"),
  what = c("frequency table: f_k, p_k, F_k; Freq(X <= x); bars / spikes / cumulative",
           "intervals / classes: w_k, c_k = p_k / w_k, F_k; histogram, ogive; approx. Freq(X <= x)",
           "one numeric variable: centre, spread, quartiles, outliers, plots",
           "numerical variable across groups: conditional summaries + boxplots",
           "SD vs CV across variables",
           "p-hat of one category + estimated SE",
           "joint / conditional distributions, chi-square, Cramer's V",
           "scatter, covariance, correlation, regression line",
           "type of each variable in a data frame; graphs and measures that fit",
           "P(A or B), P(A and B), P(A | B), independence",
           "total probability, Bayes' theorem",
           "E(X), Var(X), F(x), median of a discrete r.v.",
           "Binomial / Bernoulli probabilities (binomial: not in the 2026/27 syllabus)",
           "Uniform probabilities (not in the 2026/27 syllabus)",
           "P(X<a), P(X>a), P(a<X<b), quantiles, central interval of N(mu, sigma^2)",
           "same for Student t", "same for chi-square",
           "a_1 X_1 + ... + a_k X_k + c: E, Var, normal probabilities",
           "joint distribution: marginals, conditionals, covariance, independence",
           "sum/mean of n iid (CLT)",
           "sampling distribution of p-hat",
           "CI one mean (z and t rows, as UBStats)", "CI one proportion", "CI paired difference",
           "CI two means: all four intervals, or one case", "CI two proportions", "point estimate of a mean + SE",
           "test one mean (z and t rows)", "test one proportion", "test paired means", "test two means: all four, or one case",
           "test two proportions (d0 = 0 pooled)", "Levene: equal variances?",
           "beta & power, mean test", "beta & power, proportion test", "n for a mean (margin / width / SE)", "n for a proportion",
           "n per group for p1 - p2", "beta & power, two means (sigmas known)",
           "goodness of fit", "independence",
           "fit lm + all tests + interpretation", "CI mean response + PI individual", "H0: beta = value",
           "diagnostics + multicollinearity", "adjusted R2 + partial F", "CI for beta / for a change of c units",
           "enter a table from paper", "what is loaded now", "R one-liners",
           "which test? decision tree", "this list", "course notes", "check the installation", "the menu"),
  stringsAsFactors = FALSE)

#' @rdname guide
#' @export
sc_index <- function() {
  cat("\nSTATCRAM FUNCTIONS BY TOPIC   (type the function name + Tab for its arguments; ?test_mean for a help page)\n")
  for (tp in unique(.index$topic)) {
    cat("\n", tp, "\n", sep = "")
    sub <- .index[.index$topic == tp, ]
    for (i in seq_len(nrow(sub))) cat(sprintf("  %-15s %-9s %s\n", paste0(sub$fun[i], "()"), sub$menu[i], sub$what[i]))
  }
  invisible(.index)
}

#' @rdname guide
#' @export
sc_recipes <- function() {
  txt <- c(
    "R RECIPES FOR THE EXAM   (df = your data frame)",
    "",
    "See what is loaded ............ sc_data()      names(df)      str(df)      head(df)",
    "Type in raw values ............ x <- c(12.1, 13.4, 9.8)",
    "Table from paper .............. sc_table()     (numbered placeholders; saves a data frame)",
    "",
    "New column from others ........ df$diff  <- df$after - df$before",
    "                                df$ratio <- df$sales / df$visits",
    "                                df$logy  <- log(df$y)",
    "TRUE/FALSE indicator .......... df$high <- df$loyalty == \"High\"",
    "                                df$big  <- df$spend > 100",
    "Both conditions / either ...... df$x <- df$a == \"Yes\" & df$b > 5     |   (or)",
    "0/1 dummy ..................... df$d <- ifelse(df$spend > 100, 1, 0)",
    "Number -> classes [a,b) ....... df$band <- cut(df$age, breaks = c(18, 30, 45, 65), right = FALSE, include.lowest = TRUE)",
    "                                (book convention: closed left, open right, last class closed;",
    "                                 for tables/histograms just use desc_classes(df$age, breaks = ...))",
    "Ordinal levels in order ....... df$Satisf.F <- factor(df$Satisf, levels = c(\"VLow\", \"Low\", \"Med\", \"High\"))",
    "Treat a number as category .... df$g <- factor(df$g)",
    "Rename / merge categories ..... df$tier <- ifelse(df$tier %in% c(\"Gold\", \"Silver\"), \"Top\", \"Other\")",
    "Rename a column ............... names(df)[names(df) == \"old\"] <- \"new\"",
    "",
    "Keep some rows ................ north <- subset(df, region == \"North\")",
    "                                big   <- subset(df, spend > 100 & age < 40)",
    "One group's values ............ df$spend[df$loyalty == \"High\"]",
    "Drop missing values ........... df2 <- na.omit(df)",
    "",
    "Counts / cross-tab ............ table(df$a)      table(df$a, df$b)      prop.table(table(df$a, df$b), 1)",
    "Typed count table ............. m <- matrix(c(20, 30, 25, 25), nrow = 2, byrow = TRUE,",
    "                                            dimnames = list(gender = c(\"F\", \"M\"), buy = c(\"yes\", \"no\")))",
    "Group means ................... tapply(df$spend, df$loyalty, mean)",
    "Correlation ................... cor(df$x, df$y)      cor(df[, c(\"x\", \"y\", \"z\")])",
    "",
    "Everything you create appears immediately in sc() and sc_data().")
  cat(txt, sep = "\n")
  invisible(txt)
}

.notes_files <- c(rules = "exam-rules.md", descriptive = "descriptive.md", bivariate = "bivariate.md", estimation = "point-estimation.md",
                  inference = "inference.md", random = "random-variables.md", chisq = "chi-square.md",
                  regression = "regression.md", patterns = "past-paper-patterns.md")
.notes_titles <- c("absolute exam rules", "descriptive & grouped data", "two variables (ch. 4)", "point estimation & standard errors (ch. 6)",
                   "confidence intervals (ch. 6) & tests (ch. 7)", "probability & random variables (ch. 5)", "chi-square tests (ch. 7)",
                   "regression", "past-paper wording patterns")

.notes_path <- function(file) {
  p <- system.file("notes", file, package = "statcram")
  if (nzchar(p)) return(p)
  p <- file.path("inst", "notes", file)
  if (file.exists(p)) p else ""
}

#' @rdname guide
#' @export
sc_notes <- function(topic = NULL) {
  if (is.null(topic)) {
    cat("Course notes: sc_notes(\"<topic>\")\n")
    for (i in seq_along(.notes_files)) cat(sprintf("  %-12s %s\n", names(.notes_files)[i], .notes_titles[i]))
    return(invisible(names(.notes_files)))
  }
  topic <- match.arg(topic, names(.notes_files))
  path <- .notes_path(.notes_files[[topic]])
  if (!nzchar(path)) stop("Notes file not found; reinstall statcram.", call. = FALSE)
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  cat(txt, sep = "\n")
  invisible(txt)
}

#' @rdname guide
#' @export
sc_selftest <- function(verbose = TRUE) {
  old <- options(statcram.plot = FALSE); on.exit(options(old))
  quiet <- function(expr) { utils::capture.output(r <- expr); r }
  set.seed(42)
  a <- stats::rnorm(30, 10, 2); b <- stats::rnorm(25, 11, 3); m <- matrix(c(20, 30, 25, 25, 10, 40), nrow = 2)
  checks <- list(
    "one-mean t test = t.test" = c(quiet(test_mean(a, mu0 = 9))$p_value, stats::t.test(a, mu = 9)$p.value),
    "pooled t test = t.test(var.equal)" = c(quiet(test_2means(a, b, case = "pooled"))$p_value, stats::t.test(a, b, var.equal = TRUE)$p.value),
    "Welch t test = t.test" = c(quiet(test_2means(a, b, case = "welch"))$p_value, stats::t.test(a, b)$p.value),
    "Levene test = ANOVA on |x - median|" = c(quiet(test_levene(a, b))$p_value,
      stats::anova(stats::lm(c(abs(a - stats::median(a)), abs(b - stats::median(b))) ~ factor(rep(1:2, c(length(a), length(b))))))$`Pr(>F)`[1]),
    "paired t test = t.test(x - y)" = c(quiet(test_paired(a[1:20], b[1:20]))$p_value, stats::t.test(a[1:20] - b[1:20])$p.value),
    "one-proportion Z = prop.test" = c(quiet(test_prop(count = 120, n = 400, p0 = 0.25))$p_value,
                                       stats::prop.test(120, 400, 0.25, correct = FALSE)$p.value),
    "two-proportion Z = prop.test" = c(quiet(test_2props(count1 = 45, n1 = 100, count2 = 30, n2 = 90))$p_value,
                                       stats::prop.test(c(45, 30), c(100, 90), correct = FALSE)$p.value),
    "chi-square independence = chisq.test" = c(quiet(chisq_indep(m))$statistic, stats::chisq.test(m, correct = FALSE)$statistic),
    "chi-square GOF (0.7.2 value)" = c(quiet(chisq_gof(c(151, 117, 140, 162)))$statistic, 7.78245614035088),
    "mean CI = t.test" = c(quiet(ci_mean(a, conf = 0.9))$ci[1], stats::t.test(a, conf.level = 0.9)$conf.int[1]),
    "normal probability (0.7.2 value)" = c(quiet(prob_normal(80, 12, above = 100))$values[[1]], 1 - stats::pnorm(100, 80, 12)),
    "sample size n for a mean (0.7.2 value)" = c(quiet(n_mean(margin = 3, sigma = 12))$n, 62),
    "grouped mean (0.7.2 value)" = c(quiet(desc_classes(breaks = c(10, 25, 30, 35, 40, 45, 60), prop = c(6, 12, 24, 28, 18, 12)))$mean, 36.6),
    "regression PI = predict()" = c(quiet(reg_predict(quiet(reg_fit(mpg ~ wt, data = datasets::mtcars)), wt = 3))$predictions$pi_lower,
                                    stats::predict(stats::lm(mpg ~ wt, datasets::mtcars), data.frame(wt = 3), interval = "prediction")[, "lwr"]))
  ok <- vapply(checks, function(v) isTRUE(all.equal(unname(v[1]), unname(v[2]), tolerance = 1e-8)), logical(1))
  if (verbose) for (i in seq_along(ok)) cat(sprintf("[%s] %s\n", if (ok[i]) "PASS" else "FAIL", names(checks)[i]))
  cat(if (all(ok)) "\nALL STATCRAM SELF-TESTS PASSED.\n" else "\nSOME SELF-TESTS FAILED - do not rely on the failing procedures.\n")
  invisible(all(ok))
}
