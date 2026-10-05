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
#'   classes and tables during the exam.
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
    "Describe data (no population claim)",
    "  one numeric variable ...................................... desc_summary()   sc(1,1)",
    "  numeric variable by groups / boxplots ...................... desc_compare()   sc(1,2)",
    "  dispersion of variables in different units ................ desc_cv()        sc(1,3)",
    "  class-interval table (10-25, 25-30, ...) .................. desc_classes()   sc(1,4)",
    "  'report the proportion ... and how its SE is estimated' ... desc_prop()      sc(1,6)",
    "",
    "NUMERIC outcome (means)",
    "  one sample ................ CI: ci_mean()     test: test_mean()     sc(3,1) / sc(4,1)",
    "     sigma known -> Z | sigma unknown + normal -> t | not normal, n large -> Z (CLT)",
    "  same units measured twice (before/after, matched) ... ci_paired() / test_paired()   sc(3,3) / sc(4,3)",
    "  two independent groups ..... ci_2means() / test_2means()   sc(3,4) / sc(4,4)",
    "     variances assumed equal -> pooled t | not assumed equal -> Welch t",
    "     not normal but both n large -> Z | sigmas known -> Z",
    "",
    "CATEGORICAL outcome (an event: 'customers with High loyalty')",
    "  one sample ................ CI: ci_prop()     test: test_prop()     sc(3,2) / sc(4,2)",
    "  two independent groups .... ci_2props() / test_2props()            sc(3,5) / sc(4,5)",
    "  one variable vs stated shares (uniform, 30/50/20 ...) ... chisq_gof()     sc(6,1)",
    "  two categorical variables associated? ................... chisq_indep()   sc(6,2)",
    "",
    "Type II error / power ......... power_mean() / power_prop()          sc(5,1) / sc(5,2)",
    "Required sample size .......... n_mean() / n_prop()                  sc(5,3) / sc(5,4)",
    "Normal / t / chi-square probabilities .... prob_normal() / prob_t() / prob_chisq()   sc(2,1..3)",
    "Random variables: E, Var of X, aX+bY+c, sums/means of iid ... rv_discrete() rv_linear() rv_iid()   sc(2,4..6)",
    "",
    "Explain / predict a numeric Y from X variables (regression)",
    "  fit, t tests, F test, R2 ......... reg_fit()       sc(7,1)",
    "  predict mean (CI) / individual (PI) ... reg_predict()   sc(7,2)",
    "  H0: beta = some value ............ reg_test()      sc(7,3)",
    "  assumptions, outliers, multicollinearity ... reg_check()   sc(7,4)",
    "  compare models / adjusted R2 ..... reg_compare()   sc(7,5)",
    "",
    "Table printed on paper? sc_table() (menu 8,1) rebuilds it as a data frame first.",
    "Keywords: 'same' units twice -> paired; 'assume equal variances' -> pooled;",
    "'is X associated with Y' (both categorical) -> chisq_indep; 'follows / agrees with' shares -> chisq_gof.")
  cat(txt, sep = "\n")
  invisible(txt)
}

.index <- data.frame(
  topic = c(rep("1 Describe", 7), rep("2 Probability", 7), rep("3 Confidence intervals", 5),
            rep("4 Hypothesis tests", 5), rep("5 Power / sample size", 4), rep("6 Chi-square", 2),
            rep("7 Regression", 5), rep("8 Data tools", 3), rep("9 Guide", 5)),
  menu = c(paste0("sc(1,", 1:7, ")"), paste0("sc(2,", 1:7, ")"), paste0("sc(3,", 1:5, ")"), paste0("sc(4,", 1:5, ")"),
           paste0("sc(5,", 1:4, ")"), paste0("sc(6,", 1:2, ")"), paste0("sc(7,", 1:5, ")"),
           "sc(8,1)", "sc(8,3)", "sc(8,6)", "sc(9,1)", "sc(9,2)", "sc(9,3)", "", ""),
  fun = c("desc_summary", "desc_compare", "desc_cv", "desc_classes", "desc_freq", "desc_prop", "desc_crosstab",
          "prob_normal", "prob_t", "prob_chisq", "rv_discrete", "rv_linear", "rv_iid", "rv_prop",
          "ci_mean", "ci_prop", "ci_paired", "ci_2means", "ci_2props",
          "test_mean", "test_prop", "test_paired", "test_2means", "test_2props",
          "power_mean", "power_prop", "n_mean", "n_prop",
          "chisq_gof", "chisq_indep",
          "reg_fit", "reg_predict", "reg_test", "reg_check", "reg_compare",
          "sc_table", "sc_data", "sc_recipes",
          "sc_guide", "sc_index", "sc_notes", "sc_selftest", "sc"),
  what = c("one numeric variable: centre, spread, quartiles, outliers, plots",
           "numeric variable by group + boxplots",
           "SD vs CV across variables",
           "class-interval table: density, modal class, approx. mean/quantiles",
           "frequency table + SE of each proportion",
           "p-hat of one category + estimated SE",
           "two-way table with row/column %",
           "P(X<a), P(X>a), P(a<X<b), quantiles of N(mean, sd)",
           "same for Student t", "same for chi-square",
           "E(X), Var(X) of a discrete RV", "aX + bY + c with covariance", "sum/mean of n iid (CLT)",
           "sampling distribution of p-hat",
           "CI one mean (Z / t / large-sample)", "CI one proportion", "CI paired difference",
           "CI two means (pooled / welch / large / known)", "CI two proportions",
           "test one mean", "test one proportion", "test paired means", "test two means", "test two proportions",
           "beta & power, mean test", "beta & power, proportion test", "n for a mean", "n for a proportion",
           "goodness of fit", "independence",
           "fit lm + all tests + interpretation", "CI mean response + PI individual", "H0: beta = value",
           "diagnostics + multicollinearity", "adjusted R2 + partial F",
           "enter a table from paper", "what is loaded now", "R one-liners for the exam",
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
    "Number -> classes ............. df$band <- cut(df$age, breaks = c(18, 30, 45, 65), right = FALSE)",
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

.notes_files <- c(rules = "exam-rules.md", descriptive = "descriptive.md", estimation = "point-estimation.md",
                  inference = "inference.md", random = "random-variables.md", chisq = "chi-square.md",
                  regression = "regression.md", patterns = "past-paper-patterns.md")
.notes_titles <- c("absolute exam rules", "descriptive & grouped data", "point estimation & standard errors",
                   "confidence intervals & tests", "random variables & sampling", "chi-square tests",
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
