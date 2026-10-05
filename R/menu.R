# ------------------------------------------------------------
# sc(): the numbered menu. Each procedure asks for its inputs, builds
# the matching function call, runs it and prints the result together
# with the one-line call to re-run it.
# ------------------------------------------------------------

# ---------- shared pieces ----------

.ask_mean_case <- function() {
  c("known", "t", "z")[.ask_choice("What does the question say about the population?", c(
    "sigma (population SD) is KNOWN                          -> Z",
    "sigma unknown, population NORMAL (or assumed normal)    -> t with n - 1 df",
    "sigma unknown, normality not stated / not normal, n LARGE -> Z (CLT approximation)"))]
}

.ask_case_2means <- function() {
  c("pooled", "welch", "large", "known")[.ask_choice("What does the question say about the two populations?", c(
    "variances unknown but ASSUMED EQUAL (normal populations)   -> pooled t",
    "variances unknown, NOT assumed equal (normal populations)  -> Welch t",
    "not normal / not stated, both samples LARGE                -> Z (CLT)",
    "population sigmas KNOWN                                    -> Z"))]
}

.one_mean_args <- function(data_what) {
  d <- .ask_data(data_what, "vector", allow_summary = TRUE)
  case <- .ask_mean_case()
  a <- list()
  if (d$summary) {
    a$xbar <- .ask_num("Sample mean xbar")
    a$n <- .ask_num("Sample size n")
    if (case != "known") a$s <- .ask_num("Sample standard deviation s")
  } else a$x <- .code(d$expr)
  if (case == "known") a$sigma <- .ask_num("Population standard deviation sigma")
  list(args = a, method = if (case == "known") NULL else case)
}

.one_prop_args <- function() {
  d <- .ask_data("the variable (categorical, TRUE/FALSE or 0/1)", "vector", allow_summary = TRUE,
                 hint = "an expression like df$loyalty == \"High\" also works")
  a <- list()
  if (d$summary) {
    a$n <- .ask_num("Sample size n")
    cnt <- .ask_num("Number of successes x (Enter if you only have the sample proportion)", allow_empty = TRUE)
    if (is.null(cnt)) a$phat <- .ask_num("Sample proportion p-hat") else a$count <- cnt
  } else {
    a$x <- .code(d$expr)
    a$event <- .ask_event(d$expr)
  }
  a
}

.paired_args <- function() {
  i <- .ask_choice("How are the paired data given?", c(
    "two columns / vectors measured on the same units (e.g. after and before)",
    "one column / vector that already holds the differences D",
    "summary numbers: mean and SD of the differences",
    "summary numbers: two means, two SDs and their covariance or correlation"))
  a <- list()
  if (i == 1) {
    a$x <- .code(.ask_data("the FIRST measurement (e.g. AFTER); D = first - second", "vector")$expr)
    a$y <- .code(.ask_data("the SECOND measurement (e.g. BEFORE)", "vector")$expr)
  } else if (i == 2) {
    a$x <- .code(.ask_data("the differences D", "vector")$expr)
  } else if (i == 3) {
    a$dbar <- .ask_num("Mean difference Dbar"); a$sd_d <- .ask_num("SD of the differences s_D"); a$n <- .ask_num("Number of pairs n")
  } else {
    a$mean1 <- .ask_num("Mean of the first measurement"); a$mean2 <- .ask_num("Mean of the second measurement")
    a$s1 <- .ask_num("SD of the first measurement"); a$s2 <- .ask_num("SD of the second measurement")
    a$n <- .ask_num("Number of pairs n")
    r <- .ask_num("Correlation r (Enter if you have the covariance instead)", allow_empty = TRUE)
    if (is.null(r)) a$cov <- .ask_num("Covariance") else a$r <- r
  }
  a
}

.two_sample_args <- function(kind = c("mean", "prop")) {
  kind <- match.arg(kind)
  i <- .ask_choice("How are the two groups given?", c(
    sprintf("one %s column + a grouping column (e.g. spend by loyalty)", if (kind == "mean") "numeric" else "categorical / TRUE-FALSE"),
    "two separate vectors (one per group)",
    "summary numbers from the question"))
  a <- list()
  if (i == 1) {
    d <- .ask_data(if (kind == "mean") "the numeric outcome" else "the outcome (categorical, TRUE/FALSE or 0/1)", "vector")
    a$x <- .code(d$expr)
    if (kind == "prop") a$event <- .ask_event(d$expr)
    g <- .ask_data("the grouping variable", "vector")
    a$group <- .code(g$expr)
    a$levels <- .ask_two_levels(g$expr)
  } else if (i == 2) {
    d1 <- .ask_data("group 1", "vector"); a$x <- .code(d1$expr)
    d2 <- .ask_data("group 2", "vector"); a$y <- .code(d2$expr)
    if (kind == "prop") a$event <- .ask_event(d1$expr)
  } else {
    if (kind == "mean") {
      a$xbar1 <- .ask_num("Group 1: sample mean"); a$n1 <- .ask_num("Group 1: sample size")
      a$xbar2 <- .ask_num("Group 2: sample mean"); a$n2 <- .ask_num("Group 2: sample size")
    } else {
      a$n1 <- .ask_num("Group 1: sample size n1")
      x1 <- .ask_num("Group 1: number of successes (Enter if you only have p-hat)", allow_empty = TRUE)
      if (is.null(x1)) a$phat1 <- .ask_num("Group 1: sample proportion") else a$count1 <- x1
      a$n2 <- .ask_num("Group 2: sample size n2")
      x2 <- .ask_num("Group 2: number of successes (Enter if you only have p-hat)", allow_empty = TRUE)
      if (is.null(x2)) a$phat2 <- .ask_num("Group 2: sample proportion") else a$count2 <- x2
    }
  }
  list(args = a, summary = i == 3)
}

.two_means_args <- function() {
  t <- .two_sample_args("mean")
  a <- t$args
  case <- .ask_case_2means()
  if (case == "known") {
    a$sigma1 <- .ask_num("Group 1: population sigma"); a$sigma2 <- .ask_num("Group 2: population sigma")
  } else if (t$summary) {
    a$s1 <- .ask_num("Group 1: sample SD"); a$s2 <- .ask_num("Group 2: sample SD")
  }
  list(args = a, case = case)
}

# ---------- topic 1: describe ----------

.m_desc_summary <- function() .mk("desc_summary", list(x = .code(.ask_data("a numeric variable", "vector")$expr)))
.m_desc_compare <- function() {
  a <- list(x = .code(.ask_data("the numerical variable", "vector")$expr),
            group = .code(.ask_data("the grouping variable", "vector")$expr))
  if (.ask_yes("Condition on a second grouping variable too (combinations of the two)?", FALSE))
    a$group2 <- .code(.ask_data("the second grouping variable", "vector")$expr)
  .mk("desc_compare", a)
}
.m_desc_cv <- function() {
  i <- .ask_choice("What do you have?", c("raw data for each variable", "means and SDs from the question"))
  if (i == 2) {
    k <- .ask_num("How many variables?", 2)
    nm <- .split_values(.mq(sprintf("Their names (%d, separated by spaces)", k)))
    mu <- .ask_nums("Means (in the same order)", k); s <- .ask_nums("SDs (in the same order)", k)
    if (length(nm) == k) { names(mu) <- nm }
    return(.mk("desc_cv", list(mean = mu, sd = s)))
  }
  ex <- character()
  repeat {
    d <- .ask_data(sprintf("variable %d", length(ex) + 1), "vector")
    ex <- c(ex, d$expr)
    if (length(ex) >= 2 && !.ask_yes("Add another variable?", FALSE)) break
  }
  as.call(c(as.name("desc_cv"), lapply(ex, str2lang)))
}
# Optional Freq(X <= x) / Freq(X >= x) / Freq(a <= X <= b); levels = ordered categories (NULL for numbers).
.ask_cum_query <- function(levels = NULL, approx = FALSE) {
  i <- .ask_choice(sprintf("Also a%s proportion?", if (approx) "n (approximate)" else " cumulative"),
                   c("no", "Freq(X <= x)  (at most x)", "Freq(X >= x)  (at least x)", "Freq(a <= X <= b)  (between)"))
  if (i == 1) return(list())
  pick <- function(what) if (is.null(levels)) .ask_num(what) else levels[.ask_choice(paste0(what, ":"), levels)]
  switch(i, NULL, list(at_most = pick("x")), list(at_least = pick("x")),
         list(between = c(pick("a (lower)"), pick("b (upper)"))))
}

.m_desc_freq <- function() {
  d <- .ask_data("the variable (or a frequency list)", "both")
  x <- .eval_expr(d$expr)
  a <- list(x = .code(d$expr))
  v <- if (is.data.frame(x)) x[[1]] else x
  lv <- if (is.data.frame(x)) as.character(x[[1]]) else .cats(v[!is.na(v)])
  if (!is.numeric(v) && !is.factor(v) && is.null(.parse_intervals(lv)) && length(lv) <= 20) {
    cat("Levels now (alphabetical):\n")
    for (k in seq_along(lv)) cat(sprintf(" %2d  %s\n", k, lv[k]))
    repeat {
      o <- .mq("Ordinal? Type the levels (or their numbers) from lowest to highest; Enter = keep", "")
      if (!nzchar(o)) break
      tok <- .split_values(o)
      idx <- suppressWarnings(as.integer(tok))
      ord <- if (!anyNA(idx) && all(idx %in% seq_along(lv))) lv[idx] else tok
      if (setequal(ord, lv) && length(ord) == length(lv)) { a$order <- ord; lv <- ord; break }
      cat("  Give every level exactly once (", length(lv), " levels).\n", sep = "")
    }
  } else if (is.factor(v)) lv <- levels(droplevels(v))
  c(a, .ask_cum_query(if (is.numeric(v)) NULL else lv)) -> a
  .mk("desc_freq", a)
}

.m_desc_classes <- function() {
  i <- .ask_choice("What do you have?", c(
    "raw numeric data to group into intervals (you choose the classes)",
    "a variable measured in classes (values like [0,50) or 10-20)",
    "a class table entered with sc_table() / already loaded",
    "type the class limits and frequencies now",
    "enter the class table with the table wizard first (sc_table)"))
  if (i == 5) { sc_table(); i <- 3 }
  if (i == 1) {
    a <- list(x = .code(.ask_data("the numeric variable", "vector")$expr))
    a$breaks <- .ask_nums("Classes: K = number of equal-width classes (e.g. 5), or the class limits (e.g. 10 20 30 40 50 60)")
  } else if (i == 2) {
    a <- list(x = .code(.ask_data("the variable measured in classes", "vector")$expr))
  } else if (i == 3) {
    a <- list(x = .code(.ask_data("the class table", "table")$expr))
  } else {
    b <- .ask_nums("Class limits in order (k + 1 numbers), e.g. 0 50 100 150 200 300   (open last class '1000 or more': end with Inf)")
    vt <- .ask_choice("The values are:", c("frequencies (counts)", "percentages / proportions"))
    v <- .ask_nums("Values for each class, in order", length(b) - 1)
    a <- if (vt == 1) list(breaks = b, freq = v) else list(breaks = b, prop = v)
    if (vt == 2) a$n <- .ask_num("Sample size n, if the question gives it (Enter = unknown)", allow_empty = TRUE)
  }
  .mk("desc_classes", c(a, .ask_cum_query(NULL, approx = i != 1)))
}
.m_desc_prop <- function() {
  a <- .one_prop_args()
  if (!is.null(a$phat)) { a$count <- a$phat * a$n; a$phat <- NULL }
  .mk("desc_prop", a)
}
# Ask for the order of an ordinal variable's levels (NULL = keep).
.ask_order <- function(lv, what) {
  cat(sprintf("Levels of %s now:\n", what))
  for (k in seq_along(lv)) cat(sprintf(" %2d  %s\n", k, lv[k]))
  repeat {
    o <- .mq("Ordinal? Type the levels (or their numbers) from lowest to highest; Enter = keep", "")
    if (!nzchar(o)) return(NULL)
    tok <- .split_values(o)
    idx <- suppressWarnings(as.integer(tok))
    ord <- if (!anyNA(idx) && all(idx %in% seq_along(lv))) lv[idx] else tok
    if (setequal(ord, lv) && length(ord) == length(lv)) return(ord)
    cat("  Give every level exactly once (", length(lv), " levels).\n", sep = "")
  }
}

.m_desc_crosstab <- function() {
  i <- .ask_choice("What do you have?", c("two variables (raw data: categorical or with few values)", "a count table (entered or typed)"))
  if (i == 2) {
    d <- .ask_data("the count table", "table")
    m <- .as_count_table(.eval_expr(d$expr))
    a <- list(x = .code(d$expr))
    if (!is.null(m) && ncol(m) <= 15) a$order_y <- .ask_order(colnames(m), "the column variable")
  } else {
    dx <- .ask_data("the ROW variable X (e.g. the grouping / explanatory one)", "vector")
    dy <- .ask_data("the COLUMN variable Y (e.g. the response)", "vector")
    a <- list(x = .code(dx$expr), y = .code(dy$expr))
    for (w in c("x", "y")) {
      v <- .eval_expr(if (w == "x") dx$expr else dy$expr)
      if (is.numeric(v) && length(unique(v[!is.na(v)])) > 12) {
        cat(sprintf("%s takes many values.\n", if (w == "x") dx$expr else dy$expr))
        a[[paste0("breaks_", w)]] <- .ask_nums("Classify it: K = number of equal-width classes, or the class limits")
      }
    }
    vy <- .eval_expr(dy$expr)
    if (is.null(a$breaks_y) && !is.numeric(vy) && !is.factor(vy)) {
      lv <- .cats(vy[!is.na(vy)])
      if (length(lv) <= 15) a$order_y <- .ask_order(lv, dy$expr)
    }
  }
  if (.ask_choice("Bar chart:", c("stacked: conditional distributions of Y | X", "side by side: joint proportions")) == 2) a$plot <- "beside"
  .mk("desc_crosstab", a)
}

.m_desc_cor <- function() {
  i <- .ask_choice("What do you have?", c("two numerical variables (scatterplot, covariance, correlation, regression line)",
                                          "several numerical variables (covariance / correlation matrices)",
                                          "a joint frequency table of two discrete numerical variables"))
  if (i == 2) {
    dfn <- .ask_df(); df <- get(dfn, envir = .GlobalEnv)
    nums <- names(df)[vapply(df, is.numeric, logical(1))]
    cat("Numerical columns:\n"); for (k in seq_along(nums)) cat(sprintf(" %2d  %s\n", k, nums[k]))
    repeat {
      a <- .mq("Numbers separated by spaces, or all", "all")
      idx <- if (tolower(a) == "all") seq_along(nums) else suppressWarnings(as.integer(.split_values(a)))
      if (length(idx) >= 2 && !anyNA(idx) && all(idx %in% seq_along(nums))) break
      cat("  Choose at least two numbers from the list.\n")
    }
    cols <- paste0("c(", paste(sprintf("\"%s\"", nums[idx]), collapse = ", "), ")")
    return(.mk("desc_cor", list(x = .code(sprintf("%s[, %s]", dfn, cols)))))
  }
  if (i == 3) return(.mk("desc_cor", list(x = .code(.ask_data("the joint frequency table", "table")$expr))))
  a <- list(x = .code(.ask_data("X (horizontal axis, e.g. the explanatory variable)", "vector")$expr),
            y = .code(.ask_data("Y (vertical axis, e.g. the response)", "vector")$expr))
  if (.ask_yes("Colour the points by a third variable?", FALSE)) a$color <- .code(.ask_data("the colouring variable", "vector")$expr)
  .mk("desc_cor", a)
}

# ---------- topic 2: probability ----------

# What to compute for a continuous distribution (several answers allowed: loop).
.ask_prob_question <- function(var = "X", quantile = TRUE) {
  out <- list()
  repeat {
    opts <- c(sprintf("P(%s < a)", var), sprintf("P(%s > a)", var), sprintf("P(a < %s < b)", var))
    if (quantile) opts <- c(opts, sprintf("quantile: the value x with P(%s <= x) = alpha", var),
                            "central interval containing a given % of the distribution (e.g. 90%)")
    if (length(out)) opts <- c(opts, "nothing else")
    i <- .ask_choice(if (length(out)) "Anything else?" else "What do you need?", opts)
    if (i == length(opts) && length(out)) break
    add <- switch(i,
      list(below = .ask_num("a")),
      list(above = .ask_num("a")),
      list(between = c(.ask_num("a (lower)"), .ask_num("b (upper)"))),
      list(quantile = .ask_prob("alpha (e.g. 0.95 for the 95th percentile)", 0.95)),
      list(middle = .ask_prob("Central probability (e.g. 0.90)", 0.9)))
    for (nm in names(add)) out[[nm]] <- c(out[[nm]], add[[nm]])
    if (!.ask_yes("Another probability / quantile for the same distribution?", FALSE)) break
  }
  if (!is.null(out$between) && length(out$between) > 2) out$between <- out$between[1:2]
  if (!is.null(out$middle)) out$middle <- out$middle[1]
  out
}

.m_prob_events <- function() {
  a <- list(pA = .ask_prob_any("P(A)"), pB = .ask_prob_any("P(B)"))
  i <- .ask_choice("What do you know about A and B together?", c(
    "P(A and B)", "P(A | B)", "P(B | A)", "P(A or B)", "A and B are independent"))
  switch(i,
    a$pAB <- .ask_prob_any("P(A and B)"),
    a$pA_given_B <- .ask_prob_any("P(A | B)"),
    a$pB_given_A <- .ask_prob_any("P(B | A)"),
    a$pAorB <- .ask_prob_any("P(A or B)"),
    a$independent <- TRUE)
  .mk("prob_events", a)
}

# A probability that may be 0 or 1 (or typed as %, or as a fraction like 18/37).
.ask_prob_any <- function(prompt) {
  repeat {
    v <- .ask_num(prompt)
    if (v > 1 && v <= 100) v <- v / 100
    if (v >= 0 && v <= 1) return(v)
    cat("  A probability must be between 0 and 1 (fractions such as 18/37 are fine).\n")
  }
}

.m_prob_bayes <- function() {
  k <- .ask_num("How many events in the partition E_1, ..., E_n (e.g. 2 for D / not D)?", 2)
  nm <- .split_values(.mq(sprintf("Their names (%d, separated by spaces; Enter = E1 E2 ...)", k), ""))
  if (length(nm) != k) nm <- paste0("E", seq_len(k))
  pr <- .ask_nums(sprintf("Prior probabilities P(E_i) for %s (must sum to 1)", paste(nm, collapse = ", ")), k)
  ev <- .mq("Name of the observed event B (e.g. positive)", "B")
  lk <- .ask_nums(sprintf("P(%s | E_i) for %s", ev, paste(nm, collapse = ", ")), k)
  names(pr) <- nm
  .mk("prob_bayes", list(prior = pr, likelihood = lk, event = ev))
}

.m_rv_discrete <- function() {
  v <- .ask_nums("Values x, separated by spaces")
  p <- .ask_nums("Probabilities P(X = x) in the same order (or percentages)", length(v))
  .mk("rv_discrete", list(values = v, probs = p))
}

.m_prob_binom <- function() {
  a <- list(size = .ask_num("Number of independent trials m (1 = Bernoulli)"), prob = .ask_prob_any("Success probability p"))
  repeat {
    i <- .ask_choice("What do you need?", c("P(X = x)", "P(X <= x)  (at most)", "P(X >= x)  (at least)", "P(X < x)",
                                           "P(X > x)  (more than)", "P(a <= X <= b)", "quantile / median (qbinom)", "only E(X), Var(X)"))
    key <- c("exactly", "at_most", "at_least", "less_than", "more_than", "between", "quantile", "")[i]
    if (key == "") break
    val <- switch(key, between = c(.ask_num("a"), .ask_num("b")), quantile = .ask_prob("alpha (0.5 = median)", 0.5), .ask_num("x"))
    a[[key]] <- c(a[[key]], val)
    if (!.ask_yes("Another probability for the same distribution?", FALSE)) break
  }
  if (a$size > 1 && .ask_yes("Also show the normal approximation N(mp, mp(1-p))?", FALSE)) a$normal <- TRUE
  .mk("prob_binom", a)
}

.m_prob_unif <- function() .mk("prob_unif", c(list(min = .ask_num("Minimum a"), max = .ask_num("Maximum b")), .ask_prob_question()))

.m_prob_normal <- function() {
  a <- list(mean = .ask_num("Mean mu", 0))
  if (.ask_choice("Spread given as:", c("standard deviation sigma", "variance sigma^2")) == 1) a$sd <- .ask_num("Standard deviation", 1)
  else a$var <- .ask_num("Variance")
  .mk("prob_normal", c(a, .ask_prob_question()))
}
.m_prob_t <- function() .mk("prob_t", c(list(df = .ask_num("Degrees of freedom")), .ask_prob_question("T")))
.m_prob_chisq <- function() .mk("prob_chisq", c(list(df = .ask_num("Degrees of freedom")), .ask_prob_question("X2")))

.m_rv_lincomb <- function() {
  k <- .ask_num("How many random variables? (1 for a linear transformation a + bX)", 2)
  if (k < 1 || k != round(k)) stop("The number of variables must be a whole number >= 1.", call. = FALSE)
  cat("T = a_1 X_1 + ... + a_k X_k + c\n")
  a <- .ask_nums(sprintf("Coefficients a_1 ... a_%d (e.g. 0.6 0.4; 1 1 for a sum)", k), k)
  mu <- .ask_nums(sprintf("Expected values E(X_1) ... E(X_%d)", k), k)
  args <- list(a = a, mu = mu)
  if (.ask_choice("Spread given as:", c("standard deviations", "variances")) == 1) args$sigma <- .ask_nums("SDs", k)
  else args$var <- .ask_nums("Variances", k)
  if (k > 1) {
    i <- .ask_choice("How are the variables related?", c("independent (or uncorrelated)", "the same correlation for every pair", "the same covariance for every pair"))
    if (i == 2) args$rho <- .ask_num("Correlation rho") else if (i == 3) args$cov <- .ask_num("Covariance")
  }
  cc <- .ask_num("Constant c (e.g. -900 fixed costs, 4 fixed minutes; 0 if none)", 0)
  if (cc != 0) args$c <- cc
  if (.ask_yes("Also probabilities / quantiles of T (assuming normality)?", FALSE)) args <- c(args, .ask_prob_question("T"))
  .mk("rv_lincomb", args)
}

.m_rv_joint <- function() {
  i <- .ask_choice("Where is the joint probability table?", c("entered / loaded (matrix or sc_table count table)",
                                                               "enter it now with the table wizard (sc_table, count table)"))
  if (i == 2) sc_table()
  d <- .ask_data("the joint probability table (values of X as row labels, of Y as column labels)", "table")
  .mk("rv_joint", list(p = .code(d$expr)))
}

.m_rv_iid <- function() {
  st <- c("mean", "sum")[.ask_choice("Distribution of:", c("the sample MEAN Xbar", "the SUM / total of the n variables"))]
  a <- list(mu = .ask_num("Mean of each variable mu"), sigma = .ask_num("SD of each variable sigma (Bernoulli: sqrt(p*(1-p)))"),
            n = .ask_num("n"), stat = st)
  if (.ask_yes("Also probabilities / quantiles?", TRUE)) a <- c(a, .ask_prob_question(if (st == "mean") "Xbar" else "S"))
  .mk("rv_iid", a)
}
.m_rv_prop <- function() {
  a <- list(p = .ask_prob("Population proportion p", 0.5), n = .ask_num("Sample size n"))
  if (.ask_yes("Also probabilities / quantiles?", TRUE)) a <- c(a, .ask_prob_question("p-hat"))
  .mk("rv_prop", a)
}

# ---------- topic 3: confidence intervals ----------

.m_ci_mean <- function() {
  o <- .one_mean_args("the sample values")
  .mk("ci_mean", c(o$args[intersect("x", names(o$args))], list(conf = .ask_prob("Confidence level", 0.95), method = o$method),
                   o$args[setdiff(names(o$args), "x")]))
}
.m_ci_prop <- function() {
  a <- .one_prop_args()
  .mk("ci_prop", c(a[intersect(c("x", "event"), names(a))], list(conf = .ask_prob("Confidence level", 0.95)),
                   a[setdiff(names(a), c("x", "event"))]))
}
.m_ci_paired <- function() {
  a <- .paired_args()
  .mk("ci_paired", c(a[intersect(c("x", "y"), names(a))], list(conf = .ask_prob("Confidence level", 0.95)),
                     a[setdiff(names(a), c("x", "y"))]))
}
.m_ci_2means <- function() {
  o <- .two_means_args()
  first <- o$args[intersect(c("x", "y"), names(o$args))]
  .mk("ci_2means", c(first, list(case = o$case, conf = .ask_prob("Confidence level", 0.95)),
                     o$args[setdiff(names(o$args), names(first))]))
}
.m_ci_2props <- function() {
  t <- .two_sample_args("prop")
  first <- t$args[intersect(c("x", "y", "event"), names(t$args))]
  .mk("ci_2props", c(first, list(conf = .ask_prob("Confidence level", 0.95)), t$args[setdiff(names(t$args), names(first))]))
}

# ---------- topic 4: hypothesis tests ----------

.m_test_mean <- function() {
  o <- .one_mean_args("the sample values")
  mu0 <- .ask_num("Null value mu0  (H0: mu = mu0)")
  .mk("test_mean", c(o$args[intersect("x", names(o$args))], list(mu0 = mu0, alt = .ask_alt("mu", .f(mu0)), alpha = .ask_prob("Significance level alpha", 0.05),
                                         method = o$method), o$args[setdiff(names(o$args), "x")]))
}
.m_test_prop <- function() {
  a <- .one_prop_args()
  p0 <- .ask_prob("Null value p0  (H0: p = p0)", NULL)
  .mk("test_prop", c(a[intersect("x", names(a))], list(p0 = p0), a[intersect("event", names(a))],
                     list(alt = .ask_alt("p", .f(p0)), alpha = .ask_prob("Significance level alpha", 0.05)),
                     a[setdiff(names(a), c("x", "event"))]))
}
.m_test_paired <- function() {
  a <- .paired_args()
  d0 <- .ask_num("Null value of the mean difference (H0: mu_D = d0)", 0)
  .mk("test_paired", c(a[intersect(c("x", "y"), names(a))],
                       list(d0 = if (d0 != 0) d0, alt = .ask_alt("mu_D", .f(d0)), alpha = .ask_prob("Significance level alpha", 0.05)),
                       a[setdiff(names(a), c("x", "y"))]))
}
.m_test_2means <- function() {
  o <- .two_means_args()
  first <- o$args[intersect(c("x", "y"), names(o$args))]
  .mk("test_2means", c(first, list(case = o$case, alt = .ask_alt("mu1 - mu2", "0"), alpha = .ask_prob("Significance level alpha", 0.05)),
                       o$args[setdiff(names(o$args), names(first))]))
}
.m_test_2props <- function() {
  t <- .two_sample_args("prop")
  first <- t$args[intersect(c("x", "y", "event"), names(t$args))]
  .mk("test_2props", c(first, list(alt = .ask_alt("p1 - p2", "0"), alpha = .ask_prob("Significance level alpha", 0.05)),
                       t$args[setdiff(names(t$args), names(first))]))
}

# ---------- topic 5: power / sample size ----------

.m_power_mean <- function() {
  mu0 <- .ask_num("mu0 (value under H0)")
  .mk("power_mean", list(mu0 = mu0, mu1 = .ask_num("mu1 (the true mean)"), sigma = .ask_num("Population sigma"),
                         n = .ask_num("Sample size n"), alpha = .ask_prob("alpha", 0.05), alt = .ask_alt("mu", .f(mu0))))
}
.m_power_prop <- function() {
  p0 <- .ask_prob("p0 (value under H0)", NULL)
  .mk("power_prop", list(p0 = p0, p1 = .ask_prob("p1 (the true proportion)", NULL), n = .ask_num("Sample size n"),
                         alpha = .ask_prob("alpha", 0.05), alt = .ask_alt("p", .f(p0))))
}
.m_n_mean <- function() {
  i <- .ask_choice("Target:", c("margin of error (half-width of the CI)", "standard error"))
  s <- .ask_num("Population sigma (or a prior estimate)")
  if (i == 1) .mk("n_mean", list(margin = .ask_num("Margin of error"), sigma = s, conf = .ask_prob("Confidence level", 0.95)))
  else .mk("n_mean", list(se = .ask_num("Target standard error"), sigma = s))
}
.m_n_prop <- function() {
  i <- .ask_choice("Target:", c("margin of error (half-width of the CI)", "standard error"))
  p <- .ask_num("Prior estimate of p (Enter = 0.5, the conservative choice)", allow_empty = TRUE)
  if (i == 1) .mk("n_prop", list(margin = .ask_num("Margin of error (e.g. 0.03)"), p = p, conf = .ask_prob("Confidence level", 0.95)))
  else .mk("n_prop", list(se = .ask_num("Target standard error"), p = p))
}

# ---------- topic 6: chi-square ----------

.m_chisq_gof <- function() {
  d <- .ask_data("the categorical variable or a frequency list", "both")
  x <- .eval_expr(d$expr)
  cats <- names(.as_freq(x)) %||% .cats(if (is.data.frame(x)) x[[1]] else x)
  cat("Categories (in this order): ", paste(cats, collapse = ", "), "\n", sep = "")
  p <- .ask_nums(sprintf("Probabilities under H0 for the %d categories (Enter = all equal)", length(cats)),
                 length(cats), allow_empty = TRUE)
  .mk("chisq_gof", list(x = .code(d$expr), p = p, alpha = .ask_prob("Significance level alpha", 0.05)))
}
.m_chisq_indep <- function() {
  i <- .ask_choice("What do you have?", c("two categorical columns / vectors (raw data)", "a count table (entered with sc_table, or typed)"))
  a <- if (i == 2) list(x = .code(.ask_data("the count table", "table")$expr))
       else list(x = .code(.ask_data("the first categorical variable", "vector")$expr),
                 y = .code(.ask_data("the second categorical variable", "vector")$expr))
  .mk("chisq_indep", c(a, list(alpha = .ask_prob("Significance level alpha", 0.05))))
}

# ---------- topic 7: regression ----------

.bq <- function(nm) if (.syntactic(nm)) nm else paste0("`", nm, "`")

.ask_df <- function() {
  dfs <- Filter(function(n) is.data.frame(get(n, envir = .GlobalEnv)), ls(envir = .GlobalEnv))
  if (!length(dfs)) stop("No data frame is loaded. load(\"EXAM.RData\") or build one with sc_table().", call. = FALSE)
  if (length(dfs) == 1) { cat("Data frame: ", dfs, "\n", sep = ""); return(dfs) }
  dfs[.ask_choice("Which data frame?", sprintf("%s  (%d rows x %d cols)", dfs,
    vapply(dfs, function(n) nrow(get(n, envir = .GlobalEnv)), 1L), vapply(dfs, function(n) ncol(get(n, envir = .GlobalEnv)), 1L)))]
}

.next_free <- function(prefix) {
  i <- 1
  while (exists(paste0(prefix, i), envir = .GlobalEnv, inherits = FALSE)) i <- i + 1
  paste0(prefix, i)
}

.m_reg_fit <- function() {
  dfn <- .ask_df()
  df <- get(dfn, envir = .GlobalEnv)
  i <- .ask_choice("How do you want to give the model?", c("pick Y and the X variables from a list", "type the formula myself (e.g. y ~ x1 + log(x2))"))
  if (i == 2) {
    fml <- .mq("Formula")
  } else {
    cols <- names(df)
    tags <- vapply(cols, function(cn) .kind_tag(df[[cn]]), "")
    lab <- sprintf("%s   %s", cols, tags)
    y <- cols[.ask_choice("Dependent variable Y:", lab)]
    rest <- setdiff(seq_along(cols), match(y, cols))
    cat("Explanatory variables X:\n")
    for (j in rest) cat(sprintf(" %2d  %s\n", j, lab[j]))
    repeat {
      a <- .mq("Numbers separated by spaces (e.g. 2 4 5), or all")
      idx <- if (tolower(a) == "all") rest else suppressWarnings(as.integer(.split_values(a)))
      if (length(idx) && !anyNA(idx) && all(idx %in% rest)) break
      cat("  Type numbers from the list above.\n")
    }
    xs <- cols[idx]
    num_few <- xs[vapply(xs, function(cn) is.numeric(df[[cn]]) && length(unique(na.omit(df[[cn]]))) <= 6, logical(1))]
    terms <- vapply(xs, .bq, "")
    if (length(num_few)) {
      cat("These numeric X take few values: ", paste(num_few, collapse = ", "), "\n", sep = "")
      if (.ask_yes("Treat them as categories (factor dummies)?", FALSE))
        terms[xs %in% num_few] <- sprintf("factor(%s)", terms[xs %in% num_few])
    }
    fml <- paste(.bq(y), "~", paste(terms, collapse = " + "))
  }
  name <- .mq("Save the model as", .next_free("fit"))
  if (!.syntactic(name)) name <- .next_free("fit")
  structure(.mk("reg_fit", list(formula = .code(fml), data = .code(dfn), alpha = .ask_prob("Significance level alpha", 0.05))),
            assign_to = name)
}

.ask_model <- function(prompt = "Which model?") {
  objs <- ls(envir = .GlobalEnv)
  ok <- objs[vapply(objs, function(n) {
    o <- get(n, envir = .GlobalEnv)
    inherits(o, "lm") || (inherits(o, "sc_result") && inherits(o$model, "lm"))
  }, logical(1))]
  if (!length(ok)) stop("No fitted model yet: fit one first (menu 7 > 1, or reg_fit()).", call. = FALSE)
  labs <- vapply(ok, function(n) {
    o <- get(n, envir = .GlobalEnv); m <- if (inherits(o, "lm")) o else o$model
    sprintf("%s   %s", n, paste(deparse(formula(m)), collapse = " "))
  }, "")
  ok[.ask_choice(prompt, labs)]
}

.m_reg_predict <- function() {
  mn <- .ask_model()
  o <- get(mn, envir = .GlobalEnv); m <- .as_lm(o)
  vars <- all.vars(delete.response(terms(m)))
  mf <- model.frame(m)
  vals <- list()
  for (v in vars) {
    col <- if (v %in% names(mf)) mf[[v]] else NULL
    if (!is.null(col) && (is.factor(col) || is.character(col))) {
      lv <- .cats(col); vals[[v]] <- lv[.ask_choice(sprintf("Value of %s:", v), lv)]
    } else {
      fx <- grep(sprintf("factor\\(%s\\)", v), names(mf), value = TRUE)
      if (length(fx)) { lv <- levels(mf[[fx[1]]]); vals[[v]] <- as.numeric(lv[.ask_choice(sprintf("Value of %s:", v), lv)]) }
      else vals[[v]] <- .ask_num(sprintf("Value of %s", v))
    }
  }
  .mk("reg_predict", c(list(model = .code(mn)), vals, list(conf = .ask_prob("Confidence level", 0.95))))
}
.m_reg_test <- function() {
  mn <- .ask_model(); m <- .as_lm(get(mn, envir = .GlobalEnv))
  terms <- names(coef(m))
  term <- terms[.ask_choice("Which coefficient?", terms)]
  value <- .ask_num("Null value (H0: beta = ?)", 0)
  .mk("reg_test", list(model = .code(mn), term = term, value = value, alt = .ask_alt("beta", .f(value)),
                       alpha = .ask_prob("Significance level alpha", 0.05)))
}
.m_reg_check <- function() .mk("reg_check", list(model = .code(.ask_model())))
.m_reg_compare <- function() {
  a <- .ask_model("First (smaller) model:"); b <- .ask_model("Second (larger) model:")
  .mk("reg_compare", list(model1 = .code(a), model2 = .code(b), alpha = .ask_prob("Significance level alpha", 0.05)))
}

# ---------- topic 8: data tools ----------

.m_table <- function() { sc_table(); NULL }
.m_table_edit <- function() {
  d <- .ask_data("the table to fix", "both")
  obj <- .eval_expr(d$expr)
  if (!is.data.frame(obj)) stop("That is not a table / data frame.", call. = FALSE)
  if (!.syntactic(d$expr)) stop("Choose a table saved under its own name (e.g. tab1).", call. = FALSE)
  eval(call("sc_table", edit = as.name(d$expr)), envir = list2env(list(sc_table = sc_table), parent = .GlobalEnv))
  NULL
}
.m_sc_data <- function() { sc_data(); NULL }
.m_vector <- function() {
  name <- .mq("Name for the new vector", .next_free("x"))
  if (!.syntactic(name)) stop("Use a simple R name such as x1 or sales.", call. = FALSE)
  v <- .ask_nums("Values separated by spaces (decimal point or comma)")
  assign(name, v, envir = .GlobalEnv)
  cat(sprintf("\nSaved %s (%d values). R code:\n%s <- %s\n", name, length(v), name, .vec_code(v)))
  NULL
}
.m_command <- function() {
  cat("Type an R command; it runs in your workspace. Examples:\n",
      "  df$diff <- df$after - df$before\n",
      "  df$high <- df$loyalty == \"High\"\n",
      "  north   <- subset(df, region == \"North\")\n",
      "(sc_recipes() lists more)\n", sep = "")
  cmd <- .mq("R>")
  if (!nzchar(cmd)) return(NULL)
  res <- withVisible(eval(parse(text = cmd), envir = .GlobalEnv))
  if (res$visible) print(res$value)
  cat("Done. New/changed variables are available immediately in every menu.\n")
  NULL
}
.m_recipes <- function() { sc_recipes(); NULL }

# ---------- topic 9: guide ----------

.m_guide <- function() { sc_guide(); NULL }
.m_index <- function() { sc_index(); NULL }
.m_notes <- function() {
  tp <- names(.notes_files)
  sc_notes(tp[.ask_choice("Which notes?", sprintf("%-12s %s", tp, .notes_titles))])
  NULL
}

# ---------- the menu tree ----------

.menu <- list(
  list(title = "Describe data", items = list(
    list("Frequency table: counts, proportions, cumulative, Freq(X <= x) (+ bar / spike plot)", .m_desc_freq),
    list("Group numbers into intervals / variable measured in classes (densities, histogram, ogive)", .m_desc_classes),
    list("Summary of one numeric variable (mean, median, quartiles, SD, CV, outliers + plots)", .m_desc_summary),
    list("Numerical variable across groups: conditional summaries + side-by-side boxplots", .m_desc_compare),
    list("Compare dispersion of variables (SD vs coefficient of variation)", .m_desc_cv),
    list("Sample proportion of one category + its estimated standard error", .m_desc_prop),
    list("Two variables with few values: joint / conditional distributions, chi-square, Cramer's V", .m_desc_crosstab),
    list("Two numerical variables: scatterplot, covariance, correlation, regression line", .m_desc_cor))),
  list(title = "Probability & random variables", items = list(
    list("Events: union, intersection, conditional probability, independence", .m_prob_events),
    list("Law of total probability and Bayes' theorem", .m_prob_bayes),
    list("Discrete random variable: E(X), Var(X), SD, F(x), median", .m_rv_discrete),
    list("Binomial / Bernoulli: P(X = x), P(X <= x), P(X > x), quantiles", .m_prob_binom),
    list("Uniform distribution", .m_prob_unif),
    list("Normal: probabilities, quantiles, central interval", .m_prob_normal),
    list("Student t probability or quantile", .m_prob_t),
    list("Chi-square probability or quantile", .m_prob_chisq),
    list("Linear transformation / combination of r.v.s (a + bX, aX + bY + c, portfolios, sums)", .m_rv_lincomb),
    list("Joint distribution of two discrete r.v.s: marginals, conditionals, covariance, independence", .m_rv_joint),
    list("Sum or mean of n iid variables (CLT)", .m_rv_iid),
    list("Sampling distribution of a sample proportion", .m_rv_prop))),
  list(title = "Confidence intervals", items = list(
    list("One mean", .m_ci_mean),
    list("One proportion", .m_ci_prop),
    list("Paired mean difference (same units twice)", .m_ci_paired),
    list("Two independent means", .m_ci_2means),
    list("Two proportions", .m_ci_2props))),
  list(title = "Hypothesis tests", items = list(
    list("One mean", .m_test_mean),
    list("One proportion", .m_test_prop),
    list("Paired means (same units twice, before / after)", .m_test_paired),
    list("Two independent means", .m_test_2means),
    list("Two proportions", .m_test_2props))),
  list(title = "Type II error / power / sample size", items = list(
    list("Type II error (beta) and power: mean test, sigma known", .m_power_mean),
    list("Type II error (beta) and power: proportion test", .m_power_prop),
    list("Sample size for a mean", .m_n_mean),
    list("Sample size for a proportion", .m_n_prop))),
  list(title = "Chi-square tests", items = list(
    list("Goodness of fit (one variable vs stated shares)", .m_chisq_gof),
    list("Independence (are two categorical variables associated?)", .m_chisq_indep))),
  list(title = "Regression", items = list(
    list("Fit a model (coefficients, t tests, F test, R2, interpretation)", .m_reg_fit),
    list("Predict: CI for the mean response + PI for one individual", .m_reg_predict),
    list("Test one coefficient against any value", .m_reg_test),
    list("Diagnostics: residual plots, leverage, Cook's D, multicollinearity", .m_reg_check),
    list("Compare two models (adjusted R2, partial F test)", .m_reg_compare))),
  list(title = "Enter a table from paper / data tools", items = list(
    list("Enter a table from paper (numbered placeholders)", .m_table),
    list("Fix a table you entered", .m_table_edit),
    list("Show everything loaded right now", .m_sc_data),
    list("Type in raw values as a vector", .m_vector),
    list("Create or change a variable (type an R command)", .m_command),
    list("R recipes: new columns, subsets, factors, tables", .m_recipes))),
  list(title = "Which test? / course notes", items = list(
    list("Which test do I need? (decision tree)", .m_guide),
    list("All functions by topic", .m_index),
    list("Course notes and exam rules", .m_notes))))

.version <- function() tryCatch(as.character(utils::packageVersion("statcram")), error = function(e) "dev")

.show_main <- function() {
  cat("\nSTATCRAM ", .version(), " - main menu        (b = back, m = main menu, q = quit)\n", sep = "")
  for (i in seq_along(.menu)) cat(sprintf(" %d  %s\n", i, .menu[[i]]$title))
  cat(" 0  Quit\n")
}

# Run one procedure; navigation signals propagate, other errors are reported.
.run_item <- function(t, i) {
  item <- .menu[[t]]$items[[i]]
  cat("\n-- ", item[[1]], " --\n", sep = "")
  res <- tryCatch({
    cl <- item[[2]]()
    if (!is.null(cl)) .run_call(cl, attr(cl, "assign_to"))
    cat(sprintf("\n(shortcut: sc(%d, %d) opens this directly)\n", t, i))
    "done"
  }, sc_back = function(e) "back",
     error = function(e) {
       # m / q pressed inside a procedure: pass the signal up to sc()
       if (inherits(e, "sc_nav")) stop(e)
       cat("\nProblem: ", conditionMessage(e), "\nNothing was changed; try again.\n", sep = "")
       "error"
     })
  invisible(res)
}

.topic_loop <- function(t, i = NULL) {
  tp <- .menu[[t]]
  repeat {
    if (is.null(i)) {
      cat("\n", toupper(tp$title), "\n", sep = "")
      for (k in seq_along(tp$items)) cat(sprintf(" %2d  %s\n", k, tp$items[[k]][[1]]))
      cat("  b  back to main menu\n")
      repeat {
        a <- tolower(trimws(.read_line("Choose: ")))
        if (a %in% c("b", "m", "0")) return("main")
        if (a %in% c("q", "quit")) return("quit")
        v <- suppressWarnings(as.integer(a))
        if (!is.na(v) && v >= 1 && v <= length(tp$items)) { i <- v; break }
        cat("  Type one of the numbers (b = back, q = quit).\n")
      }
    }
    .run_item(t, i)
    i <- NULL
  }
}

#' The statcram menu
#'
#' `sc()` opens a numbered menu: topic -> procedure -> questions about your
#' data. Each procedure lists what is loaded **right now** (every data frame
#' column, every vector, every table - re-scanned each time, so variables you
#' have just created appear), accepts any typed R expression, or summary
#' numbers from the question. It then runs the matching function and prints
#' the result and the one-line call to re-run or edit it.
#'
#' At any prompt: `b` = back, `m` = main menu, `q` = quit. Inside the
#' table wizard `b` means "back one cell".
#'
#' Shortcuts: `sc(4)` opens topic 4 (hypothesis tests), `sc(4, 1)` runs the
#' one-mean test directly. `statcram()` is the same as `sc()`.
#'
#' @param topic Optional topic number (1-9).
#' @param item Optional procedure number within the topic.
#' @return `NULL`, invisibly.
#' @examples
#' \dontrun{
#' sc()        # main menu
#' sc(4, 1)    # one-mean hypothesis test
#' sc(8, 1)    # enter a table from paper
#' }
#' @export
sc <- function(topic = NULL, item = NULL) {
  if (!is.null(topic) && !is.numeric(topic))
    stop("sc() does not need the data: every data frame, column and vector loaded is listed automatically. Just run sc().", call. = FALSE)
  if (!is.null(topic) && (topic < 1 || topic > length(.menu))) stop("topic must be 1 to ", length(.menu), call. = FALSE)
  if (!is.null(item) && (is.null(topic) || item < 1 || item > length(.menu[[topic]]$items)))
    stop("item must be a procedure number of topic ", topic, call. = FALSE)
  tryCatch({
    repeat {
      if (is.null(topic)) {
        .show_main()
        repeat {
          a <- tolower(trimws(.read_line("Choose: ")))
          if (a %in% c("0", "q", "quit")) .signal("sc_quit")
          v <- suppressWarnings(as.integer(a))
          if (!is.na(v) && v >= 1 && v <= length(.menu)) { topic <- v; break }
          cat("  Type a number from 0 to ", length(.menu), ".\n", sep = "")
        }
      }
      res <- tryCatch(.topic_loop(topic, item), sc_main = function(e) "main")
      item <- NULL; topic <- NULL
      if (identical(res, "quit")) .signal("sc_quit")
    }
  }, sc_quit = function(e) cat("\nLeft statcram. sc() reopens the menu.\n"))
  invisible(NULL)
}

#' @rdname sc
#' @export
statcram <- function(topic = NULL, item = NULL) sc(topic, item)
