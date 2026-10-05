# ------------------------------------------------------------
# Descriptive statistics: summaries, group comparison, dispersion,
# class-interval (grouped) tables, frequencies, proportions, cross-tabs.
# ------------------------------------------------------------

#' Descriptive statistics
#'
#' * `desc_summary()`  one numeric variable (book ch. 3): mode(s), median,
#'   mean; five-number summary (R quartiles + the book's hand rule);
#'   percentiles; range, IQR, variance s^2 (n - 1), SD, CV; Tukey fences,
#'   whiskers and extreme values; shape read from the boxplot.
#' * `desc_compare()`  a numeric variable across groups (book 4.3): conditional
#'   summaries (n, n.a, min, Q1, median, mean, Q3, max, sd, CV, percentiles)
#'   and side-by-side boxplots (or histograms); optional second grouping variable.
#' * `desc_cv()`       compare dispersion of variables (SD vs coefficient of variation).
#' * `desc_vars()`     overview of a data frame: the statistical type of each
#'   column (identifier, qualitative nominal / ordinal, quantitative discrete /
#'   continuous) and the graphs and measures that fit it.
#' * `desc_freq()`     frequency distribution of a variable with few distinct
#'   values: counts f_k, proportions p_k, percentages, cumulative F_k;
#'   Freq(X <= x), Freq(X >= x), Freq(a <= X <= b); bar / pie / spike /
#'   cumulative plot. Ordinal variables: give the level order with `order`.
#' * `desc_classes()`  numerical data grouped into intervals [a, b) (last
#'   one closed): from raw data + `breaks` (K equal-width classes or the
#'   class limits), from a variable measured in classes ("[0,50)", "10-20"),
#'   or from a class table. Width w_k, density c_k = p_k / w_k, cumulative
#'   F_k, approximate Freq(X <= x) etc. (uniform within classes), modal
#'   class, approximate mean / quantiles; histogram + ogive.
#' * `desc_prop()`     sample proportion of one category and its estimated SE.
#' * `desc_crosstab()` two variables with few values (book 4.2): joint counts and
#'   proportions with marginals, conditional distributions Y|X and X|Y,
#'   conditional summaries (mode, quartiles), expected counts under
#'   independence, chi-square and Cramer's V; stacked / side-by-side bars.
#' * `desc_cor()`      two numerical variables (book 4.4): scatterplot,
#'   covariance, Pearson correlation, regression line b0 + b1 x; correlation
#'   matrices; covariance from a joint frequency table.
#'
#' Data can be any vector or expression (`df$spend`, `c(...)`,
#' `subset(df, g == "A")$y`), or a table typed in with [sc_table()].
#'
#' @param x Data (numeric for `desc_summary`/`desc_compare`; categorical for
#'   `desc_freq`/`desc_prop`/`desc_crosstab`). `desc_crosstab()` and
#'   `desc_freq()` also accept a typed table.
#' @param group,group2 Grouping variable(s) for `desc_compare()` (with
#'   `group2` the groups are the combinations of the two).
#' @param ... Numeric vectors to compare (`desc_cv`), named or not.
#' @param mean,sd Summary numbers for `desc_cv()` (alone or together with raw
#'   variables in `...`).
#' @param var `desc_cv()`: variances instead of `sd`.
#' @param names `desc_cv()`: labels for the variables given by `mean =`.
#' @param probs Percentiles to report (pairs such as 0.05 / 0.95 are read
#'   as "the central 90% lie between ...").
#' @param value `desc_summary()`, `desc_compare()`: value(s) to check
#'   against the outlier fences Q1 - 1.5 IQR and Q3 + 1.5 IQR (within each
#'   group for `desc_compare()`), e.g. "is a bid of 35 extremely low?".
#' @param population `desc_summary()`, `desc_cor()`: TRUE when the data are
#'   the whole population (divisor N instead of n - 1).
#' @param breaks `desc_classes()`: a single number K (K classes of equal
#'   width w = (Max - Min) / K, starting at the minimum) or the class limits
#'   `c(10, 20, 30, ...)`. Without raw data: the limits of a typed class table.
#' @param lower,upper Class limits as two vectors (alternative to `breaks`).
#' @param freq,prop Frequencies or proportions / percentages per class
#'   (typed class table). Open classes: use `Inf` / `-Inf` as the outer limit,
#'   e.g. `breaks = c(0, 300, 500, 1000, Inf)` for a last class "1000 or more".
#' @param n Sample size: for `desc_prop()` with `count`; for `desc_classes()`
#'   and `desc_freq()` when only proportions are given (enables the
#'   n / (n - 1) correction of the variance).
#' @param at_most,at_least,between Optional: proportion of values
#'   `<= at_most`, `>= at_least`, or within `between = c(a, b)`. Exact for
#'   raw values (`desc_freq`; `desc_summary`, where `between` means
#'   a <= X < b), approximate for classes (`desc_classes`, where several
#'   values or a list of pairs are allowed). For ordered categories give
#'   level names, e.g. `at_least = "High"`.
#' @param below,above `desc_summary()`: exact share of raw values `< below`
#'   or `> above` (with the `mean(condition)` line that gives it in R).
#' @param compare `desc_freq()`: a second distribution (raw values or a
#'   typed table) shown side by side in percentages, with mode, median and mean.
#' @param density `desc_classes()`: densities read off a histogram, one per
#'   class (`breaks =` gives the limits); one `NA` is obtained as 1 minus
#'   the other proportions.
#' @param split `desc_classes()`: a class limit; the classes below and above
#'   it are analysed as two subgroups (median and mean of each).
#' @param data `desc_vars()`: a data frame.
#' @param order `desc_freq()`: the levels in their substantive order, e.g.
#'   `c("VLow", "Low", "Med", "High")` (needed for cumulative frequencies of
#'   an ordinal character variable).
#' @param sort `desc_freq()`: `"none"` (standard order), `"decreasing"` or
#'   `"increasing"` frequency (Pareto-style).
#' @param plot Plot type. `desc_freq()`: `"auto"` (bars for categories,
#'   spikes for numbers), `"bars"`, `"pie"`, `"spike"`, `"cum"`.
#'   `desc_classes()`: `"both"` (histogram + ogive), `"hist"`, `"ogive"`.
#'   `desc_crosstab()`: `"stacked"` (conditional Y|X) or `"beside"` (joint).
#'   `desc_compare()`: `"boxplot"` or `"hist"`.
#' @param se `desc_freq()`: also show the estimated SE of each proportion.
#' @param y Second variable: column variable for `desc_crosstab()`, vertical
#'   axis for `desc_cor()`.
#' @param y_event `desc_crosstab()`: one or more categories of `y` to combine
#'   (e.g. `c("high", "veryhigh")`): their share within each level of `x`,
#'   compared with the marginal share expected under independence.
#' @param order_x,order_y `desc_crosstab()`: level order of ordinal row /
#'   column variables.
#' @param breaks_x,breaks_y `desc_crosstab()`: classify a numerical row /
#'   column variable into intervals first (K or the class limits).
#' @param line `desc_cor()`: add the regression line to the scatterplot.
#' @param color `desc_cor()`: a third variable used to colour the points.
#' @param event Category of interest for `desc_prop()`; for `desc_freq()`,
#'   one or more categories whose combined share is reported.
#' @param count Number of successes for `desc_prop()` (with `n`).
#' @return An `sc_result` (prints itself).
#' @examples
#' x <- c(12, 15, 9, 22, 14, 18, 30, 11)
#' desc_summary(x)
#' desc_freq(c("H", "M", "VH", "VH", "H", "M", "VH"), order = c("M", "H", "VH"))
#' tickets <- c(25, 35, 13, 21, 24, 37, 26, 46, 58, 30, 32, 13, 12, 38, 41, 43, 44, 27, 53, 27)
#' desc_classes(tickets, breaks = c(10, 20, 30, 40, 50, 60))
#' desc_classes(breaks = c(10, 25, 30, 35, 40, 45, 60),
#'              prop = c(6, 12, 24, 28, 18, 12), at_most = 37)
#' @name describe
NULL

.fences <- function(q1, q3) c(q1 - 1.5 * (q3 - q1), q3 + 1.5 * (q3 - q1))

# Modes of any vector (book 3.2): most frequent value(s). "No mode" when all
# values have the same frequency (e.g. all distinct).
.modes <- function(v) {
  tab <- table(factor(as.character(v), levels = .cats(v)))
  if (length(tab) > 1 && length(unique(as.numeric(tab))) == 1) return(list(values = character(), count = tab[[1]], n = length(v)))
  list(values = names(tab)[tab == max(tab)], count = max(tab), n = length(v))
}

.mode_line <- function(mo) {
  if (!length(mo$values)) return("Mode     = none: all values have the same frequency (equi-frequent), so the mode is useless here")
  sprintf("Mode     = %s   (%s; frequency %d = %s)%s", paste(mo$values, collapse = ", "),
          if (length(mo$values) > 1) sprintf("%d modes", length(mo$values)) else "1 mode",
          as.integer(mo$count), .pct(mo$count / mo$n),
          if (mo$count / mo$n < 0.05) "  - weak mode, not representative" else "")
}

# The book's hand rule (3.3.1): smallest value with cumulative frequency >= p
# (R's quantile type 1). The median uses the midpoint rule when F = 0.5 exactly.
.hand_quantile <- function(v, p) {
  v <- sort(v); n <- length(v)
  vapply(p, function(pp) v[max(1, ceiling(n * pp - 1e-9))], numeric(1))
}

# Book 3.3.2: compare the lower and upper parts of the box and the whiskers.
# Is a given value extreme (beyond the Tukey fences Q1 - 1.5 IQR, Q3 + 1.5 IQR)?
.value_check_lines <- function(value, q, xlab) {
  fe <- .fences(q[1], q[2]); iqr <- q[2] - q[1]
  c(sprintf("Is %s an extreme value of %s?  Fences: Q1 - 1.5 IQR = %s - 1.5 x %s = %s;  Q3 + 1.5 IQR = %s + 1.5 x %s = %s",
            paste(.f(value), collapse = ", "), xlab, .f(q[1]), .f(iqr), .f(fe[1]), .f(q[2]), .f(iqr), .f(fe[2])),
    vapply(value, function(v) sprintf("  %s: %s", .f(v),
      if (v < fe[1]) sprintf("%s < %s -> extreme LOW value (lower outlier)", .f(v), .f(fe[1]))
      else if (v > fe[2]) sprintf("%s > %s -> extreme HIGH value (upper outlier)", .f(v), .f(fe[2]))
      else sprintf("inside [%s, %s] -> not extreme", .f(fe[1]), .f(fe[2]))), character(1)))
}

.value_check_words <- function(value, q, xlab) {
  fe <- .fences(q[1], q[2])
  vapply(value, function(v) {
    if (v < fe[1]) sprintf("An extremely low value of %s is one below Q1 - 1.5 IQR = %s - 1.5 x %s = %s: since %s < %s, the value %s is extreme (a lower outlier).",
                           xlab, .f(q[1]), .f(q[2] - q[1]), .f(fe[1]), .f(v), .f(fe[1]), .f(v))
    else if (v > fe[2]) sprintf("An extremely high value of %s is one above Q3 + 1.5 IQR = %s + 1.5 x %s = %s: since %s > %s, the value %s is extreme (an upper outlier).",
                                xlab, .f(q[2]), .f(q[2] - q[1]), .f(fe[2]), .f(v), .f(fe[2]), .f(v))
    else sprintf("Values of %s are extreme if below Q1 - 1.5 IQR = %s or above Q3 + 1.5 IQR = %s: %s lies between the two thresholds, so it is not an extreme value.",
                 xlab, .f(fe[1]), .f(fe[2]), .f(v))
  }, character(1))
}

# Reading of percentiles: P_low / P_high pairs give the central share.
.pct_reading <- function(probs, pr, xlab, mx = NULL, mn = NULL) {
  out <- character()
  for (a in probs) {
    b <- 1 - a
    if (any(abs(probs - b) < 1e-9)) next
    pa <- pr[which.min(abs(probs - a))]
    if (a >= 0.5 && !is.null(mx))
      out <- c(out, sprintf("P%s = %s: the top %s%% of the units have %s above %s (between P%s and the maximum %s).",
                            round(100 * a), .f(pa), round(100 * b), xlab, .f(pa), round(100 * a), .f(mx)))
    else if (a < 0.5 && !is.null(mn))
      out <- c(out, sprintf("P%s = %s: the bottom %s%% of the units have %s at most %s (between the minimum %s and P%s).",
                            round(100 * a), .f(pa), round(100 * a), xlab, .f(pa), .f(mn), round(100 * a)))
  }
  for (a in probs[probs < 0.5]) {
    b <- 1 - a
    if (any(abs(probs - b) < 1e-9)) {
      pa <- pr[which.min(abs(probs - a))]; pb <- pr[which.min(abs(probs - b))]
      out <- c(out, sprintf("P%s = %s and P%s = %s: %s%% of the units have %s at most %s and %s%% have more than %s (equivalently, P%s is the largest value among the %s%% of units with the lowest values and P%s the smallest among the %s%% with the highest), so the central (most typical) %s%% lie between %s and %s.",
                            round(100 * a), .f(pa), round(100 * b), .f(pb), round(100 * a), xlab, .f(pa), round(100 * a), .f(pb),
                            round(100 * a), round(100 * a), round(100 * b), round(100 * a),
                            round(100 * (b - a)), .f(pa), .f(pb)))
    }
  }
  if (length(out)) paste(out, collapse = " ") else NULL
}

# Shape of a distribution read as the official answers do: the central part
# (box halves, compared with the IQR) and the whiskers; outliers by side; the
# mean - median gap as a tie-breaker. Returns the phrase and the evidence.
.shape_info <- function(a, q1, q2, q3) {
  iqr <- q3 - q1; fe <- .fences(q1, q3); reg <- a[a >= fe[1] & a <= fe[2]]
  lo <- min(reg); hi <- max(reg); n <- length(a)
  nl <- sum(a < fe[1]); nu <- sum(a > fe[2]); m <- mean(a); s <- if (n > 1) stats::sd(a) else 0
  tol <- max(iqr, 1e-12)
  dir3 <- function(lower, upper, k) if (abs(upper - lower) <= k * tol) "sym" else if (upper > lower) "right" else "left"
  box <- dir3(q2 - q1, q3 - q2, 0.1); wh <- dir3(q1 - lo, hi - q3, 0.2)
  close_mm <- abs(m - q2) <= 0.2 * max(s, 1e-12)
  one_sided <- box == "sym" || wh == "sym"
  core <- if (box == "sym" && wh == "sym") "sym"
          else if (box != "left" && wh != "left") (if (close_mm && one_sided) "slight_right" else "right")
          else if (box != "right" && wh != "right") (if (close_mm && one_sided) "slight_left" else "left")
          else if (close_mm) "sym" else "mixed"
  part <- if (box != "sym") "half of the box" else "whisker"
  side <- if (nl + nu == 0) "" else if (nl > 2 * nu) "lower" else if (nu > 2 * nl) "upper" else "both"
  strong <- (side == "lower" && nl >= 0.1 * n) || (side == "upper" && nu >= 0.1 * n)
  phrase <- if (strong) sprintf("strongly %s-skewed because of the numerous %s outliers (%s of the values)",
                                if (side == "lower") "left" else "right", side, .pct(max(nl, nu) / n, 1))
    else if (core == "sym" && side %in% c("lower", "upper"))
      sprintf("the central part is fairly symmetric, although there are some outliers, mainly on the %s side", side)
    else switch(core, sym = "fairly symmetric", right = "right-skewed (longer upper tail)", left = "left-skewed (longer lower tail)",
                slight_right = sprintf("fairly symmetric overall, at most slightly right-skewed (longer upper %s; mean close to the median)", part),
                slight_left = sprintf("fairly symmetric overall, at most slightly left-skewed (longer lower %s; mean close to the median)", part),
                mixed = "no clear skewness (box and whiskers point in different directions)")
  if (!strong && core != "sym" && side %in% c("lower", "upper")) phrase <- sprintf("%s; outliers mainly on the %s side", phrase, side)
  core <- if (strong) (if (side == "lower") "left" else "right") else if (startsWith(core, "slight_")) "sym" else core
  list(phrase = phrase, core = core,
       evidence = sprintf("box halves %s vs %s, whiskers %s vs %s, mean %s vs median %s, outliers %d low / %d high",
                          .f(q2 - q1), .f(q3 - q2), .f(q1 - lo), .f(hi - q3), .f(m), .f(q2), nl, nu),
       nl = nl, nu = nu, whiskers = c(lo, hi))
}

.box_shape <- function(lo, q1, q2, q3, hi) {
  near <- function(a, b) abs(a - b) <= 0.1 * max(a, b, 1e-12)
  box <- if (near(q2 - q1, q3 - q2)) "sym" else if (q3 - q2 > q2 - q1) "right" else "left"
  wh <- if (near(q1 - lo, hi - q3)) "sym" else if (hi - q3 > q1 - lo) "right" else "left"
  if (box == "sym" && wh == "sym") return("likely symmetric (median centred in the box, whiskers of similar length)")
  if (box != "left" && wh != "left") return("right-skewed (longer upper part of the box and/or longer upper whisker)")
  if (box != "right" && wh != "right") return("left-skewed (longer lower part of the box and/or longer lower whisker)")
  "no clear skewness (the box and the whiskers point in different directions)"
}

# Exact shares of raw values satisfying a condition, with the R line that gives them.
.share_lines <- function(v, expr, xlab, at_most, below, at_least, above, between) {
  lines <- character(); words <- character(); vals <- list()
  add <- function(key, cond, lab, rcode, txt) {
    k <- sum(cond); sh <- k / length(v)
    lines <<- c(lines, sprintf("  Freq(%s) = %s / %s = %s      in R: mean(%s)", lab, k, length(v), .f(sh), rcode))
    words <<- c(words, sprintf("%s of the units (%s of %s) have %s %s.", .pct(sh, 1), k, length(v), xlab, txt))
    vals[[key]] <<- c(vals[[key]], sh)
  }
  for (a in at_most) add("at_most", v <= a, sprintf("X <= %s", .f(a)), sprintf("%s <= %s", expr, .f(a)), sprintf("at most %s", .f(a)))
  for (a in below) add("below", v < a, sprintf("X < %s", .f(a)), sprintf("%s < %s", expr, .f(a)), sprintf("below %s", .f(a)))
  for (a in at_least) add("at_least", v >= a, sprintf("X >= %s", .f(a)), sprintf("%s >= %s", expr, .f(a)), sprintf("at least %s", .f(a)))
  for (a in above) add("above", v > a, sprintf("X > %s", .f(a)), sprintf("%s > %s", expr, .f(a)), sprintf("above %s", .f(a)))
  if (!is.null(between)) {
    if (length(between) != 2) stop("between must be two values: c(a, b) for a <= X < b.", call. = FALSE)
    a <- min(between); b <- max(between)
    add("between", v >= a & v < b, sprintf("%s <= X < %s", .f(a), .f(b)), sprintf("%s >= %s & %s < %s", expr, .f(a), expr, .f(b)),
        sprintf("at least %s and below %s", .f(a), .f(b)))
  }
  list(lines = lines, words = words, values = vals)
}

#' @rdname describe
#' @export
desc_summary <- function(x, probs = c(0.05, 0.10, 0.90, 0.95, 0.99), population = FALSE, value = NULL,
                         at_most = NULL, below = NULL, at_least = NULL, above = NULL, between = NULL) {
  sx <- substitute(x); xlab <- .label(sx)
  v <- .num(x, xlab)
  sh <- .share_lines(v, paste(deparse(sx), collapse = ""), xlab, at_most, below, at_least, above, between)
  n <- length(v); m <- mean(v); md <- median(v)
  dev2 <- sum((v - m)^2)
  if (population) { s2 <- dev2 / n; s2_txt <- sprintf("sigma^2 = sum(x_i - mu)^2 / N = %s / %s = %s", .f(dev2), n, .f(s2)) }
  else { s2 <- if (n > 1) dev2 / (n - 1) else NA; s2_txt <- sprintf("s^2 = sum(x_i - xbar)^2 / (n - 1) = %s / %s = %s", .f(dev2), n - 1, .f(s2)) }
  s <- sqrt(s2); sname <- if (population) "sigma" else "s"
  q <- quantile(v, c(.25, .75), names = FALSE)
  hq <- .hand_quantile(v, c(.25, .75))
  fe <- .fences(q[1], q[2])
  regular <- v[v >= fe[1] & v <= fe[2]]
  outl <- sort(v[v < fe[1] | v > fe[2]])
  wlo <- min(regular); whi <- max(regular)
  pr <- quantile(v, probs, names = FALSE)
  mo <- .modes(v)
  SH <- .shape_info(v, q[1], md, q[2]); shape <- SH$phrase
  mm <- if (abs(m - md) <= 0.05 * (if (is.na(s) || s == 0) 1 else s)) "mean close to the median"
        else if (m > md) "mean > median, pulled toward the upper tail" else "mean < median, pulled toward the lower tail"
  lines <- c(
    sprintf("Variable: %s   n = %s", xlab, n), "",
    "Central tendency",
    paste0("  ", .mode_line(mo)),
    sprintf("  Median   = %s   (%s)", .f(md), if (n %% 2) sprintf("middle value: position %d of %d", (n + 1) / 2, n)
            else sprintf("average of the two middle values, positions %d and %d", n / 2, n / 2 + 1)),
    sprintf("  Mean     = sum(x_i) / n = %s / %s = %s", .f(sum(v)), n, .f(m)), "",
    "Five-number summary (boxplot)",
    sprintf("  Min = %s   Q1 = %s   Q2 = median = %s   Q3 = %s   Max = %s", .f(min(v)), .f(q[1]), .f(md), .f(q[2]), .f(max(v))),
    if (isTRUE(all.equal(q, hq))) "  (quartiles from R's quantile(), the same as the book's hand rule: smallest value with F >= 0.25 / 0.75)"
    else c("  Quartiles above come from R's quantile() default, as in R/UBStats output.",
           sprintf("  Book's hand rule (smallest value with cumulative frequency >= 0.25 / 0.75): Q1 = %s, Q3 = %s", .f(hq[1]), .f(hq[2]))),
    sprintf("  Percentiles: %s", paste(sprintf("p%s = %s", round(100 * probs), .f(pr)), collapse = "   ")), "",
    "Dispersion",
    sprintf("  Range    = Max - Min = %s - %s = %s", .f(max(v)), .f(min(v)), .f(max(v) - min(v))),
    sprintf("  IQR      = Q3 - Q1 = %s - %s = %s", .f(q[2]), .f(q[1]), .f(q[2] - q[1])),
    sprintf("  Variance %s", s2_txt),
    sprintf("  SD       %s = sqrt(%s) = %s", sname, .f(s2), .f(s)),
    sprintf("  CV       = %s / |mean| = %s / %s = %s  (%s of the mean)", sname, .f(s), .f(abs(m)), .f(s / abs(m)), .pct(s / abs(m))), "",
    "Enhanced boxplot (Tukey's rule)",
    sprintf("  1.5 x IQR = %s  ->  regular values lie in [Q1 - 1.5 IQR, Q3 + 1.5 IQR] = [%s, %s]",
            .f(1.5 * (q[2] - q[1])), .f(fe[1]), .f(fe[2])),
    sprintf("  Whiskers end at the min / max regular values: %s and %s", .f(wlo), .f(whi)),
    sprintf("  Extreme values (outliers): %s", if (length(outl)) paste(.f(outl), collapse = ", ") else "none"),
    sprintf("  Share of outliers: %d low (%s) and %d high (%s) out of n = %s", sum(v < fe[1]), .pct(mean(v < fe[1]), 1),
            sum(v > fe[2]), .pct(mean(v > fe[2]), 1), n), "",
    sprintf("Shape: Q2 - Q1 = %s vs Q3 - Q2 = %s;  lower whisker %s vs upper whisker %s",
            .f(md - q[1]), .f(q[2] - md), .f(q[1] - wlo), .f(whi - q[2])),
    sprintf("  -> %s; %s", shape, mm),
    if (length(value)) c("", .value_check_lines(value, q, xlab)),
    if (length(sh$lines)) c("", "Exact shares from the raw data (count of the units satisfying the condition / n):", sh$lines))
  .with_plot(function() {
    op <- par(mfrow = c(1, 2)); on.exit(par(op))
    boxplot(v, main = paste("Boxplot:", xlab), ylab = xlab, col = "grey90")
    hist(v, main = paste("Histogram:", xlab), xlab = xlab, col = "grey85", border = "white", freq = FALSE)
    abline(v = c(m, md), col = c("firebrick", "navy"), lwd = 2, lty = c(1, 2))
    legend("topright", c("mean", "median"), col = c("firebrick", "navy"), lty = c(1, 2), bty = "n", cex = 0.8)
  })
  p95_txt <- if (any(abs(probs - 0.95) < 1e-9)) {
    p95 <- pr[which.min(abs(probs - 0.95))]
    sprintf("Upper fence Q3 + 1.5 IQR = %s vs P95 = %s: %s", .f(fe[2]), .f(p95),
            if (p95 > fe[2]) "P95 is above the fence, so MORE than 5% of the values are anomalously high (upper outliers)."
            else "P95 is not above the fence, so at most 5% of the values are anomalously high.")
  }
  wording <- c(
    .w("frame", sprintf("%s is a quantitative variable observed on n = %s units%s.", xlab, n, if (population) " (the whole population)" else " (a sample)")),
    .w("tool", "The boxplot (five-number summary and outliers) and the histogram describe location, variability and shape; mean and median are compared because the mean is attracted by long tails while the median is robust to them."),
    .w("result", sprintf("Median = %s, mean = %s; Q1 = %s, Q3 = %s (IQR = %s); values from %s to %s; standard deviation %s = %s (CV = %s of the mean).",
                         .f(md), .f(m), .f(q[1]), .f(q[2]), .f(q[2] - q[1]), .f(min(v)), .f(max(v)), sname, .f(s), .pct(s / abs(m)))),
    .w("meaning", sprintf("Half of the units have %s between %s and %s; the median %s is the maximum value reached by the lowest 50%% of the units. On average the values deviate from the mean by about %s.",
                          xlab, .f(q[1]), .f(q[2]), .f(md), .f(s)),
       sprintf("The distribution is %s (%s).", sub(" \\(.*", "", shape), SH$evidence),
       .pct_reading(probs, pr, xlab, max(v), min(v)), sh$words),
    .w("conclusion", if (grepl("close", mm)) sprintf("Mean (%s) and median (%s) offer a similar description of the centre.", .f(m), .f(md))
                     else sprintf("Mean (%s) %s median (%s): with a skewed distribution the median, which is robust to extreme values, describes the centre better; report both to show the tail.",
                                  .f(m), if (m > md) ">" else "<", .f(md)),
       if (length(value)) .value_check_words(value, q, xlab)),
    .w("caveat", if (length(outl)) sprintf("%d value(s) lie more than 1.5 IQR beyond the box and are flagged as extreme: %s%s.", length(outl),
                                           paste(.f(head(outl, 12)), collapse = ", "), if (length(outl) > 12) " ..." else "") else "No value is flagged as extreme by the 1.5 IQR rule.",
       p95_txt))
  .result("Descriptive summary", lines, wording,
          c(.dropped_note(v), if (population) "Population formulas used (divisor N)."), match.call(),
          n = n, mean = m, median = md, mode = mo$values, variance = s2, sd = s, quartiles = q, hand_quartiles = hq,
          fivenum = c(min = min(v), q1 = q[1], median = md, q3 = q[2], max = max(v)),
          range = max(v) - min(v), iqr = q[2] - q[1], cv = s / abs(m), whiskers = c(wlo, whi), outliers = outl,
          percentiles = stats::setNames(pr, paste0("p", round(100 * probs))), shares = sh$values,
          ubstats = c(.ub_call("distr.summary.x", x = sx, stats = c("central", "fivenumbers", "dispersion", .ub_pcts(probs))),
                      .ub_call("distr.plot.x", x = sx, plot.type = "boxplot"),
                      if (population) "# UBStats uses the sample formulas (divisor n - 1)"))
}

# Statistical type of one column (course terms) and what fits it.
.var_info <- function(v, nm) {
  vv <- v[!is.na(v)]; k <- length(unique(vv)); flag <- ""
  lnm <- tolower(nm)
  if (grepl("^(id|index|idx|code|row|rownum|obs)$", lnm) ||
      (is.numeric(vv) && length(vv) > 10 && k == length(vv) && all(abs(vv - round(vv)) < 1e-9) && all(diff(sort(vv)) == 1))) {
    type <- "identifier"
  } else if (is.numeric(vv) && grepl("^(year|anno|yr)$", lnm)) {
    type <- "qualitative ordinal"; flag <- "numbers used as labels (years)"
  } else if (is.logical(vv)) {
    type <- "qualitative nominal"; flag <- "binary"
  } else if (is.numeric(vv)) {
    int <- all(abs(vv - round(vv)) < 1e-9)
    type <- if (int && (k <= 15 || diff(range(vv)) <= 100)) "quantitative discrete" else "quantitative continuous"
    if (int && type == "quantitative continuous") flag <- "integers, but many distinct values"
  } else {
    lv <- .cats(vv)
    if (is.ordered(v) || (is.factor(v) && !.alphabetical(levels(droplevels(v))))) type <- "qualitative ordinal"
    else if (!is.null(.ordinal_guess(lv))) { type <- "qualitative ordinal"; g <- .ordinal_guess(lv); if (length(g) > 4) g <- c(g[1:3], "...", g[length(g)])
      flag <- sprintf("stored as text (alphabetical): order %s", paste(g, collapse = " < ")) }
    else if (all(grepl("^[0-9]{1,4}[-/.][0-9]{1,2}[-/.][0-9]{1,4}$", head(as.character(vv), 50)))) { type <- "qualitative ordinal"; flag <- "dates" }
    else { type <- "qualitative nominal"; if (k == 2) flag <- "binary" else if (k > 30) flag <- sprintf("%d categories", k) }
  }
  list(variable = nm, r_type = class(v)[1], distinct = k, type = type, note = flag)
}

#' @rdname describe
#' @export
desc_vars <- function(data) {
  sd <- substitute(data); dlab <- paste(deparse(sd), collapse = "")
  if (!is.data.frame(data)) stop("desc_vars() needs a data frame, e.g. desc_vars(pizzerie).", call. = FALSE)
  info <- lapply(names(data), function(nm) .var_info(data[[nm]], nm))
  tab <- data.frame(variable = vapply(info, `[[`, "", "variable"), R_class = vapply(info, `[[`, "", "r_type"),
                    distinct = vapply(info, `[[`, 0, "distinct"), type = vapply(info, `[[`, "", "type"),
                    note = vapply(info, `[[`, "", "note"), stringsAsFactors = FALSE)
  fits <- c("qualitative nominal" = "bar chart or pie chart; mode only",
            "qualitative ordinal" = "bar chart with the levels in order; mode, median, quartiles, cumulative frequencies",
            "quantitative discrete" = "spike plot (histogram if many values); mode, median, mean, quartiles, variance, SD",
            "quantitative continuous" = "histogram (classes) and boxplot; median, mean, quartiles, variance, SD, CV",
            "identifier" = "not a statistical variable (it only labels the units)")
  by_type <- function(t) tab$variable[tab$type == t]
  lines <- c(sprintf("Data frame %s: %d units (rows) and %d columns", dlab, nrow(data), ncol(data)), "",
             .table_lines(tab), "", "What fits each type:")
  for (t in names(fits)) if (length(by_type(t)))
    lines <- c(lines, sprintf("  %s (%s): %s", t, paste(by_type(t), collapse = ", "), fits[[t]]))
  qual <- tab$variable[grepl("^qualitative", tab$type)]; quan <- tab$variable[grepl("^quantitative", tab$type)]
  ids <- by_type("identifier"); disc <- by_type("quantitative discrete")
  wording <- c(
    .w("frame", sprintf("%s is a data frame: each row is a unit, each column a variable.", dlab)),
    .w("tool", "Each variable is classified by type: qualitative (nominal: no order; ordinal: ordered categories) or quantitative (discrete: countable values; continuous: measures). An identifier only labels the units. The type decides the graphs and the summary measures that can be used."),
    .w("result", sprintf("%s contains %d columns observed on %d units.%s So there are %d statistical variables: %d qualitative (%s) and %d quantitative (%s).",
            dlab, ncol(data), nrow(data),
            if (length(ids)) sprintf(" %s %s an identifier, not a statistical variable.", paste(ids, collapse = ", "), if (length(ids) > 1) "are" else "is") else "",
            length(qual) + length(quan), length(qual), paste(qual, collapse = ", "), length(quan), paste(quan, collapse = ", "))),
    .w("meaning", if (length(quan)) sprintf("Among the quantitative variables, %s.",
                              if (!length(disc)) "all are continuous"
                              else if (length(disc) == length(quan)) "all are discrete"
                              else sprintf("%s %s discrete and the others continuous", paste(disc, collapse = ", "), if (length(disc) > 1) "are" else "is"))),
    .w("caveat", if (any(tab$note != "" & grepl("text", tab$note)))
      sprintf("%s: ordinal but stored as text, so R sorts the categories alphabetically; define a factor with the levels in their natural order before tables and graphs.",
              paste(tab$variable[grepl("text", tab$note)], collapse = ", "))))
  .result("Variables in the data frame", lines, wording, NULL, match.call(),
          table = tab, rbase = sprintf("str(%s)", dlab), ubstats = NULL)
}

#' @rdname describe
#' @export
desc_cv <- function(..., mean = NULL, sd = NULL, var = NULL, names = NULL) {
  dots <- list(...)
  ub <- .ub_raw_note
  rows <- list()
  if (length(dots)) {
    labs <- base::names(dots) %||% rep("", length(dots))
    exprs <- as.list(substitute(list(...)))[-1]
    ub <- vapply(exprs, function(e) .ub_call("distr.summary.x", x = e, stats = c("mean", "sd", "cv")), character(1), USE.NAMES = FALSE)
    for (i in seq_along(dots)) if (!nzchar(labs[i])) labs[i] <- .label(exprs[[i]])
    vals <- lapply(seq_along(dots), function(i) .num(dots[[i]], labs[i]))
    rows[[1]] <- data.frame(variable = labs, source = "raw data",
                            mean = vapply(vals, base::mean, numeric(1)),
                            sd = vapply(vals, stats::sd, numeric(1)), stringsAsFactors = FALSE)
  }
  if (!is.null(mean)) {
    if (is.null(sd) && !is.null(var)) sd <- sqrt(var)
    if (is.null(sd) || length(mean) != length(sd))
      stop("Give mean = c(...) with sd = c(...) (or var = c(...)) of the same length.", call. = FALSE)
    labs <- names %||% base::names(mean) %||% paste0("var", seq_along(mean) + length(dots))
    rows[[2]] <- data.frame(variable = labs, source = if (is.null(var)) "mean and SD given" else "mean and variance given",
                            mean = as.numeric(mean), sd = as.numeric(sd), stringsAsFactors = FALSE)
  }
  tab <- do.call(rbind, rows)
  if (is.null(tab) || nrow(tab) < 2)
    stop("Give two or more numeric vectors, or mean = and sd = (or var =), or a mix of the two.", call. = FALSE)
  tab$variance <- tab$sd^2
  tab$CV <- tab$sd / abs(tab$mean)
  steps <- sprintf("CV(%s) = s / |mean| = %s / |%s| = %s", tab$variable,
                   ifelse(tab$source == "mean and variance given", sprintf("sqrt(%s)", .f(tab$variance)), .f(tab$sd)), .f(tab$mean), .f(tab$CV))
  lines <- c(.table_lines(.round_df(tab)), "", steps)
  big_sd <- tab$variable[which.max(tab$sd)]; big_cv <- tab$variable[which.max(tab$CV)]
  wording <- c(
    .w("frame", sprintf("We compare the dispersion of %s.", paste(tab$variable, collapse = " and "))),
    .w("tool", "To compare the dispersion of variables measured in different units (or with very different means) we need the coefficient of variation CV = s / |mean|, which is unit-free; SDs and variances can be compared only on the same scale with similar means."),
    .w("result", paste(sprintf("CV(%s) = %s / %s = %s", tab$variable, .f(tab$sd), .f(abs(tab$mean)), .f(tab$CV)), collapse = "; ")),
    .w("conclusion", sprintf("Relative to its mean, %s is the most dispersed (CV = %s)%s.", big_cv, .pct(max(tab$CV)),
                             if (!identical(big_sd, big_cv)) sprintf(", even though %s has the largest SD", big_sd) else "")))
  .result("Comparing dispersion (SD vs CV)", lines, wording, NULL, match.call(), table = tab, ubstats = ub)
}

#' @rdname describe
#' @export
desc_prop <- function(x = NULL, event = NULL, count = NULL, n = NULL) {
  sx <- substitute(x); xlab <- .label(sx)
  raw <- !is.null(x) && is.null(.as_freq(x))
  if (!is.null(x) && !is.null(.as_freq(x))) {
    fr <- .as_freq(x)
    if (is.null(event)) stop("Say which category: event = \"", names(fr)[1], "\".", call. = FALSE)
    if (!all(event %in% names(fr))) stop("event not found. Categories: ", paste(names(fr), collapse = ", "), call. = FALSE)
    count <- sum(fr[event]); n <- sum(fr); x <- NULL
  }
  P <- .one_prop(x, event, count, n, NULL, xlab)
  se <- sqrt(P$phat * (1 - P$phat) / P$n)
  lines <- c(
    P$inputs, "",
    sprintf("p-hat = x / n = %s / %s = %s  (%s)", .f(P$count), .f(P$n), .f(P$phat), .pct(P$phat)),
    sprintf("Estimated SE(p-hat) = sqrt(p-hat (1 - p-hat) / n) = sqrt(%s x %s / %s) = %s",
            .f(P$phat), .f(1 - P$phat), .f(P$n), .f(se)))
  wording <- c(
    .w("frame", sprintf("The parameter is p, the population proportion of %s; the data are a sample of n = %s units.", P$ev, .f(P$n))),
    .w("tool", sprintf("Estimator: let X_i = 1 if the i-th unit has the characteristic (%s) and 0 otherwise (Bernoulli, P(X_i = 1) = p); the estimator of p is the sample proportion P-hat = (X_1 + ... + X_n) / n, an unbiased estimator (E(P-hat) = p) with standard error sqrt(p(1 - p)/n).", P$ev)),
    .w("result", sprintf("The estimate of the population proportion of %s is the sample proportion p-hat = %s/%s = %s; estimated SE(p-hat) = sqrt[%s(1 - %s)/%s] = %s.",
                         P$ev, .f(P$count), .f(P$n), .f(P$phat), .f(P$phat), .f(P$phat), .f(P$n), .f(se))),
    .w("meaning", "The true standard error sqrt[p(1 - p)/n] depends on the unknown population proportion p, so it is estimated by substituting p-hat for p."),
    .w("caveat", "The (estimated) standard error is the expected distance of a GENERIC estimate from the unknown p: a smaller SE means the ESTIMATOR's estimates are more concentrated around p, but nothing can be concluded about how close this specific estimate is to its parameter."))
  ub <- if (!raw) .ub_raw_note
        else paste(if (is.null(event)) .ub_call("CI.prop", x = sx)
                   else if (length(event) == 1) .ub_call("CI.prop", x = sx, success = event)
                   else .ub_call("CI.prop", x = call("%in%", sx, event)), " # prints phat and its se")
  .result("Sample proportion and its estimated standard error", lines, wording, P$note, match.call(),
          estimate = P$phat, se = se, count = P$count, n = P$n, ubstats = ub)
}

