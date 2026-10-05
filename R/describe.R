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
#' * `desc_compare()`  a numeric variable by group (table + side-by-side boxplots).
#' * `desc_cv()`       compare dispersion of variables (SD vs coefficient of variation).
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
#' * `desc_crosstab()` two-way table with row / column / total percentages.
#'
#' Data can be any vector or expression (`df$spend`, `c(...)`,
#' `subset(df, g == "A")$y`), or a table typed in with [sc_table()].
#'
#' @param x Data (numeric for `desc_summary`/`desc_compare`; categorical for
#'   `desc_freq`/`desc_prop`/`desc_crosstab`). `desc_crosstab()` and
#'   `desc_freq()` also accept a typed table.
#' @param group Grouping variable for `desc_compare()`.
#' @param ... Numeric vectors to compare (`desc_cv`), named or not.
#' @param mean,sd Summary numbers for `desc_cv()` when raw data are not given.
#' @param probs Percentiles to report.
#' @param population `desc_summary()`: TRUE when the data are the whole
#'   population (variance with divisor N instead of n - 1).
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
#'   raw values (`desc_freq`), approximate for classes (`desc_classes`).
#'   For ordered categories give level names, e.g. `at_least = "High"`.
#' @param order `desc_freq()`: the levels in their substantive order, e.g.
#'   `c("VLow", "Low", "Med", "High")` (needed for cumulative frequencies of
#'   an ordinal character variable).
#' @param sort `desc_freq()`: `"none"` (standard order), `"decreasing"` or
#'   `"increasing"` frequency (Pareto-style).
#' @param plot Plot type. `desc_freq()`: `"auto"` (bars for categories,
#'   spikes for numbers), `"bars"`, `"pie"`, `"spike"`, `"cum"`.
#'   `desc_classes()`: `"both"` (histogram + ogive), `"hist"`, `"ogive"`.
#' @param se `desc_freq()`: also show the estimated SE of each proportion.
#' @param y Second categorical variable for `desc_crosstab()`.
#' @param event Category of interest for `desc_prop()`.
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
.box_shape <- function(lo, q1, q2, q3, hi) {
  near <- function(a, b) abs(a - b) <= 0.1 * max(a, b, 1e-12)
  box <- if (near(q2 - q1, q3 - q2)) "sym" else if (q3 - q2 > q2 - q1) "right" else "left"
  wh <- if (near(q1 - lo, hi - q3)) "sym" else if (hi - q3 > q1 - lo) "right" else "left"
  if (box == "sym" && wh == "sym") return("likely symmetric (median centred in the box, whiskers of similar length)")
  if (box != "left" && wh != "left") return("right-skewed (longer upper part of the box and/or longer upper whisker)")
  if (box != "right" && wh != "right") return("left-skewed (longer lower part of the box and/or longer lower whisker)")
  "no clear skewness (the box and the whiskers point in different directions)"
}

#' @rdname describe
#' @export
desc_summary <- function(x, probs = c(0.10, 0.90, 0.95, 0.99), population = FALSE) {
  xlab <- .label(substitute(x))
  v <- .num(x, xlab)
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
  shape <- .box_shape(wlo, q[1], md, q[2], whi)
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
    sprintf("  Extreme values (outliers): %s", if (length(outl)) paste(.f(outl), collapse = ", ") else "none"), "",
    sprintf("Shape: Q2 - Q1 = %s vs Q3 - Q2 = %s;  lower whisker %s vs upper whisker %s",
            .f(md - q[1]), .f(q[2] - md), .f(q[1] - wlo), .f(whi - q[2])),
    sprintf("  -> %s; %s", shape, mm))
  .with_plot(function() {
    op <- par(mfrow = c(1, 2)); on.exit(par(op))
    boxplot(v, main = paste("Boxplot:", xlab), ylab = xlab, col = "grey90")
    hist(v, main = paste("Histogram:", xlab), xlab = xlab, col = "grey85", border = "white", freq = FALSE)
    abline(v = c(m, md), col = c("firebrick", "navy"), lwd = 2, lty = c(1, 2))
    legend("topright", c("mean", "median"), col = c("firebrick", "navy"), lty = c(1, 2), bty = "n", cex = 0.8)
  })
  wording <- c(
    sprintf("The distribution of %s (n = %s) has median %s and mean %s (%s). Half of the observations lie between Q1 = %s and Q3 = %s (IQR = %s); the values range from %s to %s.",
            xlab, n, .f(md), .f(m), mm, .f(q[1]), .f(q[2]), .f(q[2] - q[1]), .f(min(v)), .f(max(v))),
    sprintf("The standard deviation is %s, i.e. on average the values deviate from the mean by about %s (CV = %s of the mean). The boxplot suggests a %s distribution.%s",
            .f(s), .f(s), .pct(s / abs(m)), sub(" \\(.*", "", shape),
            if (length(outl)) sprintf(" %d value(s) lie more than 1.5 IQR beyond the box and are flagged as extreme: %s.", length(outl), paste(.f(outl), collapse = ", ")) else " No value is flagged as extreme."))
  .result("Descriptive summary", lines, wording,
          c(.dropped_note(v), if (population) "Population formulas used (divisor N)."), match.call(),
          n = n, mean = m, median = md, mode = mo$values, variance = s2, sd = s, quartiles = q, hand_quartiles = hq,
          fivenum = c(min = min(v), q1 = q[1], median = md, q3 = q[2], max = max(v)),
          range = max(v) - min(v), iqr = q[2] - q[1], cv = s / abs(m), whiskers = c(wlo, whi), outliers = outl,
          percentiles = stats::setNames(pr, paste0("p", round(100 * probs))))
}

#' @rdname describe
#' @export
desc_compare <- function(x, group) {
  xlab <- .label(substitute(x)); glab <- .label(substitute(group))
  xx <- .one_column(x, xlab); g <- .one_column(group, glab)
  if (length(xx) != length(g)) stop("x and group must have the same length.", call. = FALSE)
  xx <- suppressWarnings(as.numeric(xx))
  ok <- !is.na(xx) & !is.na(g)
  lv <- .cats(g[ok])
  rows <- lapply(lv, function(z) {
    a <- xx[ok & as.character(g) == z]
    q <- quantile(a, c(.25, .5, .75), names = FALSE)
    data.frame(group = z, n = length(a), mean = mean(a), median = q[2], sd = if (length(a) > 1) sd(a) else NA,
               Q1 = q[1], Q3 = q[3], IQR = q[3] - q[1], min = min(a), max = max(a),
               CV = if (length(a) > 1) sd(a) / abs(mean(a)) else NA)
  })
  tab <- do.call(rbind, rows)
  .with_plot(function() {
    boxplot(xx[ok] ~ factor(as.character(g[ok]), levels = lv), xlab = glab, ylab = xlab,
            main = paste(xlab, "by", glab), col = "grey90")
    points(seq_along(lv), tab$mean, pch = 18, col = "firebrick", cex = 1.4)
  })
  hi_med <- tab$group[which.max(tab$median)]; hi_iqr <- tab$group[which.max(tab$IQR)]
  lines <- c(sprintf("Variable: %s   by   %s", xlab, glab), "", .table_lines(.round_df(tab)))
  wording <- c(
    sprintf("Central tendency: the median of %s is highest for %s (%s) and lowest for %s (%s).",
            xlab, hi_med, .f(max(tab$median)), tab$group[which.min(tab$median)], .f(min(tab$median))),
    sprintf("Variability: the IQR (box width, the spread of the middle 50%%) is largest for %s (%s); compare also the whisker lengths and any outliers for the tails.",
            hi_iqr, .f(max(tab$IQR))),
    "Shape: in each group compare mean vs median and the position of the median inside the box to describe skewness. State every comparison in the context of the variable.")
  .result("Comparison across groups", lines, wording,
          if (sum(!ok)) sprintf("%d row(s) with a missing value were removed.", sum(!ok)), match.call(),
          table = tab)
}

#' @rdname describe
#' @export
desc_cv <- function(..., mean = NULL, sd = NULL) {
  dots <- list(...)
  if (length(dots)) {
    labs <- names(dots) %||% rep("", length(dots))
    exprs <- as.list(substitute(list(...)))[-1]
    for (i in seq_along(dots)) if (!nzchar(labs[i])) labs[i] <- .label(exprs[[i]])
    vals <- lapply(seq_along(dots), function(i) .num(dots[[i]], labs[i]))
    tab <- data.frame(variable = labs,
                      mean = vapply(vals, base::mean, numeric(1)),
                      sd = vapply(vals, stats::sd, numeric(1)),
                      variance = vapply(vals, stats::var, numeric(1)),
                      range = vapply(vals, function(v) diff(range(v)), numeric(1)),
                      IQR = vapply(vals, stats::IQR, numeric(1)))
  } else {
    if (is.null(mean) || is.null(sd) || length(mean) != length(sd))
      stop("Give two or more numeric vectors, or mean = c(...) and sd = c(...) of equal length.", call. = FALSE)
    labs <- names(mean) %||% paste0("var", seq_along(mean))
    tab <- data.frame(variable = labs, mean = mean, sd = sd, variance = sd^2)
  }
  tab$CV <- tab$sd / abs(tab$mean)
  steps <- sprintf("CV(%s) = %s / |%s| = %s", tab$variable, .f(tab$sd), .f(tab$mean), .f(tab$CV))
  lines <- c(.table_lines(.round_df(tab)), "", steps)
  big_sd <- tab$variable[which.max(tab$sd)]; big_cv <- tab$variable[which.max(tab$CV)]
  wording <- c(
    "Rule: compare SDs/variances only when the variables are measured on the same scale and have similar means. When units or means differ, use the coefficient of variation CV = s/|mean|, which is unit-free.",
    sprintf("%s has the largest SD, but relative to its mean the most dispersed variable is %s (CV = %s).",
            big_sd, big_cv, .pct(max(tab$CV))))
  .result("Comparing dispersion (SD vs CV)", lines, wording, NULL, match.call(), table = tab)
}

#' @rdname describe
#' @export
desc_prop <- function(x = NULL, event = NULL, count = NULL, n = NULL) {
  xlab <- .label(substitute(x))
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
  wording <- sprintf(
    "The estimate of the population proportion of %s is the sample proportion p-hat = %s/%s = %s. The true standard error sqrt[p(1 - p)/n] depends on the unknown population proportion p, so it is estimated by substituting p-hat for p: SE(p-hat) = sqrt[%s(1 - %s)/%s] = %s.",
    P$ev, .f(P$count), .f(P$n), .f(P$phat), .f(P$phat), .f(P$phat), .f(P$n), .f(se))
  .result("Sample proportion and its estimated standard error", lines, wording, P$note, match.call(),
          estimate = P$phat, se = se, count = P$count, n = P$n)
}

#' @rdname describe
#' @export
desc_crosstab <- function(x, y = NULL) {
  xlab <- .label(substitute(x)); ylab <- .label(substitute(y))
  m <- .as_count_table(x)
  if (is.null(m)) {
    if (is.null(y)) stop("Give two categorical variables (x, y) or a count table.", call. = FALSE)
    a <- .one_column(x, xlab); b <- .one_column(y, ylab)
    ok <- !is.na(a) & !is.na(b)
    m <- unclass(table(a[ok], b[ok])) + 0
    names(dimnames(m)) <- c(xlab, ylab)
  }
  tot <- sum(m)
  with_m <- rbind(cbind(m, Total = rowSums(m)), Total = c(colSums(m), tot))
  rowp <- 100 * m / rowSums(m); colp <- 100 * t(t(m) / colSums(m))
  lines <- c("Counts (with totals):", .table_lines(with_m, row.names = TRUE), "",
             "Row percentages (each row sums to 100%):", .table_lines(round(rowp, 2), row.names = TRUE), "",
             "Column percentages (each column sums to 100%):", .table_lines(round(colp, 2), row.names = TRUE), "",
             "Percent of grand total:", .table_lines(round(100 * m / tot, 2), row.names = TRUE))
  .with_plot(function() mosaicplot(m, main = "Mosaic plot", color = TRUE, las = 1))
  .result("Two-way table", lines,
          "Compare the row (or column) percentages across categories: if the conditional distributions are very different, the two variables appear associated in the sample. Use chisq_indep() to test whether the association holds in the population.",
          NULL, match.call(), counts = m, row_percent = rowp, col_percent = colp)
}
