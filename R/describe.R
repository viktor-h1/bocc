# ------------------------------------------------------------
# Descriptive statistics: summaries, group comparison, dispersion,
# class-interval (grouped) tables, frequencies, proportions, cross-tabs.
# ------------------------------------------------------------

#' Descriptive statistics
#'
#' * `desc_summary()`  one numeric variable: mean, median, quartiles,
#'   percentiles, SD, variance, CV, IQR, outlier fences, skewness hint;
#'   boxplot + histogram.
#' * `desc_compare()`  a numeric variable by group (table + side-by-side boxplots).
#' * `desc_cv()`       compare dispersion of variables (SD vs coefficient of variation).
#' * `desc_classes()`  class-interval (grouped) table: density, modal class,
#'   approximate mean / SD / quantiles, share below a value; histogram.
#' * `desc_freq()`     frequency table of a categorical variable.
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
#' @param lower,upper,breaks Class limits for `desc_classes()` (give
#'   `breaks` OR `lower` and `upper`), or pass a class table from
#'   [sc_table()] as the first argument.
#' @param freq,prop Frequencies or proportions/percentages per class.
#' @param below Optional value: approximate share of units below it.
#' @param y Second categorical variable for `desc_crosstab()`.
#' @param event Category of interest for `desc_prop()`.
#' @param count,n Summary numbers for `desc_prop()`.
#' @return An `sc_result` (prints itself).
#' @examples
#' x <- c(12, 15, 9, 22, 14, 18, 30, 11)
#' desc_summary(x)
#' desc_classes(breaks = c(10, 25, 30, 35, 40, 45, 60),
#'              prop = c(6, 12, 24, 28, 18, 12))
#' @name describe
NULL

.fences <- function(q1, q3) c(q1 - 1.5 * (q3 - q1), q3 + 1.5 * (q3 - q1))

.mode_values <- function(v) {
  tab <- table(v)
  if (length(tab) == length(v) || max(tab) == 1) return(NULL)
  as.numeric(names(tab)[tab == max(tab)])
}

#' @rdname describe
#' @export
desc_summary <- function(x, probs = c(0.10, 0.25, 0.5, 0.75, 0.90, 0.95)) {
  xlab <- .label(substitute(x))
  v <- .num(x, xlab)
  n <- length(v); m <- mean(v); md <- median(v); s <- if (n > 1) sd(v) else NA
  q <- quantile(v, c(.25, .75), names = FALSE)
  fe <- .fences(q[1], q[2])
  outl <- v[v < fe[1] | v > fe[2]]
  pr <- quantile(v, probs, names = FALSE)
  mo <- .mode_values(v)
  skew <- if (is.na(s) || s == 0) "no spread" else if (abs(m - md) < 0.05 * s) "roughly symmetric (mean close to median)"
          else if (m > md) "right / positively skewed (mean > median)" else "left / negatively skewed (mean < median)"
  lines <- c(
    sprintf("Variable: %s   n = %s", xlab, n),
    "",
    sprintf("Mean      = %s", .f(m)),
    sprintf("Median    = %s", .f(md)),
    if (!is.null(mo) && length(mo) <= 3) sprintf("Mode      = %s", paste(.f(mo), collapse = ", ")),
    sprintf("SD (s)    = %s      Variance (s^2) = %s   (divisor n - 1)", .f(s), .f(s^2)),
    sprintf("Min = %s   Q1 = %s   Q3 = %s   Max = %s", .f(min(v)), .f(q[1]), .f(q[2]), .f(max(v))),
    sprintf("Range = %s   IQR = Q3 - Q1 = %s", .f(diff(range(v))), .f(q[2] - q[1])),
    sprintf("CV = s / |mean| = %s / %s = %s (%s)", .f(s), .f(abs(m)), .f(s / abs(m)), .pct(s / abs(m))),
    sprintf("Percentiles: %s", paste(sprintf("p%s = %s", round(100 * probs), .f(pr)), collapse = "   ")),
    sprintf("Outlier fences: Q1 - 1.5 IQR = %s,  Q3 + 1.5 IQR = %s", .f(fe[1]), .f(fe[2])),
    sprintf("Outliers: %s", if (length(outl)) paste(.f(sort(outl)), collapse = ", ") else "none"),
    sprintf("Shape: %s", skew))
  .with_plot(function() {
    op <- par(mfrow = c(1, 2)); on.exit(par(op))
    boxplot(v, main = paste("Boxplot of", xlab), ylab = xlab, col = "grey90")
    hist(v, main = paste("Histogram of", xlab), xlab = xlab, col = "grey85", border = "white")
    abline(v = c(m, md), col = c("firebrick", "navy"), lwd = 2, lty = c(1, 2))
    legend("topright", c("mean", "median"), col = c("firebrick", "navy"), lty = c(1, 2), bty = "n", cex = 0.8)
  })
  wording <- sprintf(
    "The distribution of %s (n = %s) has mean %s and median %s; it is %s. The middle 50%% of the observations lie between Q1 = %s and Q3 = %s (IQR = %s), and the standard deviation is %s (CV = %s). %s",
    xlab, n, .f(m), .f(md), skew, .f(q[1]), .f(q[2]), .f(q[2] - q[1]), .f(s), .pct(s / abs(m)),
    if (length(outl)) sprintf("There are %d observation(s) outside the 1.5 IQR fences (potential outliers).", length(outl))
    else "No observation lies outside the 1.5 IQR fences.")
  .result("Descriptive summary", lines, wording, c(.dropped_note(v),
          "Quartiles/percentiles use R's default definition (type 7)."), match.call(),
          n = n, mean = m, median = md, sd = s, quartiles = q, cv = s / abs(m), outliers = outl)
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

.classes_from <- function(x, lower, upper, breaks, freq, prop) {
  value_type <- NULL
  if (is.data.frame(x)) {
    if (!all(c("lower", "upper") %in% names(x)))
      stop("A class table needs columns lower and upper (as made by sc_table()).", call. = FALSE)
    lower <- x$lower; upper <- x$upper
    vcols <- setdiff(names(x)[vapply(x, is.numeric, logical(1))], c("lower", "upper"))
    if (!length(vcols)) stop("The class table has no frequency column.", call. = FALSE)
    if (length(vcols) > 1) message("Using the first value column: ", vcols[1])
    vals <- x[[vcols[1]]]
    value_type <- attr(x, "statcram_values") %||% if (vcols[1] %in% c("percent", "prop", "proportion")) "percent" else "freq"
    if (value_type == "percent") prop <- vals else freq <- vals
  } else if (!is.null(breaks)) {
    if (length(breaks) < 2) stop("breaks needs at least two numbers.", call. = FALSE)
    lower <- head(breaks, -1); upper <- tail(breaks, -1)
  }
  if (is.null(lower) || is.null(upper)) stop("Give breaks = c(...) or lower = and upper =, or a class table.", call. = FALSE)
  list(lower = lower, upper = upper, freq = freq, prop = prop)
}

#' @rdname describe
#' @export
desc_classes <- function(x = NULL, lower = NULL, upper = NULL, breaks = NULL, freq = NULL, prop = NULL,
                         probs = c(0.25, 0.5, 0.75, 0.9, 0.95), below = NULL) {
  C <- .classes_from(x, lower, upper, breaks, freq, prop)
  lower <- C$lower; upper <- C$upper; freq <- C$freq; prop <- C$prop
  k <- length(lower)
  if (length(upper) != k) stop("lower and upper must have the same length.", call. = FALSE)
  if (any(upper <= lower)) stop("Each upper limit must be larger than its lower limit.", call. = FALSE)
  if (is.null(prop)) {
    if (is.null(freq) || length(freq) != k) stop("Give freq = (counts) or prop = (proportions / %), one per class.", call. = FALSE)
    total <- sum(freq); prop <- freq / total
  } else {
    if (length(prop) != k) stop("prop must have one value per class.", call. = FALSE)
    if (abs(sum(prop) - 1) > 1e-6) prop <- prop / sum(prop)
    total <- NULL
  }
  width <- upper - lower; mid <- (lower + upper) / 2
  dens <- prop / width; cum <- cumsum(prop)
  modal <- which.max(dens)
  amean <- sum(mid * prop)
  avar <- sum(prop * (mid - amean)^2)
  qfun <- function(q) {
    j <- which(cum >= q - 1e-12)[1]
    prev <- if (j == 1) 0 else cum[j - 1]
    lower[j] + (q - prev) / dens[j]
  }
  qs <- vapply(probs, qfun, numeric(1))
  tab <- data.frame(class = paste0("[", .f(lower), ", ", .f(upper), ")"), width = width, midpoint = mid)
  if (!is.null(freq)) tab$freq <- freq
  tab$rel_freq <- prop; tab$density <- dens; tab$cum_rel <- cum
  lines <- c(
    .table_lines(.round_df(tab)),
    "",
    "density = relative frequency / class width;  midpoint = (lower + upper) / 2",
    sprintf("Approx. mean = sum(midpoint x rel_freq) = %s", .f(amean)),
    sprintf("Approx. variance = sum(rel_freq x (midpoint - mean)^2) = %s   SD = %s%s", .f(avar), .f(sqrt(avar)),
            if (!is.null(total) && total > 1) sprintf("   (with n - 1 divisor: variance = %s, SD = %s)",
                                                       .f(avar * total / (total - 1)), .f(sqrt(avar * total / (total - 1)))) else ""),
    sprintf("Modal class (highest DENSITY, not highest frequency) = [%s, %s)", .f(lower[modal]), .f(upper[modal])),
    "Approx. quantiles (values spread uniformly inside each class):",
    sprintf("  %s", paste(sprintf("p%s = %s", round(100 * probs), .f(qs)), collapse = "   ")),
    sprintf("  e.g. median: class [%s, %s) -> %s + (0.5 - %s) / %s = %s",
            .f(lower[which(cum >= 0.5 - 1e-12)[1]]), .f(upper[which(cum >= 0.5 - 1e-12)[1]]),
            .f(lower[which(cum >= 0.5 - 1e-12)[1]]),
            .f(if (which(cum >= 0.5 - 1e-12)[1] == 1) 0 else cum[which(cum >= 0.5 - 1e-12)[1] - 1]),
            .f(dens[which(cum >= 0.5 - 1e-12)[1]]), .f(qfun(0.5))))
  share_below <- NULL
  if (!is.null(below)) {
    share_below <- if (below <= lower[1]) 0 else if (below >= upper[k]) 1 else {
      j <- max(which(lower <= below)); (if (j == 1) 0 else cum[j - 1]) + (below - lower[j]) * dens[j]
    }
    lines <- c(lines, sprintf("Approx. share below %s = %s (%s)", .f(below), .f(share_below), .pct(share_below)))
  }
  .with_plot(function() {
    plot(NA, xlim = range(c(lower, upper)), ylim = c(0, max(dens) * 1.1), xlab = "value", ylab = "density",
         main = "Histogram (density scale)", las = 1)
    rect(lower, 0, upper, dens, col = "grey85", border = "grey30")
    abline(v = amean, col = "firebrick", lwd = 2); abline(v = qfun(0.5), col = "navy", lwd = 2, lty = 2)
    legend("topright", c("approx. mean", "approx. median"), col = c("firebrick", "navy"), lty = c(1, 2), bty = "n", cex = 0.8)
  })
  wording <- c(
    "Because only the class-interval table is available (not the raw values), these location and dispersion measures are approximations: the mean uses class midpoints and the quantiles assume values are spread uniformly within each class.",
    sprintf("The approximate mean is %s and the approximate median is %s. With unequal class widths the modal class is the one with the highest density: [%s, %s).",
            .f(amean), .f(qfun(0.5)), .f(lower[modal]), .f(upper[modal])))
  .result("Class-interval (grouped) data", lines, wording, NULL, match.call(),
          table = tab, mean = amean, variance = avar, quantiles = stats::setNames(qs, paste0("p", round(100 * probs))),
          modal_class = c(lower[modal], upper[modal]), share_below = share_below)
}

#' @rdname describe
#' @export
desc_freq <- function(x) {
  xlab <- .label(substitute(x))
  fr <- .as_freq(x)
  if (is.null(fr)) {
    v <- .one_column(x, xlab)
    v <- v[!is.na(v)]
    lv <- .cats(v)
    fr <- stats::setNames(as.numeric(table(factor(as.character(v), levels = lv))), lv)
  }
  n <- sum(fr); p <- fr / n
  tab <- data.frame(category = names(fr), count = fr, proportion = p, percent = 100 * p,
                    cum_percent = 100 * cumsum(p), se_phat = sqrt(p * (1 - p) / n))
  lines <- c(sprintf("Variable: %s   n = %s", xlab, .f(n)), "", .table_lines(.round_df(tab)), "",
             "se_phat = sqrt(p-hat (1 - p-hat) / n): the estimated standard error of each sample proportion")
  .with_plot(function() barplot(p, main = paste("Relative frequencies of", xlab), ylab = "proportion", col = "grey80", las = 1))
  .result("Frequency table", lines, NULL, NULL, match.call(), table = tab)
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
