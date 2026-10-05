# ------------------------------------------------------------
# Frequency distributions (book chapter 2): tables of distinct values,
# cumulative frequencies, data grouped into intervals / measured in
# classes, densities, histograms and ogives.
# Notation follows the book: f_k counts, p_k proportions,
# F_k cumulative proportions, w_k widths, c_k = p_k / w_k densities.
# ------------------------------------------------------------

# Character table with a TOTAL row, for display.
.with_total <- function(df, total, digits = list()) {
  out <- as.data.frame(lapply(names(df), function(nm) {
    v <- df[[nm]]
    if (is.numeric(v)) .f(v, digits[[nm]] %||% .digits()) else as.character(v)
  }), stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- names(df)
  tot <- as.list(rep("", ncol(out))); names(tot) <- names(out)
  tot[[1]] <- "TOTAL"
  for (nm in names(total)) tot[[nm]] <- .f(total[[nm]])
  rbind(out, as.data.frame(tot, stringsAsFactors = FALSE, check.names = FALSE))
}

# ---------- distinct values ----------

# Position of a query value among ordered categories / numeric values.
.cum_query_lines <- function(vals, p, numeric, at_most, at_least, between, xlab) {
  lines <- character(); out <- list()
  cumF <- cumsum(p)
  pos_le <- function(x) {
    if (numeric) return(sum(as.numeric(vals) <= x))
    j <- match(as.character(x), vals)
    if (is.na(j)) stop("'", x, "' is not a category of ", xlab, ". Categories: ", paste(vals, collapse = ", "), call. = FALSE)
    j
  }
  pos_lt <- function(x) if (numeric) sum(as.numeric(vals) < x) else pos_le(x) - 1
  fmt <- function(x) if (numeric) .f(x) else as.character(x)
  sum_txt <- function(idx) if (!length(idx)) "0" else if (length(idx) <= 6) paste(.f(p[idx]), collapse = " + ") else
    sprintf("%s + ... + %s", .f(p[idx[1]]), .f(p[idx[length(idx)]]))
  if (!is.null(at_most)) {
    k <- pos_le(at_most); v <- if (k) cumF[k] else 0
    lines <- c(lines, sprintf("Freq(X <= %s) = F(%s) = %s = %s", fmt(at_most), fmt(at_most), sum_txt(seq_len(k)), .f(v)))
    out$at_most <- v
  }
  if (!is.null(at_least)) {
    k <- pos_lt(at_least); v <- 1 - (if (k) cumF[k] else 0)
    lines <- c(lines, sprintf("Freq(X >= %s) = 1 - Freq(X < %s) = %s = %s", fmt(at_least), fmt(at_least),
                              sum_txt(setdiff(seq_along(p), seq_len(k))), .f(v)))
    out$at_least <- v
  }
  if (!is.null(between)) {
    if (length(between) != 2) stop("between must be two values: c(a, b).", call. = FALSE)
    a <- between[1]; b <- between[2]
    hi <- pos_le(b); lo <- pos_lt(a)
    idx <- if (hi > lo) (lo + 1):hi else integer()
    v <- sum(p[idx])
    lines <- c(lines, sprintf("Freq(%s <= X <= %s) = %s = %s", fmt(a), fmt(b), sum_txt(idx), .f(v)))
    out$between <- v
  }
  list(lines = lines, values = out)
}

#' @rdname describe
#' @export
desc_freq <- function(x, order = NULL, sort = c("none", "decreasing", "increasing"),
                      at_most = NULL, at_least = NULL, between = NULL,
                      plot = c("auto", "bars", "pie", "spike", "cum"), se = FALSE, n = NULL) {
  n_given <- n
  sort <- match.arg(sort); plot <- match.arg(plot)
  sx <- substitute(x); xlab <- .label(sx)
  notes <- NULL
  fr <- .as_freq(x)
  raw <- is.null(fr); intervals <- FALSE
  numeric <- FALSE; is_props <- FALSE
  if (is.null(fr)) {
    v <- .one_column(x, xlab)
    ordered <- is.numeric(v) || is.factor(v) || !is.null(order) || !is.null(.parse_intervals(v))
    dropped <- sum(is.na(v)); v <- v[!is.na(v)]
    if (!length(v)) stop(xlab, " has no non-missing values.", call. = FALSE)
    numeric <- is.numeric(v)
    intervals <- is.null(order) && !is.null(.parse_intervals(v))
    lv <- .cats(v)
    if (!is.null(order)) {
      miss <- setdiff(lv, as.character(order))
      if (length(miss)) stop("order is missing these values of ", xlab, ": ", paste(miss, collapse = ", "), call. = FALSE)
      lv <- as.character(order); numeric <- FALSE
    }
    fr <- stats::setNames(as.numeric(table(factor(as.character(v), levels = lv))), lv)
    if (dropped) notes <- c(notes, sprintf("%d missing value(s) were removed.", dropped))
    if (is.character(v) && is.null(order) && is.null(.parse_intervals(v)))
      notes <- c(notes, "Text values are in alphabetical order. If the variable is ordinal, give the right order: order = c(\"lowest\", ..., \"highest\") - cumulative frequencies need it.")
    if (numeric && length(lv) > 20)
      notes <- c(notes, sprintf("%s has %d distinct values: a table of values is not effective. Group them into intervals with desc_classes(%s, breaks = K).", xlab, length(lv), xlab))
  } else {
    if (!is.null(order)) fr <- fr[as.character(order)]
    tot <- sum(fr)
    is_props <- abs(tot - 1) < 1e-6 || (abs(tot - 100) < 1e-6 && any(fr != round(fr)))
    labvals <- vapply(names(fr), function(l) .open_value(l)$value, numeric(1))
    numeric <- all(!is.na(suppressWarnings(as.numeric(names(fr)))))
    ordered <- !is.null(order) || all(!is.na(labvals))
    if (is_props && is.null(n_given)) notes <- c(notes, "The values were read as proportions / percentages: the sample size n is unknown (give n = ... if the question states it).")
  }
  if (sort != "none") {
    ordered <- FALSE
    fr <- fr[base::order(fr, decreasing = sort == "decreasing")]
    notes <- c(notes, "Categories are ordered by frequency, so cumulative frequencies are not meaningful here.")
  }
  n <- sum(fr); p <- fr / n
  tab <- data.frame(value = names(fr), Count = as.numeric(fr), Prop = as.numeric(p), Percent = 100 * as.numeric(p),
                    Cum.Count = cumsum(as.numeric(fr)), Cum.Prop = cumsum(as.numeric(p)), stringsAsFactors = FALSE)
  if (is_props) { tab$Count <- NULL; tab$Cum.Count <- NULL; n <- NA }
  if (se && !is_props) tab$SE_prop <- sqrt(tab$Prop * (1 - tab$Prop) / n)
  names(tab)[1] <- xlab
  shown <- .with_total(tab, if (is_props) list(Prop = 1, Percent = 100) else list(Count = n, Prop = 1, Percent = 100))
  lines <- c(sprintf("Variable: %s   n = %s   K = %d distinct values", xlab, if (is_props) "unknown" else .f(n), length(fr)), "",
             .table_lines(shown),
             "", if (is_props) "Prop = p_k   Cum.Prop = F_k = p_1 + ... + p_k" else "Count = f_k   Prop = p_k = f_k / n   Cum.Prop = F_k = p_1 + ... + p_k")
  S <- .freq_summary(names(fr), as.numeric(p), if (is_props && !is.null(n_given)) n_given else n, ordered)
  lines <- c(lines, "", S$lines)
  if (se) lines <- c(lines, "SE_prop = sqrt(p_k (1 - p_k) / n): estimated standard error of each sample proportion")
  Q <- .cum_query_lines(names(fr), as.numeric(p), numeric, at_most, at_least, between, xlab)
  if (length(Q$lines)) lines <- c(lines, "", Q$lines)
  ptype <- if (plot == "auto") (if (numeric) "spike" else "bars") else plot
  .with_plot(function() {
    main <- paste(switch(ptype, bars = "Bar plot:", pie = "Pie chart:", spike = "Spike plot:", cum = "Cumulative freq:"), xlab)
    if (ptype == "bars") barplot(p, main = main, ylab = "Proportions", col = "grey80", las = 1)
    else if (ptype == "pie") pie(p, main = main, col = grDevices::gray.colors(length(p)))
    else if (ptype == "spike") {
      xs <- suppressWarnings(as.numeric(names(fr)))
      if (anyNA(xs)) xs <- seq_along(fr)
      plot(xs, p, type = "h", lwd = 2, xlab = xlab, ylab = "Proportions", main = main, las = 1, ylim = c(0, max(p) * 1.05))
      points(xs, p, pch = 19, cex = 0.7)
    } else .plot_cum(names(fr), as.numeric(p), numeric, xlab)
  })
  ub <- if (!raw) .ub_raw_note else {
    xe <- .ub_ordered(sx, order)
    iv <- if (intervals) TRUE
    c(.ub_call("distr.table.x", x = xe, freq = if (ordered) c("counts", "proportions", "cumulative"), interval = iv),
      .ub_call("distr.plot.x", x = xe, freq = "proportions",
               plot.type = switch(ptype, bars = "bars", pie = "pie", spike = "spike", cum = "cumulative"),
               ord.freq = if (sort != "none" && ptype %in% c("bars", "pie")) sort, interval = iv),
      if (!is.null(order)) "# factor(..., levels = ) gives UBStats the order (otherwise alphabetical)")
  }
  .result("Frequency distribution", lines, NULL, notes, match.call(),
          table = tab, n = n, values = Q$values, mode = S$mode, median = S$median, quartiles = S$quartiles,
          mean = S$mean, variance = S$variance, sd = S$sd, ubstats = ub)
}

# Summary measures from a frequency distribution (book 3.2-3.4):
# mode; median and quartiles as the smallest value with F >= 0.5 / 0.25 / 0.75
# (ordered data only; numeric median = midpoint when F is exactly 0.5);
# mean = sum(x*_k p_k) and s^2 = n / (n - 1) [sum(x*_k^2 p_k) - mean^2] for numbers.
.freq_summary <- function(labs, p, n, ordered) {
  K <- length(labs); out <- "Summary measures (from the frequency distribution)"
  res <- list(mode = NULL, median = NULL, quartiles = NULL, mean = NULL, variance = NULL, sd = NULL)
  if (K > 1 && all(abs(p - p[1]) < 1e-12)) {
    out <- c(out, "  Mode     = none: all values have the same frequency")
  } else {
    md <- labs[abs(p - max(p)) < 1e-12]
    res$mode <- md
    out <- c(out, sprintf("  Mode     = %s   (%s, p = %s)", paste(md, collapse = ", "),
                          if (length(md) > 1) sprintf("%d modes", length(md)) else "highest frequency", .f(max(p))))
  }
  xv <- suppressWarnings(as.numeric(labs))
  if (ordered) {
    F <- cumsum(p)
    first <- function(pp) which(F >= pp - 1e-9)[1]
    j <- first(0.5)
    med <- labs[j]; med_txt <- sprintf("smallest value with F >= 0.5; F = %s", .f(F[j]))
    if (abs(F[j] - 0.5) < 1e-9 && j < K && !is.na(xv[j]) && !is.na(xv[j + 1])) {
      med <- .f((xv[j] + xv[j + 1]) / 2)
      med_txt <- sprintf("F is exactly 0.5 at %s, so the midpoint of %s and %s", labs[j], labs[j], labs[j + 1])
    }
    res$median <- med; res$quartiles <- c(Q1 = labs[first(0.25)], Q3 = labs[first(0.75)])
    out <- c(out, sprintf("  Median   = %s   (%s)", med, med_txt),
             sprintf("  Q1 = %s, Q3 = %s   (smallest values with F >= 0.25 and F >= 0.75)", res$quartiles[1], res$quartiles[2]))
  } else {
    out <- c(out, "  Median / quartiles: not available - the categories have no order (give order = c(...) if the variable is ordinal)")
  }
  if (all(!is.na(xv))) {
    m <- sum(xv * p); ex2 <- sum(xv^2 * p)
    terms <- sprintf("%s x %s", .f(xv), .f(p))
    expand <- if (K <= 8) paste(terms, collapse = " + ") else paste(c(head(terms, 3), "...", tail(terms, 1)), collapse = " + ")
    out <- c(out, sprintf("  Mean     = sum(x*_k p_k) = %s = %s", expand, .f(m)))
    if (!is.na(n) && n > 1) {
      v <- n / (n - 1) * (ex2 - m^2)
      out <- c(out, sprintf("  Variance s^2 = n/(n-1) [sum(x*_k^2 p_k) - mean^2] = %s/%s x (%s - %s^2) = %s",
                            .f(n), .f(n - 1), .f(ex2), .f(m), .f(v)))
    } else {
      v <- ex2 - m^2
      out <- c(out, sprintf("  Variance ~ sum(x*_k^2 p_k) - mean^2 = %s - %s^2 = %s   (n unknown: the factor n/(n-1) is ignored, ~1 for large n)",
                            .f(ex2), .f(m), .f(v)))
    }
    out <- c(out, sprintf("  SD       = sqrt(%s) = %s     CV = SD / |mean| = %s", .f(v), .f(sqrt(v)), .f(sqrt(v) / abs(m))))
    res$mean <- m; res$variance <- v; res$sd <- sqrt(v)
  } else {
    open <- labs[!is.na(vapply(labs, function(l) .open_value(l)$open, ""))]
    if (length(open)) out <- c(out, sprintf("  Mean / variance: cannot be computed - the open-ended class '%s' has no exact value.", open[1]))
  }
  list(lines = out, mode = res$mode, median = res$median, quartiles = res$quartiles,
       mean = res$mean, variance = res$variance, sd = res$sd)
}

.plot_cum <- function(vals, p, numeric, xlab) {
  cumF <- cumsum(p)
  if (numeric) {
    xs <- as.numeric(vals)
    rng <- range(xs); pad <- diff(rng) * 0.05 + (diff(rng) == 0)
    plot(NA, xlim = rng + c(-pad, pad), ylim = c(0, 1), xlab = xlab, ylab = "Cumulative Proportions",
         main = paste("Cumulative freq:", xlab), las = 1)
    segments(c(rng[1] - pad, xs), c(0, cumF), c(xs, rng[2] + pad), c(0, cumF))
    points(xs, cumF, pch = 19, cex = 0.7)
    lines(rng, c(0, 1), col = "firebrick", lty = 2)
  } else {
    k <- length(vals)
    plot(NA, xlim = c(0.5, k + 0.5), ylim = c(0, 1), xaxt = "n", xlab = xlab, ylab = "Cumulative Proportions",
         main = paste("Cumulative freq:", xlab), las = 1)
    axis(1, at = seq_len(k), labels = vals)
    segments(seq_len(k) - 0.4, cumF, seq_len(k) + 0.4, cumF, lwd = 2)
  }
}

# ---------- classes ----------

# Gather lower / upper limits and frequencies (or proportions) from any input.
# The first class may be open on the left (-Inf) and the last open on the right (Inf).
.classes_input <- function(x, breaks, lower, upper, freq, prop, xlab) {
  steps <- character(); notes <- character(); source <- "table"; labels <- NULL
  if (!is.null(x) && is.data.frame(x) && all(c("lower", "upper") %in% names(x))) {
    lower <- x$lower; upper <- x$upper
    vcols <- setdiff(names(x)[vapply(x, is.numeric, logical(1))], c("lower", "upper"))
    if (!length(vcols)) stop("The class table has no frequency column.", call. = FALSE)
    if (length(vcols) > 1) notes <- c(notes, sprintf("Using the first value column (%s).", vcols[1]))
    vals <- x[[vcols[1]]]
    vt <- attr(x, "statcram_values") %||% if (vcols[1] %in% c("percent", "prop", "proportion")) "percent" else "freq"
    if (vt == "percent") prop <- vals else freq <- vals
  } else if (!is.null(x) && (is.character(x) || is.factor(x))) {
    v <- x[!is.na(x)]
    iv <- .parse_intervals(v)
    if (is.null(iv)) stop(xlab, " is not measured in classes: its values are not intervals like [0,50), 10-20 or '1000 or more'.", call. = FALSE)
    lower <- iv$lower; upper <- iv$upper; labels <- iv$label
    freq <- as.numeric(table(factor(as.character(v), levels = iv$label)))
    source <- "classes"
    steps <- c(steps, sprintf("%s is measured in classes: raw values are not available, so everything below is approximate.", xlab))
  } else if (!is.null(x)) {
    v <- .num(x, xlab)
    if (is.null(breaks)) stop("Give breaks = K (number of equal-width classes) or breaks = c(...) (the class limits).", call. = FALSE)
    if (length(breaks) == 1) {
      K <- breaks
      if (K < 1 || K != round(K)) stop("breaks = K must be a whole number of classes.", call. = FALSE)
      mn <- min(v); mx <- max(v); w <- (mx - mn) / K
      b <- mn + w * (0:K); b[K + 1] <- mx
      steps <- c(steps, sprintf("Equal-width classes: w = (Max - Min) / K = (%s - %s) / %d = %s", .f(mx), .f(mn), K, .f(w)))
    } else {
      b <- sort(unique(breaks))
      out <- sum(v < b[1] | v > b[length(b)])
      if (out) notes <- c(notes, sprintf("%d value(s) outside [%s, %s] are not classified.", out, .f(b[1]), .f(b[length(b)])))
    }
    inside <- v[v >= b[1] & v <= b[length(b)]]
    idx <- findInterval(inside, b, rightmost.closed = TRUE)
    freq <- tabulate(idx, nbins = length(b) - 1)
    lower <- head(b, -1); upper <- tail(b, -1)
    source <- "raw"
    if (attr(v, "dropped")) notes <- c(notes, .dropped_note(v))
  } else if (!is.null(breaks)) {
    if (length(breaks) < 2) stop("Without raw data, breaks must be the class limits c(a, b, c, ...) (use Inf for an open last class).", call. = FALSE)
    b <- sort(breaks); lower <- head(b, -1); upper <- tail(b, -1)
  }
  if (is.null(lower) || is.null(upper))
    stop("Give raw data + breaks, a variable measured in classes, a class table, or breaks = c(...) with freq = / prop =.", call. = FALSE)
  k <- length(lower)
  if (length(upper) != k) stop("lower and upper must have the same length.", call. = FALSE)
  if (any(upper <= lower)) stop("Each upper limit must be larger than its lower limit.", call. = FALSE)
  if (any(is.infinite(lower[-1])) || any(is.infinite(upper[-k])))
    stop("Only the first class can be open on the left and only the last one open on the right.", call. = FALSE)
  if (k > 1 && any(head(upper, -1) > tail(lower, -1) + 1e-9)) stop("The classes overlap; check the limits.", call. = FALSE)
  if (k > 1 && any(head(upper, -1) < tail(lower, -1) - 1e-9)) notes <- c(notes, "There are gaps between some classes.")
  if (is.null(prop)) {
    if (is.null(freq) || length(freq) != k) stop("Give freq = (counts) or prop = (proportions / %), one per class.", call. = FALSE)
    n <- sum(freq); p <- freq / n
  } else {
    if (length(prop) != k) stop("prop must have one value per class.", call. = FALSE)
    n <- NULL; p <- prop / sum(prop)
    if (abs(sum(prop) - 1) > 1e-6 && abs(sum(prop) - 100) > 1e-6) notes <- c(notes, "The proportions did not sum to 1 (or 100%); they were rescaled.")
  }
  list(lower = lower, upper = upper, labels = labels, freq = if (is.null(prop)) freq else NULL, p = p, n = n,
       steps = steps, notes = notes, source = source)
}

.class_labels <- function(lower, upper) {
  k <- length(lower)
  out <- paste0("[", .f(lower), ", ", .f(upper), c(rep(")", k - 1), "]"))
  out[is.infinite(upper) & upper > 0] <- paste(.f(lower[is.infinite(upper) & upper > 0]), "or more")
  out[is.infinite(lower) & lower < 0] <- paste("less than", .f(upper[is.infinite(lower) & lower < 0]))
  out
}

# Approximate F(x) = Freq(X <= x) for grouped data (uniform within classes).
.class_F <- function(x, lower, upper, p, dens) {
  cumF <- cumsum(p); k <- length(lower)
  if (x <= lower[1]) return(list(v = 0, txt = sprintf("0 (%s is at or below the first class)", .f(x))))
  if (x >= upper[k]) return(list(v = 1, txt = sprintf("1 (%s is at or above the last class)", .f(x))))
  j <- max(which(lower <= x | is.infinite(lower)))
  if (x > upper[j]) return(list(v = cumF[j], txt = sprintf("%s (in a gap between classes)", .f(cumF[j]))))
  before <- if (j == 1) 0 else cumF[j - 1]
  if (isTRUE(all.equal(x, lower[j]))) return(list(v = before, txt = sprintf("%s (sum of the classes below %s, exact)", .f(before), .f(x))))
  if (is.na(dens[j])) return(list(v = NA, txt = sprintf("cannot be approximated: %s falls in the open-ended class", .f(x))))
  v <- before + dens[j] * (x - lower[j])
  list(v = v, txt = sprintf("Freq(X < %s) + c_%d x (%s - %s) = %s + %s x %s = %s",
                            .f(lower[j]), j, .f(x), .f(lower[j]), .f(before), .f(dens[j], 6), .f(x - lower[j]), .f(v)))
}

# Quantile of grouped data: Q = l_k + (q - F_(k-1)) / c_k  (book 3.3.1).
.class_Q <- function(q, lower, upper, p, dens) {
  cumF <- cumsum(p)
  j <- which(cumF >= q - 1e-9)[1]
  prev <- if (j == 1) 0 else cumF[j - 1]
  if (is.na(dens[j])) return(list(v = NA, txt = sprintf("falls in the open-ended class %s: cannot be determined", .class_labels(lower, upper)[j])))
  v <- lower[j] + (q - prev) / dens[j]
  list(v = v, txt = sprintf("l + (%s - F_%d) / c_%d = %s + (%s - %s) / %s = %s",
                            .f(q), j - 1, j, .f(lower[j]), .f(q), .f(prev), .f(dens[j], 6), .f(v)))
}

.expand_sum <- function(a, b, k_max = 6) {
  t <- sprintf("%s x %s", .f(a), .f(b))
  if (length(t) <= k_max) paste(t, collapse = " + ") else paste(c(head(t, 3), "...", tail(t, 1)), collapse = " + ")
}

#' @rdname describe
#' @export
desc_classes <- function(x = NULL, breaks = NULL, lower = NULL, upper = NULL, freq = NULL, prop = NULL, n = NULL,
                         at_most = NULL, at_least = NULL, between = NULL,
                         probs = c(0.25, 0.5, 0.75, 0.9, 0.95), plot = c("both", "hist", "ogive")) {
  plot <- match.arg(plot)
  sx <- substitute(x); sb <- substitute(breaks)
  xlab <- if (is.null(x)) "X" else .label(sx)
  C <- .classes_input(x, breaks, lower, upper, freq, prop, xlab)
  if (!is.null(n)) { if (!is.null(C$n) && C$n != n) stop("n = ", n, " does not match the total of the frequencies (", C$n, ").", call. = FALSE); C$n <- n }
  lower <- C$lower; upper <- C$upper; p <- C$p; k <- length(lower)
  open <- is.infinite(lower) | is.infinite(upper)
  w <- upper - lower; mid <- (lower + upper) / 2
  w[open] <- NA; mid[open] <- NA
  dens <- p / w; cumF <- cumsum(p)
  labs <- C$labels %||% .class_labels(lower, upper)
  tab <- data.frame(class = labs, w_k = w, m_k = mid, stringsAsFactors = FALSE)
  if (!is.null(C$freq)) tab$f_k <- C$freq
  tab$p_k <- p; tab$c_k <- dens; tab$F_k <- cumF
  total <- list(p_k = 1)
  if (!is.null(C$freq)) total$f_k <- sum(C$freq)
  shown <- .with_total(tab, total, list(c_k = max(5, .digits() + 1)))
  shown[shown == "NA"] <- "-"
  lines <- c(C$steps,
             sprintf("Classes are closed on the left and open on the right, except the last one.%s",
                     if (!is.null(C$n)) sprintf("   n = %s", .f(C$n)) else ""), "",
             .table_lines(shown), "",
             "w_k = width   m_k = midpoint   f_k = count   p_k = proportion   c_k = p_k / w_k = density   F_k = cumulative proportion")
  notes <- C$notes
  if (any(open)) notes <- c(notes, "Open-ended class: its width, midpoint and density cannot be evaluated; the median and quantiles that fall in closed classes can still be found, the mean and variance cannot.")
  # queries (uniform within classes)
  vals <- list()
  if (!is.null(at_most) || !is.null(at_least) || !is.null(between)) {
    lines <- c(lines, "", "Proportions assuming values are spread uniformly within each class (approximations):")
    if (!is.null(at_most)) {
      r <- .class_F(at_most, lower, upper, p, dens); vals$at_most <- r$v
      lines <- c(lines, sprintf("Freq(X <= %s) = %s", .f(at_most), r$txt))
    }
    if (!is.null(at_least)) {
      r <- .class_F(at_least, lower, upper, p, dens); vals$at_least <- 1 - r$v
      lines <- c(lines, sprintf("Freq(X >= %s) = 1 - Freq(X < %s) = 1 - %s = %s", .f(at_least), .f(at_least), .f(r$v), .f(1 - r$v)),
                 sprintf("   where Freq(X < %s) = %s", .f(at_least), r$txt))
    }
    if (!is.null(between)) {
      if (length(between) != 2) stop("between must be two values: c(a, b).", call. = FALSE)
      ra <- .class_F(min(between), lower, upper, p, dens); rb <- .class_F(max(between), lower, upper, p, dens)
      vals$between <- rb$v - ra$v
      lines <- c(lines, sprintf("Freq(%s <= X <= %s) = F(%s) - F(%s) = %s - %s = %s", .f(min(between)), .f(max(between)),
                                .f(max(between)), .f(min(between)), .f(rb$v), .f(ra$v), .f(rb$v - ra$v)),
                 sprintf("   F(%s) = %s", .f(max(between)), rb$txt), sprintf("   F(%s) = %s", .f(min(between)), ra$txt))
    }
    if (!is.null(C$n)) {
      nm <- c(at_most = "Freq(X <= %s)", at_least = "Freq(X >= %s)", between = "Freq(%s <= X <= %s)")
      for (k in names(vals)) {
        arg <- switch(k, at_most = .f(at_most), at_least = .f(at_least), between = .f(sort(between)))
        lines <- c(lines, sprintf("   Number of units: n x %s = %s x %s = %s  (approximately %s units)",
                                  do.call(sprintf, c(list(nm[[k]]), as.list(arg))), .f(C$n), .f(vals[[k]]), .f(C$n * vals[[k]]),
                                  .f(floor(C$n * vals[[k]] + 0.5))))
      }
    }
  }
  # central tendency and dispersion (approximations, book 3.2.1 / 3.4)
  closed <- which(!open)
  modal <- closed[abs(dens[closed] - max(dens[closed])) < 1e-12 * max(dens[closed])]
  Qs <- lapply(c(0.25, 0.5, 0.75), .class_Q, lower = lower, upper = upper, p = p, dens = dens)
  lines <- c(lines, "", "Summary measures (approximate: values assumed uniform within classes)",
             sprintf("  Modal class%s = %s   (highest DENSITY c_k, not highest frequency)", if (length(modal) > 1) "es (tie)" else "",
                     paste(labs[modal], collapse = " and ")),
             sprintf("  Q1     = %s", Qs[[1]]$txt), sprintf("  Median = %s", Qs[[2]]$txt), sprintf("  Q3     = %s", Qs[[3]]$txt))
  amean <- avar <- NA
  if (any(open)) {
    lines <- c(lines, "  Mean / variance: cannot be computed (an open-ended class has no midpoint).")
  } else {
    amean <- sum(mid * p); ex2 <- sum(mid^2 * p)
    lines <- c(lines, sprintf("  Mean   ~ sum(m_k p_k) = %s = %s", .expand_sum(mid, p), .f(amean)))
    if (!is.null(C$n) && C$n > 1) {
      avar <- C$n / (C$n - 1) * (ex2 - amean^2)
      lines <- c(lines, sprintf("  Variance s^2 ~ n/(n-1) [sum(m_k^2 p_k) - mean^2] = %s/%s x (%s - %s^2) = %s",
                                .f(C$n), .f(C$n - 1), .f(ex2), .f(amean), .f(avar)),
                 sprintf("     (without the n/(n-1) factor: %s - %s^2 = %s, SD = %s; almost the same for large n)",
                         .f(ex2), .f(amean), .f(ex2 - amean^2), .f(sqrt(ex2 - amean^2))))
    } else {
      avar <- ex2 - amean^2
      lines <- c(lines, sprintf("  Variance ~ sum(m_k^2 p_k) - mean^2 = %s - %s^2 = %s   (n unknown: n/(n-1) ignored, ~1 for large n)",
                                .f(ex2), .f(amean), .f(avar)))
    }
    lines <- c(lines, sprintf("  SD     ~ sqrt(%s) = %s     CV = SD / |mean| = %s", .f(avar), .f(sqrt(avar)), .f(sqrt(avar) / abs(amean))))
  }
  qs <- vapply(probs, function(q) .class_Q(q, lower, upper, p, dens)$v, numeric(1))
  lines <- c(lines, sprintf("  Quantiles: %s", paste(sprintf("p%s = %s", round(100 * probs), ifelse(is.na(qs), "n.a. (open class)", .f(qs))), collapse = "   ")),
             vapply(probs[probs != 0.5 & probs != 0.25 & probs != 0.75], function(q)
               sprintf("    p%s = %s", round(100 * q), .class_Q(q, lower, upper, p, dens)$txt), character(1)))
  .with_plot(function() {
    if (plot == "both") { op <- par(mfrow = c(1, 2)); on.exit(par(op)) }
    lo <- lower[closed]; up <- upper[closed]
    if (plot %in% c("both", "hist")) {
      plot(NA, xlim = range(c(lo, up)), ylim = c(0, max(dens[closed]) * 1.1), xlab = xlab, ylab = "Densities",
           main = paste("Histogram:", xlab), las = 1)
      rect(lo, 0, up, dens[closed], col = "grey85", border = "grey30")
    }
    if (plot %in% c("both", "ogive")) {
      xs <- c(lower[closed[1]], upper[closed]); ys <- c(if (closed[1] == 1) 0 else cumF[closed[1] - 1], cumF[closed])
      plot(xs, ys, type = "b", pch = 19, cex = 0.7, ylim = c(0, 1), xlab = xlab,
           ylab = "Cumulative Proportions", main = paste("Ogive:", xlab), las = 1)
      lines(range(xs), c(0, 1), col = "firebrick", lty = 2)
    }
  })
  wf <- w[closed]
  wording <- c(
    if (C$source == "raw") "The data were grouped into intervals; the frequency distribution and the histogram depend on the chosen classes."
    else "Only the classes are known (not the raw values), so proportions within a class, the median, the quartiles and the mean are approximations assuming values are spread uniformly within each class (the mean assigns each class's frequency to its midpoint).",
    if (length(unique(round(wf, 8))) > 1) "The classes have different widths, so the histogram must be built with densities c_k = p_k / w_k (proportion of cases per unit interval); comparing proportions alone would be misleading."
    else "All classes have the same width, so densities are proportional to the proportions.",
    sprintf("The highest concentration of data is in %s (highest density); the median is approximately %s.",
            paste(labs[modal], collapse = " and "), if (is.na(Qs[[2]]$v)) "not determinable" else .f(Qs[[2]]$v)))
  ub <- if (C$source == "table") .ub_raw_note else {
    cl <- if (C$source == "raw") list(breaks = sb) else list(interval = TRUE)
    pl <- switch(plot, both = c("histogram", "cumulative"), hist = "histogram", ogive = "cumulative")
    c(do.call(.ub_call, c(list("distr.table.x", x = sx, freq = c("counts", "proportions", "densities", "cumulative")), cl), quote = TRUE),
      vapply(pl, function(pt) do.call(.ub_call, c(list("distr.plot.x", x = sx, freq = if (pt == "histogram") "densities",
                                                       plot.type = pt), cl), quote = TRUE), character(1), USE.NAMES = FALSE))
  }
  .result("Frequency distribution of classes", lines, wording, notes, match.call(), ubstats = ub,
          table = tab, n = C$n, mean = amean, variance = avar,
          median = Qs[[2]]$v, quartiles = c(Q1 = Qs[[1]]$v, Q3 = Qs[[3]]$v),
          quantiles = stats::setNames(qs, paste0("p", round(100 * probs))),
          modal_class = c(lower[modal[1]], upper[modal[1]]), values = vals)
}
