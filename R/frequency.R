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
                      plot = c("auto", "bars", "pie", "spike", "cum"), se = FALSE) {
  sort <- match.arg(sort); plot <- match.arg(plot)
  xlab <- .label(substitute(x))
  notes <- NULL
  fr <- .as_freq(x)
  numeric <- FALSE
  if (is.null(fr)) {
    v <- .one_column(x, xlab)
    dropped <- sum(is.na(v)); v <- v[!is.na(v)]
    if (!length(v)) stop(xlab, " has no non-missing values.", call. = FALSE)
    numeric <- is.numeric(v)
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
  } else if (!is.null(order)) {
    fr <- fr[as.character(order)]
  }
  if (sort != "none") {
    fr <- fr[base::order(fr, decreasing = sort == "decreasing")]
    notes <- c(notes, "Categories are ordered by frequency, so cumulative frequencies are not meaningful here.")
  }
  n <- sum(fr); p <- fr / n
  tab <- data.frame(value = names(fr), Count = as.numeric(fr), Prop = as.numeric(p), Percent = 100 * as.numeric(p),
                    Cum.Count = cumsum(as.numeric(fr)), Cum.Prop = cumsum(as.numeric(p)), stringsAsFactors = FALSE)
  if (se) tab$SE_prop <- sqrt(tab$Prop * (1 - tab$Prop) / n)
  names(tab)[1] <- xlab
  shown <- .with_total(tab, list(Count = n, Prop = 1, Percent = 100))
  lines <- c(sprintf("Variable: %s   n = %s   K = %d distinct values", xlab, .f(n), length(fr)), "",
             .table_lines(shown),
             "", "Count = f_k   Prop = p_k = f_k / n   Cum.Prop = F_k = p_1 + ... + p_k")
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
  .result("Frequency distribution", lines, NULL, notes, match.call(),
          table = tab, n = n, values = Q$values)
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
.classes_input <- function(x, breaks, lower, upper, freq, prop, xlab) {
  steps <- character(); notes <- character(); source <- "table"
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
    if (is.null(iv)) stop(xlab, " is not measured in classes: its values are not intervals like [0,50) or 10-20.", call. = FALSE)
    lower <- iv$lower; upper <- iv$upper
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
    if (length(breaks) < 2) stop("Without raw data, breaks must be the class limits c(a, b, c, ...).", call. = FALSE)
    b <- sort(breaks); lower <- head(b, -1); upper <- tail(b, -1)
  }
  if (is.null(lower) || is.null(upper))
    stop("Give raw data + breaks, a variable measured in classes, a class table, or breaks = c(...) with freq = / prop =.", call. = FALSE)
  k <- length(lower)
  if (length(upper) != k) stop("lower and upper must have the same length.", call. = FALSE)
  if (any(upper <= lower)) stop("Each upper limit must be larger than its lower limit.", call. = FALSE)
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
  list(lower = lower, upper = upper, freq = if (is.null(prop)) freq else NULL, p = p, n = n,
       steps = steps, notes = notes, source = source)
}

.class_labels <- function(lower, upper) {
  k <- length(lower)
  paste0("[", .f(lower), ", ", .f(upper), c(rep(")", k - 1), "]"))
}

# Approximate F(x) = Freq(X <= x) for grouped data (uniform within classes).
.class_F <- function(x, lower, upper, p, dens) {
  cumF <- cumsum(p); k <- length(lower)
  if (x <= lower[1]) return(list(v = 0, txt = sprintf("0 (%s is at or below the first class)", .f(x))))
  if (x >= upper[k]) return(list(v = 1, txt = sprintf("1 (%s is at or above the last class)", .f(x))))
  j <- max(which(lower <= x))
  if (x > upper[j]) return(list(v = cumF[j], txt = sprintf("%s (in a gap between classes)", .f(cumF[j]))))
  before <- if (j == 1) 0 else cumF[j - 1]
  if (isTRUE(all.equal(x, lower[j]))) return(list(v = before, txt = sprintf("%s (sum of the classes below %s, exact)", .f(before), .f(x))))
  v <- before + dens[j] * (x - lower[j])
  list(v = v, txt = sprintf("Freq(X < %s) + c_%d x (%s - %s) = %s + %s x %s = %s",
                            .f(lower[j]), j, .f(x), .f(lower[j]), .f(before), .f(dens[j], 6), .f(x - lower[j]), .f(v)))
}

#' @rdname describe
#' @export
desc_classes <- function(x = NULL, breaks = NULL, lower = NULL, upper = NULL, freq = NULL, prop = NULL,
                         at_most = NULL, at_least = NULL, between = NULL,
                         probs = c(0.25, 0.5, 0.75, 0.9, 0.95), plot = c("both", "hist", "ogive")) {
  plot <- match.arg(plot)
  xlab <- if (is.null(x)) "X" else .label(substitute(x))
  C <- .classes_input(x, breaks, lower, upper, freq, prop, xlab)
  lower <- C$lower; upper <- C$upper; p <- C$p; k <- length(lower)
  w <- upper - lower; mid <- (lower + upper) / 2
  dens <- p / w; cumF <- cumsum(p)
  tab <- data.frame(class = .class_labels(lower, upper), w_k = w, m_k = mid, stringsAsFactors = FALSE)
  if (!is.null(C$freq)) tab$f_k <- C$freq
  tab$p_k <- p; tab$c_k <- dens; tab$F_k <- cumF
  total <- list(w_k = NULL, p_k = 1)
  if (!is.null(C$freq)) total$f_k <- sum(C$freq)
  lines <- c(C$steps,
             sprintf("Classes are closed on the left and open on the right, except the last one.%s",
                     if (!is.null(C$n)) sprintf("   n = %s", .f(C$n)) else ""), "",
             .table_lines(.with_total(tab, total[!vapply(total, is.null, logical(1))], list(c_k = max(5, .digits() + 1)))), "",
             "w_k = width   m_k = midpoint   f_k = count   p_k = proportion   c_k = p_k / w_k = density   F_k = cumulative proportion")
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
  }
  # location measures (approximations; see also chapter 3)
  modal <- which(abs(dens - max(dens)) < 1e-12 * max(dens))
  amean <- sum(mid * p)
  avar <- sum(p * (mid - amean)^2)
  qfun <- function(q) {
    j <- which(cumF >= q - 1e-12)[1]
    prev <- if (j == 1) 0 else cumF[j - 1]
    lower[j] + (q - prev) / dens[j]
  }
  qs <- vapply(probs, qfun, numeric(1))
  lines <- c(lines, "",
             sprintf("Modal class%s (highest DENSITY c_k, not highest frequency) = %s", if (length(modal) > 1) "es (tie)" else "",
                     paste(tab$class[modal], collapse = " and ")),
             sprintf("Approx. mean = sum(m_k x p_k) = %s", .f(amean)),
             sprintf("Approx. variance = sum(p_k x (m_k - mean)^2) = %s   SD = %s", .f(avar), .f(sqrt(avar))),
             sprintf("Approx. quantiles (uniform within classes): %s", paste(sprintf("p%s = %s", round(100 * probs), .f(qs)), collapse = "   ")))
  .with_plot(function() {
    if (plot == "both") { op <- par(mfrow = c(1, 2)); on.exit(par(op)) }
    if (plot %in% c("both", "hist")) {
      plot(NA, xlim = range(c(lower, upper)), ylim = c(0, max(dens) * 1.1), xlab = xlab, ylab = "Densities",
           main = paste("Histogram:", xlab), las = 1)
      rect(lower, 0, upper, dens, col = "grey85", border = "grey30")
    }
    if (plot %in% c("both", "ogive")) {
      plot(c(lower[1], upper), c(0, cumF), type = "b", pch = 19, cex = 0.7, ylim = c(0, 1), xlab = xlab,
           ylab = "Cumulative Proportions", main = paste("Ogive:", xlab), las = 1)
      lines(c(lower[1], upper[k]), c(0, 1), col = "firebrick", lty = 2)
    }
  })
  wording <- c(
    if (C$source == "raw") "The data were grouped into intervals; the frequency distribution and the histogram depend on the chosen classes."
    else "Only the classes are known (not the raw values), so proportions within a class, the mean and the quantiles are approximations assuming values are spread uniformly within each class.",
    if (length(unique(round(w, 8))) > 1) "The classes have different widths, so the histogram must be built with densities c_k = p_k / w_k (proportion of cases per unit interval); comparing proportions alone would be misleading."
    else "All classes have the same width, so densities are proportional to the proportions.",
    sprintf("The highest concentration of data is in %s (highest density).", paste(tab$class[modal], collapse = " and ")))
  .result("Frequency distribution of classes", lines, wording, C$notes, match.call(),
          table = tab, n = C$n, mean = amean, variance = avar, quantiles = stats::setNames(qs, paste0("p", round(100 * probs))),
          modal_class = c(lower[modal[1]], upper[modal[1]]), values = vals)
}
