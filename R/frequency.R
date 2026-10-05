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

# Ordered scales of common ordinal labels (lower case, letters only).
.ordinal_scales <- list(
  c(none = 0, verylow = 1, low = 2, mediumlow = 3, lowmedium = 3, medium = 4, mid = 4, moderate = 4,
    mediumhigh = 5, highmedium = 5, high = 6, veryhigh = 7),
  c(young = 1, middle = 2, adult = 2, middleaged = 2, senior = 3, old = 3, elderly = 3),
  c(never = 1, rarely = 2, seldom = 2, sometimes = 3, often = 4, usually = 4, always = 5),
  c(verypoor = 0, poor = 1, fair = 2, average = 2, good = 3, verygood = 4, excellent = 5),
  c(stronglydisagree = 1, disagree = 2, neutral = 3, agree = 4, stronglyagree = 5),
  c(verysmall = 0, small = 1, medium = 2, large = 3, verylarge = 4),
  stats::setNames(1:12, tolower(month.name)),
  stats::setNames(1:12, tolower(month.abb)),
  c(monday = 1, tuesday = 2, wednesday = 3, thursday = 4, friday = 5, saturday = 6, sunday = 7))

# Natural order of labels that look ordinal (Young / Middle / Senior, Low / Medium / High, months);
# NULL when no scale is recognised or the labels are already in that order.
.ordinal_match <- function(lv) {
  if (length(lv) < 2) return(NULL)
  key <- gsub("[^a-z]", "", tolower(lv))
  for (sc in .ordinal_scales) if (all(key %in% names(sc))) return(lv[base::order(sc[key])])
  NULL
}
.ordinal_guess <- function(lv) {
  o <- .ordinal_match(lv)
  if (is.null(o) || identical(o, lv)) NULL else o
}

# Labels in alphabetical order (as R sorts text)?
.alphabetical <- function(lv) identical(lv, lv[base::order(tolower(lv), lv)])

# Counts of the distinct values of raw data or of a typed frequency table.
.freq_counts <- function(x, xlab, order = NULL) {
  notes <- NULL; guess <- NULL
  fr <- .as_freq(x)
  raw <- is.null(fr); intervals <- FALSE; is_props <- FALSE
  if (raw) {
    v <- .one_column(x, xlab)
    dropped <- sum(is.na(v)); v <- v[!is.na(v)]
    if (!length(v)) stop(xlab, " has no non-missing values.", call. = FALSE)
    numeric <- is.numeric(v)
    intervals <- is.null(order) && !numeric && !is.null(.parse_intervals(v))
    lv <- .cats(v)
    # factors whose levels are not alphabetical were put in order on purpose (factor(levels =))
    ordered <- numeric || intervals || is.ordered(v) || (is.factor(v) && !.alphabetical(lv))
    if (!is.null(order)) {
      miss <- setdiff(lv, as.character(order))
      if (length(miss)) stop("order is missing these values of ", xlab, ": ", paste(miss, collapse = ", "), call. = FALSE)
      lv <- as.character(order); numeric <- FALSE; ordered <- TRUE
    }
    fr <- stats::setNames(as.numeric(table(factor(as.character(v), levels = lv))), lv)
    if (dropped) notes <- c(notes, sprintf("%d missing value(s) were removed.", dropped))
    if (!ordered && !numeric) guess <- .ordinal_guess(lv)
    if (numeric && length(lv) > 20)
      notes <- c(notes, sprintf("%s has %d distinct values: a table of values is not effective. Group them into intervals with desc_classes(%s, breaks = K).", xlab, length(lv), xlab))
  } else {
    if (!is.null(order)) fr <- fr[as.character(order)]
    tot <- sum(fr)
    is_props <- abs(tot - 1) < 1e-6 || (abs(tot - 100) < 1e-6 && any(fr != round(fr)))
    labvals <- vapply(names(fr), function(l) .open_value(l)$value, numeric(1))
    numeric <- all(!is.na(suppressWarnings(as.numeric(names(fr)))))
    if (numeric) {
      fr <- fr[base::order(as.numeric(names(fr)))]
      if (!is.null(order)) numeric <- FALSE
    }
    ordered <- !is.null(order) || numeric || all(!is.na(labvals)) || identical(.ordinal_match(names(fr)), names(fr))
    if (!ordered) guess <- .ordinal_guess(names(fr))
  }
  list(fr = fr, raw = raw, numeric = numeric, ordered = ordered, intervals = intervals,
       is_props = is_props, notes = notes, guess = guess)
}

# Type of the variable, as the course names it.
.freq_type <- function(Fc, K) {
  if (Fc$intervals) "classes" else if (Fc$numeric) "discrete" else if (Fc$ordered) "ordinal" else if (K == 2) "binary" else "nominal"
}

# Position of query values among ordered categories / numeric values.
.cum_query_lines <- function(vals, p, numeric, at_most, at_least, between, xlab) {
  lines <- character(); words <- character(); out <- list()
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
  for (a in at_most) {
    k <- pos_le(a); v <- if (k) cumF[k] else 0
    lines <- c(lines, sprintf("Freq(X <= %s) = F(%s) = %s = %s", fmt(a), fmt(a), sum_txt(seq_len(k)), .f(v)))
    words <- c(words, sprintf("%s of the units have %s at most %s (cumulative frequency F(%s) = %s).", .pct(v, 1), xlab, fmt(a), fmt(a), .f(v)))
    out$at_most <- c(out$at_most, v)
  }
  for (a in at_least) {
    k <- pos_lt(a); v <- 1 - (if (k) cumF[k] else 0)
    lines <- c(lines, sprintf("Freq(X >= %s) = 1 - Freq(X < %s) = %s = %s", fmt(a), fmt(a),
                              sum_txt(setdiff(seq_along(p), seq_len(k))), .f(v)))
    words <- c(words, sprintf("%s of the units have %s at least %s.", .pct(v, 1), xlab, fmt(a)))
    out$at_least <- c(out$at_least, v)
  }
  if (!is.null(between)) {
    if (length(between) != 2) stop("between must be two values: c(a, b).", call. = FALSE)
    a <- between[1]; b <- between[2]
    hi <- pos_le(b); lo <- pos_lt(a)
    idx <- if (hi > lo) (lo + 1):hi else integer()
    v <- sum(p[idx])
    lines <- c(lines, sprintf("Freq(%s <= X <= %s) = %s = %s", fmt(a), fmt(b), sum_txt(idx), .f(v)))
    words <- c(words, sprintf("%s of the units have %s between %s and %s (both included).", .pct(v, 1), xlab, fmt(a), fmt(b)))
    out$between <- v
  }
  list(lines = lines, words = words, values = out)
}

# Exam wording for a frequency distribution of distinct values.
.freq_wording <- function(type, xlab, labs, p, n, S, guess, zero_vals) {
  K <- length(labs); pc <- function(i) .pct(p[i], 1)
  ord <- base::order(p, decreasing = TRUE)
  unit <- if (type == "discrete") "values" else "categories"
  if (!is.null(guess)) {
    # ordinal labels stored as text: R lists them alphabetically
    type_txt <- sprintf("%s is a qualitative ordinal variable (%s), but it is stored as text, so R lists its categories in alphabetical order (%s).",
                        xlab, paste(guess, collapse = " < "), paste(labs, collapse = ", "))
    graph_txt <- sprintf("Graph: a bar chart with the categories in their natural order (%s), preferable to a pie chart, which hides the order; to get it, define the factor with the levels in that order.",
                         paste(guess, collapse = ", "))
    measure_txt <- sprintf("With the order given (order = c(%s), or factor(x, levels = ...)) the median and the cumulative frequencies can be computed; the mean cannot.",
                           paste0("\"", guess, "\"", collapse = ", "))
  }
  if (is.null(guess)) type_txt <- switch(type,
    binary = sprintf("%s is a qualitative (categorical) nominal variable with two categories (binary): %s.", xlab, paste(labs, collapse = " / ")),
    nominal = sprintf("%s is a qualitative nominal variable: its %d categories have no natural order.", xlab, K),
    ordinal = sprintf("%s is a qualitative ordinal variable: its categories have a natural order (%s).", xlab, paste(labs, collapse = " < ")),
    discrete = sprintf("%s is a quantitative discrete variable with %d distinct values.", xlab, K),
    classes = sprintf("%s is measured in classes (intervals of values).", xlab))
  if (is.null(guess)) graph_txt <- switch(type,
    binary = , nominal = "Graph: a pie chart or a bar chart of the relative frequencies. In a bar chart of a nominal variable R lists the categories in alphabetical order, so their position on the axis carries no information; absolute and relative frequencies give the same picture (the heights are proportional).",
    ordinal = sprintf("Graph: a bar chart with the categories in their natural order (%s), preferable to a pie chart, which hides the order and makes the slices hard to compare. Cumulative frequencies are meaningful because the categories are ordered.", paste(labs, collapse = ", ")),
    discrete = paste0("Graph: a spike plot (one vertical line at each observed value), not a bar chart: the horizontal axis must respect the distances between the values",
                      if (length(zero_vals)) sprintf(" (e.g. %s, with frequency 0, keeps its empty position)", paste(zero_vals, collapse = ", ")) else "",
                      ". The cumulative frequencies are a step function."),
    classes = "Graph: a histogram built with densities.")
  shares <- if (K <= 6) sprintf("Relative frequencies: %s.", paste(sprintf("%s %s", labs, vapply(seq_len(K), pc, "")), collapse = ", "))
            else sprintf("The most frequent value is %s (%s) and the least frequent %s (%s).", labs[ord[1]], pc(ord[1]), labs[ord[K]], pc(ord[K]))
  if (K >= 2 && diff(range(p)) <= 0.05 && type %in% c("binary", "nominal", "ordinal"))
    shares <- paste(shares, sprintf("The categories have approximately the same weight (the shares differ by at most %s points%s).",
                                    .f(100 * diff(range(p)), 1), if (!is.na(n)) sprintf(", i.e. %s units out of n = %s", .f(round(diff(range(p)) * n)), .f(n)) else ""))
  mode_txt <- NULL
  if (!is.null(S$mode)) {
    m1 <- ord[1]; m2 <- ord[2]
    rep_txt <- if (K >= 2 && p[m1] - p[m2] < 0.03) sprintf("the second category, %s (%s), has almost the same frequency, so the mode is poorly representative", labs[m2], pc(m2))
      else if (p[m1] >= 0.5) sprintf("it covers the majority of the units (%s), so it is representative", pc(m1))
      else sprintf("it covers only %s of the units%s, so it is not very representative", pc(m1),
                   if (K >= 2) sprintf(" (with %d %s a uniform distribution would give %s to each)", K, unit, .pct(1 / K, 1)) else "")
    mode_txt <- sprintf("The mode is %s (%s): %s.", paste(S$mode, collapse = " and "), pc(m1), rep_txt)
  }
  if (is.null(guess)) measure_txt <- switch(type,
    binary = , nominal = "For a nominal variable the mode is the only measure of central tendency that can be computed: the median needs ordered categories and the mean needs numbers.",
    ordinal = if (!is.null(S$median)) sprintf("The median is %s: the first category whose cumulative frequency reaches 50%% (mode and median can be computed for an ordinal variable, the mean cannot).", S$median),
    discrete = if (!is.null(S$mean)) sprintf("The median is %s (the first value whose cumulative frequency reaches 50%%) and the mean is %s = sum(x_k p_k)%s.", S$median, .f(S$mean),
                                            if (abs(S$mean - as.numeric(S$median)) <= 0.1 * (S$sd %||% 0)) "; mean and median are close" else if (S$mean > as.numeric(S$median)) "; mean > median, a sign of a longer right tail" else "; mean < median, a sign of a longer left tail"),
    classes = NULL)
  c(.w("frame", type_txt), .w("tool", graph_txt), .w("result", shares), .w("meaning", mode_txt), .w("conclusion", measure_txt))
}

#' @rdname describe
#' @export
desc_freq <- function(x, order = NULL, sort = c("none", "decreasing", "increasing"),
                      at_most = NULL, at_least = NULL, between = NULL,
                      plot = c("auto", "bars", "pie", "spike", "cum"), se = FALSE, n = NULL,
                      event = NULL, compare = NULL) {
  n_given <- n
  sort <- match.arg(sort); plot <- match.arg(plot)
  sx <- substitute(x); xlab <- .label(sx)
  Fc <- .freq_counts(x, xlab, order)
  fr <- Fc$fr; raw <- Fc$raw; numeric <- Fc$numeric; ordered <- Fc$ordered; intervals <- Fc$intervals; is_props <- Fc$is_props
  notes <- Fc$notes
  if (!is.null(Fc$guess)) {
    q <- paste0("\"", Fc$guess, "\"", collapse = ", ")
    notes <- c(notes, sprintf("%s looks ordinal (%s) but R keeps text in alphabetical order. Re-run with order = c(%s), or create the factor: %s <- factor(%s, levels = c(%s)).",
                              xlab, paste(Fc$guess, collapse = " < "), q, paste0(xlab, "_f"), paste(deparse(sx), collapse = ""), q))
  } else if (raw && !ordered && !numeric && length(fr) <= 15)
    notes <- c(notes, "Categories are listed in alphabetical order. If the variable is ordinal, give the order: order = c(\"lowest\", ..., \"highest\") - the median and cumulative frequencies need it.")
  if (is_props && is.null(n_given)) notes <- c(notes, "The values were read as proportions / percentages: the sample size n is unknown (give n = ... if the question states it).")
  if (sort != "none") {
    ordered <- FALSE
    fr <- fr[base::order(fr, decreasing = sort == "decreasing")]
    notes <- c(notes, "Categories are ordered by frequency, so cumulative frequencies are not meaningful here.")
  }
  n <- sum(fr); p <- fr / n
  type <- .freq_type(list(intervals = intervals, numeric = numeric, ordered = ordered), length(fr))
  tab <- data.frame(value = names(fr), Count = as.numeric(fr), Prop = as.numeric(p), Percent = 100 * as.numeric(p),
                    Cum.Count = cumsum(as.numeric(fr)), Cum.Prop = cumsum(as.numeric(p)), stringsAsFactors = FALSE)
  if (is_props) { tab$Count <- NULL; tab$Cum.Count <- NULL; n <- NA }
  if (!ordered) { tab$Cum.Count <- NULL; tab$Cum.Prop <- NULL }
  if (se && !is_props) tab$SE_prop <- sqrt(tab$Prop * (1 - tab$Prop) / n)
  names(tab)[1] <- xlab
  shown <- .with_total(tab, if (is_props) list(Prop = 1, Percent = 100) else list(Count = n, Prop = 1, Percent = 100))
  type_lab <- switch(type, binary = "qualitative nominal (binary)", nominal = "qualitative nominal", ordinal = "qualitative ordinal",
                     discrete = "quantitative discrete", classes = "measured in classes")
  lines <- c(sprintf("Variable: %s   n = %s   K = %d distinct values   type: %s", xlab, if (is_props) "unknown" else .f(n), length(fr), type_lab), "",
             .table_lines(shown),
             "", paste0(if (is_props) "Prop = p_k" else "Count = f_k   Prop = p_k = f_k / n", if (ordered) "   Cum.Prop = F_k = p_1 + ... + p_k" else
                          "   (no cumulative frequencies: the categories have no order)"))
  S <- .freq_summary(names(fr), as.numeric(p), if (is_props && !is.null(n_given)) n_given else n, ordered)
  lines <- c(lines, "", S$lines)
  if (se) lines <- c(lines, "SE_prop = sqrt(p_k (1 - p_k) / n): estimated standard error of each sample proportion")
  if (!ordered && (length(at_most) || length(at_least) || length(between)))
    stop("at_most / at_least / between need ordered values: give order = c(...) for an ordinal variable.", call. = FALSE)
  Q <- .cum_query_lines(names(fr), as.numeric(p), numeric, at_most, at_least, between, xlab)
  if (length(Q$lines)) lines <- c(lines, "", Q$lines)
  zero_vals <- if (numeric) names(fr)[fr == 0] else character()
  wording <- c(.freq_wording(type, xlab, names(fr), as.numeric(p), n, S, Fc$guess, zero_vals), .w("result", Q$words))
  # combined share of several categories
  ev_share <- NULL
  if (length(event)) {
    ev <- as.character(event); bad <- setdiff(ev, names(fr))
    if (length(bad)) stop("event: not a category of ", xlab, ": ", paste(bad, collapse = ", "), ". Categories: ", paste(names(fr), collapse = ", "), call. = FALSE)
    idx <- match(ev, names(fr)); ev_share <- sum(p[idx])
    lines <- c(lines, "", sprintf("Freq(%s in {%s}) = %s = %s%s", xlab, paste(ev, collapse = ", "), paste(.f(p[idx]), collapse = " + "), .f(ev_share),
                                  if (!is_props) sprintf("   (%s of %s units)", .f(sum(fr[idx])), .f(n)) else ""))
    wording <- c(wording, .w("conclusion", sprintf("Taken together, %s account for %s of the units (%s); the other categories account for %s.",
                                  paste(ev, collapse = ", "), .pct(ev_share, 1), paste(.f(p[idx]), collapse = " + "), .pct(1 - ev_share, 1))))
  }
  # a second distribution, side by side in percentages
  cmp <- NULL
  if (!is.null(compare)) {
    sc <- substitute(compare); clab <- .label(sc)
    if (identical(clab, xlab)) { xlab2 <- paste(xlab, "(1)"); clab <- paste(clab, "(2)") } else xlab2 <- xlab
    B <- .freq_counts(compare, clab, order)
    fr2 <- B$fr
    vals <- if (numeric && B$numeric) as.character(sort(unique(as.numeric(c(names(fr), names(fr2)))))) else unique(c(names(fr), names(fr2)))
    pA <- as.numeric(fr[vals]) / sum(fr); pB <- as.numeric(fr2[vals]) / sum(fr2)
    pA[is.na(pA)] <- 0; pB[is.na(pB)] <- 0
    cmp <- data.frame(value = vals, a = 100 * pA, b = 100 * pB, stringsAsFactors = FALSE)
    names(cmp) <- c(xlab, paste("%", xlab2), paste("%", clab))
    SB <- .freq_summary(vals, pB, if (B$is_props) NA else sum(fr2), ordered || B$ordered)
    SA <- .freq_summary(vals, pA, n, ordered || B$ordered)
    meas <- data.frame(measure = c("n", "Mode", if (!is.null(SA$median)) "Median", if (!is.null(SA$mean)) "Mean"),
                       a = c(if (is_props) "unknown" else .f(n), paste(SA$mode, collapse = ", "), if (!is.null(SA$median)) SA$median, if (!is.null(SA$mean)) .f(SA$mean, 2)),
                       b = c(if (B$is_props) "unknown" else .f(sum(fr2)), paste(SB$mode, collapse = ", "), if (!is.null(SB$median)) SB$median, if (!is.null(SB$mean)) .f(SB$mean, 2)),
                       stringsAsFactors = FALSE)
    names(meas) <- c("", xlab2, clab)
    lines <- c(lines, "", sprintf("Comparison with %s (relative frequencies, %%):", clab), .table_lines(.round_df(cmp)), "",
               .table_lines(meas))
    d <- pB - pA; top <- base::order(abs(d), decreasing = TRUE)[seq_len(min(2, length(d)))]
    only <- vals[(pA == 0) != (pB == 0)]
    wording <- c(wording,
      .w("tool", sprintf("The two distributions come from samples of different sizes (n = %s and n = %s): compare them through relative frequencies (percentages), not counts.",
              if (is_props) "unknown" else .f(n), if (B$is_props) "unknown" else .f(sum(fr2)))),
      .w("result", sprintf("Mode: %s vs %s%s%s.", paste(SA$mode, collapse = ", "), paste(SB$mode, collapse = ", "),
              if (!is.null(SA$median)) sprintf("; median: %s vs %s", SA$median, SB$median) else "",
              if (!is.null(SA$mean)) sprintf("; mean: %s vs %s", .f(SA$mean, 2), .f(SB$mean, 2)) else "")),
      .w("meaning", sprintf("The largest differences are at %s.", paste(sprintf("%s (%s%% vs %s%%)", vals[top], .f(100 * pA[top], 1), .f(100 * pB[top], 1)), collapse = " and ")),
         if (length(only)) sprintf("Observed in only one of the two: %s.", paste(only, collapse = ", "))))
  }
  ptype <- if (plot == "auto") (if (numeric) "spike" else "bars") else plot
  .with_plot(function() {
    one <- function(pp, lab, ylim = NULL) {
      main <- paste(switch(ptype, bars = "Bar plot:", pie = "Pie chart:", spike = "Spike plot:", cum = "Cumulative freq:"), lab)
      if (ptype == "bars") barplot(pp, main = main, ylab = "Proportions", col = "grey80", las = 1, ylim = ylim)
      else if (ptype == "pie") pie(pp, main = main, col = grDevices::gray.colors(length(pp)))
      else if (ptype == "spike") {
        xs <- suppressWarnings(as.numeric(names(pp)))
        if (anyNA(xs)) xs <- seq_along(pp)
        plot(xs, pp, type = "h", lwd = 2, xlab = lab, ylab = "Proportions", main = main, las = 1, ylim = ylim %||% c(0, max(pp) * 1.05))
        points(xs, pp, pch = 19, cex = 0.7)
      } else .plot_cum(names(pp), as.numeric(pp), numeric, lab)
    }
    if (is.null(cmp)) one(p, xlab)
    else {
      op <- par(mfrow = c(1, 2)); on.exit(par(op))
      yl <- c(0, max(c(pA, pB)) * 1.05)
      one(stats::setNames(pA, vals), xlab2, yl); one(stats::setNames(pB, vals), clab, yl)
    }
  })
  ub <- if (!raw) .ub_raw_note else {
    xe <- .ub_ordered(sx, order)
    iv <- if (intervals) TRUE
    c(.ub_call("distr.table.x", x = xe, freq = if (ordered) c("counts", "proportions", "cumulative") else c("counts", "proportions"), interval = iv),
      .ub_call("distr.plot.x", x = xe, freq = "proportions",
               plot.type = switch(ptype, bars = "bars", pie = "pie", spike = "spike", cum = "cumulative"),
               ord.freq = if (sort != "none" && ptype %in% c("bars", "pie")) sort, interval = iv),
      if (!is.null(order)) "# factor(..., levels = ) gives UBStats the order (otherwise alphabetical)")
  }
  .result("Frequency distribution", lines, wording, notes, match.call(),
          table = tab, n = n, type = type, values = Q$values, mode = S$mode, median = S$median, quartiles = S$quartiles,
          mean = S$mean, variance = S$variance, sd = S$sd, event_share = ev_share, comparison = cmp, ubstats = ub)
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
.classes_input <- function(x, breaks, lower, upper, freq, prop, xlab, density = NULL) {
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
  if (!is.null(density)) {
    if (length(density) != k) stop("density must have one value per class (NA for the one class to obtain as 1 - the others).", call. = FALSE)
    if (sum(is.na(density)) > 1) stop("At most one density can be NA (it is obtained as 1 minus the other proportions).", call. = FALSE)
    if (any(is.infinite(lower) | is.infinite(upper))) stop("With densities all classes must be closed.", call. = FALSE)
    w <- upper - lower; prop <- density * w
    steps <- c(steps, sprintf("Proportions from the densities: p_k = c_k x w_k = %s",
                              paste(ifelse(is.na(density), "?", sprintf("%s x %s = %s", .fd(density), .f(w), .f(prop))), collapse = ";  ")))
    if (anyNA(prop)) {
      j <- which(is.na(prop)); prop[j] <- 1 - sum(prop, na.rm = TRUE)
      steps <- c(steps, sprintf("Class %d: p_%d = 1 - (sum of the other proportions) = %s, density c_%d = %s / %s = %s",
                                j, j, .f(prop[j]), j, .f(prop[j]), .f(w[j]), .fd(prop[j] / w[j])))
    }
    if (abs(sum(prop) - 1) > 1e-3) notes <- c(notes, sprintf("The proportions obtained from the densities sum to %s (values read off a graph are rounded); they are used as they are.", .f(sum(prop))))
    return(list(lower = lower, upper = upper, labels = labels, freq = NULL, p = prop, n = NULL,
                steps = steps, notes = notes, source = "table"))
  }
  if (is.null(prop)) {
    if (is.null(freq) || length(freq) != k) stop("Give freq = (counts), prop = (proportions / %) or density =, one per class.", call. = FALSE)
    n <- sum(freq); p <- freq / n
  } else {
    if (length(prop) != k) stop("prop must have one value per class.", call. = FALSE)
    n <- NULL; p <- prop / sum(prop)
    if (abs(sum(prop) - 1) > 1e-6 && abs(sum(prop) - 100) > 1e-6) notes <- c(notes, "The proportions did not sum to 1 (or 100%); they were rescaled.")
  }
  list(lower = lower, upper = upper, labels = labels, freq = if (is.null(prop)) freq else NULL, p = p, n = n,
       steps = steps, notes = notes, source = source)
}

# Densities are small numbers: 4 significant digits, never scientific notation.
.fd <- function(x) ifelse(is.na(x), "-", formatC(signif(x, 4), format = "fg", digits = 4))

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
                            .f(lower[j]), j, .f(x), .f(lower[j]), .f(before), .fd(dens[j]), .f(x - lower[j]), .f(v)))
}

# Quantile of grouped data: Q = l_k + (q - F_(k-1)) / c_k  (book 3.3.1).
.class_Q <- function(q, lower, upper, p, dens) {
  cumF <- cumsum(p)
  j <- which(cumF >= q - 1e-9)[1]
  prev <- if (j == 1) 0 else cumF[j - 1]
  if (is.na(dens[j])) return(list(v = NA, txt = sprintf("falls in the open-ended class %s: cannot be determined", .class_labels(lower, upper)[j])))
  v <- lower[j] + (q - prev) / dens[j]
  list(v = v, txt = sprintf("l + (%s - F_%d) / c_%d = %s + (%s - %s) / %s = %s",
                            .f(q), j - 1, j, .f(lower[j]), .f(q), .f(prev), .fd(dens[j]), .f(v)))
}

.expand_sum <- function(a, b, k_max = 6) {
  t <- sprintf("%s x %s", .f(a), .f(b))
  if (length(t) <= k_max) paste(t, collapse = " + ") else paste(c(head(t, 3), "...", tail(t, 1)), collapse = " + ")
}

#' @rdname describe
#' @export
desc_classes <- function(x = NULL, breaks = NULL, lower = NULL, upper = NULL, freq = NULL, prop = NULL, n = NULL,
                         at_most = NULL, at_least = NULL, between = NULL,
                         probs = c(0.25, 0.5, 0.75, 0.9, 0.95), plot = c("both", "hist", "ogive"),
                         density = NULL, split = NULL) {
  plot <- match.arg(plot)
  sx <- substitute(x); sb <- substitute(breaks)
  xlab <- if (is.null(x)) "X" else .label(sx)
  C <- .classes_input(x, breaks, lower, upper, freq, prop, xlab, density)
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
  tshow <- tab; tshow$c_k <- .fd(dens)
  shown <- .with_total(tshow, total)
  shown[shown == "NA"] <- "-"
  lines <- c(C$steps,
             sprintf("Classes are closed on the left and open on the right, except the last one.%s",
                     if (!is.null(C$n)) sprintf("   n = %s", .f(C$n)) else ""), "",
             .table_lines(shown), "",
             "w_k = width   m_k = midpoint   f_k = count   p_k = proportion   c_k = p_k / w_k = density   F_k = cumulative proportion")
  notes <- C$notes
  if (any(open)) notes <- c(notes, "Open-ended class: its width, midpoint and density cannot be evaluated; the median and quantiles that fall in closed classes can still be found, the mean and variance cannot.")
  # queries (uniform within classes)
  vals <- list(); qwords <- character()
  if (!is.null(between) && !is.list(between)) between <- list(between)
  if (length(at_most) || length(at_least) || length(between)) {
    lines <- c(lines, "", "Proportions assuming values are spread uniformly within each class (approximations):")
    units <- function(lab, v) if (!is.null(C$n)) sprintf("   Number of units: n x %s = %s x %s = %s  (approximately %s units)",
                                                       lab, .f(C$n), .f(v), .f(C$n * v), .f(floor(C$n * v + 0.5)))
    say <- function(v, txt) qwords <<- c(qwords, sprintf("About %s of the units%s have %s %s (approximation: uniform within classes).",
                                                         .pct(v, 2), if (!is.null(C$n)) sprintf(" (%s of %s)", .f(floor(C$n * v + 0.5)), .f(C$n)) else "", xlab, txt))
    for (a in at_most) {
      r <- .class_F(a, lower, upper, p, dens); vals$at_most <- c(vals$at_most, r$v)
      lab <- sprintf("Freq(X <= %s)", .f(a))
      lines <- c(lines, sprintf("%s = %s", lab, r$txt), units(lab, r$v)); say(r$v, sprintf("at most %s", .f(a)))
    }
    for (a in at_least) {
      r <- .class_F(a, lower, upper, p, dens); v <- 1 - r$v; vals$at_least <- c(vals$at_least, v)
      lab <- sprintf("Freq(X >= %s)", .f(a))
      lines <- c(lines, sprintf("%s = 1 - Freq(X < %s) = 1 - %s = %s", lab, .f(a), .f(r$v), .f(v)),
                 sprintf("   where Freq(X < %s) = %s", .f(a), r$txt), units(lab, v)); say(v, sprintf("at least %s", .f(a)))
    }
    for (bw in between) {
      if (length(bw) != 2) stop("between must be two values: c(a, b) (or a list of such pairs).", call. = FALSE)
      lo_b <- min(bw); hi_b <- max(bw)
      ra <- .class_F(lo_b, lower, upper, p, dens); rb <- .class_F(hi_b, lower, upper, p, dens)
      v <- rb$v - ra$v; vals$between <- c(vals$between, v)
      lab <- sprintf("Freq(%s <= X <= %s)", .f(lo_b), .f(hi_b))
      lines <- c(lines, sprintf("%s = F(%s) - F(%s) = %s - %s = %s", lab, .f(hi_b), .f(lo_b), .f(rb$v), .f(ra$v), .f(v)),
                 sprintf("   F(%s) = %s", .f(hi_b), rb$txt), sprintf("   F(%s) = %s", .f(lo_b), ra$txt), units(lab, v))
      say(v, sprintf("between %s and %s", .f(lo_b), .f(hi_b)))
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
    sq_terms <- sprintf("%s^2 x %s", .f(mid), .f(p))
    lines <- c(lines, sprintf("  Mean   ~ sum(m_k p_k) = %s = %s", .expand_sum(mid, p), .f(amean)),
               sprintf("  Mean of the squares sum(m_k^2 p_k) = %s = %s;  mean^2 = %s^2 = %s",
                       if (length(sq_terms) <= 6) paste(sq_terms, collapse = " + ") else paste(c(head(sq_terms, 3), "...", tail(sq_terms, 1)), collapse = " + "),
                       .f(ex2), .f(amean), .f(amean^2, 6)))
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
  # two subgroups split at a class limit (each re-normalised)
  split_res <- NULL
  if (!is.null(split)) {
    if (length(split) != 1 || !any(abs(c(lower, upper) - split) < 1e-9))
      stop("split must be one of the class limits: ", paste(.f(unique(c(lower[is.finite(lower)], upper[is.finite(upper)]))), collapse = ", "), call. = FALSE)
    grp <- list(below = which(upper <= split + 1e-9), above = which(lower >= split - 1e-9))
    split_res <- list()
    for (g in names(grp)) {
      idx <- grp[[g]]
      if (!length(idx)) next
      ps <- p[idx] / sum(p[idx]); ds <- ps / w[idx]
      glab <- sprintf("X %s %s", if (g == "below") "<=" else ">", .f(split))
      mq <- .class_Q(0.5, lower[idx], upper[idx], ps, ds)
      gm <- if (any(open[idx])) NA else sum(mid[idx] * ps)
      size <- if (!is.null(C$freq)) sprintf("n = %s, %s of all units", .f(sum(C$freq[idx])), .pct(sum(p[idx]), 2)) else sprintf("%s of all units", .pct(sum(p[idx]), 2))
      lines <- c(lines, "", sprintf("Subgroup %s (%s): relative frequencies recomputed within the subgroup", glab, size),
                 sprintf("  %s", paste(sprintf("%s: %s", labs[idx], .f(ps)), collapse = "   ")),
                 sprintf("  Median = %s", mq$txt),
                 if (!is.na(gm)) sprintf("  Mean   = sum(m_k p_k) = %s = %s", .expand_sum(mid[idx], ps), .f(gm)))
      split_res[[g]] <- c(median = mq$v, mean = gm, share = sum(p[idx]))
    }
  }
  # shape: skewness from mean vs median, peaks of the density, classes with most cases vs most density
  md_v <- Qs[[2]]$v
  shape_txt <- if (!is.na(amean) && !is.na(md_v) && !is.na(avar)) {
    if (abs(amean - md_v) <= 0.05 * sqrt(avar)) sprintf("Mean (%s) and median (%s) are close: the distribution is roughly symmetric.", .f(amean), .f(md_v))
    else sprintf("Mean (%s) %s median (%s): the distribution is %s-skewed (the mean is attracted by the %s tail; the median is robust to it).",
                 .f(amean), if (amean > md_v) ">" else "<", .f(md_v), if (amean > md_v) "right" else "left", if (amean > md_v) "long right" else "long left")
  }
  dc <- dens[closed]
  peaks <- closed[vapply(seq_along(dc), function(i) all(dc[i] > dc[setdiff(c(i - 1, i + 1), c(0, length(dc) + 1))]), logical(1))]
  peak_txt <- if (length(peaks) >= 2 && length(closed) <= 8)
    sprintf("The density has %d peaks (%s): the data suggest %d typical behaviours, i.e. groups of units concentrated in different ranges of values.",
            length(peaks), paste(labs[peaks], collapse = " and "), length(peaks))
  low_txt <- NULL
  if (!is.na(md_v) && !is.na(amean)) {
    cls <- function(x) { j <- max(which(lower <= x)); if (length(j) && x <= upper[j]) j else NA }
    jm <- cls(amean); jd <- cls(md_v)
    if (!is.na(jm) && !is.na(jd) && jm == jd && !(jm %in% modal) && !is.na(dens[jm]) && dens[jm] < 0.5 * max(dc))
      low_txt <- sprintf("Both the mean (%s) and the median (%s) fall in %s, a class with low density: they summarise the whole distribution but do not describe a typical unit.",
                         .f(amean), .f(md_v), labs[jm])
  }
  topf <- closed[which.max(p[closed])]
  freq_txt <- if (!(topf %in% modal) && length(unique(round(wf, 8))) > 1)
    sprintf("%s has the highest frequency (%s) but it is %s wide: per unit of width the densest class is %s (density %s vs %s), so the modal class is %s.",
            labs[topf], .pct(p[topf], 1), .f(w[topf]), labs[modal[1]], .fd(dens[modal[1]]), .fd(dens[topf]), labs[modal[1]])
  split_txt <- if (length(split_res) == 2)
    sprintf("Splitting at %s: units with X <= %s have median %s and mean %s; units with X > %s have median %s and mean %s. Each subgroup's measures now fall in a class where its data concentrate, so they describe the two groups better than the overall mean and median.",
            .f(split), .f(split), .f(split_res$below[["median"]]), .f(split_res$below[["mean"]]), .f(split), .f(split_res$above[["median"]]), .f(split_res$above[["mean"]]))
  wording <- c(
    .w("frame", sprintf("%s is a quantitative variable %s in %d classes%s.", xlab, if (C$source == "raw") "grouped" else "measured", k,
                        if (!is.null(C$n)) sprintf(" (n = %s units)", .f(C$n)) else "")),
    .w("tool", if (length(unique(round(wf, 8))) > 1) "The classes have different widths, so the histogram must be built with densities c_k = p_k / w_k (proportion of cases per unit interval) on the y-axis: with counts or proportions, wider classes would look more important than they are. The cumulative frequencies are shown by the ogive."
               else "All classes have the same width, so the histogram has the same shape with counts, proportions or densities on the y-axis (densities are proportional to the proportions). The cumulative frequencies are shown by the ogive."),
    .w("result", sprintf("Modal class %s; median approximately %s%s%s.", paste(labs[modal], collapse = " and "),
                         if (is.na(md_v)) "not determinable" else .f(md_v),
                         if (is.na(md_v)) "" else " (graphically: the value where the ogive reaches 0.5)",
                         if (is.na(amean)) "" else sprintf("; mean approximately %s, SD approximately %s", .f(amean), .f(sqrt(avar)))),
       qwords),
    .w("meaning", sprintf("The highest concentration of data is in %s (the class with the highest density).", paste(labs[modal], collapse = " and ")),
       freq_txt, shape_txt, peak_txt),
    .w("conclusion", split_txt),
    .w("caveat", if (C$source == "raw") "The data were grouped into intervals; the frequency distribution and the histogram depend on the chosen classes. The mean and quantiles are approximations from the classes: with raw data, the exact values come from desc_summary()."
                 else "Only the classes are known (not the raw values), so proportions within a class, the median, the quartiles and the mean are approximations assuming values are spread uniformly within each class (the mean assigns each class's frequency to its midpoint).",
       low_txt))
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
          modal_class = c(lower[modal[1]], upper[modal[1]]), values = vals, split = split_res)
}
