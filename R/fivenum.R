# ------------------------------------------------------------
# Side-by-side five-number summaries of several variables or samples,
# whatever form each one comes in: raw data, a class table (typed with
# sc_table() or a data frame with lower / upper / freq or prop), or the
# quartiles given in the question.
# ------------------------------------------------------------

.fn_stat_names <- c("min", "Q1", "median", "Q3", "max")

# One variable or sample -> list(kind, n, stats = min/Q1/median/Q3/max, mean, sd, raw values)
.fn_one <- function(x, lab) {
  is_cls <- is.data.frame(x) && ((!is.na(.kind_attr(x)) && .kind_attr(x) == "classes") || all(c("lower", "upper") %in% base::names(x)))
  if (is_cls) {
    old <- options(statcram.plot = FALSE); on.exit(options(old))
    r <- desc_classes(x)
    lo <- x$lower[1]; hi <- x$upper[nrow(x)]
    return(list(kind = "classes", n = if (is.finite(r$n)) r$n else NA,
                stats = c(min = if (is.finite(lo)) lo else NA, Q1 = unname(r$quartiles["Q1"]), median = unname(r$median),
                          Q3 = unname(r$quartiles["Q3"]), max = if (is.finite(hi)) hi else NA),
                mean = if (is.finite(r$mean)) r$mean else NA, sd = if (is.finite(r$variance)) sqrt(r$variance) else NA, v = NULL))
  }
  nm <- base::names(x)
  if (is.numeric(x) && is.null(dim(x)) && !is.null(nm) && all(nzchar(nm))) {
    key <- c(min = "min", minimum = "min", q1 = "Q1", median = "median", me = "median", q2 = "median", q3 = "Q3",
             max = "max", maximum = "max", n = "n", mean = "mean", sd = "sd")
    k <- key[tolower(nm)]
    if (anyNA(k)) stop(sprintf("%s: name the given numbers min, Q1, median (or Q2), Q3, max, n, mean, sd; got %s.",
                               lab, paste(nm[is.na(k)], collapse = ", ")), call. = FALSE)
    g <- stats::setNames(as.numeric(x), k)
    if (!all(c("Q1", "median", "Q3") %in% k)) stop(sprintf("%s: give at least Q1, median and Q3.", lab), call. = FALSE)
    st <- vapply(.fn_stat_names, function(s) if (s %in% k) g[[s]] else NA_real_, 0)
    return(list(kind = "given", n = if ("n" %in% k) g[["n"]] else NA, stats = st,
                mean = if ("mean" %in% k) g[["mean"]] else NA, sd = if ("sd" %in% k) g[["sd"]] else NA, v = NULL))
  }
  v <- .num(x, lab); v <- v[!is.na(v)]
  if (length(v) < 2) stop(lab, " needs at least 2 values.", call. = FALSE)
  q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
  list(kind = "raw", n = length(v), stats = c(min = min(v), Q1 = q[1], median = q[2], Q3 = q[3], max = max(v)),
       mean = mean(v), sd = stats::sd(v), v = v)
}

#' Compare five-number summaries side by side
#'
#' Puts the five-number summaries (min, Q1, median, Q3, max) of two or more
#' variables or samples next to each other, with range, IQR, the two halves
#' of the box, mean and SD where available, and draws side-by-side boxplots
#' on one scale. Each argument can be in a different form:
#' * raw data (a numeric vector, e.g. `Bidding$Bid`): exact quartiles, as R's
#'   `quantile()` (the same values as [desc_summary()]);
#' * a class table (made with [sc_table()], or a data frame with `lower`,
#'   `upper` and `freq` or `prop`): quartiles by interpolation within the
#'   classes, as in [desc_classes()] (approximate), min and max = the class
#'   limits;
#' * numbers given in the question: a named vector such as
#'   `c(Q1 = 10, median = 24, Q3 = 36)` (names min, Q1, median or Q2, Q3,
#'   max, n, mean, sd).
#'
#' @param ... The variables or samples (two or more); name them to set the
#'   labels, e.g. `desc_fivenum(Original = Bidding$Bid, New = Bid_N)`.
#' @param labels Optional labels, one per variable.
#' @return A result with `table` (statistics in rows, one column per variable).
#' @examples
#' desc_fivenum(Bid = c(10, 30, 36, 37, 44, 54), Bid_N = c(Q1 = 10, median = 24, Q3 = 36))
#' @export
desc_fivenum <- function(..., labels = NULL) {
  args <- list(...)
  if (length(args) < 2) stop("Give two or more variables or samples to compare.", call. = FALSE)
  ex <- as.list(substitute(list(...)))[-1]
  labs <- vapply(ex, function(e) .label(e), "")
  an <- base::names(args)
  if (!is.null(an)) labs[nzchar(an)] <- an[nzchar(an)]
  if (!is.null(labels)) { if (length(labels) != length(args)) stop("labels must have one label per variable.", call. = FALSE); labs <- labels }
  labs <- make.unique(labs)
  R <- lapply(seq_along(args), function(i) .fn_one(args[[i]], labs[i]))
  S <- vapply(R, `[[`, numeric(5), "stats")
  colnames(S) <- labs
  kind <- vapply(R, `[[`, "", "kind")
  iqr <- S["Q3", ] - S["Q1", ]
  lowh <- S["median", ] - S["Q1", ]; uph <- S["Q3", ] - S["median", ]
  tab <- data.frame(statistic = c("data", "n", "Min", "Q1", "Median", "Q3", "Max", "Range", "IQR", "Q2 - Q1", "Q3 - Q2", "Mean", "SD"),
                    check.names = FALSE, stringsAsFactors = FALSE)
  for (j in seq_along(R)) {
    r <- R[[j]]
    num <- c(r$n, S[, j], S["max", j] - S["min", j], iqr[j], lowh[j], uph[j], r$mean, r$sd)
    tab[[labs[j]]] <- c(switch(r$kind, raw = "raw data", classes = "classes (~)", given = "given"),
                        ifelse(is.na(num), "-", .f(num)))
  }
  # ---- reading ----
  ord <- order(S["median", ])
  lo <- ord[1]; hi <- ord[length(ord)]
  shape <- vapply(seq_along(R), function(j) {
    d <- uph[j] - lowh[j]
    if (abs(d) <= 0.1 * max(iqr[j], 1e-12)) sprintf("%s: the two halves of the box are similar (Q2 - Q1 = %s, Q3 - Q2 = %s), central part roughly symmetric",
                                                   labs[j], .f(lowh[j]), .f(uph[j]))
    else sprintf("%s: the %s half of the box is longer (Q2 - Q1 = %s, Q3 - Q2 = %s), %s-skewed central part", labs[j],
                 if (d > 0) "upper" else "lower", .f(lowh[j]), .f(uph[j]), if (d > 0) "right" else "left")
  }, "")
  sep <- character()
  for (i in seq_along(R)) for (j in seq_along(R)) if (i != j) {
    if (S["Q3", i] < S["Q1", j])
      sep <- c(sep, sprintf("Q3 of %s (%s) < Q1 of %s (%s): the central halves do not overlap; at least 75%% of the values of %s are below at least 75%% of those of %s.",
                            labs[i], .f(S["Q3", i]), labs[j], .f(S["Q1", j]), labs[i], labs[j]))
    else if (S["Q3", i] < S["median", j])
      sep <- c(sep, sprintf("Q3 of %s (%s) < median of %s (%s): at least 75%% of the values of %s are below a level that half of the values of %s exceed.",
                            labs[i], .f(S["Q3", i]), labs[j], .f(S["median", j]), labs[i], labs[j]))
  }
  ij <- which.max(iqr); ik <- which.min(iqr)
  var_txt <- if (iqr[ik] > 0) sprintf("%s has the widest central 50%% (IQR %s), %s the narrowest (IQR %s): %s times as wide.",
                                      labs[ij], .f(iqr[ij]), labs[ik], .f(iqr[ik]), .f(iqr[ij] / iqr[ik], 2)) else NULL
  approx <- any(kind == "classes"); given <- any(kind == "given")
  src <- vapply(R, function(r) switch(r$kind, raw = paste0("n = ", .f(r$n)), classes = "grouped data", given = "values given"), "")
  lines <- c("Five-number summaries side by side (~ = approximate, from classes; - = not available)", "",
             .table_lines(tab), "",
             "Shape of the central part (halves of the box):", paste0("  ", shape),
             if (length(sep)) c("", sep))
  wording <- c(
    .w("frame", sprintf("We compare the distributions of %s through their five-number summaries (%s).", .and_list(labs),
                        paste(sprintf("%s: %s", labs, src), collapse = "; "))),
    .w("tool", "Side-by-side boxplots on the same scale, with the quartiles, the IQR and the halves of the box, show location, variability and shape."),
    .w("result", sprintf("Medians: %s. IQR: %s.", paste(sprintf("%s %s", labs, .f(S["median", ])), collapse = ", "),
                         paste(sprintf("%s %s", labs, .f(iqr)), collapse = ", "))),
    .w("meaning",
       sprintf("Location: %s has the lowest median (%s) and %s the highest (%s), so the values of %s tend to be lower.",
               labs[lo], .f(S["median", lo]), labs[hi], .f(S["median", hi]), labs[lo]),
       if (length(sep)) paste(sep, collapse = " "),
       if (!is.null(var_txt)) paste("Variability:", var_txt),
       paste0("Shape: ", paste(shape, collapse = "; "), ".")),
    .w("caveat",
       if (approx) "Quartiles from a class table are approximate (values assumed spread evenly within each class), and its min and max are class limits, not observed values.",
       if (given) "Values marked 'given' are taken from the question as stated.",
       if (any(kind == "raw")) "Quartiles from raw data follow R's quantile() default (as in R/UBStats output)."))
  .with_plot(function() {
    stt <- S
    for (j in seq_along(R)) {
      if (R[[j]]$kind == "raw") stt[, j] <- grDevices::boxplot.stats(R[[j]]$v)$stats
      if (is.na(stt["min", j])) stt["min", j] <- stt["Q1", j]
      if (is.na(stt["max", j])) stt["max", j] <- stt["Q3", j]
    }
    outs <- lapply(R, function(r) if (r$kind == "raw") grDevices::boxplot.stats(r$v)$out else numeric(0))
    graphics::bxp(list(stats = unname(stt), n = vapply(R, function(r) if (is.na(r$n)) 1 else r$n, 0), names = labs,
                       out = unlist(outs), group = rep(seq_along(outs), lengths(outs))),
                  main = "Five-number summaries", boxfill = "grey90", las = 1)
  })
  ub <- vapply(which(kind == "raw"), function(j) sprintf("distr.summary.x(x = %s, stats = \"fivenumbers\")", paste(deparse(ex[[j]]), collapse = "")), "")
  .result("Comparing five-number summaries", lines, wording, NULL, match.call(),
          table = tab, stats = S, ubstats = if (length(ub)) ub else "UBStats functions need the raw data: with summary numbers, use the table above.")
}
