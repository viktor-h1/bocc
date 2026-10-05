# ------------------------------------------------------------
# Bivariate descriptive analysis (book chapter 4):
#   desc_crosstab()  two variables with few values (joint / conditional
#                    distributions, chi-square, Cramer's V)
#   desc_compare()   a numerical variable across groups
#   desc_cor()       two numerical variables (covariance, correlation,
#                    regression line)
# ------------------------------------------------------------

# Classify numbers into book-style intervals [a, b) (last one closed).
.classify_factor <- function(v, breaks, what) {
  v <- suppressWarnings(as.numeric(v))
  ok <- v[!is.na(v)]
  if (length(breaks) == 1) {
    K <- breaks; b <- min(ok) + (max(ok) - min(ok)) / K * (0:K); b[K + 1] <- max(ok)
  } else b <- sort(unique(breaks))
  idx <- findInterval(v, b, rightmost.closed = TRUE)
  idx[!is.na(v) & (v < b[1] | v > b[length(b)])] <- NA
  labs <- .class_labels(head(b, -1), tail(b, -1))
  factor(labs[idx], levels = labs)
}

.apply_order <- function(m, ord, margin, what) {
  if (is.null(ord)) return(m)
  have <- dimnames(m)[[margin]]
  if (!setequal(have, as.character(ord)))
    stop("order for ", what, " must list exactly its levels: ", paste(have, collapse = ", "), call. = FALSE)
  if (margin == 1) m[as.character(ord), , drop = FALSE] else m[, as.character(ord), drop = FALSE]
}

.fmt_mat <- function(m, d = .digits(), totals = FALSE) {
  if (totals) m <- rbind(cbind(m, TOTAL = rowSums(m)), TOTAL = c(colSums(m), sum(m)))
  out <- matrix(.f(m, d), nrow(m), dimnames = dimnames(m))
  paste0("  ", utils::capture.output(print(out, quote = FALSE, right = TRUE)))
}

#' @rdname describe
#' @export
desc_crosstab <- function(x, y = NULL, order_x = NULL, order_y = NULL, breaks_x = NULL, breaks_y = NULL,
                          plot = c("stacked", "beside"), y_event = NULL) {
  plot <- match.arg(plot)
  qx <- substitute(x); qy <- substitute(y); sbx <- substitute(breaks_x); sby <- substitute(breaks_y)
  xlab <- .label(qx); ylab <- .label(qy)
  notes <- NULL
  m <- .as_count_table(x)
  raw <- is.null(m)
  if (is.null(m) && is.data.frame(x) && is.null(y) && ncol(x) == 2) {
    xlab <- names(x)[1]; ylab <- names(x)[2]; y <- x[[2]]; x <- x[[1]]
    qy <- call("[[", qx, 2); qx <- call("[[", qx, 1)
  }
  if (is.null(m)) {
    if (is.null(y)) stop("Give two variables (x, y) or a count table.", call. = FALSE)
    a <- .one_column(x, xlab); b <- .one_column(y, ylab)
    if (length(a) != length(b)) stop("x and y must have the same length.", call. = FALSE)
    if (!is.null(breaks_x)) a <- .classify_factor(a, breaks_x, xlab)
    if (!is.null(breaks_y)) b <- .classify_factor(b, breaks_y, ylab)
    ok <- !is.na(a) & !is.na(b)
    if (sum(!ok)) notes <- c(notes, sprintf("%d case(s) with a missing value were removed.", sum(!ok)))
    la <- .cats(a[ok]); lb <- .cats(b[ok])
    m <- unclass(table(factor(as.character(a[ok]), levels = la), factor(as.character(b[ok]), levels = lb))) + 0
    names(dimnames(m)) <- c(xlab, ylab)
    y_ordered <- is.numeric(b) || is.factor(b) || !is.null(order_y)
  } else {
    dn <- names(dimnames(m))
    if (!is.null(dn) && nzchar(dn[1])) xlab <- dn[1] else xlab <- "X (rows)"
    if (!is.null(dn) && length(dn) > 1 && nzchar(dn[2])) ylab <- dn[2] else ylab <- "Y (columns)"
    names(dimnames(m)) <- c(xlab, ylab)
    y_ordered <- !is.null(order_y) || !anyNA(suppressWarnings(as.numeric(colnames(m))))
  }
  m <- .apply_order(m, order_x, 1, xlab); m <- .apply_order(m, order_y, 2, ylab)
  K <- nrow(m); J <- ncol(m); n <- sum(m)
  if (K < 2 || J < 2) stop("Need at least two categories for each variable.", call. = FALSE)
  R <- rowSums(m); C <- colSums(m)
  rowp <- m / R; colp <- t(t(m) / C)
  ex <- outer(R, C) / n
  chi <- sum((m - ex)^2 / ex)
  V <- sqrt(chi / (n * (min(K, J) - 1)))
  # conditional summaries of Y | X
  summ <- lapply(seq_len(K), function(k) {
    p <- rowp[k, ]; Fk <- cumsum(p)
    mode <- paste(colnames(m)[abs(p - max(p)) < 1e-12], collapse = ", ")
    out <- data.frame(n = R[k], mode = mode, `mode%` = .pct(max(p)), check.names = FALSE, stringsAsFactors = FALSE)
    if (y_ordered) {
      q <- vapply(c(0.25, 0.5, 0.75), function(pp) colnames(m)[which(Fk >= pp - 1e-9)[1]], "")
      out$Q1 <- q[1]; out$median <- q[2]; out$Q3 <- q[3]
    }
    yv <- suppressWarnings(as.numeric(colnames(m)))
    if (!anyNA(yv)) out$mean <- .f(sum(yv * p))
    out
  })
  summ <- do.call(rbind, summ); rownames(summ) <- rownames(m)
  lines <- c(
    sprintf("%s (rows, K = %d) x %s (columns, J = %d)   n = %s", xlab, K, ylab, J, .f(n)), "",
    "Joint counts f_kj (with marginal totals R_k and C_j):", .fmt_mat(m, 0, TRUE), "",
    "Joint proportions p_kj = f_kj / n:", .fmt_mat(m / n, 3, TRUE), "",
    sprintf("Conditional distributions of %s | %s (each row sums to 1): f_kj / R_k", ylab, xlab), .fmt_mat(rowp, 3), "",
    sprintf("Conditional distributions of %s | %s (each column sums to 1): f_kj / C_j", xlab, ylab), .fmt_mat(colp, 3), "",
    sprintf("Conditional summaries of %s | %s%s:", ylab, xlab,
            if (y_ordered) "" else " (no median / quartiles: give order_y = c(...) if the column variable is ordinal)"),
    .table_lines(summ, row.names = TRUE), "",
    "Association (descriptive):",
    "Expected counts under independence f*_kj = n R_k C_j = (row total x column total) / n:", .fmt_mat(ex, 2), "",
    sprintf("Chi-square = sum (f_kj - f*_kj)^2 / f*_kj = %s", .f(chi)),
    sprintf("Cramer's V = sqrt(chi-square / (n (min(K, J) - 1))) = sqrt(%s / (%s x %d)) = %s   (0 = independence, 1 = perfect association)",
            .f(chi), .f(n), min(K, J) - 1, .f(V)))
  .with_plot(function() {
    cols <- grDevices::gray.colors(J, start = 0.95, end = 0.25)
    if (plot == "stacked") {
      barplot(t(rowp), col = cols, ylab = "Proportions", xlab = xlab, main = sprintf("Bars: %s | %s", ylab, xlab),
              legend.text = colnames(m), args.legend = list(x = "topright", bty = "n", cex = 0.7, title = ylab), las = 1,
              xlim = c(0, K * 1.2 + 1.5))
    } else {
      barplot(t(m / n), beside = TRUE, col = cols, ylab = "Proportions", xlab = xlab, main = sprintf("Bars: %s, %s", xlab, ylab),
              las = 1, ylim = c(0, max(m / n) * 1.35))
      legend("topleft", legend = colnames(m), fill = cols, bty = "n", cex = 0.7, title = ylab, ncol = min(J, 3))
    }
  })
  modes_row <- vapply(seq_len(K), function(k) colnames(m)[which.max(rowp[k, ])], "")
  cond_txt <- if (K * J <= 16) paste(vapply(seq_len(K), function(k) sprintf("%s: %s", rownames(m)[k],
                 paste(sprintf("%s %s", colnames(m), .f(rowp[k, ], 3)), collapse = ", ")), ""), collapse = "; ")
  wording <- c(
    .w("frame", sprintf("We study the joint distribution of %s (rows, %d categories) and %s (columns, %d categories) on n = %s units; to compare the levels of %s we use the conditional distributions of %s | %s.",
                        xlab, K, ylab, J, .f(n), xlab, ylab, xlab)),
    .w("tool", sprintf("Conditional (row) percentages and the stacked bar chart of %s | %s, because the groups of %s have different sizes: joint counts or joint percentages would mix the size of a group with the behaviour within it.",
                       ylab, xlab, xlab)),
    .w("result", if (!is.null(cond_txt)) sprintf("Conditional distributions of %s | %s - %s.", ylab, xlab, cond_txt),
       sprintf("Chi-square = %s, Cramer's V = %s.", .f(chi), .f(V))),
    .w("meaning", sprintf("The most frequent category of %s is %s.", ylab,
                          paste(sprintf("%s for %s = %s", modes_row, xlab, rownames(m)), collapse = ", ")),
       sprintf("If %s and %s were independent, the conditional distributions of %s would be the same for every level of %s (and equal to the marginal distribution), i.e. p_kj = R_k C_j for every pair.", xlab, ylab, ylab, xlab)),
    .w("conclusion", if (V < 0.1) sprintf("The conditional distributions are very similar (Cramer's V = %s, close to 0): there is little or no association between %s and %s.", .f(V), xlab, ylab)
                     else sprintf("The conditional distributions differ (Cramer's V = %s, on a scale from 0 = independence to 1 = perfect association): %s and %s are associated.", .f(V), xlab, ylab)),
    .w("caveat", "Cramer's V is a relative measure, not inflated by the sample size or the table dimensions. Comparisons of conditional distributions are only meaningful if the groups are homogeneous with respect to other (confounding) factors: aggregated data can hide or reverse relations within subgroups (Simpson's paradox)."))
  if (any(ex < 5)) notes <- c(notes, "Some expected counts are below 5 (relevant for the chi-square TEST in chisq_indep()).")
  ub <- if (!raw) .ub_raw_note else {
    xe <- .ub_ordered(qx, order_x); ye <- .ub_ordered(qy, order_y)
    ab <- list(breaks.x = if (!is.null(breaks_x)) sbx, breaks.y = if (!is.null(breaks_y)) sby)
    c(do.call(.ub_call, c(list("distr.table.xy", x = xe, y = ye, freq = c("counts", "proportions"),
                               freq.type = c("joint", "y|x", "x|y")), ab), quote = TRUE),
      do.call(.ub_call, c(list("distr.plot.xy", x = xe, y = ye, plot.type = "bars", freq = "proportions",
                               freq.type = if (plot == "stacked") "y|x" else "joint",
                               bar.type = if (plot == "beside") "beside"), ab), quote = TRUE))
  }
  share <- NULL
  if (length(y_event)) {
    y_event <- as.character(y_event)
    bad <- setdiff(y_event, colnames(m))
    if (length(bad)) stop("y_event not found among the categories of ", ylab, ": ", paste(bad, collapse = ", "),
                          ". Categories: ", paste(colnames(m), collapse = ", "), call. = FALSE)
    ev <- paste(y_event, collapse = " or ")
    share <- rowSums(rowp[, y_event, drop = FALSE]); marg <- sum(C[y_event]) / n
    sl <- vapply(seq_len(K), function(k) sprintf("  Freq(%s = %s | %s = %s) = %s%s   (%s of %s)",
      ylab, ev, xlab, rownames(m)[k], if (length(y_event) > 1) paste0(paste(.f(rowp[k, y_event], 3), collapse = " + "), " = ") else "",
      .f(share[k], 3), .f(sum(m[k, y_event])), .f(R[k])), character(1))
    lines <- c(lines, "", sprintf("Share of %s = %s within each level of %s (conditional on %s):", ylab, ev, xlab, xlab), sl,
               sprintf("  Marginal (all units): Freq(%s = %s) = %s / %s = %s", ylab, ev, .f(sum(C[y_event])), .f(n), .f(marg, 3)))
    hi <- rownames(m)[which.max(share)]; lo <- rownames(m)[which.min(share)]
    wording <- c(.w("frame", sprintf("We are interested in the percentage of units with %s = %s conditional on %s.", ylab, ev, xlab)),
                 .w("result", paste(sprintf("Among the units with %s = %s, the share with %s = %s is %s (%s/%s).", xlab, rownames(m), ylab, ev,
                                            .pct(share, 1), .f(rowSums(m[, y_event, drop = FALSE])), .f(R)), collapse = " ")),
                 .w("conclusion", sprintf("Among the units with %s = %s, the share with %s = %s is %s; among those with %s = %s it is %s (conditional relative frequencies: %s). It would be wrong to answer based on joint counts or joint percentages, because the groups have different sizes.",
                         xlab, hi, ylab, ev, .pct(max(share), 1), xlab, lo, .pct(min(share), 1),
                         paste(sprintf("%s %s", rownames(m), .f(share, 3)), collapse = ", ")),
                    sprintf("If %s and %s were independent, all these conditional shares would be equal to the marginal share %s; here they %s.",
                         xlab, ylab, .f(marg, 3), if (max(share) - min(share) < 0.05) "are close to it, consistent with weak or no association"
                         else "differ markedly, so the two variables appear to be associated")),
                 wording)
    names(share) <- rownames(m)
  }
  .result("Joint and conditional distributions", lines, wording, notes, match.call(), event_share = share,
          counts = m, joint = m / n, row_cond = rowp, col_cond = colp, expected = ex,
          chisq = chi, cramer_v = V, cond_summary = summ, ubstats = ub)
}

#' @rdname describe
#' @export
desc_compare <- function(x, group, group2 = NULL, probs = c(0.05, 0.10, 0.90, 0.95), plot = c("boxplot", "hist"),
                         value = NULL) {
  plot <- match.arg(plot)
  qx <- substitute(x); qg <- substitute(group); qg2 <- substitute(group2)
  xlab <- .label(qx); glab <- .label(qg)
  xx <- .one_column(x, xlab); g <- .one_column(group, glab)
  if (length(xx) != length(g)) stop("x and group must have the same length.", call. = FALSE)
  if (!is.null(group2)) {
    g2lab <- .label(substitute(group2)); g2 <- .one_column(group2, g2lab)
    if (length(g2) != length(g)) stop("group2 must have the same length as group.", call. = FALSE)
    lv <- as.vector(outer(.cats(g), .cats(g2), paste, sep = " / "))
    g <- ifelse(is.na(g) | is.na(g2), NA, paste(g, g2, sep = " / "))
    glab <- paste(glab, "/", g2lab)
    lv <- lv[lv %in% g]
  } else lv <- .cats(g[!is.na(g)])
  xx <- suppressWarnings(as.numeric(if (is.factor(xx)) as.character(xx) else xx))
  gnum <- as.character(g)
  rows <- lapply(c(lv, "(all)"), function(z) {
    sel <- if (z == "(all)") !is.na(gnum) else !is.na(gnum) & gnum == z
    a0 <- xx[sel]; a <- a0[!is.na(a0)]
    q <- if (length(a)) quantile(a, c(.25, .5, .75), names = FALSE) else rep(NA, 3)
    pr <- if (length(a)) quantile(a, probs, names = FALSE) else rep(NA, length(probs))
    out <- data.frame(group = z, n = length(a), n.a = sum(is.na(a0)), min = if (length(a)) min(a) else NA, Q1 = q[1],
                      median = q[2], mean = if (length(a)) mean(a) else NA, Q3 = q[3], max = if (length(a)) max(a) else NA,
                      sd = if (length(a) > 1) sd(a) else NA, check.names = FALSE)
    out$CV <- out$sd / abs(out$mean)
    for (i in seq_along(probs)) out[[paste0("p", round(100 * probs[i]))]] <- pr[i]
    out
  })
  tab <- do.call(rbind, rows)
  grp <- tab[tab$group != "(all)" & tab$n > 0, , drop = FALSE]
  .with_plot(function() {
    keep <- !is.na(gnum) & !is.na(xx)
    if (plot == "boxplot") {
      boxplot(xx[keep] ~ factor(gnum[keep], levels = lv), xlab = glab, ylab = xlab,
              main = sprintf("%s | %s", xlab, glab), col = "grey90", las = 1)
      points(seq_along(lv), tab$mean[match(lv, tab$group)], pch = 18, col = "firebrick", cex = 1.3)
    } else {
      br <- pretty(range(xx[keep]), 12)
      k <- length(lv); op <- par(mfrow = c(ceiling(k / min(k, 3)), min(k, 3))); on.exit(par(op))
      for (z in lv) hist(xx[keep & gnum == z], breaks = br, freq = FALSE, main = z, xlab = xlab, col = "grey85", border = "white")
    }
  })
  lines <- c(sprintf("Conditional distributions of %s | %s", xlab, glab), "",
             .table_lines(.round_df(tab)))
  # shape of each conditional distribution, from the boxplot quantities
  SHs <- lapply(seq_len(nrow(grp)), function(i) {
    a <- xx[!is.na(gnum) & gnum == grp$group[i] & !is.na(xx)]
    if (length(a) < 4) return(NULL)
    .shape_info(a, grp$Q1[i], grp$median[i], grp$Q3[i])
  })
  shapes <- vapply(seq_len(nrow(grp)), function(i) {
    if (is.null(SHs[[i]])) return(sprintf("%s: too few values", grp$group[i]))
    sprintf("%s: %s (%s)", grp$group[i], SHs[[i]]$phrase, SHs[[i]]$evidence)
  }, character(1))
  # non-overlapping central halves: Q3 of one group below Q1 of another
  sep <- character()
  for (i in seq_len(nrow(grp))) for (j in seq_len(nrow(grp))) if (i != j && grp$Q3[i] < grp$Q1[j])
    sep <- c(sep, sprintf("Q3 of %s (%s) < Q1 of %s (%s): at least 75%% of the values for %s are lower than at least 75%% of those for %s.",
                          grp$group[i], .f(grp$Q3[i]), grp$group[j], .f(grp$Q1[j]), grp$group[i], grp$group[j]))
  iqr <- grp$Q3 - grp$Q1; miqr <- mean(iqr, na.rm = TRUE)
  ilo <- which.min(grp$median); ihi <- which.max(grp$median)
  lo <- grp$group[ilo]; hi <- grp$group[ihi]
  differ <- c(location = diff(range(grp$median)) >= 0.25 * miqr,
              variability = max(iqr) >= 1.25 * min(iqr),
              shape = length(unique(vapply(SHs, function(z) if (is.null(z)) "" else z$core, ""))) > 1)
  open_txt <- if (all(differ)) sprintf("The distributions of %s | %s differ in location, variability and shape.", xlab, glab)
              else if (!any(differ)) sprintf("The distributions of %s | %s are similar in location, variability and shape.", xlab, glab)
              else sprintf("The distributions of %s | %s differ in %s, while they are similar in %s.", xlab, glab,
                           paste(names(differ)[differ], collapse = " and "), paste(names(differ)[!differ], collapse = " and "))
  lowest_all <- ilo == which.min(grp$Q1) && ilo == which.min(grp$Q3)
  highest_all <- ihi == which.max(grp$Q1) && ihi == which.max(grp$Q3)
  loc_txt <- sprintf("%s has the %s (median %s%s): its values tend to be lower; %s has the %s (median %s).",
                     lo, if (lowest_all) "lowest measures of location" else "lowest median", .f(grp$median[ilo]),
                     if (lowest_all) sprintf(", Q1 %s, Q3 %s", .f(grp$Q1[ilo]), .f(grp$Q3[ilo])) else "",
                     hi, if (highest_all) "highest measures of location" else "highest median", .f(grp$median[ihi]))
  iq_lo <- which.min(iqr); iq_hi <- which.max(iqr)
  var_txt <- sprintf("%s has the smallest IQR (%s): its central 50%% is more concentrated than for the other groups; %s has the largest (%s).",
                     grp$group[iq_lo], .f(iqr[iq_lo]), grp$group[iq_hi], .f(iqr[iq_hi]))
  med_txt <- sprintf("The median of %s (%s) is the maximum value reached by the lowest 50%% of its values, lower than in the other groups.",
                     lo, .f(grp$median[ilo]))
  # which centre measures (mean vs median, group by group)
  close <- abs(grp$mean - grp$median) <= 0.2 * pmax(grp$sd, 1e-12)
  sim_txt <- if (any(close)) sprintf("For %s mean and median offer a similar description of the centre (%s).",
                                     .and_list(grp$group[close]),
                                     paste(sprintf("%s vs %s", .f(grp$mean[close], 2), .f(grp$median[close], 2)), collapse = "; "))
  tail_txt <- if (any(!close)) paste(vapply(which(!close), function(i)
    sprintf("For %s the mean (%s) is %s the median (%s): it is attracted by the long %s tail.", grp$group[i], .f(grp$mean[i], 2),
            if (grp$mean[i] < grp$median[i]) "below" else "above", .f(grp$median[i], 2), if (grp$mean[i] < grp$median[i]) "left (lower)" else "right (upper)"),
    character(1)), collapse = " ")
  pair_txt <- NULL
  for (i in seq_len(nrow(grp))) for (j in seq_len(nrow(grp))) if (i < j && is.null(pair_txt) &&
      abs(grp$median[i] - grp$median[j]) <= 0.1 * miqr && abs(grp$mean[i] - grp$mean[j]) > 0.1 * miqr && (!close[i] || !close[j]))
    pair_txt <- sprintf("The medians of %s and %s (%s and %s) are aligned and do not reflect the long tail of %s, which is captured by comparing the means (%s and %s).",
                        grp$group[i], grp$group[j], .f(grp$median[i], 2), .f(grp$median[j], 2), if (!close[i]) grp$group[i] else grp$group[j],
                        .f(grp$mean[i], 2), .f(grp$mean[j], 2))
  verdict <- if (any(!close)) "To summarise the centre of these distributions report both the means and the medians."
             else "Mean and median agree in every group, so either summarises the centre (the median is robust to outliers, the mean uses all the values)."
  ub <- c(.ub_call("distr.summary.x", x = qx, stats = c("central", "fivenumbers", "dispersion", .ub_pcts(probs)),
                    by1 = qg, by2 = if (!is.null(group2)) qg2),
          if (is.null(group2)) .ub_call("distr.plot.xy", x = qg, y = qx, plot.type = "boxplot"))
  lines <- c(lines, "", "Shape of each conditional distribution (boxplot: box halves, whiskers, outliers):", paste0("  ", shapes),
             if (length(sep)) c("", sep))
  wording <- c(
    .w("frame", sprintf("We compare the conditional distributions of %s | %s in %d groups (%s).", xlab, glab, nrow(grp),
                        paste(sprintf("%s: n = %s", grp$group, grp$n), collapse = ", "))),
    .w("tool", "Side-by-side boxplots (one per group, on the same scale) show location, variability and shape at a glance, including the outliers; the conditional summaries give the numbers."),
    .w("result", sprintf("Medians: %s. Means: %s. IQR: %s.", paste(sprintf("%s %s", grp$group, .f(grp$median)), collapse = ", "),
                         paste(sprintf("%s %s", grp$group, .f(grp$mean, 2)), collapse = ", "),
                         paste(sprintf("%s %s", grp$group, .f(iqr)), collapse = ", "))),
    .w("meaning", open_txt,
       sprintf("Shape - %s.", paste(vapply(seq_len(nrow(grp)), function(i) if (is.null(SHs[[i]])) "" else
         sprintf("%s: %s", grp$group[i], SHs[[i]]$phrase), ""), collapse = "; ")),
       loc_txt, var_txt, med_txt, if (length(sep)) paste(sep, collapse = " ")),
    .w("conclusion", paste(c(verdict, sim_txt, tail_txt, pair_txt), collapse = " ")),
    .w("caveat", "With a numerical variable the conditional distributions always differ somewhat: what matters is whether they differ substantially in location, variability or shape; if they do, the numerical variable and the grouping variable are associated."))
  if (length(value)) {
    vl <- unlist(lapply(seq_len(nrow(grp)), function(i) {
      fe <- .fences(grp$Q1[i], grp$Q3[i])
      vapply(value, function(v) sprintf("  %s = %s within %s: fences [Q1 - 1.5 IQR, Q3 + 1.5 IQR] = [%s - 1.5 x %s, %s + 1.5 x %s] = [%s, %s] -> %s",
                                        xlab, .f(v), grp$group[i], .f(grp$Q1[i]), .f(grp$Q3[i] - grp$Q1[i]), .f(grp$Q3[i]),
                                        .f(grp$Q3[i] - grp$Q1[i]), .f(fe[1]), .f(fe[2]),
                                        if (v < fe[1]) "extreme LOW" else if (v > fe[2]) "extreme HIGH" else "not extreme"), character(1))
    }))
    lines <- c(lines, "", "Is the value extreme within each group?", vl)
    vw <- unlist(lapply(seq_len(nrow(grp)), function(i) {
      fe <- .fences(grp$Q1[i], grp$Q3[i])
      vapply(value, function(v) {
        if (v < fe[1]) sprintf("Within %s an extremely low value is below Q1 - 1.5 IQR = %s - 1.5 x %s = %s: since %s < %s, %s is extremely low.",
                               grp$group[i], .f(grp$Q1[i]), .f(iqr[i]), .f(fe[1]), .f(v), .f(fe[1]), .f(v))
        else if (v > fe[2]) sprintf("Within %s an extremely high value is above Q3 + 1.5 IQR = %s + 1.5 x %s = %s: since %s > %s, %s is extremely high.",
                                    grp$group[i], .f(grp$Q3[i]), .f(iqr[i]), .f(fe[2]), .f(v), .f(fe[2]), .f(v))
        else sprintf("Within %s an extremely low value is below Q1 - 1.5 IQR = %s - 1.5 x %s = %s (extremely high above %s): %s is not extreme.",
                     grp$group[i], .f(grp$Q1[i]), .f(iqr[i]), .f(fe[1]), .f(fe[2]), .f(v))
      }, character(1))
    }))
    wording <- c(wording, .w("conclusion", vw))
  }
  .result("Comparing a numerical variable across groups", lines, wording,
          if (any(tab$n.a > 0)) "n.a = missing values of the numerical variable within each group (excluded).", match.call(),
          table = tab, ubstats = ub)
}

#' @rdname describe
#' @export
desc_cor <- function(x, y = NULL, line = TRUE, color = NULL, population = FALSE) {
  qx <- substitute(x); qy <- substitute(y); qc <- substitute(color)
  xlab <- .label(qx); ylab <- .label(qy); clab <- .label(qc)
  div <- function(n) if (population) n else n - 1
  # joint frequency table of two discrete numerical variables (numeric row / column labels)
  m <- if (is.null(y)) .as_count_table(x) else NULL
  joint <- !is.null(m) && !anyNA(suppressWarnings(as.numeric(rownames(m)))) &&
    !anyNA(suppressWarnings(as.numeric(colnames(m))))
  # matrix of several numerical variables
  if (!joint && is.null(y) && (is.data.frame(x) || is.matrix(x))) {
    d <- as.data.frame(x)
    d <- d[vapply(d, is.numeric, logical(1))]
    if (ncol(d) < 2) stop("Give two numerical variables (x, y) or a data frame with at least two numerical columns.", call. = FALSE)
    cc <- stats::complete.cases(d); d <- d[cc, , drop = FALSE]
    cv <- stats::cov(d) * (nrow(d) - 1) / div(nrow(d)); cr <- stats::cor(d)
    .with_plot(function() graphics::pairs(d, pch = 19, cex = 0.5, col = "grey30"))
    return(.result("Covariance and correlation matrices",
                   c(sprintf("n = %d complete cases%s", nrow(d), if (sum(!cc)) sprintf(" (%d with missing values removed)", sum(!cc)) else ""), "",
                     sprintf("Covariances (divisor %s):", if (population) "N" else "n - 1"), .fmt_mat(cv), "",
                     "Pearson correlations r:", .fmt_mat(cr, 3)),
                   "Covariances depend on the units and the dispersion of the variables, so they show only the DIRECTION of the linear relations; correlations are unit-free (between -1 and 1) and allow comparing their STRENGTH.",
                   NULL, match.call(), covariance = cv, correlation = cr))
  }
  # covariance from the joint frequency distribution (Appendix 4.1)
  if (joint) {
    xv <- as.numeric(rownames(m)); yv <- as.numeric(colnames(m))
    n <- sum(m); p <- m / n
    props <- abs(n - 1) < 1e-6
    mx <- sum(xv * rowSums(p)); my <- sum(yv * colSums(p))
    exy <- sum(outer(xv, yv) * p)
    fac <- if (props || population) 1 else n / (n - 1)
    sxy <- fac * (exy - mx * my)
    sx <- sqrt(fac * (sum(xv^2 * rowSums(p)) - mx^2)); sy <- sqrt(fac * (sum(yv^2 * colSums(p)) - my^2))
    r <- sxy / (sx * sy)
    lines <- c("Covariance from the joint frequency distribution (grouped data):",
               sprintf("xbar = sum(x*_k R_k) = %s   ybar = sum(y*_j C_j) = %s   sum sum x*_k y*_j p_kj = %s", .f(mx), .f(my), .f(exy)),
               sprintf("s_XY = %s [sum sum x*_k y*_j p_kj - xbar ybar] = %s x (%s - %s x %s) = %s",
                       if (fac == 1) "" else sprintf("n/(n-1) = %s/%s x", .f(n), .f(n - 1)), .f(fac), .f(exy), .f(mx), .f(my), .f(sxy)),
               sprintf("s_X = %s   s_Y = %s   r_XY = s_XY / (s_X s_Y) = %s", .f(sx), .f(sy), .f(r)))
    return(.result("Covariance and correlation (joint frequency table)", lines, NULL,
                   if (props && !population) "Proportions given: n unknown, so the factor n/(n-1) is ignored." else NULL,
                   match.call(), covariance = sxy, correlation = r, mean_x = mx, mean_y = my))
  }
  if (is.null(y)) stop("Give two numerical variables: desc_cor(x, y).", call. = FALSE)
  a <- suppressWarnings(as.numeric(.one_column(x, xlab))); b <- suppressWarnings(as.numeric(.one_column(y, ylab)))
  if (length(a) != length(b)) stop("x and y must have the same length (one pair of values per case).", call. = FALSE)
  cvar <- if (!is.null(color)) .one_column(color, "color") else NULL
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]; if (!is.null(cvar)) cvar <- cvar[ok]
  n <- length(a)
  if (n < 3) stop("At least three complete pairs are needed.", call. = FALSE)
  mx <- mean(a); my <- mean(b)
  sxy <- sum((a - mx) * (b - my)) / div(n)
  sx <- sqrt(sum((a - mx)^2) / div(n)); sy <- sqrt(sum((b - my)^2) / div(n))
  r <- sxy / (sx * sy)
  b1 <- sxy / sx^2; b0 <- my - b1 * mx
  conc <- sum((a - mx) * (b - my) > 0); disc <- sum((a - mx) * (b - my) < 0)
  dname <- if (population) "N" else "n - 1"
  lines <- c(
    sprintf("X = %s (horizontal)   Y = %s (vertical)   n = %d pairs", xlab, ylab, n), "",
    sprintf("Means: xbar = %s   ybar = %s;   SDs: s_X = %s   s_Y = %s", .f(mx), .f(my), .f(sx), .f(sy)),
    sprintf("Concordant pairs (both above or both below the means): %d   discordant: %d", conc, disc), "",
    sprintf("Covariance  s_XY = sum (x_i - xbar)(y_i - ybar) / (%s) = %s / %s = %s", dname, .f(sum((a - mx) * (b - my))), .f(div(n)), .f(sxy)),
    sprintf("   short-cut: [sum x_i y_i - n xbar ybar] / (%s) = [%s - %d x %s x %s] / %s = %s",
            dname, .f(sum(a * b)), n, .f(mx), .f(my), .f(div(n)), .f((sum(a * b) - n * mx * my) / div(n))),
    sprintf("Correlation r_XY = s_XY / (s_X s_Y) = %s / (%s x %s) = %s", .f(sxy), .f(sx), .f(sy), .f(r)), "",
    sprintf("Regression line of %s on %s (least squares):", ylab, xlab),
    sprintf("   b1 = s_XY / s_X^2 = %s / %s = %s   (= r_XY s_Y / s_X)", .f(sxy), .f(sx^2), .f(b1)),
    sprintf("   b0 = ybar - b1 xbar = %s - %s x %s = %s", .f(my), .f(b1), .f(mx), .f(b0)),
    sprintf("   %s-hat = %s %s %s x %s", ylab, .f(b0), if (b1 < 0) "-" else "+", .f(abs(b1)), xlab))
  .with_plot(function() {
    colv <- "grey30"; lev <- NULL
    if (!is.null(cvar)) {
      f <- if (is.numeric(cvar) && length(unique(cvar)) > 8) cut(cvar, 5) else factor(cvar)
      lev <- levels(f); pal <- grDevices::hcl.colors(length(lev), "Dark 3"); colv <- pal[as.integer(f)]
    }
    plot(a, b, pch = 19, cex = 0.7, col = colv, xlab = xlab, ylab = ylab, main = sprintf("Scatter: %s, %s", xlab, ylab), las = 1)
    abline(v = mx, h = my, lty = 3, col = "grey50")
    if (line) abline(b0, b1, col = "firebrick", lwd = 2)
    if (!is.null(lev)) legend("topleft", legend = lev, col = pal, pch = 19, bty = "n", cex = 0.75, title = clab)
  })
  # linearity check: does a curve fit clearly better than the straight line?
  nonlin <- FALSE; lin_txt <- NULL
  if (n >= 10 && length(unique(a)) >= 4) {
    m1 <- stats::lm(b ~ a); m2 <- stats::lm(b ~ a + I(a^2))
    r2l <- summary(m1)$r.squared; r2q <- summary(m2)$r.squared; pq <- stats::coef(summary(m2))[3, 4]
    rk <- rank(a, ties.method = "first") / n
    g3 <- factor(ifelse(rk <= 0.2, "low", ifelse(rk > 0.8, "high", "middle")), levels = c("low", "middle", "high"))
    rm <- tapply(stats::resid(m1), g3, mean)
    nonlin <- (r2q - r2l) >= 0.05 && pq < 0.01
    lines <- c(lines, "",
               sprintf("Linearity check: R^2 of the straight line = %s; with a curve (quadratic term) = %s (p-value of the curvature %s)",
                       .f(r2l), .f(r2q), .fp(pq)),
               sprintf("   mean residual (observed - line) for the lowest 20%% / middle 60%% / highest 20%% of %s: %s",
                       xlab, paste(.f(rm, 3), collapse = " / ")),
               sprintf("   -> %s", if (nonlin) "the relationship is NOT linear: the line misses the data at the ends" else "no clear departure from linearity"))
    if (nonlin) {
      ends <- sign(rm[c(1, 3)]); mid <- sign(rm[2])
      lin_txt <- if (ends[1] == ends[2] && mid != ends[1])
        sprintf("The relationship is not linear: the fitted straight line lies %s the observed values at very low and very high %s and %s them in the middle, so it is a compromise that describes the centre of the data but not the tails.",
                if (ends[1] < 0) "above" else "below", xlab, if (ends[1] < 0) "below" else "above")
      else sprintf("The relationship is not linear: a curve describes the data clearly better than the straight line (R^2 %s vs %s), so the line describes only part of the data.",
                   .f(r2q), .f(r2l))
    }
  }
  ar <- abs(r)
  size <- if (ar >= 0.7) "high in absolute value" else if (ar >= 0.4) "moderate" else "weak"
  dir_txt <- if (r < 0) sprintf("The two variables are linked by an inverse relationship: as %s increases, %s tends to decrease (covariance %s < 0).", xlab, ylab, .f(sxy))
             else if (r > 0) sprintf("The two variables are linked by a direct relationship: as %s increases, %s tends to increase (covariance %s > 0).", xlab, ylab, .f(sxy))
             else "There is no linear relationship (covariance 0)."
  wording <- c(
    .w("frame", sprintf("We study the relationship between two numerical variables, %s (X) and %s (Y), observed on n = %d units.", xlab, ylab, n)),
    .w("tool", "The fundamental tool is the scatterplot: it shows the direction and the form of the relationship and highlights departures from linearity and outliers, so it tells whether the correlation coefficient is a reliable measure of the strength of the relationship. The covariance gives the direction; the correlation coefficient r (unit-free, between -1 and 1) measures the strength of the LINEAR relationship."),
    .w("result", sprintf("s_XY = %s, r = %s; least-squares line %s-hat = %s %s %s x %s.", .f(sxy), .f(r), ylab, .f(b0), if (b1 < 0) "-" else "+", .f(abs(b1)), xlab)),
    .w("meaning", dir_txt, lin_txt,
       sprintf("On average %s changes by %s for a one-unit increase in %s (the slope does not measure the strength: b1 = r s_Y / s_X).", ylab, .f(b1), xlab)),
    .w("conclusion", if (nonlin) sprintf("The correlation coefficient, %s (r = %s), is not particularly reliable as a measure of the strength, since it does not reflect a general tendency of the data to cluster around a single straight line.", size, .f(r))
                     else sprintf("The points cluster around a straight line, so r = %s (%s) is a reliable measure of the strength of the linear relationship.", .f(r), size)),
    .w("caveat", "Outliers can inflate or deflate r; correlation does not imply causation (confounding factors, reverse causality)."))
  ub <- if (is.null(y)) .ub_raw_note
        else c(.ub_call("distr.plot.xy", x = qx, y = qy, plot.type = "scatter", fitline = if (line) TRUE, var.c = if (!is.null(color)) qc),
               sprintf("cov(%s, %s, use = \"complete.obs\"); cor(%s, %s, use = \"complete.obs\")   # base R",
                       .call_text(qx), .call_text(qy), .call_text(qx), .call_text(qy)))
  .result("Covariance, correlation and regression line", lines, wording,
          if (sum(!ok)) sprintf("%d case(s) with a missing value were removed (complete pairs only).", sum(!ok)), match.call(),
          covariance = sxy, correlation = r, intercept = b0, slope = b1, mean_x = mx, mean_y = my, sd_x = sx, sd_y = sy, n = n,
          ubstats = ub)
}
