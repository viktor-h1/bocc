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
                          plot = c("stacked", "beside")) {
  plot <- match.arg(plot)
  xlab <- .label(substitute(x)); ylab <- .label(substitute(y))
  notes <- NULL
  m <- .as_count_table(x)
  if (is.null(m) && is.data.frame(x) && is.null(y) && ncol(x) == 2) {
    xlab <- names(x)[1]; ylab <- names(x)[2]; y <- x[[2]]; x <- x[[1]]
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
  wording <- c(
    sprintf("Two variables are statistically independent when the conditional distributions of %s are the same for every level of %s (and equal to its marginal distribution), i.e. when p_kj = R_k C_j for every pair.", ylab, xlab),
    sprintf("The more the conditional distributions differ, the stronger the association. Here the chi-square statistic is %s and Cramer's V = %s (a relative measure between 0 and 1, not inflated by the sample size or the table dimensions).", .f(chi), .f(V)),
    "Comparisons of conditional distributions are only meaningful if the groups are homogeneous with respect to other (confounding) factors: aggregated data can hide or reverse relations within subgroups (Simpson's paradox).")
  if (any(ex < 5)) notes <- c(notes, "Some expected counts are below 5 (relevant for the chi-square TEST in chisq_indep()).")
  .result("Joint and conditional distributions", lines, wording, notes, match.call(),
          counts = m, joint = m / n, row_cond = rowp, col_cond = colp, expected = ex,
          chisq = chi, cramer_v = V, cond_summary = summ)
}

#' @rdname describe
#' @export
desc_compare <- function(x, group, group2 = NULL, probs = c(0.10, 0.90), plot = c("boxplot", "hist")) {
  plot <- match.arg(plot)
  xlab <- .label(substitute(x)); glab <- .label(substitute(group))
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
  hi <- grp$group[which.max(grp$median)]; lo <- grp$group[which.min(grp$median)]
  wording <- c(
    sprintf("Centre: the median of %s is highest for %s (%s) and lowest for %s (%s); compare also the means (in red on the boxplots), which are pulled toward long tails.",
            xlab, hi, .f(max(grp$median)), lo, .f(min(grp$median))),
    sprintf("Spread: the IQR (box width) ranges from %s to %s and the standard deviation from %s to %s; check the whiskers, the extreme values and the upper / lower percentiles to compare the tails.",
            .f(min(grp$Q3 - grp$Q1)), .f(max(grp$Q3 - grp$Q1)), .f(min(grp$sd, na.rm = TRUE)), .f(max(grp$sd, na.rm = TRUE))),
    "With a numerical variable the conditional distributions always differ somewhat; the question is whether they differ substantially in location, dispersion or shape (side-by-side boxplots plus summaries), not whether the variables are exactly independent.")
  .result("Comparing a numerical variable across groups", lines, wording,
          if (any(tab$n.a > 0)) "n.a = missing values of the numerical variable within each group (excluded).", match.call(),
          table = tab)
}

#' @rdname describe
#' @export
desc_cor <- function(x, y = NULL, line = TRUE, color = NULL, population = FALSE) {
  xlab <- .label(substitute(x)); ylab <- .label(substitute(y)); clab <- .label(substitute(color))
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
  direction <- if (r > 0) "positive (direct): the two variables tend to increase together" else if (r < 0) "negative (inverse): when one increases the other tends to decrease" else "absent"
  wording <- c(
    sprintf("The covariance (%s) shows the DIRECTION of the linear relation, which is %s. Its size depends on the units and dispersion of the variables, so it does not measure the strength.", .f(sxy), direction),
    sprintf("The correlation coefficient r = %s (between -1 and 1, unit-free) measures the STRENGTH of the LINEAR relation: the closer |r| is to 1, the more the points cluster around a straight line; r = 0 means no linear relation.", .f(r)),
    sprintf("The regression line %s-hat = %s %s %s x %s gives, on average, a change of %s in %s for a one-unit increase in %s; the slope does not measure the strength of the relation (b1 = r s_Y / s_X).",
            ylab, .f(b0), if (b1 < 0) "-" else "+", .f(abs(b1)), xlab, .f(b1), ylab, xlab),
    "Always check the scatterplot: outliers can inflate or deflate r, a low r does not exclude a strong NON-linear relation, and correlation does not imply causation (confounding factors, reverse causality, ecological data).")
  .result("Covariance, correlation and regression line", lines, wording,
          if (sum(!ok)) sprintf("%d case(s) with a missing value were removed (complete pairs only).", sum(!ok)), match.call(),
          covariance = sxy, correlation = r, intercept = b0, slope = b1, mean_x = mx, mean_y = my, sd_x = sx, sd_y = sy, n = n)
}
