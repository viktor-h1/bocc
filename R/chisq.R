# ------------------------------------------------------------
# Chi-square goodness-of-fit and independence tests.
# Accept raw categorical vectors, typed count tables (matrix / table),
# or tables entered with sc_table().
# ------------------------------------------------------------

#' Chi-square tests
#'
#' * `chisq_gof()`   goodness of fit: does one categorical variable follow
#'   the stated category probabilities? (equal shares if `p` is omitted)
#' * `chisq_indep()` independence: are two categorical variables associated?
#'
#' `x` can be raw categorical data (`df$channel`), named counts
#' (`c(A = 151, B = 117, C = 140)`), a typed matrix / `table`, or a table
#' entered with [sc_table()]. In `chisq_gof()` a short unnamed numeric vector
#' (up to 30 values) is read as typed counts; longer ones as raw values.
#'
#' @param x Data or table (see above).
#' @param y Second categorical variable (independence test with raw data).
#' @param p Probabilities under H0, in the order of the categories (or
#'   named). Percentages are accepted.
#' @param alpha Significance level.
#' @param estimated Number of parameters estimated from the data (reduces
#'   the degrees of freedom; default 0).
#' @return An `sc_result` with `statistic`, `df`, `p_value`, `expected`.
#' @examples
#' chisq_gof(c(A = 151, B = 117, C = 140, D = 162))
#' m <- matrix(c(20, 30, 25, 25), nrow = 2,
#'             dimnames = list(gender = c("F", "M"), buy = c("yes", "no")))
#' chisq_indep(m)
#' @name chisq
NULL

#' @rdname chisq
#' @export
chisq_gof <- function(x, p = NULL, alpha = 0.05, estimated = 0) {
  alpha <- .prob(alpha, "alpha")
  sx <- substitute(x); sp <- substitute(p)
  xlab <- if (is.call(sx) && identical(sx[[1]], as.name("c"))) "the variable" else .label(sx)
  obs <- .as_freq(x)
  how <- NULL; raw <- FALSE
  if (is.null(obs) && is.numeric(x) && is.null(dim(x)) && length(x) <= 30 && !anyNA(x) && all(x >= 0)) {
    obs <- stats::setNames(as.numeric(x), paste0("cat", seq_along(x)))
    how <- "x was read as typed counts, one per category. For raw values pass factor(x) instead."
  }
  if (is.null(obs)) {
    raw <- TRUE
    if (is.numeric(x)) how <- "x was read as raw values and each distinct value was counted. To give counts, use named counts c(A = 10, B = 20) or a table."
    v <- .one_column(x, xlab); v <- v[!is.na(v)]
    lv <- .cats(v)
    obs <- stats::setNames(as.numeric(table(factor(as.character(v), levels = lv))), lv)
  }
  k <- length(obs)
  uniform <- is.null(p)
  if (uniform) p <- rep(1 / k, k)
  if (!is.null(names(p)) && all(names(obs) %in% names(p))) p <- p[names(obs)]
  if (length(p) != k) stop("p needs one probability per category (", k, "): ", paste(names(obs), collapse = ", "), call. = FALSE)
  pct <- all(p >= 0) && abs(sum(p) - 100) < 1e-6
  if (pct) p <- p / 100
  if (any(p <= 0) || abs(sum(p) - 1) > 1e-6) stop("p must be positive and sum to 1 (or 100%).", call. = FALSE)
  n <- sum(obs); ex <- n * p; contrib <- (obs - ex)^2 / ex; resid <- (obs - ex) / sqrt(ex)
  chi <- sum(contrib); df <- k - 1 - estimated; pv <- 1 - pchisq(chi, df); crit <- qchisq(1 - alpha, df)
  tab <- data.frame(category = names(obs), observed = obs, p_H0 = p, expected = ex, residual = resid, contribution = contrib)
  lines <- c(
    "H0: p_k = p_k0 for every category k (the population follows the stated probabilities)",
    "H1: p_k != p_k0 for at least one k", "",
    .table_lines(.round_df(tab)), "",
    sprintf("Expected count E_k = n x p_k0 (n = %s);  residual = (O - E) / sqrt(E) (sign = direction);  contribution = (O - E)^2 / E", .f(n)),
    sprintf("Chi-square = sum of contributions = %s", .f(chi)),
    sprintf("df = k - 1%s = %s", if (estimated) sprintf(" - %s", estimated) else "", df),
    sprintf("Critical value chi-square(%s; %s) = %s  ->  reject H0 if chi-square > %s", .f(1 - alpha), df, .f(crit), .f(crit)),
    sprintf("p-value = P(chi-square(%s) > %s) = %s", df, .f(chi), .fp(pv)), "",
    .decision_lines(pv, alpha))
  notes <- c(how, .expected_note(ex))
  big <- names(obs)[which.max(abs(resid))]
  wording <- c(sprintf(
    "We test whether the population distribution of %s follows the theoretical distribution specified under H0; under H1 at least one category probability differs. The Pearson chi-square statistic is %s with %s degrees of freedom (right-tail test), with p-value %s. %s to conclude that the population distribution differs from the specified one.",
    xlab, .f(chi), df, .fp(pv), .decision_words(pv, alpha)),
    sprintf("The largest contribution comes from %s (residual %s: %s than expected under H0). The test itself does not say which categories differ; the residuals do. With a very large n even small, practically negligible differences lead to rejection.",
            big, .f(resid[big]), if (resid[big] > 0) "more" else "fewer"))
  rx <- if (raw) call("table", sx) else if (is.data.frame(x)) unname(obs) else sx
  rp <- if (uniform) NULL else if (!pct && !is.null(sp) && !is.name(sp)) sp else round(unname(p), 6)
  rb <- .ub_call("chisq.test", x = rx, p = rp)
  .plot_test(chi, "greater", alpha, "chisq", df, main = "Chi-square goodness of fit")
  .result("Chi-square goodness-of-fit test", lines, wording, notes, match.call(),
          statistic = chi, df = df, p_value = pv, critical = crit, expected = stats::setNames(ex, names(obs)),
          residuals = stats::setNames(resid, names(obs)), contributions = stats::setNames(contrib, names(obs)),
          decision = if (pv < alpha) "reject H0" else "fail to reject H0", rbase = rb)
}

#' @rdname chisq
#' @export
chisq_indep <- function(x, y = NULL, alpha = 0.05) {
  alpha <- .prob(alpha, "alpha")
  sx <- substitute(x); sy <- substitute(y)
  xlab <- .label(sx); ylab <- .label(sy)
  obs <- .as_count_table(x)
  rx <- if (!is.null(obs)) list(x = sx) else list(x = sx, y = sy)
  if (is.null(obs) && is.data.frame(x) && is.null(y) && ncol(x) == 2) {
    y <- x[[2]]; ylab <- names(x)[2]; xlab <- names(x)[1]; x <- x[[1]]
    rx <- list(x = call("table", sx))
  }
  if (is.null(obs)) {
    if (is.null(y)) stop("Give two categorical variables (x, y) or a count table.", call. = FALSE)
    a <- .one_column(x, xlab); b <- .one_column(y, ylab)
    if (length(a) != length(b)) stop("x and y must have the same length.", call. = FALSE)
    ok <- !is.na(a) & !is.na(b)
    obs <- unclass(table(a[ok], b[ok])) + 0
    names(dimnames(obs)) <- c(xlab, ylab)
  } else {
    dn <- names(dimnames(obs))
    if (!is.null(dn)) { if (nzchar(dn[1])) xlab <- dn[1]; if (nzchar(dn[2])) ylab <- dn[2] }
    if (is.null(dn) || !nzchar(dn[1])) xlab <- "the row variable"
    if (is.null(dn) || !nzchar(dn[2])) ylab <- "the column variable"
  }
  if (nrow(obs) < 2 || ncol(obs) < 2) stop("Need at least 2 rows and 2 columns.", call. = FALSE)
  n <- sum(obs)
  ex <- outer(rowSums(obs), colSums(obs)) / n
  contrib <- (obs - ex)^2 / ex; resid <- (obs - ex) / sqrt(ex)
  chi <- sum(contrib); df <- (nrow(obs) - 1) * (ncol(obs) - 1)
  pv <- 1 - pchisq(chi, df); crit <- qchisq(1 - alpha, df)
  V <- sqrt(chi / (n * (min(dim(obs)) - 1)))
  two <- all(dim(obs) == 2)
  with_m <- rbind(cbind(obs, Total = rowSums(obs)), Total = c(colSums(obs), n))
  lines <- c(
    sprintf("H0: %s and %s are independent      H1: they are associated", xlab, ylab), "",
    "Observed counts (with totals):", .table_lines(with_m, row.names = TRUE), "",
    "Row percentages:", .table_lines(round(100 * obs / rowSums(obs), 2), row.names = TRUE), "",
    "Expected counts E = row total x column total / grand total:", .table_lines(round(ex, 4), row.names = TRUE), "",
    "Contributions (O - E)^2 / E:", .table_lines(round(contrib, 4), row.names = TRUE), "",
    "Residuals (O - E) / sqrt(E)  (+ = more than expected under independence):", .table_lines(round(resid, 4), row.names = TRUE), "",
    sprintf("Chi-square = sum of contributions = %s", .f(chi)),
    sprintf("df = (rows - 1)(columns - 1) = (%s - 1)(%s - 1) = %s", nrow(obs), ncol(obs), df),
    sprintf("Critical value chi-square(%s; %s) = %s  ->  reject H0 if chi-square > %s", .f(1 - alpha), df, .f(crit), .f(crit)),
    sprintf("p-value = P(chi-square(%s) > %s) = %s", df, .f(chi), .fp(pv)), "",
    .decision_lines(pv, alpha),
    .p_reading(pv), "",
    sprintf("Strength (descriptive, ch. 4): Cramer's V = sqrt(chi-square / (n (min(rows, cols) - 1))) = sqrt(%s / (%s x %s)) = %s",
            .f(chi), .f(n), min(dim(obs)) - 1, .f(V)))
  notes <- c(.expected_note(ex),
             if (two) "2 x 2 table: R's chisq.test() applies Yates' continuity correction by default, so its X-squared differs from the formula above; use chisq.test(..., correct = FALSE) to match.")
  wording <- c(sprintf(
    "We test H0: %s and %s are independent in the population (p_kj = R_k C_j for every cell) against H1: the two variables are associated. Expected counts under independence are row total x column total / grand total. The Pearson chi-square statistic is %s with %s degrees of freedom and p-value %s. %s to conclude that %s and %s are associated in the population.",
    xlab, ylab, .f(chi), df, .fp(pv), .decision_words(pv, alpha), xlab, ylab),
    sprintf("Rejecting independence does not mean a strong association: the test only says the variables are not independent. Cramer's V = %s measures the strength, and the residuals show which combinations occur more or less often than expected.", .f(V)))
  rb <- do.call(.ub_call, c(list("chisq.test"), rx, if (two) list(correct = FALSE)), quote = TRUE)
  .plot_test(chi, "greater", alpha, "chisq", df, main = "Chi-square independence")
  .result("Chi-square test of independence", lines, wording, notes, match.call(),
          statistic = chi, df = df, p_value = pv, critical = crit, observed = obs, expected = ex,
          residuals = resid, cramer_v = V,
          decision = if (pv < alpha) "reject H0" else "fail to reject H0", rbase = rb)
}

# Rule of thumb E >= 5 (book 7.6.2); some authors allow up to 20% below 5 if none is below 1.
.expected_note <- function(ex) {
  k <- sum(ex < 5)
  if (!k) return(NULL)
  sprintf("%d of %d expected counts are below 5 (smallest %s): the chi-square approximation may be unreliable. %s",
          k, length(ex), .f(min(ex)),
          if (min(ex) >= 1 && k / length(ex) <= 0.2) "The relaxed rule (at most 20% below 5, none below 1) is still met."
          else "Even the relaxed rule (at most 20% below 5, none below 1) is not met: merge categories if possible.")
}
