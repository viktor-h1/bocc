# ------------------------------------------------------------
# Probability: normal / t / chi-square probabilities and quantiles,
# discrete random variables, linear combinations, iid sums / means
# (CLT) and the sampling distribution of a sample proportion.
# ------------------------------------------------------------

#' Probabilities, quantiles and random variables
#'
#' * `prob_normal()` P(X < a), P(X > a), P(a < X < b) or the p-quantile of
#'   N(mean, sd^2), with the standardisation step shown and the area shaded.
#' * `prob_t()`, `prob_chisq()` the same for Student t and chi-square.
#' * `rv_discrete()` E(X), Var(X), SD(X) and the CDF of a discrete random variable.
#' * `rv_linear()`  T = aX + bY + c: mean, variance (with covariance), SD and
#'   optional normal probabilities.
#' * `rv_iid()`     sum or mean of n iid variables: mean, variance, SE and
#'   probabilities via the normal / CLT approximation.
#' * `rv_prop()`    sampling distribution of a sample proportion p-hat.
#'
#' @param mean,sd Mean and standard deviation of the normal distribution.
#' @param df Degrees of freedom.
#' @param below,above Value a for P(X < a) / P(X > a).
#' @param between Two values c(a, b) for P(a < X < b).
#' @param quantile Probability p: find x with P(X <= x) = p.
#' @param values,probs Values of a discrete RV and their probabilities (or %).
#' @param a,b,c Coefficients in T = aX + bY + c.
#' @param mu_x,sd_x,mu_y,sd_y Means and SDs of X and Y.
#' @param var_x,var_y Variances (alternative to `sd_x`, `sd_y`).
#' @param cov,rho Covariance or correlation of X and Y (default 0).
#' @param mu,sigma,var Mean and SD (or variance) of each iid variable.
#' @param n Number of iid variables / sample size.
#' @param stat `"mean"` (sample mean) or `"sum"` (total).
#' @param p Population proportion.
#' @return An `sc_result` (prints itself).
#' @examples
#' prob_normal(mean = 80, sd = 12, above = 100)
#' rv_discrete(values = 0:3, probs = c(0.1, 0.3, 0.4, 0.2))
#' rv_linear(a = 1, mu_x = 10, sd_x = 1, b = 2, mu_y = 8, sd_y = 1, rho = 0.2)
#' rv_iid(mu = 50, sigma = 10, n = 25, stat = "mean", above = 53)
#' @name probability
NULL

# Shared engine: dist in z/t/chisq; for z we standardise with mean/sd.
.prob_engine <- function(dist, mean, sd, df, below, above, between, quantile, title, xname = "X") {
  cdf <- switch(dist, z = function(q) pnorm(q, mean, sd), t = function(q) pt(q, df), chisq = function(q) pchisq(q, df))
  qf <- switch(dist, z = function(p) qnorm(p, mean, sd), t = function(p) qt(p, df), chisq = function(p) qchisq(p, df))
  std <- function(a) if (dist == "z" && !(mean == 0 && sd == 1))
    sprintf("   [z = (%s - %s) / %s = %s]", .f(a), .f(mean), .f(sd), .f((a - mean) / sd)) else ""
  dlab <- switch(dist, z = sprintf("N(%s, %s^2)", .f(mean), .f(sd)), t = sprintf("t(%s)", .f(df)), chisq = sprintf("chi-square(%s)", .f(df)))
  lines <- sprintf("%s ~ %s", xname, dlab)
  out <- list(); lo <- numeric(); hi <- numeric()
  if (!is.null(below)) for (a in below) {
    v <- cdf(a); out[[sprintf("P(%s < %s)", xname, .f(a))]] <- v
    lines <- c(lines, sprintf("P(%s < %s) = %s%s", xname, .f(a), .f(v, 6), std(a))); lo <- c(lo, -Inf); hi <- c(hi, a)
  }
  if (!is.null(above)) for (a in above) {
    v <- 1 - cdf(a); out[[sprintf("P(%s > %s)", xname, .f(a))]] <- v
    lines <- c(lines, sprintf("P(%s > %s) = 1 - P(%s < %s) = %s%s", xname, .f(a), xname, .f(a), .f(v, 6), std(a)))
    lo <- c(lo, a); hi <- c(hi, Inf)
  }
  if (!is.null(between)) {
    if (length(between) != 2) stop("between must be two numbers: c(a, b).", call. = FALSE)
    a <- min(between); b <- max(between); v <- cdf(b) - cdf(a)
    out[[sprintf("P(%s < %s < %s)", .f(a), xname, .f(b))]] <- v
    lines <- c(lines, sprintf("P(%s < %s < %s) = P(%s < %s) - P(%s < %s) = %s - %s = %s", .f(a), xname, .f(b),
                              xname, .f(b), xname, .f(a), .f(cdf(b), 6), .f(cdf(a), 6), .f(v, 6)),
               if (nzchar(std(a))) paste0("   ", trimws(std(a)), "  and  ", trimws(std(b))))
    lo <- c(lo, a); hi <- c(hi, b)
  }
  if (!is.null(quantile)) for (p in quantile) {
    p <- .prob(p, "quantile")
    v <- qf(p); out[[sprintf("q(%s)", .f(p))]] <- v
    lines <- c(lines, sprintf("Value x with P(%s <= x) = %s:  x = %s%s", xname, .f(p), .f(v, 6),
                              if (dist == "z" && !(mean == 0 && sd == 1))
                                sprintf("   [= mean + z x sd = %s + %s x %s]", .f(mean), .f(qnorm(p)), .f(sd)) else ""))
    lo <- c(lo, -Inf); hi <- c(hi, v)
  }
  if (!length(out)) stop("Say what you need: below = , above = , between = c(a, b) or quantile = .", call. = FALSE)
  .plot_area(dist, lo, hi, mean, sd, df, main = paste(title, dlab))
  list(lines = lines, values = unlist(out))
}

#' @rdname probability
#' @export
prob_normal <- function(mean = 0, sd = 1, below = NULL, above = NULL, between = NULL, quantile = NULL) {
  if (sd <= 0) stop("sd must be positive.", call. = FALSE)
  E <- .prob_engine("z", mean, sd, NULL, below, above, between, quantile, "Normal")
  .result("Normal probability", E$lines, NULL, NULL, match.call(), values = E$values)
}

#' @rdname probability
#' @export
prob_t <- function(df, below = NULL, above = NULL, between = NULL, quantile = NULL) {
  E <- .prob_engine("t", 0, 1, df, below, above, between, quantile, "Student", "T")
  .result("Student t probability", E$lines, NULL, NULL, match.call(), values = E$values)
}

#' @rdname probability
#' @export
prob_chisq <- function(df, below = NULL, above = NULL, between = NULL, quantile = NULL) {
  E <- .prob_engine("chisq", 0, 1, df, below, above, between, quantile, "", "X2")
  .result("Chi-square probability", E$lines, NULL, NULL, match.call(), values = E$values)
}

#' @rdname probability
#' @export
rv_discrete <- function(values, probs) {
  if (length(values) != length(probs)) stop("values and probs must have the same length.", call. = FALSE)
  if (all(probs >= 0) && abs(sum(probs) - 100) < 1e-6) probs <- probs / 100
  if (any(probs < 0) || abs(sum(probs) - 1) > 1e-6) stop("probs must be non-negative and sum to 1 (or 100%).", call. = FALSE)
  mu <- sum(values * probs); ex2 <- sum(values^2 * probs); v <- ex2 - mu^2
  tab <- data.frame(x = values, p = probs, x_times_p = values * probs, x2_times_p = values^2 * probs, cdf = cumsum(probs))
  lines <- c(.table_lines(.round_df(tab)), "",
             sprintf("E(X)   = sum x p(x)   = %s", .f(mu)),
             sprintf("E(X^2) = sum x^2 p(x) = %s", .f(ex2)),
             sprintf("Var(X) = E(X^2) - E(X)^2 = %s - %s^2 = %s", .f(ex2), .f(mu), .f(v)),
             sprintf("SD(X)  = sqrt(Var(X)) = %s", .f(sqrt(v))))
  .with_plot(function() plot(values, probs, type = "h", lwd = 6, col = "grey40", xlab = "x", ylab = "P(X = x)",
                             main = "Probability distribution", las = 1))
  .result("Discrete random variable", lines, NULL, NULL, match.call(), mean = mu, variance = v, sd = sqrt(v), table = tab)
}

#' @rdname probability
#' @export
rv_linear <- function(a = 1, mu_x, sd_x = NULL, b = 0, mu_y = 0, sd_y = 0, c = 0, cov = NULL, rho = 0,
                      var_x = NULL, var_y = NULL, below = NULL, above = NULL, between = NULL) {
  if (missing(mu_x)) stop("Give mu_x = (and sd_x = or var_x =).", call. = FALSE)
  vx <- var_x %||% (if (is.null(sd_x)) stop("Give sd_x = or var_x =.", call. = FALSE) else sd_x^2)
  vy <- var_y %||% sd_y^2
  cxy <- cov %||% (rho * sqrt(vx) * sqrt(vy))
  mu <- a * mu_x + b * mu_y + c
  v <- a^2 * vx + b^2 * vy + 2 * a * b * cxy
  if (v < 0) stop("The variance came out negative; check the inputs.", call. = FALSE)
  lines <- c(sprintf("T = %s X%s%s", .f(a), if (b != 0) sprintf(" + %s Y", .f(b)) else "", if (c != 0) sprintf(" + %s", .f(c)) else ""),
             if (b != 0 && is.null(cov)) sprintf("Cov(X, Y) = rho x SD(X) x SD(Y) = %s x %s x %s = %s", .f(rho), .f(sqrt(vx)), .f(sqrt(vy)), .f(cxy)),
             sprintf("E(T) = a E(X) + b E(Y) + c = %s x %s + %s x %s + %s = %s", .f(a), .f(mu_x), .f(b), .f(mu_y), .f(c), .f(mu)),
             sprintf("Var(T) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X,Y) = %s^2 x %s + %s^2 x %s + 2 x %s x %s x %s = %s",
                     .f(a), .f(vx), .f(b), .f(vy), .f(a), .f(b), .f(cxy), .f(v)),
             sprintf("SD(T) = sqrt(Var(T)) = %s", .f(sqrt(v))))
  vals <- NULL
  if (!is.null(below) || !is.null(above) || !is.null(between)) {
    E <- .prob_engine("z", mu, sqrt(v), NULL, below, above, between, NULL, "T ~", "T")
    lines <- c(lines, "", "Assuming T is normal (X, Y jointly normal):", E$lines[-1]); vals <- E$values
  }
  .result("Linear combination of random variables", lines, NULL, NULL, match.call(),
          mean = mu, variance = v, sd = sqrt(v), covariance = cxy, values = vals)
}

#' @rdname probability
#' @export
rv_iid <- function(mu, sigma = NULL, n, stat = c("mean", "sum"), var = NULL,
                   below = NULL, above = NULL, between = NULL) {
  stat <- match.arg(stat)
  v1 <- var %||% (if (is.null(sigma)) stop("Give sigma = or var =.", call. = FALSE) else sigma^2)
  if (n < 1 || n != round(n)) stop("n must be a positive whole number.", call. = FALSE)
  if (stat == "mean") {
    m <- mu; v <- v1 / n
    lines <- c(sprintf("Xbar = mean of n = %s iid variables with mean %s and variance %s", n, .f(mu), .f(v1)),
               sprintf("E(Xbar) = mu = %s", .f(m)),
               sprintf("Var(Xbar) = sigma^2 / n = %s / %s = %s", .f(v1), n, .f(v)),
               sprintf("SE(Xbar) = sigma / sqrt(n) = %s", .f(sqrt(v))))
    nm <- "Xbar"
  } else {
    m <- n * mu; v <- n * v1
    lines <- c(sprintf("S = sum of n = %s iid variables with mean %s and variance %s", n, .f(mu), .f(v1)),
               sprintf("E(S) = n mu = %s x %s = %s", n, .f(mu), .f(m)),
               sprintf("Var(S) = n sigma^2 = %s x %s = %s", n, .f(v1), .f(v)),
               sprintf("SD(S) = sqrt(n) sigma = %s", .f(sqrt(v))))
    nm <- "S"
  }
  vals <- NULL
  if (!is.null(below) || !is.null(above) || !is.null(between)) {
    E <- .prob_engine("z", m, sqrt(v), NULL, below, above, between, NULL, paste(nm, "~"), nm)
    lines <- c(lines, "", "Normal distribution (exact if the population is normal; CLT approximation if n is large):", E$lines[-1])
    vals <- E$values
  }
  .result(if (stat == "mean") "Sampling distribution of the sample mean" else "Sum of iid random variables",
          lines, NULL, NULL, match.call(), mean = m, variance = v, sd = sqrt(v), values = vals)
}

#' @rdname probability
#' @export
rv_prop <- function(p, n, below = NULL, above = NULL, between = NULL) {
  p <- .prob(p, "p")
  se <- sqrt(p * (1 - p) / n)
  lines <- c(sprintf("p-hat from a sample of n = %s, population proportion p = %s", .f(n), .f(p)),
             sprintf("E(p-hat) = p = %s", .f(p)),
             sprintf("SE(p-hat) = sqrt(p (1 - p) / n) = sqrt(%s x %s / %s) = %s", .f(p), .f(1 - p), .f(n), .f(se)),
             sprintf("Check: n p = %s, n (1 - p) = %s (both should be at least 5 for the normal approximation)", .f(n * p), .f(n * (1 - p))))
  vals <- NULL
  if (!is.null(below) || !is.null(above) || !is.null(between)) {
    E <- .prob_engine("z", p, se, NULL, below, above, between, NULL, "p-hat ~", "p-hat")
    lines <- c(lines, "", "Normal approximation (CLT):", E$lines[-1]); vals <- E$values
  }
  .result("Sampling distribution of a sample proportion", lines, NULL, NULL, match.call(),
          mean = p, se = se, values = vals)
}
