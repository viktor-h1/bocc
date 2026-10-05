# ------------------------------------------------------------
# Type II error / power, and required sample sizes.
# ------------------------------------------------------------

#' Type II error, power and sample size
#'
#' * `power_mean()` beta and power of a one-mean Z test (sigma known) when
#'   the true mean is `mu1`; shows the decision cut-off(s) on the xbar scale.
#' * `power_prop()` the same for a one-proportion Z test when the true
#'   proportion is `p1`.
#' * `n_mean()`     sample size so that the margin of error (or the SE) of a
#'   mean is at most a target. Always rounded UP.
#' * `n_prop()`     the same for a proportion (p = 0.5 if no prior guess).
#' * `n_2props()`   equal group sizes for the CI of p1 - p2 to have a given
#'   margin of error / width.
#'
#' @param mu0,mu1 Mean under H0 and the true (alternative) mean.
#' @param p0 Proportion under H0 (`power_prop()`).
#' @param sigma Population standard deviation.
#' @param n Sample size.
#' @param alpha Significance level.
#' @param alt Alternative of the test: `"<"`, `">"` or `"!="`.
#' @param margin Target margin of error (half-width of the CI).
#' @param width Target width of the CI (= 2 x margin), as many questions phrase it.
#' @param se Target standard error (instead of `margin`).
#' @param s Sample SD when sigma is unknown (`n_mean()`; approximate result).
#'   With the current sample size `n` the t quantile with n - 1 df is used.
#' @param p1,p2 `power_prop()`: `p1` is the true (alternative) proportion.
#'   `n_2props()`: prior guesses of the two proportions (0.5 = conservative).
#' @param conf Confidence level for `margin`.
#' @param p Prior guess of the proportion (default 0.5, the conservative choice).
#' @return An `sc_result` (prints itself).
#' @examples
#' power_mean(mu0 = 2500, mu1 = 2400, sigma = 850, n = 100, alt = "<")
#' n_mean(margin = 3, sigma = 12, conf = 0.95)
#' n_prop(margin = 0.03)
#' n_prop(width = 0.01)
#' n_2props(width = 0.08)
#' @name power
NULL

.plot_power <- function(mu0, mu1, se0, se1, cut, alt, xlab) {
  .with_plot(function() {
    xr <- range(c(mu0 + c(-4, 4) * se0, mu1 + c(-4, 4) * se1))
    xs <- seq(xr[1], xr[2], length.out = 400)
    f0 <- function(x) dnorm(x, mu0, se0); f1 <- function(x) dnorm(x, mu1, se1)
    plot(xs, f0(xs), type = "l", lwd = 2, ylim = c(0, max(f0(xs), f1(xs)) * 1.1), xlab = xlab, ylab = "density",
         main = "H0 (black) vs true value (blue)", las = 1)
    lines(xs, f1(xs), lwd = 2, col = "navy")
    beta_col <- grDevices::adjustcolor("orange", 0.5)
    if (alt == "less") .shade(f1, cut, xr[2], beta_col)
    if (alt == "greater") .shade(f1, xr[1], cut, beta_col)
    if (alt == "two.sided") .shade(f1, cut[1], cut[2], beta_col)
    abline(v = cut, lty = 2, col = "firebrick")
    legend("topright", c("beta = P(fail to reject | true value)", "cut-off"), fill = c(beta_col, NA),
           border = NA, lty = c(NA, 2), col = c(NA, "firebrick"), bty = "n", cex = 0.8)
  })
}

.power_core <- function(est0, est1, se0, se1, alpha, alt) {
  if (alt == "less") {
    cut <- est0 + qnorm(alpha) * se0; power <- pnorm(cut, est1, se1)
  } else if (alt == "greater") {
    cut <- est0 + qnorm(1 - alpha) * se0; power <- 1 - pnorm(cut, est1, se1)
  } else {
    cut <- est0 + c(-1, 1) * qnorm(1 - alpha / 2) * se0
    power <- pnorm(cut[1], est1, se1) + 1 - pnorm(cut[2], est1, se1)
  }
  list(cut = cut, power = power, beta = 1 - power)
}

#' @rdname power
#' @export
power_mean <- function(mu0, mu1, sigma, n, alpha = 0.05, alt) {
  if (missing(alt)) stop("Give the test's alternative: alt = \"<\", \">\" or \"!=\".", call. = FALSE)
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  se <- sigma / sqrt(n)
  P <- .power_core(mu0, mu1, se, se, alpha, alt)
  z <- .crit(alt, alpha, "z")
  acc <- switch(alt, less = sprintf("xbar >= %s", .f(P$cut)), greater = sprintf("xbar <= %s", .f(P$cut)),
                two.sided = sprintf("%s <= xbar <= %s", .f(P$cut[1]), .f(P$cut[2])))
  beta_txt <- switch(alt,
    less = sprintf("P(Xbar >= %s | mu = %s) = P(Z >= (%s - %s) / %s) = P(Z >= %s)", .f(P$cut), .f(mu1), .f(P$cut), .f(mu1), .f(se), .f((P$cut - mu1) / se)),
    greater = sprintf("P(Xbar <= %s | mu = %s) = P(Z <= (%s - %s) / %s) = P(Z <= %s)", .f(P$cut), .f(mu1), .f(P$cut), .f(mu1), .f(se), .f((P$cut - mu1) / se)),
    two.sided = sprintf("P(%s <= Xbar <= %s | mu = %s)", .f(P$cut[1]), .f(P$cut[2]), .f(mu1)))
  lines <- c(
    sprintf("Test: H0: mu = %s vs H1: mu %s %s, sigma = %s, n = %s, alpha = %s", .f(mu0), .alt_sym(alt), .f(mu0), .f(sigma), n, .f(alpha)),
    sprintf("True mean mu1 = %s", .f(mu1)), "",
    sprintf("SE = sigma / sqrt(n) = %s / sqrt(%s) = %s", .f(sigma), n, .f(se)),
    sprintf("Cut-off(s) on the xbar scale: mu0 + z x SE = %s + %s x %s = %s", .f(mu0), paste(.f(z), collapse = " / "), .f(se), paste(.f(P$cut), collapse = " and ")),
    sprintf("We fail to reject H0 when %s", acc),
    sprintf("beta = %s = %s", beta_txt, .f(P$beta, 6)),
    sprintf("Power = 1 - beta = %s", .f(P$power, 6)))
  .plot_power(mu0, mu1, se, se, P$cut, alt, "xbar")
  .result("Type II error and power (mean, sigma known)", lines,
          sprintf("The Type II error probability is the probability of failing to reject H0 when the true mean is %s: beta = %s. The power of the test, the probability of correctly rejecting H0 when mu = %s, is 1 - beta = %s.",
                  .f(mu1), .f(P$beta), .f(mu1), .f(P$power)),
          NULL, match.call(), beta = P$beta, power = P$power, cutoff = P$cut, se = se)
}

#' @rdname power
#' @export
power_prop <- function(p0, p1, n, alpha = 0.05, alt) {
  if (missing(alt)) stop("Give the test's alternative: alt = \"<\", \">\" or \"!=\".", call. = FALSE)
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha"); p0 <- .prob(p0, "p0"); p1 <- .prob(p1, "p1")
  se0 <- sqrt(p0 * (1 - p0) / n); se1 <- sqrt(p1 * (1 - p1) / n)
  P <- .power_core(p0, p1, se0, se1, alpha, alt)
  lines <- c(
    sprintf("Test: H0: p = %s vs H1: p %s %s, n = %s, alpha = %s; true p1 = %s", .f(p0), .alt_sym(alt), .f(p0), n, .f(alpha), .f(p1)), "",
    sprintf("SE under H0 = sqrt(p0 (1 - p0) / n) = %s", .f(se0)),
    sprintf("SE under p1 = sqrt(p1 (1 - p1) / n) = %s", .f(se1)),
    sprintf("Cut-off(s) on the p-hat scale: %s", paste(.f(P$cut), collapse = " and ")),
    sprintf("beta = P(fail to reject H0 | p = %s) = %s", .f(p1), .f(P$beta, 6)),
    sprintf("Power = 1 - beta = %s", .f(P$power, 6)))
  .plot_power(p0, p1, se0, se1, P$cut, alt, "p-hat")
  .result("Type II error and power (one proportion)", lines,
          sprintf("If the true proportion is %s, the probability of failing to reject H0 (Type II error) is beta = %s and the power is %s.",
                  .f(p1), .f(P$beta), .f(P$power)),
          "Normal approximation; the SE under the true p1 is used for beta.", match.call(),
          beta = P$beta, power = P$power, cutoff = P$cut)
}

#' @rdname power
#' @export
n_mean <- function(margin = NULL, sigma = NULL, conf = 0.95, se = NULL, width = NULL, s = NULL, n = NULL) {
  M <- .margin_from(margin, width, se)
  if (is.null(sigma) && is.null(s)) stop("Give sigma = (known population SD), or s = (sample SD, approximate result).", call. = FALSE)
  lines <- M$lines; notes <- NULL
  sd0 <- sigma %||% s; sname <- if (!is.null(sigma)) "sigma" else "s"
  if (!is.null(M$se)) {
    raw <- (sd0 / M$se)^2
    lines <- c(lines, sprintf("SE = %s / sqrt(n) <= %s  ->  n >= (%s / SE*)^2 = (%s / %s)^2 = %s", sname, .f(M$se), sname, .f(sd0), .f(M$se), .f(raw)))
  } else {
    conf <- .prob(conf, "conf")
    if (is.null(sigma) && !is.null(n)) {
      q <- qt((1 + conf) / 2, n - 1); qtxt <- sprintf("t(%s; df = %s)", .f((1 + conf) / 2), .f(n - 1))
    } else {
      q <- qnorm((1 + conf) / 2); qtxt <- sprintf("z(%s)", .f((1 + conf) / 2))
    }
    raw <- (q * sd0 / M$margin)^2
    lines <- c(lines, sprintf("ME = %s x %s / sqrt(n) <= %s  ->  n >= (%s x %s / ME*)^2 = (%s x %s / %s)^2 = %s",
                              qtxt, sname, .f(M$margin), qtxt, sname, .f(q), .f(sd0), .f(M$margin), .f(raw)))
  }
  if (is.null(sigma)) notes <- "sigma unknown: s replaces it, so the result is only approximate (the actual width depends on the s of the sample that will be drawn)."
  nn <- ceiling(raw - 1e-9)
  lines <- c(lines, sprintf("Required n = %s (always round UP to the next integer)", nn))
  .result("Sample size for a mean", lines, NULL, notes, match.call(), n = nn)
}

# Target from margin of error, interval width (= 2 ME) or standard error.
.margin_from <- function(margin, width, se) {
  if (!is.null(se)) return(list(se = se, margin = NULL, lines = character()))
  if (!is.null(width)) return(list(se = NULL, margin = width / 2,
                                   lines = sprintf("Width = 2 x ME <= %s  ->  ME* = %s / 2 = %s", .f(width), .f(width), .f(width / 2))))
  if (is.null(margin)) stop("Give margin = (margin of error), width = (width of the interval = 2 x margin) or se = (target SE).", call. = FALSE)
  list(se = NULL, margin = margin, lines = character())
}

#' @rdname power
#' @export
n_prop <- function(margin = NULL, p = 0.5, conf = 0.95, se = NULL, width = NULL) {
  M <- .margin_from(margin, width, se)
  p <- .prob(p, "p")
  lines <- M$lines
  if (!is.null(M$se)) {
    raw <- p * (1 - p) / M$se^2
    lines <- c(lines, sprintf("n >= p (1 - p) / SE*^2 = %s x %s / %s^2 = %s", .f(p), .f(1 - p), .f(M$se), .f(raw)))
  } else {
    conf <- .prob(conf, "conf"); z <- qnorm((1 + conf) / 2)
    raw <- z^2 * p * (1 - p) / M$margin^2
    lines <- c(lines, sprintf("n >= z^2 p (1 - p) / ME*^2 = %s^2 x %s x %s / %s^2 = %s", .f(z), .f(p), .f(1 - p), .f(M$margin), .f(raw)))
  }
  nn <- ceiling(raw - 1e-9)
  lines <- c(lines, sprintf("Required n = %s (always round UP to the next integer)", nn),
             if (p == 0.5) "p = 0.5 used: p(1 - p) is largest there, so this n works whatever the true p (conservative choice).")
  .result("Sample size for a proportion", lines, NULL, NULL, match.call(), n = nn)
}

#' @rdname power
#' @export
n_2props <- function(margin = NULL, p1 = 0.5, p2 = 0.5, conf = 0.95, width = NULL) {
  M <- .margin_from(margin, width, NULL)
  p1 <- .prob(p1, "p1"); p2 <- .prob(p2, "p2"); conf <- .prob(conf, "conf"); z <- qnorm((1 + conf) / 2)
  v <- p1 * (1 - p1) + p2 * (1 - p2)
  raw <- z^2 * v / M$margin^2
  nn <- ceiling(raw - 1e-9)
  lines <- c(M$lines,
             sprintf("Equal sample sizes n in both groups:  ME = z sqrt(p1(1 - p1)/n + p2(1 - p2)/n) <= %s", .f(M$margin)),
             sprintf("n >= z^2 [p1(1 - p1) + p2(1 - p2)] / ME*^2 = %s^2 x (%s + %s) / %s^2 = %s",
                     .f(z), .f(p1 * (1 - p1)), .f(p2 * (1 - p2)), .f(M$margin), .f(raw)),
             sprintf("Required n = %s in EACH group (round UP)", nn),
             if (p1 == 0.5 && p2 == 0.5) "p1 = p2 = 0.5 used: the conservative choice.")
  .result("Sample size for the difference of two proportions", lines, NULL, NULL, match.call(), n = nn)
}
