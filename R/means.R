# ------------------------------------------------------------
# Means: one mean, paired means, two independent means.
# Every function takes raw data (any vector / expression) OR the
# summary numbers given in the question text.
# ------------------------------------------------------------

.alt_words <- function(alt) switch(alt, less = "less than", greater = "greater than", two.sided = "different from")

# ---------- one mean ----------

.one_mean <- function(x, xbar, s, n, sigma, method, xlab, sum_x = NULL, sum_x2 = NULL) {
  steps <- character()
  if (!is.null(x)) {
    v <- .num(x, xlab)
    n <- length(v); xbar <- mean(v); s <- if (n > 1) sd(v) else NA
    note <- .dropped_note(v)
    from <- sprintf("Data: %s", xlab)
  } else if (!is.null(sum_x)) {
    .need(n, msg = "With sum_x = give the sample size n =.")
    xbar <- sum_x / n
    steps <- sprintf("xbar = sum(x_i) / n = %s / %s = %s", .f(sum_x), .f(n), .f(xbar))
    if (is.null(s) && is.null(sigma)) {
      if (is.null(sum_x2)) stop("Give sum_x2 = (sum of squares), s = or sigma =.", call. = FALSE)
      s2 <- (sum_x2 - n * xbar^2) / (n - 1)
      if (s2 <= 0) stop("sum_x2 - n xbar^2 is not positive; check the sums.", call. = FALSE)
      s <- sqrt(s2)
      steps <- c(steps, sprintf("s^2 = [sum(x_i^2) - n xbar^2] / (n - 1) = (%s - %s x %s^2) / %s = %s   ->  s = %s",
                                .f(sum_x2), .f(n), .f(xbar), .f(n - 1), .f(s2), .f(s)))
    }
    note <- NULL
    from <- "Sums from the question"
  } else {
    .need(xbar, n, msg = "Give the data (x = ...) or the summary numbers xbar = , n = , and s = (or sigma = ), or sum_x = , sum_x2 = , n =.")
    if (is.null(s) && is.null(sigma)) stop("Give s = (sample SD) or sigma = (population SD, if known).", call. = FALSE)
    note <- NULL
    from <- "Summary numbers from the question"
  }
  if (!is.null(sigma)) {
    case <- "known"; dist <- "z"; df <- NULL; sdv <- sigma
    se_txt <- sprintf("SE = sigma / sqrt(n) = %s / sqrt(%s) = %s", .f(sigma), .f(n), .f(sigma / sqrt(n)))
    reason <- "The population standard deviation sigma is known, so the standardised sample mean follows a standard normal distribution (Z)"
    label <- "z (sigma known)"
  } else {
    method <- match.arg(method, c("t", "z"))
    if (n < 2) stop("At least two observations are needed to estimate s.", call. = FALSE)
    sdv <- s
    se_txt <- sprintf("SE = s / sqrt(n) = %s / sqrt(%s) = %s", .f(s), .f(n), .f(s / sqrt(n)))
    if (method == "t") {
      case <- "t"; dist <- "t"; df <- n - 1
      reason <- "The population standard deviation is unknown and the population is assumed normal, so we use the Student t distribution with n - 1 degrees of freedom"
      label <- sprintf("t with n - 1 = %s df", .f(df))
    } else {
      case <- "large"; dist <- "z"; df <- NULL
      reason <- "The population distribution is not assumed normal and sigma is unknown, but the sample is large, so by the Central Limit Theorem we use the normal approximation (Z) with s in place of sigma"
      label <- "z (large-sample normal approximation)"
    }
  }
  inputs <- sprintf("%s   n = %s   xbar = %s   %s", from, .f(n), .f(xbar),
                    if (case == "known") sprintf("sigma = %s", .f(sigma)) else sprintf("s = %s", .f(s)))
  list(n = n, xbar = xbar, s = s, sigma = sigma, se = sdv / sqrt(n), dist = dist, df = df,
       case = case, reason = reason, label = label, inputs = inputs, se_txt = se_txt, note = note, steps = steps)
}

#' Hypothesis tests
#'
#' One function per test (book chapter 7). Each accepts **raw data** (any
#' vector or R expression: `df$spend`, `c(12, 15, 9)`, `df$after - df$before`,
#' `subset(df, region == "North")$spend`) **or the summary numbers** given in
#' the question. Output: H0 / H1 (a one-sided H0 is composite, e.g.
#' "mu <= 15", and its boundary value is used), a table like the course
#' package UBStats (Normal.Approx and Student-t rows when a variance is
#' unknown), each formula with the numbers plugged in, the critical value /
#' rejection region, the p-value and its reading, the decision, exam wording,
#' the matching UBStats call and the call to re-run it.
#'
#' * `test_mean()`   one population mean (sigma known -> Z; unknown -> z and t rows,
#'   `method` chooses the one worked through).
#' * `test_paired()` paired / before-after mean difference, D = x - y (sigma_D known -> Z).
#' * `test_2means()` two independent means. Without `case`, the four UBStats
#'   tests (variances equal or different x Normal or t); with raw data also
#'   Levene's test for equal variances.
#' * `test_levene()` Levene's test for equal variances of two groups (raw
#'   data; absolute deviations from the medians, as UBStats `var.test = TRUE`).
#' * `test_prop()`   one population proportion (Z test, SE uses p0).
#' * `test_2props()` two independent proportions (Z test; pooled p under H0
#'   when `d0 = 0`, separate p-hats otherwise).
#'
#' @param x Data. For means: numeric values. For proportions: a logical
#'   vector (`df$loyalty == "High"`), a 0/1 vector, or a categorical vector
#'   together with `event`. In `test_2means()`/`test_2props()` this is the
#'   first group (or the whole outcome when `group` is given).
#' @param y Second sample (paired: second measurement, D = x - y;
#'   two-sample: the second group).
#' @param mu0 Null value of the population mean.
#' @param p0 Null value of the population proportion (0.25 or 25).
#' @param d0 Null value of the difference (default 0): mu_D, mu_x - mu_y or
#'   p_x - p_y under H0 (UBStats `mdiff0` / `pdiff0`).
#' @param alt Alternative: `"<"`, `">"` or `"!="` (also `"less"`,
#'   `"greater"`, `"two.sided"`).
#' @param alpha Significance level (0.05 or 5).
#' @param method For means with sigma unknown: `"t"` (normal population,
#'   Student t) or `"z"` (large sample, normal approximation).
#' @param case Two independent means: `"pooled"` (variances unknown but
#'   assumed equal), `"welch"` (unknown, not assumed equal), `"large"`
#'   (non-normal / unknown distribution, large samples, Z) or `"known"`
#'   (population sigmas known, give `sigma1`, `sigma2`). Omit it to see all
#'   four tests.
#' @param group,levels Grouping variable and the two groups to compare,
#'   e.g. `group = df$loyalty, levels = c("High", "Medium")`.
#' @param event Category of `x` that counts as a "success", e.g. `"High"`.
#' @param xbar,s,n,sigma Summary numbers for one mean (sigma = known
#'   population SD).
#' @param sum_x,sum_x2 `test_mean()`: sum of the observations and of their
#'   squares (with `n`), when the question gives the sums.
#' @param dbar,sd_d Mean and SD of the paired differences.
#' @param mean1,mean2,s1,s2,cov,r Paired summary numbers when only the two
#'   means/SDs and their covariance or correlation are given.
#' @param xbar1,n1,xbar2,n2,sigma1,sigma2 Summary numbers for two samples.
#' @param count,phat Number of successes or sample proportion (with `n`).
#' @param sigma_d `test_paired()`: known standard deviation of the
#'   differences (Z test).
#' @param var_test Two means: Levene's test is shown automatically with raw
#'   data (`NULL`); `FALSE` hides it.
#' @param count1,count2,phat1,phat2 Two-sample successes / proportions.
#' @return An `sc_result` (prints itself) with `statistic`, `p_value`,
#'   `critical`, `se`, `df`, `decision`.
#' @examples
#' x <- c(52, 48, 55, 60, 49, 53, 58, 51)
#' test_mean(x, mu0 = 50, alt = ">")
#' test_mean(xbar = 52.3, s = 6.1, n = 40, mu0 = 50, alt = ">")
#' test_prop(count = 120, n = 400, p0 = 0.25, alt = ">")
#' test_2means(xbar1 = 0.16, s1 = 0.35, n1 = 44, xbar2 = 0.06, s2 = 0.48, n2 = 42, alt = ">")
#' test_2props(count1 = 138, n1 = 1150, count2 = 96, n2 = 1200, alt = ">")
#' @name tests
NULL

#' @rdname tests
#' @export
test_mean <- function(x = NULL, mu0, alt = "two.sided", alpha = 0.05, method = c("t", "z"),
                      xbar = NULL, s = NULL, n = NULL, sigma = NULL, sum_x = NULL, sum_x2 = NULL) {
  if (missing(mu0)) stop("Give the null value: mu0 = ...", call. = FALSE)
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  sx <- substitute(x); xlab <- .label(sx)
  m <- .one_mean(x, xbar, s, n, sigma, method, xlab, sum_x, sum_x2)
  stat <- (m$xbar - mu0) / m$se
  sname <- if (m$dist == "z") "Z" else "t"
  p <- .pval(stat, alt, m$dist, m$df)
  crit <- .crit(alt, alpha, m$dist, m$df)
  xcut <- mu0 + crit * m$se
  p_z <- p_t <- NULL
  if (m$case == "known") {
    tab <- data.frame(row.names = "Normal", n = m$n, xbar = m$xbar, sigma_X = m$sigma, se = m$se, stat = stat,
                      `p-value` = .p_cells(p), check.names = FALSE)
  } else {
    R <- .test_rows(m$xbar, m$se, mu0, alt, m$n - 1)
    p_z <- R$p[1]; p_t <- R$p[2]
    tab <- data.frame(row.names = rownames(R), n = m$n, xbar = m$xbar, s_X = m$s, se = m$se, stat = R$stat,
                      `p-value` = .p_cells(R$p), check.names = FALSE)
  }
  lines <- c(
    .hyp_lines("mu", .f(mu0), alt),
    "",
    sprintf("Test on the mean   (variance %s)", if (m$case == "known") "known" else "unknown"),
    .ci_table_lines(tab),
    "",
    m$inputs,
    sprintf("Explained below: %s", m$label),
    "",
    m$steps,
    m$se_txt,
    sprintf("%s = (xbar - mu0) / SE = (%s - %s) / %s = %s", sname, .f(m$xbar), .f(mu0), .f(m$se), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, sname)),
    sprintf("  on the xbar scale: %s", .reject_region(alt, xcut, "xbar")),
    sprintf("p-value = %s = %s", .p_text(alt, .dist_label(m$dist, m$df), stat), .fp(p)),
    .r_pval_line(stat, alt, m$dist, m$df),
    "",
    .decision_lines(p, alpha),
    .p_reading(p))
  wording <- c(sprintf(
    "Let mu denote the population mean of %s. We test %s. %s. The sample mean is %s with standard error %s, giving a test statistic %s = %s%s. %s to conclude that the population mean of %s is %s %s.",
    xlab, .hyp_text("mu", .f(mu0), alt), m$reason, .f(m$xbar), .f(m$se), sname, .f(stat),
    if (!is.null(m$df)) sprintf(" with %s degrees of freedom", .f(m$df)) else "",
    .decision_words(p, alpha), xlab, .alt_words(alt), .f(mu0)),
    .zt_disagree(p_z, p_t, alpha),
    .p_meaning(p, sname, stat, alt, sprintf("mu = %s", .f(mu0))),
    if (m$case != "known") .se_meaning(m$se, "the sample mean"))
  ub <- if (!is.null(x)) .ub_call("TEST.mean", x = sx, sigma = sigma, mu0 = mu0, alternative = .ub_alt(alt)) else .ub_raw_note
  .plot_test(stat, alt, alpha, m$dist, m$df, main = "One-mean test")
  .result("One-mean test", lines, wording, m$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = m$se, df = m$df,
          estimate = m$xbar, n = m$n, decision = if (p < alpha) "reject H0" else "fail to reject H0",
          p_z = p_z, p_t = p_t, cutoff = xcut, table = tab, ubstats = ub)
}

#' Point estimates and confidence intervals
#'
#' One function per parameter (book chapter 6). Each accepts raw data (any
#' vector or R expression) or the summary numbers from the question, and
#' prints a table like the course package UBStats (Normal.Approx and
#' Student-t rows when the variance is unknown), the point estimate, the
#' standard error, the reliability factor, the margin of error, the
#' interval, exam wording and the matching UBStats call.
#'
#' * `est_mean()`   point estimate of a mean and its standard error
#'   (sigma / sqrt(n) known, s / sqrt(n) estimated); see [desc_prop()] for a proportion.
#' * `ci_mean()`    one mean (sigma known -> Z; unknown -> both z and t shown,
#'   `method` chooses the one explained step by step).
#' * `ci_paired()`  mean of paired differences D = x - y (sigma_D known -> Z).
#' * `ci_2means()`  difference of two independent means. Without `case`, the
#'   four intervals of the book / UBStats (variances equal or different x
#'   Normal or t); with `case` the chosen one is explained step by step.
#' * `ci_prop()`    one proportion (SE uses p-hat).
#' * `ci_2props()`  difference of two proportions (unpooled SE).
#'
#' @inheritParams tests
#' @param conf Confidence level (0.95 or 95).
#' @param sum_x,sum_x2 Sum of the observations and sum of their squares
#'   (with `n`), when the question gives these instead of xbar and s.
#' @param sigma_d Known standard deviation of the paired differences.
#' @param var_test `ci_2means()`: with raw data Levene's test for equal variances is
#'   shown automatically (`NULL`); `FALSE` hides it.
#' @param se_target `est_mean()`: target standard error; prints the sample
#'   size needed, n >= (sigma / SE*)^2.
#' @return An `sc_result` with `estimate`, `se`, `critical`, `margin`, `ci`
#'   (and `ci_z`, `ci_t` or `intervals` when several intervals are shown).
#' @examples
#' ci_mean(xbar = 82, sigma = 20, n = 64, conf = 0.95)
#' ci_mean(sum_x = 2755, sum_x2 = 585203, n = 15)
#' ci_prop(count = 120, n = 2000, conf = 90)
#' ci_2means(xbar1 = 1120, s1 = 310, n1 = 85, xbar2 = 970, s2 = 280, n2 = 100)
#' @name ci
NULL

.ci_lines <- function(est_txt, se_txt, crit, crit_txt, se, est, conf, what) {
  me <- crit * se
  ci <- c(est - me, est + me)
  list(ci = ci, me = me, lines = c(
    est_txt,
    se_txt,
    sprintf("Critical value: %s = %s", crit_txt, .f(crit)),
    sprintf("Margin of error = critical x SE = %s x %s = %s", .f(crit), .f(se), .f(me)),
    "",
    sprintf("%s%% CI for %s = %s +/- %s = [%s, %s]", .f(100 * conf, 2), what, .f(est), .f(me), .f(ci[1]), .f(ci[2]))))
}

.ci_wording <- function(conf, what, ci) {
  c(sprintf("With a level of confidence of %s%%, we can conclude that the %s lies between %s and %s.",
            .f(100 * conf, 2), what, .f(ci[1]), .f(ci[2])),
    sprintf("The confidence level refers to the procedure: over repeated sampling, about %s%% of the intervals built this way contain the true parameter. The parameter is fixed (not random), so this particular interval either contains it or not: it is NOT contained in [%s, %s] \"with probability %s\". A two-sided test at alpha = %s rejects H0: parameter = v exactly for the values v outside the interval.",
            .f(100 * conf, 2), .f(ci[1]), .f(ci[2]), .f(conf), .f(1 - conf)))
}

.crit_ci <- function(dist, conf, df) {
  if (dist == "z") list(v = qnorm((1 + conf) / 2), txt = sprintf("z(%s)", .f((1 + conf) / 2)))
  else list(v = qt((1 + conf) / 2, df), txt = sprintf("t(%s; df = %s)", .f((1 + conf) / 2), .f(df, 2)))
}

# UBStats-style rows for a mean-type interval: Normal.Approx and Student-t.
.zt_rows <- function(est, se, conf, df) {
  z <- qnorm((1 + conf) / 2); tq <- qt((1 + conf) / 2, df)
  data.frame(row.names = c("Normal.Approx", "Student-t"), crit = c(z, tq), se = se,
             Lower = est - c(z, tq) * se, Upper = est + c(z, tq) * se)
}

.ci_table_lines <- function(tab) .table_lines(.round_df(tab), row.names = TRUE)

#' @rdname ci
#' @export
ci_mean <- function(x = NULL, conf = 0.95, method = c("t", "z"),
                    xbar = NULL, s = NULL, n = NULL, sigma = NULL, sum_x = NULL, sum_x2 = NULL) {
  conf <- .prob(conf, "conf")
  sx <- substitute(x); xlab <- .label(sx)
  m <- .one_mean(x, xbar, s, n, sigma, method, xlab, sum_x, sum_x2)
  cr <- .crit_ci(m$dist, conf, m$df)
  what <- sprintf("population mean of %s", xlab)
  b <- .ci_lines(sprintf("Point estimate: xbar = %s", .f(m$xbar)), m$se_txt, cr$v, cr$txt, m$se, m$xbar, conf, "mu")
  ci_z <- ci_t <- NULL
  if (m$case == "known") {
    tab <- data.frame(row.names = "Normal", n = m$n, xbar = m$xbar, sigma_X = m$sigma, SE = m$se, Lower = b$ci[1], Upper = b$ci[2])
    head <- sprintf("Confidence interval for the mean   (confidence level %s, variance known)", .f(conf))
  } else {
    zt <- .zt_rows(m$xbar, m$se, conf, m$n - 1)
    tab <- data.frame(row.names = rownames(zt), n = m$n, xbar = m$xbar, s_X = m$s, se = m$se, Lower = zt$Lower, Upper = zt$Upper)
    ci_z <- c(zt$Lower[1], zt$Upper[1]); ci_t <- c(zt$Lower[2], zt$Upper[2])
    head <- sprintf("Confidence interval for the mean   (confidence level %s, variance unknown)", .f(conf))
  }
  lines <- c(head, .ci_table_lines(tab), "", m$inputs, m$steps,
             sprintf("Explained below: %s", m$label), "", b$lines)
  wording <- c(paste0(m$reason, "."),
               if (m$case != "known") "With sigma unknown the t interval is exact for a normal population; for a large sample the normal approximation (z) is also valid, and the t interval is the conservative choice (slightly wider). Both are shown above, as in UBStats.",
               .ci_wording(conf, what, b$ci))
  ub <- if (!is.null(x)) .ub_call("CI.mean", x = sx, sigma = sigma, conf.level = conf) else .ub_raw_note
  .result("Confidence interval for a mean", lines, wording, m$note, match.call(),
          estimate = m$xbar, se = m$se, critical = cr$v, margin = b$me, ci = b$ci, df = m$df,
          ci_z = ci_z, ci_t = ci_t, table = tab, ubstats = ub)
}

#' @rdname ci
#' @export
est_mean <- function(x = NULL, xbar = NULL, s = NULL, n = NULL, sigma = NULL, sum_x = NULL, sum_x2 = NULL,
                     se_target = NULL) {
  sx <- substitute(x); xlab <- .label(sx)
  m <- .one_mean(x, xbar, s, n, sigma, "t", xlab, sum_x, sum_x2)
  known <- m$case == "known"
  lines <- c(m$inputs, m$steps, "",
             sprintf("Point estimate of mu: xbar = %s   (the sample mean is an unbiased estimator: E(Xbar) = mu)", .f(m$xbar)),
             if (known) sprintf("Standard error SE(Xbar) = sigma / sqrt(n) = %s / sqrt(%s) = %s", .f(m$sigma), .f(m$n), .f(m$se))
             else sprintf("Estimated standard error se = s / sqrt(n) = sqrt(s^2 / n) = %s / sqrt(%s) = %s", .f(m$s), .f(m$n), .f(m$se)),
             sprintf("Relative size: se / xbar = %s", .pct(m$se / abs(m$xbar))))
  n_need <- NULL
  if (!is.null(se_target)) {
    sd0 <- if (known) m$sigma else m$s
    raw <- (sd0 / se_target)^2; n_need <- ceiling(raw - 1e-9)
    lines <- c(lines, "", sprintf("Sample size for SE <= %s: n >= (%s / SE*)^2 = (%s / %s)^2 = %s  ->  n = %d%s",
                                  .f(se_target), if (known) "sigma" else "s", .f(sd0), .f(se_target), .f(raw), n_need,
                                  if (known) "" else "   (approximate: s replaces the unknown sigma)"))
  }
  wording <- c(
    sprintf("The estimate of the population mean of %s is the sample mean, %s. Its %sstandard error, %s, is the expected deviation of a GENERIC estimate (over all possible samples of size %s) from mu: it does not tell how far this particular xbar is from mu, which remains unknown.",
            xlab, .f(m$xbar), if (known) "" else "estimated ", .f(m$se), .f(m$n)),
    "A smaller standard error means estimates more concentrated around mu: larger samples give more precise estimates. Comparing two standard errors says which ESTIMATOR is more reliable (its estimates more tightly clustered around the parameter), not how close each specific realised estimate is to its parameter.",
    "Unbiasedness: an estimator T of a parameter theta is unbiased if E(T) = theta for every possible value of theta. The sample mean Xbar is an unbiased estimator of mu, E(Xbar) = mu, and Var(Xbar) = sigma^2 / n: proportional to the population variance, inversely proportional to n, and tending to 0 as n grows (consistency).")
  ub <- if (!is.null(x)) paste(.ub_call("CI.mean", x = sx, sigma = sigma), " # also prints xbar and its SE") else .ub_raw_note
  .result("Point estimate of a mean and its standard error", lines, wording, m$note, match.call(),
          estimate = m$xbar, se = m$se, n = m$n, n_needed = n_need, ubstats = ub)
}

# ---------- paired ----------

.paired <- function(x, y, dbar, sd_d, n, mean1, mean2, s1, s2, cov, r, xlab, ylab, sigma_d = NULL) {
  steps <- character(); mx <- my <- NULL
  # one measurement as raw data, the other (and the correlation / covariance) from the question
  mixed_txt <- NULL
  if (!is.null(x) && is.null(y) && !is.null(mean2)) {
    v <- .num(x, xlab); v <- v[!is.na(v)]
    mean1 <- mean(v); s1 <- sd(v); n <- n %||% length(v)
    mixed_txt <- sprintf("First measurement (%s) from the data: n = %d, mean1 = %s, s1 = %s; second measurement, correlation / covariance from the question.",
                         xlab, length(v), .f(mean1), .f(s1))
    x <- NULL
  } else if (is.null(x) && !is.null(y) && !is.null(mean1)) {
    v <- .num(y, ylab); v <- v[!is.na(v)]
    mean2 <- mean(v); s2 <- sd(v); n <- n %||% length(v)
    mixed_txt <- sprintf("Second measurement (%s) from the data: n = %d, mean2 = %s, s2 = %s; first measurement, correlation / covariance from the question.",
                         ylab, length(v), .f(mean2), .f(s2))
    y <- NULL
  }
  if (!is.null(x)) {
    if (!is.null(y)) {
      a <- .one_column(x, xlab); b <- .one_column(y, ylab)
      if (length(a) != length(b)) stop("Paired data need x and y of the same length (same units).", call. = FALSE)
      a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
      ok <- !is.na(a) & !is.na(b)
      d <- a[ok] - b[ok]; mx <- mean(a[ok]); my <- mean(b[ok])
      def <- sprintf("D = %s - %s", xlab, ylab)
      note <- if (sum(!ok)) sprintf("%d pair(s) with a missing value were removed.", sum(!ok)) else NULL
    } else {
      d <- .num(x, xlab); def <- sprintf("D = %s", xlab); note <- .dropped_note(d)
    }
    n <- length(d); dbar <- mean(d); sd_d <- sd(d)
    inputs <- sprintf("%s   n = %s   Dbar = %s   s_D = %s", def, .f(n), .f(dbar), .f(sd_d))
  } else if (!is.null(dbar)) {
    .need(sd_d, n, msg = "Give dbar = , sd_d = and n = (mean and SD of the differences).")
    def <- "D = first - second measurement"; note <- NULL
    inputs <- sprintf("Summary numbers   n = %s   Dbar = %s   s_D = %s", .f(n), .f(dbar), .f(sd_d))
  } else {
    .need(mean1, mean2, s1, s2, n,
          msg = "Give x and y (raw data), or dbar/sd_d/n, or mean1/mean2/s1/s2/n with cov = or r =.")
    if (is.null(cov) && is.null(r)) stop("Give the covariance (cov = ) or correlation (r = ) between the two measurements.", call. = FALSE)
    if (is.null(cov)) {
      cov <- r * s1 * s2
      steps <- c(steps, sprintf("Cov = r x s1 x s2 = %s x %s x %s = %s", .f(r), .f(s1), .f(s2), .f(cov)))
    }
    dbar <- mean1 - mean2
    v <- s1^2 + s2^2 - 2 * cov
    if (v <= 0) stop("Var(D) = s1^2 + s2^2 - 2 cov is not positive; check the inputs.", call. = FALSE)
    sd_d <- sqrt(v)
    steps <- c(steps,
      sprintf("Dbar = mean1 - mean2 = %s - %s = %s", .f(mean1), .f(mean2), .f(dbar)),
      sprintf("s_D = sqrt(s1^2 + s2^2 - 2 Cov) = sqrt(%s^2 + %s^2 - 2 x %s) = %s", .f(s1), .f(s2), .f(cov), .f(sd_d)))
    def <- "D = first - second measurement"; note <- mixed_txt; mx <- mean1; my <- mean2
    inputs <- sprintf("Summary numbers   n = %s   mean1 = %s   mean2 = %s", .f(n), .f(mean1), .f(mean2))
  }
  if (n < 2) stop("At least two pairs are needed.", call. = FALSE)
  if (!is.null(sigma_d)) {
    se <- sigma_d / sqrt(n)
    se_step <- sprintf("SE = sigma_D / sqrt(n) = %s / sqrt(%s) = %s   (sigma_D known)", .f(sigma_d), .f(n), .f(se))
  } else {
    se <- sd_d / sqrt(n)
    se_step <- sprintf("SE = s_D / sqrt(n) = %s / sqrt(%s) = %s", .f(sd_d), .f(n), .f(se))
  }
  list(n = n, dbar = dbar, sd_d = sd_d, sigma_d = sigma_d, se = se, def = def, note = note, inputs = inputs,
       mean_x = mx, mean_y = my, steps = c(steps, se_step))
}

#' @rdname tests
#' @export
test_paired <- function(x = NULL, y = NULL, d0 = 0, alt = "two.sided", alpha = 0.05, method = c("t", "z"),
                        dbar = NULL, sd_d = NULL, n = NULL,
                        mean1 = NULL, mean2 = NULL, s1 = NULL, s2 = NULL, cov = NULL, r = NULL, sigma_d = NULL) {
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha"); method <- match.arg(method)
  sx <- substitute(x); sy <- substitute(y)
  P <- .paired(x, y, dbar, sd_d, n, mean1, mean2, s1, s2, cov, r, .label(sx), .label(sy), sigma_d)
  known <- !is.null(sigma_d)
  dist <- if (known || method == "z") "z" else "t"; df <- if (dist == "t") P$n - 1 else NULL
  sname <- if (dist == "t") "t" else "Z"
  stat <- (P$dbar - d0) / P$se
  p <- .pval(stat, alt, dist, df); crit <- .crit(alt, alpha, dist, df)
  base <- data.frame(n = P$n)
  if (!is.null(P$mean_x)) { base$xbar <- P$mean_x; base$ybar <- P$mean_y }
  base$dbar <- P$dbar
  p_z <- p_t <- NULL
  if (known) {
    tab <- cbind(base, sigma_D = sigma_d, se = P$se, stat = stat, `p-value` = .p_cells(p)); rownames(tab) <- "Normal"
  } else {
    R <- .test_rows(P$dbar, P$se, d0, alt, P$n - 1)
    p_z <- R$p[1]; p_t <- R$p[2]
    tab <- cbind(base[c(1, 1), , drop = FALSE], s_D = P$sd_d, se = P$se, stat = R$stat, `p-value` = .p_cells(R$p))
    rownames(tab) <- rownames(R)
  }
  names(tab)[names(tab) == "`p-value`"] <- "p-value"
  lines <- c(
    .hyp_lines("mu_D", .f(d0), alt),
    sprintf("    %s   (mu_D = mu_x - mu_y)", P$def),
    "",
    sprintf("Test on mu_x - mu_y   (paired samples, variance of the differences %s)", if (known) "known" else "unknown"),
    .ci_table_lines(tab),
    "",
    P$inputs,
    sprintf("Explained below: %s", if (dist == "t") sprintf("Student t with n - 1 = %s df", .f(df)) else "normal (z)"),
    "",
    P$steps,
    sprintf("%s = (Dbar - d0) / SE = (%s - %s) / %s = %s", sname, .f(P$dbar), .f(d0), .f(P$se), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, sname)),
    sprintf("p-value = %s = %s", .p_text(alt, .dist_label(dist, df), stat), .fp(p)),
    .r_pval_line(stat, alt, dist, df),
    "",
    .decision_lines(p, alpha),
    .p_reading(p))
  wording <- c(sprintf(
    "The measurements are paired because they are repeated measurements on the same statistical units, so we work with the differences %s. Let mu_D denote the population mean difference. We test %s. The mean paired difference is %s with standard error %s, giving %s = %s%s. %s to conclude that the population mean difference is %s %s.",
    P$def, .hyp_text("mu_D", .f(d0), alt), .f(P$dbar), .f(P$se), sname, .f(stat),
    if (!is.null(df)) sprintf(" with %s degrees of freedom", .f(df)) else "",
    .decision_words(p, alpha), .alt_words(alt), .f(d0)),
    .zt_disagree(p_z, p_t, alpha),
    .p_meaning(p, sname, stat, alt, sprintf("mu_D = %s", .f(d0))),
    if (!known) .se_meaning(P$se, "the mean of the differences"))
  mdiff <- if (d0 != 0) d0
  ub <- if (!is.null(x) && !is.null(y))
          .ub_call("TEST.diffmean", x = sx, y = sy, type = "paired", sigma.d = sigma_d, mdiff0 = mdiff, alternative = .ub_alt(alt))
        else if (!is.null(x) && is.null(mean2)) paste(.ub_call("TEST.mean", x = sx, sigma = sigma_d, mu0 = d0, alternative = .ub_alt(alt)), " # on the differences")
        else .ub_raw_note
  .plot_test(stat, alt, alpha, dist, df, main = "Paired test")
  .result("Paired test (mean difference)", lines, wording, P$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = P$se, df = df, estimate = P$dbar,
          n = P$n, decision = if (p < alpha) "reject H0" else "fail to reject H0",
          p_z = p_z, p_t = p_t, table = tab, ubstats = ub)
}

#' @rdname ci
#' @export
ci_paired <- function(x = NULL, y = NULL, conf = 0.95, method = c("t", "z"),
                      dbar = NULL, sd_d = NULL, n = NULL,
                      mean1 = NULL, mean2 = NULL, s1 = NULL, s2 = NULL, cov = NULL, r = NULL, sigma_d = NULL) {
  conf <- .prob(conf, "conf"); method <- match.arg(method)
  sx <- substitute(x); sy <- substitute(y)
  P <- .paired(x, y, dbar, sd_d, n, mean1, mean2, s1, s2, cov, r, .label(sx), .label(sy), sigma_d)
  known <- !is.null(sigma_d)
  dist <- if (known || method == "z") "z" else "t"; df <- if (dist == "t") P$n - 1 else NULL
  cr <- .crit_ci(dist, conf, df)
  b <- .ci_lines(sprintf("Point estimate: Dbar = xbar - ybar = %s", .f(P$dbar)), P$steps, cr$v, cr$txt, P$se, P$dbar, conf, "mu_D")
  base <- data.frame(n = P$n)
  if (!is.null(P$mean_x)) { base$xbar <- P$mean_x; base$ybar <- P$mean_y }
  base$dbar <- P$dbar
  ci_z <- ci_t <- NULL
  if (known) {
    tab <- cbind(base, sigma_D = sigma_d, se = P$se, Lower = b$ci[1], Upper = b$ci[2]); rownames(tab) <- "Normal"
  } else {
    zt <- .zt_rows(P$dbar, P$se, conf, P$n - 1)
    tab <- cbind(base[c(1, 1), , drop = FALSE], s_D = P$sd_d, se = P$se, Lower = zt$Lower, Upper = zt$Upper)
    rownames(tab) <- rownames(zt)
    ci_z <- c(zt$Lower[1], zt$Upper[1]); ci_t <- c(zt$Lower[2], zt$Upper[2])
  }
  lines <- c(sprintf("Confidence interval for mu_x - mu_y   (paired samples, confidence level %s, variance %s)",
                     .f(conf), if (known) "known" else "unknown"),
             .ci_table_lines(tab), "", P$def, P$inputs,
             sprintf("Explained below: %s", if (dist == "t") sprintf("Student t with n - 1 = %s df", .f(P$n - 1)) else "normal (z)"),
             "", b$lines)
  wording <- c("The samples are paired (two measurements on the same units), so the interval is built on the differences D: mu_x - mu_y = mu_D.",
               if (!known) "The t interval is exact if the two populations are jointly normal; for a large sample the normal approximation is also valid (both are shown, as in UBStats).",
               .ci_wording(conf, sprintf("population mean difference (%s)", P$def), b$ci),
               "The interval concerns the population MEAN of the differences: it does not say, with any confidence, how much a specific unit changes.")
  ub <- if (!is.null(x) && !is.null(y)) .ub_call("CI.diffmean", x = sx, y = sy, type = "paired", sigma.d = sigma_d, conf.level = conf)
        else if (!is.null(x) && is.null(mean2)) paste(.ub_call("CI.mean", x = sx, sigma = sigma_d, conf.level = conf), " # the differences: same interval")
        else .ub_raw_note
  .result("Confidence interval for a paired mean difference", lines, wording, P$note, match.call(),
          estimate = P$dbar, se = P$se, critical = cr$v, margin = b$me, ci = b$ci, df = df,
          ci_z = ci_z, ci_t = ci_t, table = tab, ubstats = ub)
}

# ---------- two independent means ----------

.case_help <- paste(
  "Choose case = one of:",
  "  \"pooled\"  population variances unknown but ASSUMED EQUAL (normal populations) -> pooled t",
  "  \"welch\"   variances unknown and NOT assumed equal (normal populations)       -> Welch t",
  "  \"large\"   distribution unknown / not normal, both samples large             -> Z (CLT)",
  "  \"known\"   population sigmas known (give sigma1 = , sigma2 = )               -> Z",
  sep = "\n")

.two_means <- function(x, y, group, levels, xbar1, s1, n1, xbar2, s2, n2, sigma1, sigma2, case, xlab, ylab,
                       allow_all = FALSE) {
  note <- NULL; mixed <- NULL
  if (!is.null(x)) {
    if (!is.null(group)) {
      sp <- .split2(x, group, levels)
      a <- .num(sp$x1, sp$levels[1]); b <- .num(sp$x2, sp$levels[2])
      labs <- sp$levels; vlab <- xlab
    } else if (is.null(y) && !is.null(xbar2)) {
      # group 1 from the data, group 2 from summary numbers (e.g. last year's campaign)
      .need(n2, msg = "With summary numbers for group 2 give xbar2 = , n2 = and s2 = (or sigma2 =).")
      a <- .num(x, xlab); labs <- c(xlab, "group 2 (summary numbers)"); vlab <- "the variable"
      note <- c(.dropped_note(a), sprintf("Group 1 (%s) computed from the data: n = %d, mean = %s, s = %s; group 2 from the summary numbers.",
                                          xlab, sum(!is.na(a)), .f(mean(a, na.rm = TRUE)), .f(sd(a, na.rm = TRUE))))
      a <- a[!is.na(a)]
      n1 <- length(a); xbar1 <- mean(a); s1 <- sd(a)
      raw <- NULL; mixed <- TRUE
    } else {
      if (is.null(y)) stop("Give the second sample y = ..., the grouping variable group = ..., or group 2's summary numbers (xbar2, s2, n2).", call. = FALSE)
      a <- .num(x, xlab); b <- .num(y, ylab); labs <- c(xlab, ylab); vlab <- "the variable"
    }
    if (is.null(mixed)) {
      note <- .dropped_note(a, b)
      n1 <- length(a); n2 <- length(b); xbar1 <- mean(a); xbar2 <- mean(b); s1 <- sd(a); s2 <- sd(b)
      raw <- list(a = a[!is.na(a)], b = b[!is.na(b)])
    }
  } else {
    raw <- NULL
    .need(xbar1, n1, xbar2, n2, msg = "Give data (x, y or x + group) or xbar1, n1, xbar2, n2 with s1, s2 (or sigma1, sigma2).")
    labs <- c("group 1", "group 2"); vlab <- "the variable"
  }
  if (missing(case) || is.null(case)) {
    if (!is.null(sigma1) && !is.null(sigma2)) case <- "known"
    else if (allow_all) case <- "all"
    else stop(.case_help, call. = FALSE)
  }
  case <- match.arg(case, c("pooled", "welch", "large", "known", "all"))
  if (case == "known") {
    .need(sigma1, sigma2, msg = "case = \"known\" needs sigma1 = and sigma2 =.")
  } else if (is.null(s1) || is.null(s2)) stop("Give the sample SDs s1 = and s2 =.", call. = FALSE)
  steps <- character(); se <- dist <- df <- reason <- NULL
  if (case == "all") {
    # computed by the caller (ci_2means shows all four intervals)
  } else if (case == "pooled") {
    sp2 <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
    se <- sqrt(sp2 * (1 / n1 + 1 / n2)); dist <- "t"; df <- n1 + n2 - 2
    steps <- c(sprintf("Pooled variance sp^2 = [(n1-1)s1^2 + (n2-1)s2^2] / (n1+n2-2) = [%s x %s^2 + %s x %s^2] / %s = %s",
                       .f(n1 - 1), .f(s1), .f(n2 - 1), .f(s2), .f(df), .f(sp2)),
               sprintf("SE = sqrt(sp^2 (1/n1 + 1/n2)) = sqrt(%s x (1/%s + 1/%s)) = %s", .f(sp2), .f(n1), .f(n2), .f(se)),
               sprintf("df = n1 + n2 - 2 = %s", .f(df)))
    reason <- "The population variances are unknown but assumed equal (normal populations), so we use the pooled two-sample t procedure"
  } else if (case == "welch") {
    v1 <- s1^2 / n1; v2 <- s2^2 / n2
    se <- sqrt(v1 + v2); dist <- "t"
    df <- (v1 + v2)^2 / (v1^2 / (n1 - 1) + v2^2 / (n2 - 1))
    steps <- c(sprintf("SE = sqrt(s1^2/n1 + s2^2/n2) = sqrt(%s^2/%s + %s^2/%s) = %s", .f(s1), .f(n1), .f(s2), .f(n2), .f(se)),
               sprintf("Welch df = (s1^2/n1 + s2^2/n2)^2 / [(s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1)] = %s", .f(df)))
    reason <- "The population variances are unknown and not assumed equal (normal populations), so we use the Welch two-sample t procedure"
  } else if (case == "large") {
    se <- sqrt(s1^2 / n1 + s2^2 / n2); dist <- "z"; df <- NULL
    steps <- sprintf("SE = sqrt(s1^2/n1 + s2^2/n2) = sqrt(%s^2/%s + %s^2/%s) = %s", .f(s1), .f(n1), .f(s2), .f(n2), .f(se))
    reason <- "The populations are not assumed normal, but both samples are large, so by the Central Limit Theorem we use the normal approximation (Z)"
  } else {
    se <- sqrt(sigma1^2 / n1 + sigma2^2 / n2); dist <- "z"; df <- NULL
    steps <- sprintf("SE = sqrt(sigma1^2/n1 + sigma2^2/n2) = sqrt(%s^2/%s + %s^2/%s) = %s",
                     .f(sigma1), .f(n1), .f(sigma2), .f(n2), .f(se))
    reason <- "The population standard deviations are known, so we use the Z procedure"
  }
  tab <- data.frame(group = labs, n = c(n1, n2), mean = c(xbar1, xbar2),
                    sd = c(s1 %||% NA, s2 %||% NA))
  list(n1 = n1, n2 = n2, xbar1 = xbar1, xbar2 = xbar2, s1 = s1, s2 = s2, diff = xbar1 - xbar2, se = se, dist = dist, df = df,
       case = case, reason = reason, steps = steps, labs = labs, vlab = vlab, note = note, raw = raw, mixed = isTRUE(mixed),
       table = .table_lines(.round_df(tab)))
}

#' @rdname tests
#' @export
test_2means <- function(x = NULL, y = NULL, case, d0 = 0, alt = "two.sided", alpha = 0.05,
                        group = NULL, levels = NULL,
                        xbar1 = NULL, s1 = NULL, n1 = NULL, xbar2 = NULL, s2 = NULL, n2 = NULL,
                        sigma1 = NULL, sigma2 = NULL, var_test = NULL) {
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  sx <- substitute(x); sy <- substitute(y); sg <- substitute(group)
  M <- .two_means(x, y, group, levels, xbar1, s1, n1, xbar2, s2, n2, sigma1, sigma2,
                  if (missing(case)) NULL else case, .label(sx), .label(sy), allow_all = TRUE)
  head <- sprintf("Test on mu_x - mu_y   (independent samples)   x = %s, y = %s", M$labs[1], M$labs[2])
  lines <- c(.hyp_lines("mu_x - mu_y", .f(d0), alt), "")
  stat <- p <- crit <- se <- df <- tests <- decision <- NULL
  if (M$case != "all") {
    sname <- if (M$dist == "t") "t" else "Z"
    stat <- (M$diff - d0) / M$se; se <- M$se; df <- M$df
    p <- .pval(stat, alt, M$dist, M$df); crit <- .crit(alt, alpha, M$dist, M$df)
    decision <- if (p < alpha) "reject H0" else "fail to reject H0"
    work <- c(
      sprintf("Difference in sample means = %s - %s = %s", .f(M$xbar1), .f(M$xbar2), .f(M$diff)),
      M$steps,
      sprintf("%s = (xbar - ybar - d0) / SE = (%s - %s) / %s = %s", sname, .f(M$diff), .f(d0), .f(M$se), .f(stat)),
      sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, sname)),
      sprintf("p-value = %s = %s", .p_text(alt, .dist_label(M$dist, M$df), stat), .fp(p)),
      .r_pval_line(stat, alt, M$dist, M$df),
      "",
      .decision_lines(p, alpha),
      .p_reading(p))
    wording <- sprintf(
      "Let mu_x and mu_y denote the population means of %s for %s and %s. The two samples contain different statistical units and are therefore independent. %s. We test %s. The observed difference between the sample means is %s with standard error %s, giving %s = %s%s. %s to conclude that the population mean of %s for %s is %s that for %s.",
      M$vlab, M$labs[1], M$labs[2], M$reason, .hyp_text("mu_x - mu_y", .f(d0), alt), .f(M$diff), .f(M$se), sname, .f(stat),
      if (!is.null(M$df)) sprintf(" with %s degrees of freedom", .f(M$df, 2)) else "",
      .decision_words(p, alpha), M$vlab, M$labs[1],
      if (d0 == 0) .alt_words(alt) else sprintf("%s (by %s)", .alt_words(alt), .f(d0)), M$labs[2])
    wording <- c(wording, .p_meaning(p, sname, stat, alt, sprintf("mu_x - mu_y = %s", .f(d0))),
                 if (M$case != "known") .se_meaning(M$se, "the difference between the sample means"))
  }
  if (M$case == "known") {
    tab <- data.frame(row.names = "Normal", n_x = M$n1, n_y = M$n2, xbar = M$xbar1, ybar = M$xbar2, `xbar-ybar` = M$diff,
                      sigma_X = sigma1, sigma_Y = sigma2, se = M$se, stat = stat, `p-value` = .p_cells(p), check.names = FALSE)
    lines <- c(lines, paste(head, "  variances known"), .ci_table_lines(tab), "", work)
  } else {
    R <- .diffmean_rows(M, d0 = d0, alt = alt, alpha = alpha)
    tests <- R$intervals
    lines <- c(lines, paste(head, "  variances unknown"), "", R$lines)
    if (M$case != "all") {
      pick <- switch(M$case, pooled = 2, welch = 4, large = 3)
      lines <- c(lines, "", sprintf("Chosen (case = \"%s\"): variances %s, %s", M$case, tests$variances[pick], tests$method[pick]), work)
    } else {
      rej <- tests$p < alpha
      wording <- c(
        sprintf("The samples are independent (different units). We test %s. The four tests for mu_x - mu_y (x = %s, y = %s) differ only in their assumptions: Student-t tests are exact for normal populations (pooled variance with n_x + n_y - 2 df if the variances can be assumed equal, Welch-Satterthwaite df otherwise); for large samples the normal approximation is valid whatever the distribution, and the t test is the conservative choice.",
                .hyp_text("mu_x - mu_y", .f(d0), alt), M$labs[1], M$labs[2]),
        if (all(rej)) sprintf("At alpha = %s H0 is rejected under all four sets of assumptions: the conclusion does not depend on them.", .f(alpha))
        else if (!any(rej)) sprintf("At alpha = %s H0 is not rejected under any of the four sets of assumptions: there is insufficient empirical evidence against H0.", .f(alpha))
        else sprintf("At alpha = %s the decision depends on the assumptions (H0 rejected only for: %s): justify the case you choose.", .f(alpha),
                     paste(sprintf("%s variances %s", tests$variances[rej], tests$method[rej]), collapse = "; ")),
        "Choose the case with case = \"pooled\", \"welch\" or \"large\" to get the step-by-step working for one test.")
    }
  }
  L <- .levene_block(M, var_test)
  if (!is.null(L)) { lines <- c(lines, "", L$lines); wording <- c(wording, L$wording) }
  mdiff <- if (d0 != 0) d0
  vt <- if (!is.null(L)) TRUE
  ub <- if (is.null(x) || M$mixed) .ub_raw_note
        else if (!is.null(group)) {
          if (.ub_by_ok(group, levels) && is.null(sigma1))
            .ub_call("TEST.diffmean", x = sx, by = sg, mdiff0 = mdiff, alternative = .ub_alt(alt), var.test = vt)
          else .ub_call("TEST.diffmean", x = .ub_subset(sx, sg, M$labs[1]), y = .ub_subset(sx, sg, M$labs[2]),
                        sigma.x = sigma1, sigma.y = sigma2, mdiff0 = mdiff, alternative = .ub_alt(alt), var.test = vt)
        } else .ub_call("TEST.diffmean", x = sx, y = sy, sigma.x = sigma1, sigma.y = sigma2, mdiff0 = mdiff,
                        alternative = .ub_alt(alt), var.test = vt)
  if (!is.null(stat)) .plot_test(stat, alt, alpha, M$dist, M$df, main = "Two independent means")
  .result("Two independent means test", lines, wording, M$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = se, df = df, estimate = M$diff,
          decision = decision, tests = tests, levene = L$res, ubstats = ub)
}

# The four UBStats rows for mu_x - mu_y with unknown variances: intervals
# (conf given) or tests (alt given).
.diffmean_rows <- function(M, conf = NULL, d0 = 0, alt = NULL, alpha = 0.05) {
  n1 <- M$n1; n2 <- M$n2; s1 <- M$s1; s2 <- M$s2; d <- M$diff
  sp2 <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
  se_eq <- sqrt(sp2 / n1 + sp2 / n2); df_eq <- n1 + n2 - 2
  v1 <- s1^2 / n1; v2 <- s2^2 / n2
  se_un <- sqrt(v1 + v2); df_w <- (v1 + v2)^2 / (v1^2 / (n1 - 1) + v2^2 / (n2 - 1))
  se <- c(se_eq, se_eq, se_un, se_un)
  iv <- data.frame(variances = rep(c("equal", "different"), each = 2), method = rep(c("Normal.Approx", "Student-t"), 2),
                   df = c(NA, df_eq, NA, df_w), stringsAsFactors = FALSE)
  if (!is.null(conf)) {
    z <- qnorm((1 + conf) / 2)
    iv$crit <- c(z, qt((1 + conf) / 2, df_eq), z, qt((1 + conf) / 2, df_w))
    iv$se <- se; iv$Lower <- d - iv$crit * se; iv$Upper <- d + iv$crit * se
    tails <- c(sprintf(";  z = %s, t(df = n_x+n_y-2 = %s) = %s", .f(z), .f(df_eq), .f(iv$crit[2])), "",
               sprintf(";  t = %s", .f(iv$crit[4])))
  } else {
    iv$se <- se; iv$stat <- (d - d0) / se
    iv$p <- c(.pval(iv$stat[1], alt, "z"), .pval(iv$stat[2], alt, "t", df_eq),
              .pval(iv$stat[3], alt, "z"), .pval(iv$stat[4], alt, "t", df_w))
    iv$decision <- ifelse(iv$p < alpha, "reject H0", "fail to reject H0")
    tails <- c(sprintf(";  stat = (xbar - ybar - d0) / se = (%s - %s) / %s = %s", .f(d), .f(d0), .f(se_eq), .f(iv$stat[1])),
               sprintf(";  stat = (%s - %s) / %s = %s", .f(d), .f(d0), .f(se_un), .f(iv$stat[3])), "")
  }
  mk <- function(rows) {
    t <- data.frame(n_x = n1, n_y = n2, xbar = M$xbar1, ybar = M$xbar2, `xbar-ybar` = d, s_X = s1, s_Y = s2,
                    se = iv$se[rows], check.names = FALSE)
    if (!is.null(conf)) { t$Lower <- iv$Lower[rows]; t$Upper <- iv$Upper[rows] }
    else { t$stat <- iv$stat[rows]; t$`p-value` <- .p_cells(iv$p[rows]) }
    rownames(t) <- iv$method[rows]
    .ci_table_lines(t)
  }
  steps <- c(
    sprintf("Equal variances:  s^2_pool = [(n_x-1)s_X^2 + (n_y-1)s_Y^2] / (n_x+n_y-2) = [%s x %s^2 + %s x %s^2] / %s = %s",
            .f(n1 - 1), .f(s1), .f(n2 - 1), .f(s2), .f(df_eq), .f(sp2)),
    sprintf("                  se = sqrt(s^2_pool/n_x + s^2_pool/n_y) = %s%s", .f(se_eq), tails[1]),
    sprintf("Different:        se = sqrt(s_X^2/n_x + s_Y^2/n_y) = sqrt(%s^2/%s + %s^2/%s) = %s%s", .f(s1), .f(n1), .f(s2), .f(n2), .f(se_un), tails[2]),
    sprintf("                  Welch-Satterthwaite df = (s_X^2/n_x + s_Y^2/n_y)^2 / [(s_X^2/n_x)^2/(n_x-1) + (s_Y^2/n_y)^2/(n_y-1)] = %s%s",
            .f(df_w), tails[3]))
  list(intervals = iv, lines = c("Unknown variances assumed to be equal", mk(1:2), "",
                                 "Unknown variances assumed to be different", mk(3:4), "", steps))
}

# ---------- Levene's test (equal variances?) ----------

# As UBStats (var.test = TRUE): absolute deviations from the group MEDIANS,
# one-way ANOVA F on them with df (1, n_x + n_y - 2).
.levene <- function(a, b) {
  za <- abs(a - median(a)); zb <- abs(b - median(b))
  na <- length(a); nb <- length(b); zbar <- mean(c(za, zb))
  num <- na * (mean(za) - zbar)^2 + nb * (mean(zb) - zbar)^2
  den <- (na - 1) * var(za) + (nb - 1) * var(zb)
  Fs <- (na + nb - 2) * num / den
  list(s2_x = var(a), s2_y = var(b), F = Fs, df1 = 1, df2 = na + nb - 2, p_value = 1 - pf(Fs, 1, na + nb - 2),
       mz = c(mean(za), mean(zb)), zbar = zbar, num = num, den = den, n = c(na, nb))
}

.levene_table <- function(L) {
  .ci_table_lines(data.frame(row.names = "", s2_x = L$s2_x, s2_y = L$s2_y, `F-stat` = L$F, df1 = L$df1, df2 = L$df2,
                             `p-value` = .p_cells(L$p_value), check.names = FALSE))
}

.levene_reading <- function(p) {
  if (p < 0.05) sprintf("Levene p-value = %s < 0.05: equal variances are rejected at the 5%% level -> use the 'different variances' rows (Welch).", .fp(p))
  else sprintf("Levene p-value = %s >= 0.05: equal variances are not rejected (the test is conservative toward H0: equal variances) -> the 'equal variances' rows (pooled) are acceptable.", .fp(p))
}

# Levene block for ci_2means / test_2means: raw data only; var_test = FALSE turns it off.
.levene_block <- function(M, var_test) {
  if (isFALSE(var_test) || is.null(M$raw) || M$case == "known") return(NULL)
  L <- .levene(M$raw$a, M$raw$b)
  list(res = L,
       lines = c("Levene test for homogeneity of variance   H0: sigma2_x = sigma2_y   H1: sigma2_x != sigma2_y",
                 .levene_table(L), .levene_reading(L$p_value)),
       wording = sprintf("Levene's test for equal variances gives F = %s with (1, %s) df and p-value %s: %s",
                         .f(L$F), .f(L$df2), .fp(L$p_value),
                         if (L$p_value < 0.05) "the hypothesis of equal variances is rejected at the 5% level, so the procedure for different variances is preferred."
                         else "the hypothesis of equal variances cannot be rejected at the usual levels, so assuming equal variances is acceptable."))
}

#' @rdname tests
#' @export
test_levene <- function(x = NULL, y = NULL, group = NULL, levels = NULL, alpha = 0.05) {
  alpha <- .prob(alpha, "alpha")
  sx <- substitute(x); sy <- substitute(y); sg <- substitute(group)
  if (is.null(x)) stop("Levene's test needs the raw data of the two groups (x and y, or x and group =).", call. = FALSE)
  if (!is.null(group)) {
    sp <- .split2(x, group, levels)
    a <- .num(sp$x1, sp$levels[1]); b <- .num(sp$x2, sp$levels[2]); labs <- sp$levels
  } else {
    if (is.null(y)) stop("Give the second sample y = ..., or the grouping variable group = ...", call. = FALSE)
    a <- .num(x, .label(sx)); b <- .num(y, .label(sy)); labs <- c(.label(sx), .label(sy))
  }
  note <- .dropped_note(a, b)
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (length(a) < 2 || length(b) < 2) stop("Each group needs at least two values.", call. = FALSE)
  L <- .levene(a, b)
  crit <- qf(1 - alpha, 1, L$df2)
  lines <- c(
    "H0: sigma2_x = sigma2_y (equal population variances)      H1: sigma2_x != sigma2_y",
    sprintf("x = %s, y = %s   (independent samples)", labs[1], labs[2]),
    "",
    "Levene test for homogeneity of variance",
    .levene_table(L),
    "",
    sprintf("Step 1: absolute deviations from each group's MEDIAN: z = |x - median_x| (median %s), |y - median_y| (median %s)",
            .f(median(a)), .f(median(b))),
    sprintf("Step 2: means of z: zbar_x = %s, zbar_y = %s, overall zbar = %s", .f(L$mz[1]), .f(L$mz[2]), .f(L$zbar)),
    sprintf("Step 3: F = (n - 2) x [n_x (zbar_x - zbar)^2 + n_y (zbar_y - zbar)^2] / [(n_x - 1) s^2_zx + (n_y - 1) s^2_zy] = %s x %s / %s = %s",
            .f(L$df2), .f(L$num), .f(L$den), .f(L$F)),
    sprintf("        (a one-way ANOVA F on the deviations, df = 1 and n_x + n_y - 2 = %s)", .f(L$df2)),
    sprintf("Critical value F(%s; 1, %s) = %s  ->  reject H0 if F > %s", .f(1 - alpha), .f(L$df2), .f(crit), .f(crit)),
    sprintf("p-value = P(F(1, %s) > %s) = %s", .f(L$df2), .f(L$F), .fp(L$p_value)),
    .r_pval_line(L$F, "greater", "F", 1, L$df2),
    "",
    .decision_lines(L$p_value, alpha),
    if (L$p_value < alpha) "-> treat the variances as different: ci_2means / test_2means with case = \"welch\" (or the 'different' rows)."
    else "-> equal variances are acceptable: case = \"pooled\" (or the 'equal' rows).")
  wording <- sprintf(
    "We test H0: the two population variances are equal (the status quo, retained unless the data clearly contradict it) against H1: they differ, using Levene's test on the absolute deviations from the group medians. The statistic is F = %s with 1 and %s degrees of freedom and p-value %s. %s that the variances of the two populations differ.",
    .f(L$F), .f(L$df2), .fp(L$p_value), .decision_words(L$p_value, alpha))
  two <- !is.null(group) && length(.cats(.one_column(group, "group")[!is.na(.one_column(group, "group"))])) == 2
  ub <- if (!is.null(group)) {
          if (two) .ub_call("TEST.diffvar", x = sx, by = sg)
          else .ub_call("TEST.diffvar", x = .ub_subset(sx, sg, labs[1]), y = .ub_subset(sx, sg, labs[2]))
        } else .ub_call("TEST.diffvar", x = sx, y = sy)
  .result("Levene test (equal variances?)", lines, wording, note, match.call(),
          statistic = L$F, df = c(L$df1, L$df2), p_value = L$p_value, critical = crit, s2 = c(L$s2_x, L$s2_y),
          decision = if (L$p_value < alpha) "reject H0" else "fail to reject H0",
          ubstats = c(ub, "# same Levene test as TEST.diffmean(..., var.test = TRUE)"))
}

#' @rdname ci
#' @export
ci_2means <- function(x = NULL, y = NULL, case, conf = 0.95, group = NULL, levels = NULL,
                      xbar1 = NULL, s1 = NULL, n1 = NULL, xbar2 = NULL, s2 = NULL, n2 = NULL,
                      sigma1 = NULL, sigma2 = NULL, var_test = NULL) {
  conf <- .prob(conf, "conf")
  sx <- substitute(x); sy <- substitute(y); sg <- substitute(group)
  M <- .two_means(x, y, group, levels, xbar1, s1, n1, xbar2, s2, n2, sigma1, sigma2,
                  if (missing(case)) NULL else case, .label(sx), .label(sy), allow_all = TRUE)
  head <- sprintf("Confidence interval for mu_x - mu_y   (independent samples, confidence level %s)   x = %s, y = %s",
                  .f(conf), M$labs[1], M$labs[2])
  ci <- me <- crit <- se <- df <- NULL; intervals <- NULL
  if (M$case == "known") {
    cr <- .crit_ci("z", conf, NULL)
    b <- .ci_lines(sprintf("Point estimate: xbar - ybar = %s - %s = %s", .f(M$xbar1), .f(M$xbar2), .f(M$diff)),
                   M$steps, cr$v, cr$txt, M$se, M$diff, conf, "mu_x - mu_y")
    tab <- data.frame(row.names = "Normal", n_x = M$n1, n_y = M$n2, xbar = M$xbar1, ybar = M$xbar2, `xbar-ybar` = M$diff,
                      sigma_X = sigma1, sigma_Y = sigma2, se = M$se, Lower = b$ci[1], Upper = b$ci[2], check.names = FALSE)
    lines <- c(paste(head, "  variances known"), .ci_table_lines(tab), "", b$lines)
    ci <- b$ci; me <- b$me; crit <- cr$v; se <- M$se
    wording <- c(paste0(M$reason, "."),
                 .ci_wording(conf, sprintf("difference between the population means of %s (%s minus %s)", M$vlab, M$labs[1], M$labs[2]), ci))
    zero_txt <- ci
  } else {
    R <- .diffmean_rows(M, conf)
    intervals <- R$intervals
    lines <- c(paste(head, "  variances unknown"), "", R$lines)
    if (M$case != "all") {
      pick <- switch(M$case, pooled = 2, welch = 4, large = 3)
      cr <- list(v = intervals$crit[pick],
                 txt = if (is.na(intervals$df[pick])) sprintf("z(%s)", .f((1 + conf) / 2)) else sprintf("t(%s; df = %s)", .f((1 + conf) / 2), .f(intervals$df[pick], 2)))
      b <- .ci_lines(sprintf("Point estimate: xbar - ybar = %s - %s = %s", .f(M$xbar1), .f(M$xbar2), .f(M$diff)),
                     M$steps, cr$v, cr$txt, intervals$se[pick], M$diff, conf, "mu_x - mu_y")
      lines <- c(lines, "", sprintf("Chosen (case = \"%s\"): variances %s, %s", M$case, intervals$variances[pick], intervals$method[pick]), b$lines)
      ci <- b$ci; me <- b$me; crit <- cr$v; se <- intervals$se[pick]; df <- intervals$df[pick]
      wording <- c(paste0(M$reason, "."),
                   .ci_wording(conf, sprintf("difference between the population means of %s (%s minus %s)", M$vlab, M$labs[1], M$labs[2]), ci))
      zero_txt <- ci
    } else {
      wording <- c(
        sprintf("The samples are independent (different units). The four intervals for mu_x - mu_y (x = %s, y = %s) differ only in their assumptions: Student-t intervals are exact for normal populations (pooled variance with n_x + n_y - 2 df if the variances can be assumed equal, Welch-Satterthwaite df otherwise); for large samples the normal approximation is valid whatever the distribution, and the t interval is the conservative choice.",
                M$labs[1], M$labs[2]),
        "Whether the variances can be assumed equal is assessed with the F test of chapter 7 (or by comparing s_X and s_Y). Choose the case with case = \"pooled\", \"welch\" or \"large\" to get the step-by-step working for one interval.")
      zero_txt <- cbind(intervals$Lower, intervals$Upper)
    }
  }
  zero <- if (is.matrix(zero_txt)) {
    if (all(zero_txt[, 1] > 0 | zero_txt[, 2] < 0)) "All the intervals exclude 0: the data indicate a difference between the two population means."
    else if (all(zero_txt[, 1] <= 0 & zero_txt[, 2] >= 0)) "All the intervals contain 0: a zero difference between the population means is plausible, so the direction of the difference cannot be determined."
    else "Some intervals contain 0 and some do not: the conclusion depends on the assumptions."
  } else if (zero_txt[1] > 0 || zero_txt[2] < 0) "The interval does not contain 0, so the data indicate a difference between the two population means."
    else "The interval contains 0, so a zero difference between the population means is plausible."
  wording <- c(wording, zero,
               "The interval concerns the difference between the population AVERAGES of the two groups: it is not a statement about the difference (or the change) for a specific unit.")
  L <- .levene_block(M, var_test)
  if (!is.null(L)) { lines <- c(lines, "", L$lines); wording <- c(wording, L$wording) }
  vt <- if (!is.null(L)) TRUE
  ub <- if (is.null(x) || M$mixed) .ub_raw_note
        else if (!is.null(group)) {
          if (.ub_by_ok(group, levels) && is.null(sigma1)) .ub_call("CI.diffmean", x = sx, by = sg, conf.level = conf, var.test = vt)
          else .ub_call("CI.diffmean", x = .ub_subset(sx, sg, M$labs[1]), y = .ub_subset(sx, sg, M$labs[2]),
                        sigma.x = sigma1, sigma.y = sigma2, conf.level = conf, var.test = vt)
        } else .ub_call("CI.diffmean", x = sx, y = sy, sigma.x = sigma1, sigma.y = sigma2, conf.level = conf, var.test = vt)
  .result("Confidence interval for two independent means", lines, wording, M$note, match.call(),
          estimate = M$diff, se = se, critical = crit, margin = me, ci = ci, df = df, intervals = intervals,
          levene = L$res, ubstats = ub)
}
