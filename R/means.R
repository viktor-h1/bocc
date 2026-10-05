# ------------------------------------------------------------
# Means: one mean, paired means, two independent means.
# Every function takes raw data (any vector / expression) OR the
# summary numbers given in the question text.
# ------------------------------------------------------------

.alt_words <- function(alt) switch(alt, less = "less than", greater = "greater than", two.sided = "different from")

# ---------- one mean ----------

.one_mean <- function(x, xbar, s, n, sigma, method, xlab) {
  if (!is.null(x)) {
    v <- .num(x, xlab)
    n <- length(v); xbar <- mean(v); s <- if (n > 1) sd(v) else NA
    note <- .dropped_note(v)
    from <- sprintf("Data: %s", xlab)
  } else {
    .need(xbar, n, msg = "Give the data (x = ...) or the summary numbers xbar = , n = , and s = (or sigma = ).")
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
       case = case, reason = reason, label = label, inputs = inputs, se_txt = se_txt, note = note)
}

#' Hypothesis tests
#'
#' One function per test. Each accepts **raw data** (any vector or R
#' expression: `df$spend`, `c(12, 15, 9)`, `df$after - df$before`,
#' `subset(df, region == "North")$spend`) **or the summary numbers** given in
#' the question. Output shows H0/H1, each formula with the numbers plugged
#' in, the critical value / rejection region, the p-value, the decision, an
#' exam-wording paragraph and the call to re-run it.
#'
#' * `test_mean()`   one population mean (sigma known -> Z; unknown -> t, or Z for large samples).
#' * `test_paired()` paired / before-after mean difference, D = x - y.
#' * `test_2means()` two independent means (`case` = "pooled", "welch", "large" or "known").
#' * `test_prop()`   one population proportion (Z test, SE uses p0).
#' * `test_2props()` two independent proportions (Z test, pooled p under H0).
#'
#' @param x Data. For means: numeric values. For proportions: a logical
#'   vector (`df$loyalty == "High"`), a 0/1 vector, or a categorical vector
#'   together with `event`. In `test_2means()`/`test_2props()` this is the
#'   first group (or the whole outcome when `group` is given).
#' @param y Second sample (paired: second measurement, D = x - y;
#'   two-sample: the second group).
#' @param mu0 Null value of the population mean.
#' @param p0 Null value of the population proportion (0.25 or 25).
#' @param d0 Null value of the difference (default 0).
#' @param alt Alternative: `"<"`, `">"` or `"!="` (also `"less"`,
#'   `"greater"`, `"two.sided"`).
#' @param alpha Significance level (0.05 or 5).
#' @param method For means with sigma unknown: `"t"` (normal population,
#'   Student t) or `"z"` (large sample, normal approximation).
#' @param case Two independent means: `"pooled"` (variances unknown but
#'   assumed equal), `"welch"` (unknown, not assumed equal), `"large"`
#'   (non-normal / unknown distribution, large samples, Z) or `"known"`
#'   (population sigmas known, give `sigma1`, `sigma2`).
#' @param group,levels Grouping variable and the two groups to compare,
#'   e.g. `group = df$loyalty, levels = c("High", "Medium")`.
#' @param event Category of `x` that counts as a "success", e.g. `"High"`.
#' @param xbar,s,n,sigma Summary numbers for one mean (sigma = known
#'   population SD).
#' @param dbar,sd_d Mean and SD of the paired differences.
#' @param mean1,mean2,s1,s2,cov,r Paired summary numbers when only the two
#'   means/SDs and their covariance or correlation are given.
#' @param xbar1,n1,xbar2,n2,sigma1,sigma2 Summary numbers for two samples.
#' @param count,phat Number of successes or sample proportion (with `n`).
#' @param count1,count2,phat1,phat2 Two-sample successes / proportions.
#' @return An `sc_result` (prints itself) with `statistic`, `p_value`,
#'   `critical`, `se`, `df`, `decision`.
#' @examples
#' x <- c(52, 48, 55, 60, 49, 53, 58, 51)
#' test_mean(x, mu0 = 50, alt = ">")
#' test_mean(xbar = 52.3, s = 6.1, n = 40, mu0 = 50, alt = ">")
#' test_prop(count = 120, n = 400, p0 = 0.25, alt = ">")
#' @name tests
NULL

#' @rdname tests
#' @export
test_mean <- function(x = NULL, mu0, alt = "two.sided", alpha = 0.05, method = c("t", "z"),
                      xbar = NULL, s = NULL, n = NULL, sigma = NULL) {
  if (missing(mu0)) stop("Give the null value: mu0 = ...", call. = FALSE)
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  xlab <- .label(substitute(x))
  m <- .one_mean(x, xbar, s, n, sigma, method, xlab)
  stat <- (m$xbar - mu0) / m$se
  sname <- if (m$dist == "z") "Z" else "t"
  p <- .pval(stat, alt, m$dist, m$df)
  crit <- .crit(alt, alpha, m$dist, m$df)
  xcut <- mu0 + crit * m$se
  lines <- c(
    sprintf("H0: mu = %s      H1: mu %s %s", .f(mu0), .alt_sym(alt), .f(mu0)),
    m$inputs,
    sprintf("Case: %s", m$label),
    "",
    m$se_txt,
    sprintf("%s = (xbar - mu0) / SE = (%s - %s) / %s = %s", sname, .f(m$xbar), .f(mu0), .f(m$se), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, sname)),
    sprintf("  on the xbar scale: %s", .reject_region(alt, xcut, "xbar")),
    sprintf("p-value = %s = %s", .p_text(alt, .dist_label(m$dist, m$df), stat), .fp(p)),
    "",
    .decision_lines(p, alpha))
  wording <- sprintf(
    "Let mu denote the population mean of %s. We test H0: mu = %s against H1: mu %s %s. %s. The sample mean is %s with standard error %s, giving a test statistic %s = %s%s. %s to conclude that the population mean of %s is %s %s.",
    xlab, .f(mu0), .alt_sym(alt), .f(mu0), m$reason, .f(m$xbar), .f(m$se), sname, .f(stat),
    if (!is.null(m$df)) sprintf(" with %s degrees of freedom", .f(m$df)) else "",
    .decision_words(p, alpha), xlab, .alt_words(alt), .f(mu0))
  .plot_test(stat, alt, alpha, m$dist, m$df, main = "One-mean test")
  .result("One-mean test", lines, wording, m$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = m$se, df = m$df,
          estimate = m$xbar, n = m$n, decision = if (p < alpha) "reject H0" else "fail to reject H0")
}

#' Confidence intervals
#'
#' One function per interval. Each accepts raw data (any vector or R
#' expression) or the summary numbers from the question, and prints the
#' point estimate, standard error, critical value, margin of error, the
#' interval and exam wording.
#'
#' * `ci_mean()`    one mean (sigma known -> Z; unknown -> t, or Z for large samples).
#' * `ci_paired()`  mean of paired differences D = x - y.
#' * `ci_2means()`  difference of two independent means (`case` as in [test_2means()]).
#' * `ci_prop()`    one proportion (SE uses p-hat).
#' * `ci_2props()`  difference of two proportions (unpooled SE).
#'
#' @inheritParams tests
#' @param conf Confidence level (0.95 or 95).
#' @return An `sc_result` with `estimate`, `se`, `critical`, `margin`, `ci`.
#' @examples
#' ci_mean(xbar = 52.3, s = 6.1, n = 40, conf = 0.95)
#' ci_prop(count = 120, n = 400, conf = 90)
#' ci_2means(xbar1 = 50, s1 = 8, n1 = 35, xbar2 = 46, s2 = 9, n2 = 30, case = "welch")
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
  sprintf("Based on the observed sample, the %s is estimated to lie between %s and %s with a confidence level of %s%%. That is, %s%% of intervals built this way would contain the true parameter.",
          what, .f(ci[1]), .f(ci[2]), .f(100 * conf, 2), .f(100 * conf, 2))
}

.crit_ci <- function(dist, conf, df) {
  if (dist == "z") list(v = qnorm((1 + conf) / 2), txt = sprintf("z(%s)", .f((1 + conf) / 2)))
  else list(v = qt((1 + conf) / 2, df), txt = sprintf("t(%s; df = %s)", .f((1 + conf) / 2), .f(df, 2)))
}

#' @rdname ci
#' @export
ci_mean <- function(x = NULL, conf = 0.95, method = c("t", "z"),
                    xbar = NULL, s = NULL, n = NULL, sigma = NULL) {
  conf <- .prob(conf, "conf")
  xlab <- .label(substitute(x))
  m <- .one_mean(x, xbar, s, n, sigma, method, xlab)
  cr <- .crit_ci(m$dist, conf, m$df)
  what <- sprintf("population mean of %s", xlab)
  b <- .ci_lines(sprintf("Point estimate: xbar = %s", .f(m$xbar)), m$se_txt, cr$v, cr$txt, m$se, m$xbar, conf, "mu")
  lines <- c(m$inputs, sprintf("Case: %s", m$label), "", b$lines)
  .result("Confidence interval for a mean", lines, c(paste0(m$reason, "."), .ci_wording(conf, what, b$ci)),
          m$note, match.call(), estimate = m$xbar, se = m$se, critical = cr$v, margin = b$me, ci = b$ci, df = m$df)
}

# ---------- paired ----------

.paired <- function(x, y, dbar, sd_d, n, mean1, mean2, s1, s2, cov, r, xlab, ylab) {
  steps <- character()
  if (!is.null(x)) {
    if (!is.null(y)) {
      a <- .one_column(x, xlab); b <- .one_column(y, ylab)
      if (length(a) != length(b)) stop("Paired data need x and y of the same length (same units).", call. = FALSE)
      a <- suppressWarnings(as.numeric(a)); b <- suppressWarnings(as.numeric(b))
      ok <- !is.na(a) & !is.na(b)
      d <- a[ok] - b[ok]
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
    def <- "D = first - second measurement"; note <- NULL
    inputs <- sprintf("Summary numbers   n = %s   mean1 = %s   mean2 = %s", .f(n), .f(mean1), .f(mean2))
  }
  if (n < 2) stop("At least two pairs are needed.", call. = FALSE)
  se <- sd_d / sqrt(n)
  list(n = n, dbar = dbar, sd_d = sd_d, se = se, def = def, note = note, inputs = inputs,
       steps = c(steps, sprintf("SE = s_D / sqrt(n) = %s / sqrt(%s) = %s", .f(sd_d), .f(n), .f(se))))
}

#' @rdname tests
#' @export
test_paired <- function(x = NULL, y = NULL, d0 = 0, alt = "two.sided", alpha = 0.05, method = c("t", "z"),
                        dbar = NULL, sd_d = NULL, n = NULL,
                        mean1 = NULL, mean2 = NULL, s1 = NULL, s2 = NULL, cov = NULL, r = NULL) {
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha"); method <- match.arg(method)
  P <- .paired(x, y, dbar, sd_d, n, mean1, mean2, s1, s2, cov, r, .label(substitute(x)), .label(substitute(y)))
  dist <- if (method == "t") "t" else "z"; df <- if (method == "t") P$n - 1 else NULL
  sname <- if (dist == "t") "t" else "Z"
  stat <- (P$dbar - d0) / P$se
  p <- .pval(stat, alt, dist, df); crit <- .crit(alt, alpha, dist, df)
  lines <- c(
    sprintf("H0: mu_D = %s      H1: mu_D %s %s      (%s)", .f(d0), .alt_sym(alt), .f(d0), P$def),
    P$inputs,
    sprintf("Case: paired data (same units measured twice) -> %s", if (dist == "t") sprintf("t with n - 1 = %s df", .f(df)) else "z (large sample)"),
    "",
    P$steps,
    sprintf("%s = (Dbar - d0) / SE = (%s - %s) / %s = %s", sname, .f(P$dbar), .f(d0), .f(P$se), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, sname)),
    sprintf("p-value = %s = %s", .p_text(alt, .dist_label(dist, df), stat), .fp(p)),
    "",
    .decision_lines(p, alpha))
  wording <- sprintf(
    "The measurements are paired because they are repeated measurements on the same statistical units, so we work with the differences %s. Let mu_D denote the population mean difference. We test H0: mu_D = %s against H1: mu_D %s %s. The mean paired difference is %s with standard error %s, giving %s = %s%s. %s to conclude that the population mean difference is %s %s.",
    P$def, .f(d0), .alt_sym(alt), .f(d0), .f(P$dbar), .f(P$se), sname, .f(stat),
    if (!is.null(df)) sprintf(" with %s degrees of freedom", .f(df)) else "",
    .decision_words(p, alpha), .alt_words(alt), .f(d0))
  .plot_test(stat, alt, alpha, dist, df, main = "Paired test")
  .result("Paired test (mean difference)", lines, wording, P$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = P$se, df = df, estimate = P$dbar,
          n = P$n, decision = if (p < alpha) "reject H0" else "fail to reject H0")
}

#' @rdname ci
#' @export
ci_paired <- function(x = NULL, y = NULL, conf = 0.95, method = c("t", "z"),
                      dbar = NULL, sd_d = NULL, n = NULL,
                      mean1 = NULL, mean2 = NULL, s1 = NULL, s2 = NULL, cov = NULL, r = NULL) {
  conf <- .prob(conf, "conf"); method <- match.arg(method)
  P <- .paired(x, y, dbar, sd_d, n, mean1, mean2, s1, s2, cov, r, .label(substitute(x)), .label(substitute(y)))
  dist <- if (method == "t") "t" else "z"; df <- if (method == "t") P$n - 1 else NULL
  cr <- .crit_ci(dist, conf, df)
  b <- .ci_lines(sprintf("Point estimate: Dbar = %s", .f(P$dbar)), P$steps, cr$v, cr$txt, P$se, P$dbar, conf, "mu_D")
  lines <- c(P$def, P$inputs, "", b$lines)
  .result("Confidence interval for a paired mean difference", lines,
          .ci_wording(conf, sprintf("population mean difference (%s)", P$def), b$ci), P$note, match.call(),
          estimate = P$dbar, se = P$se, critical = cr$v, margin = b$me, ci = b$ci, df = df)
}

# ---------- two independent means ----------

.case_help <- paste(
  "Choose case = one of:",
  "  \"pooled\"  population variances unknown but ASSUMED EQUAL (normal populations) -> pooled t",
  "  \"welch\"   variances unknown and NOT assumed equal (normal populations)       -> Welch t",
  "  \"large\"   distribution unknown / not normal, both samples large             -> Z (CLT)",
  "  \"known\"   population sigmas known (give sigma1 = , sigma2 = )               -> Z",
  sep = "\n")

.two_means <- function(x, y, group, levels, xbar1, s1, n1, xbar2, s2, n2, sigma1, sigma2, case, xlab, ylab) {
  note <- NULL
  if (!is.null(x)) {
    if (!is.null(group)) {
      sp <- .split2(x, group, levels)
      a <- .num(sp$x1, sp$levels[1]); b <- .num(sp$x2, sp$levels[2])
      labs <- sp$levels; vlab <- xlab
    } else {
      if (is.null(y)) stop("Give the second sample y = ..., or the grouping variable group = ...", call. = FALSE)
      a <- .num(x, xlab); b <- .num(y, ylab); labs <- c(xlab, ylab); vlab <- "the variable"
    }
    note <- .dropped_note(a, b)
    n1 <- length(a); n2 <- length(b); xbar1 <- mean(a); xbar2 <- mean(b); s1 <- sd(a); s2 <- sd(b)
  } else {
    .need(xbar1, n1, xbar2, n2, msg = "Give data (x, y or x + group) or xbar1, n1, xbar2, n2 with s1, s2 (or sigma1, sigma2).")
    labs <- c("group 1", "group 2"); vlab <- "the variable"
  }
  if (missing(case) || is.null(case)) {
    if (!is.null(sigma1) && !is.null(sigma2)) case <- "known" else stop(.case_help, call. = FALSE)
  }
  case <- match.arg(case, c("pooled", "welch", "large", "known"))
  if (case == "known") {
    .need(sigma1, sigma2, msg = "case = \"known\" needs sigma1 = and sigma2 =.")
  } else if (is.null(s1) || is.null(s2)) stop("Give the sample SDs s1 = and s2 =.", call. = FALSE)
  steps <- character()
  if (case == "pooled") {
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
  list(n1 = n1, n2 = n2, xbar1 = xbar1, xbar2 = xbar2, diff = xbar1 - xbar2, se = se, dist = dist, df = df,
       case = case, reason = reason, steps = steps, labs = labs, vlab = vlab, note = note,
       table = .table_lines(.round_df(tab)))
}

#' @rdname tests
#' @export
test_2means <- function(x = NULL, y = NULL, case, d0 = 0, alt = "two.sided", alpha = 0.05,
                        group = NULL, levels = NULL,
                        xbar1 = NULL, s1 = NULL, n1 = NULL, xbar2 = NULL, s2 = NULL, n2 = NULL,
                        sigma1 = NULL, sigma2 = NULL) {
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  M <- .two_means(x, y, group, levels, xbar1, s1, n1, xbar2, s2, n2, sigma1, sigma2,
                  if (missing(case)) NULL else case, .label(substitute(x)), .label(substitute(y)))
  sname <- if (M$dist == "t") "t" else "Z"
  stat <- (M$diff - d0) / M$se
  p <- .pval(stat, alt, M$dist, M$df); crit <- .crit(alt, alpha, M$dist, M$df)
  lines <- c(
    sprintf("H0: mu1 - mu2 = %s      H1: mu1 - mu2 %s %s", .f(d0), .alt_sym(alt), .f(d0)),
    sprintf("Group 1 = %s,  Group 2 = %s   (independent samples, case = \"%s\")", M$labs[1], M$labs[2], M$case),
    M$table,
    "",
    sprintf("Difference in sample means = %s - %s = %s", .f(M$xbar1), .f(M$xbar2), .f(M$diff)),
    M$steps,
    sprintf("%s = (diff - d0) / SE = (%s - %s) / %s = %s", sname, .f(M$diff), .f(d0), .f(M$se), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, sname)),
    sprintf("p-value = %s = %s", .p_text(alt, .dist_label(M$dist, M$df), stat), .fp(p)),
    "",
    .decision_lines(p, alpha))
  wording <- sprintf(
    "Let mu1 and mu2 denote the population means of %s for %s and %s. The two samples contain different statistical units and are therefore independent. %s. We test H0: mu1 - mu2 = %s against H1: mu1 - mu2 %s %s. The observed difference between the sample means is %s with standard error %s, giving %s = %s%s. %s to conclude that the population mean of %s for %s is %s that for %s.",
    M$vlab, M$labs[1], M$labs[2], M$reason, .f(d0), .alt_sym(alt), .f(d0), .f(M$diff), .f(M$se), sname, .f(stat),
    if (!is.null(M$df)) sprintf(" with %s degrees of freedom", .f(M$df, 2)) else "",
    .decision_words(p, alpha), M$vlab, M$labs[1],
    if (d0 == 0) .alt_words(alt) else sprintf("%s (by %s)", .alt_words(alt), .f(d0)), M$labs[2])
  .plot_test(stat, alt, alpha, M$dist, M$df, main = "Two independent means")
  .result("Two independent means test", lines, wording, M$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = M$se, df = M$df, estimate = M$diff,
          decision = if (p < alpha) "reject H0" else "fail to reject H0")
}

#' @rdname ci
#' @export
ci_2means <- function(x = NULL, y = NULL, case, conf = 0.95, group = NULL, levels = NULL,
                      xbar1 = NULL, s1 = NULL, n1 = NULL, xbar2 = NULL, s2 = NULL, n2 = NULL,
                      sigma1 = NULL, sigma2 = NULL) {
  conf <- .prob(conf, "conf")
  M <- .two_means(x, y, group, levels, xbar1, s1, n1, xbar2, s2, n2, sigma1, sigma2,
                  if (missing(case)) NULL else case, .label(substitute(x)), .label(substitute(y)))
  cr <- .crit_ci(M$dist, conf, M$df)
  b <- .ci_lines(sprintf("Point estimate: xbar1 - xbar2 = %s - %s = %s", .f(M$xbar1), .f(M$xbar2), .f(M$diff)),
                 M$steps, cr$v, cr$txt, M$se, M$diff, conf, "mu1 - mu2")
  lines <- c(sprintf("Group 1 = %s,  Group 2 = %s   (case = \"%s\")", M$labs[1], M$labs[2], M$case), M$table, "", b$lines)
  wording <- c(paste0(M$reason, "."),
               .ci_wording(conf, sprintf("difference between the population means of %s (%s minus %s)", M$vlab, M$labs[1], M$labs[2]), b$ci),
               if (b$ci[1] > 0 || b$ci[2] < 0) "The interval does not contain 0, so the data indicate a difference between the two population means."
               else "The interval contains 0, so a zero difference between the population means is plausible.")
  .result("Confidence interval for two independent means", lines, wording, M$note, match.call(),
          estimate = M$diff, se = M$se, critical = cr$v, margin = b$me, ci = b$ci, df = M$df)
}
