# ------------------------------------------------------------
# Probability and random variables (book chapter 5): events,
# total probability and Bayes, discrete r.v.s, Binomial, Uniform,
# Normal / t / chi-square, linear combinations, joint distributions,
# sums and means of iid r.v.s (CLT), sampling distribution of p-hat.
# Quantile notation as in the book: q_alpha = x_(1 - alpha).
# ------------------------------------------------------------

#' Probabilities, quantiles and random variables
#'
#' Events and probability rules (book 5.1):
#' * `prob_events()` P(A or B), P(A and B), P(A | B), complements, independence.
#' * `prob_bayes()`  law of total probability and Bayes' theorem for a
#'   partition E_1, ..., E_n (priors) and P(B | E_i) (likelihoods).
#'
#' Distributions (book 5.2):
#' * `rv_discrete()` E(X), Var(X), SD(X), cumulative F(x), median / quartiles
#'   of a discrete random variable given its probability function.
#' * `prob_binom()`  Binomial(m, p) (Bernoulli when m = 1): P(X = x),
#'   P(X <= x), P(X > x), ..., quantiles, E = mp, Var = mp(1 - p); optional
#'   normal approximation.
#' * `prob_unif()`   Uniform(a, b): probabilities, quantiles, E, Var.
#' * `prob_normal()` P(X < a), P(X > a), P(a < X < b), quantiles and the
#'   central interval of N(mean, sd^2), with the standardisation shown.
#' * `prob_t()`, `prob_chisq()` the same for Student t and chi-square.
#'
#' Several random variables (book 5.3-5.4):
#' * `rv_lincomb()` a_1 X_1 + ... + a_n X_n + c (any n, with correlations):
#'   expected value, variance, SD, and normal probabilities / quantiles.
#'   `rv_linear()` is the two-variable form T = aX + bY + c.
#' * `rv_joint()`   joint distribution of two discrete r.v.s: marginals,
#'   conditionals, E, Var, E(XY), covariance, correlation, independence.
#' * `rv_iid()`     sum or mean of n iid variables: mean, variance, SE and
#'   probabilities / quantiles (exact if normal, CLT approximation otherwise).
#' * `rv_prop()`    sampling distribution of a sample proportion p-hat.
#'
#' @param mean,sd Mean and standard deviation of the normal distribution.
#' @param var Variance (alternative to `sd` / `sigma`).
#' @param df Degrees of freedom.
#' @param below,above Value a for P(X < a) / P(X > a) (continuous: < and <=
#'   are the same).
#' @param between Two values c(a, b) for P(a < X < b).
#' @param quantile Probability alpha: the alpha-quantile q_alpha = x_(1-alpha),
#'   the value with P(X <= x) = alpha (several allowed).
#' @param middle Probability, e.g. 0.90: the interval containing the central
#'   90% of the distribution (between the 5th and 95th percentiles).
#' @param pA,pB,pAB P(A), P(B) and P(A and B).
#' @param pA_given_B,pB_given_A,pAorB Alternatives to `pAB`.
#' @param independent `prob_events()`: assume A and B independent.
#' @param prior Probabilities of the partition events E_1, ..., E_n (named
#'   vector; % accepted). They must sum to 1.
#' @param likelihood P(B | E_i) for each event of the partition.
#' @param event Label of the observed event B (for printing).
#' @param values,probs Values of a discrete r.v. and their probabilities (or %).
#' @param size,prob Number of trials m and success probability p.
#' @param exactly,at_most,at_least,less_than,more_than Discrete
#'   probabilities P(X = x), P(X <= x), P(X >= x), P(X < x), P(X > x);
#'   `between = c(a, b)` is P(a <= X <= b) for discrete r.v.s.
#' @param normal `prob_binom()`: also show the normal approximation
#'   N(mp, mp(1 - p)) (no continuity correction, as in the book).
#' @param min,max Limits of the Uniform distribution.
#' @param a,b,c Coefficients: T = aX + bY + c (`rv_linear`); `a` is the
#'   vector of coefficients in `rv_lincomb`, `c` the constant.
#' @param mu_x,sd_x,mu_y,sd_y Means and SDs of X and Y.
#' @param var_x,var_y Variances (alternative to `sd_x`, `sd_y`).
#' @param cov,rho Covariance or correlation. In `rv_lincomb()`: a single
#'   correlation for every pair, or a correlation / covariance matrix.
#' @param mu,sigma Means and SDs (vectors in `rv_lincomb`; single values
#'   for the iid variables in `rv_iid`).
#' @param p Joint probability table for `rv_joint()` (matrix or count table
#'   with the values of X as row names and of Y as column names; counts are
#'   turned into proportions); population proportion for `rv_prop()`.
#' @param n Number of iid variables / sample size.
#' @param stat `"mean"` (sample mean) or `"sum"` (total).
#' @return An `sc_result` (prints itself).
#' @examples
#' prob_bayes(prior = c(D = 1/2000, notD = 1999/2000), likelihood = c(0.99, 0.02), event = "positive test")
#' prob_binom(size = 15, prob = 0.65, exactly = 5, more_than = 7)
#' prob_normal(mean = 71, sd = 18, below = 30, quantile = 0.9, middle = 0.9)
#' rv_discrete(values = c(0, 25, 50, 100, 200), probs = c(0.8, 0.008, 0.079, 0.068, 0.045))
#' rv_lincomb(a = c(0.6, 0.4), mu = c(0, 0), sigma = c(12, 18), rho = 0.72)
#' rv_iid(mu = 22, sigma = 9, n = 80, stat = "mean", above = 25)
#' @name probability
NULL

# ---------- distributions as objects for the shared engine ----------

.D_normal <- function(mean, sd, sd_txt = NULL) {
  std <- !(mean == 0 && sd == 1)
  args <- if (std) sprintf(", mean = %s, sd = %s", .f(mean, 6), sd_txt %||% .f(sd, 6)) else ""
  list(cdf = function(q) pnorm(q, mean, sd), qf = function(p) qnorm(p, mean, sd), dens = function(x) dnorm(x, mean, sd),
       rp = function(q) sprintf("pnorm(%s%s)", .f(q, 6), args), rq = function(p) sprintf("qnorm(%s%s)", .f(p, 6), args),
       label = sprintf("N(%s, %s^2)", .f(mean), .f(sd)), xr = mean + c(-4, 4) * sd,
       std = if (std) function(a) sprintf("   [z = (%s - %s) / %s = %s]", .f(a), .f(mean), .f(sd), .f((a - mean) / sd)),
       qstd = if (std) function(p) sprintf("   [= mu + z x sigma = %s + %s x %s]", .f(mean), .f(qnorm(p)), .f(sd)))
}
.D_t <- function(df) list(cdf = function(q) pt(q, df), qf = function(p) qt(p, df), dens = function(x) dt(x, df),
                          rp = function(q) sprintf("pt(%s, df = %s)", .f(q, 6), .f(df)), rq = function(p) sprintf("qt(%s, df = %s)", .f(p, 6), .f(df)),
                          label = sprintf("t(%s)", .f(df)), xr = c(-1, 1) * max(4, qt(0.995, df)))
.D_chisq <- function(df) list(cdf = function(q) pchisq(q, df), qf = function(p) qchisq(p, df), dens = function(x) dchisq(x, df),
                              rp = function(q) sprintf("pchisq(%s, df = %s)", .f(q, 6), .f(df)), rq = function(p) sprintf("qchisq(%s, df = %s)", .f(p, 6), .f(df)),
                              label = sprintf("chi-square(%s)", .f(df)), xr = c(0, qchisq(0.999, df)))
.D_unif <- function(a, b) {
  w <- b - a
  list(cdf = function(q) punif(q, a, b), qf = function(p) qunif(p, a, b), dens = function(x) dunif(x, a, b),
       rp = function(q) sprintf("punif(%s, min = %s, max = %s)", .f(q, 6), .f(a), .f(b)),
       rq = function(p) sprintf("qunif(%s, min = %s, max = %s)", .f(p, 6), .f(a), .f(b)),
       label = sprintf("Uniform(%s, %s)", .f(a), .f(b)), xr = c(a - 0.15 * w, b + 0.15 * w),
       std = function(x) sprintf("   [= (%s - %s) / (%s - %s), clipped to [0, 1]]", .f(x), .f(a), .f(b), .f(a)),
       qstd = function(p) sprintf("   [= a + (b - a) x alpha = %s + %s x %s]", .f(a), .f(w), .f(p)))
}

# Shared engine for continuous distributions.
.prob_engine <- function(D, below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL,
                         xname = "X", title = "") {
  s1 <- function(f, a) if (is.null(f)) "" else f(a)
  lines <- sprintf("%s ~ %s", xname, D$label)
  out <- list(); lo <- numeric(); hi <- numeric()
  for (a in below) {
    v <- D$cdf(a); out[[sprintf("P(%s < %s)", xname, .f(a))]] <- v
    lines <- c(lines, sprintf("P(%s < %s) = F(%s) = %s%s", xname, .f(a), .f(a), .f(v, 6), s1(D$std, a)),
               sprintf("  in R: %s", D$rp(a))); lo <- c(lo, -Inf); hi <- c(hi, a)
  }
  for (a in above) {
    v <- 1 - D$cdf(a); out[[sprintf("P(%s > %s)", xname, .f(a))]] <- v
    lines <- c(lines, sprintf("P(%s > %s) = 1 - F(%s) = 1 - %s = %s%s", xname, .f(a), .f(a), .f(D$cdf(a), 6), .f(v, 6), s1(D$std, a)),
               sprintf("  in R: 1 - %s", D$rp(a)))
    lo <- c(lo, a); hi <- c(hi, Inf)
  }
  if (!is.null(between)) {
    if (length(between) != 2) stop("between must be two numbers: c(a, b).", call. = FALSE)
    a <- min(between); b <- max(between); v <- D$cdf(b) - D$cdf(a)
    out[[sprintf("P(%s < %s < %s)", .f(a), xname, .f(b))]] <- v
    lines <- c(lines, sprintf("P(%s < %s < %s) = F(%s) - F(%s) = %s - %s = %s", .f(a), xname, .f(b),
                              .f(b), .f(a), .f(D$cdf(b), 6), .f(D$cdf(a), 6), .f(v, 6)),
               if (!is.null(D$std)) paste0("   ", trimws(D$std(a)), "  and  ", trimws(D$std(b))),
               sprintf("  in R: %s - %s", D$rp(b), D$rp(a)))
    lo <- c(lo, a); hi <- c(hi, b)
  }
  for (p in quantile) {
    p <- .prob(p, "quantile")
    v <- D$qf(p); out[[sprintf("q_%s", .f(p))]] <- v
    lines <- c(lines, sprintf("q_%s = x_%s (value with P(%s <= x) = %s; %s above it) = %s%s", .f(p), .f(1 - p), xname, .f(p),
                              .pct(1 - p, 1), .f(v, 6), s1(D$qstd, p)),
               sprintf("  in R: %s", D$rq(p)))
    lo <- c(lo, -Inf); hi <- c(hi, v)
  }
  if (!is.null(middle)) {
    m <- .prob(middle, "middle"); l <- D$qf((1 - m) / 2); u <- D$qf((1 + m) / 2)
    out[["middle_lower"]] <- l; out[["middle_upper"]] <- u
    lines <- c(lines, sprintf("Central %s interval (the %s most typical values): [q_%s, q_%s] = [%s, %s]",
                              .pct(m, 1), .pct(m, 1), .f((1 - m) / 2), .f((1 + m) / 2), .f(l, 6), .f(u, 6)),
               sprintf("  in R: %s and %s", D$rq((1 - m) / 2), D$rq((1 + m) / 2)))
    lo <- c(lo, l); hi <- c(hi, u)
  }
  if (!length(out)) stop("Say what you need: below = , above = , between = c(a, b), quantile = or middle = .", call. = FALSE)
  .plot_area(D, lo, hi, main = paste(title, D$label), xlab = xname)
  list(lines = lines, values = unlist(out))
}

.has_query <- function(...) any(!vapply(list(...), is.null, logical(1)))

# Wording for probabilities and quantiles: result + meaning, read on the units or,
# for a sampling distribution, on all possible samples of size n.
.prob_wording <- function(vals, X, samples = NULL) {
  if (is.null(vals) || !length(vals)) return(NULL)
  nm <- names(vals); res <- character(); mean_txt <- character()

  for (i in seq_along(vals)) {
    k <- nm[i]; v <- unname(vals[[i]])
    if (startsWith(k, "P(")) {
      cond <- sub("^P\\((.*)\\)$", "\\1", k)
      res <- c(res, sprintf("%s = %s", k, .f(v)))
      mean_txt <- c(mean_txt, if (is.null(samples)) sprintf("The probability that %s is %s (about %s).", cond, .f(v), .pct(v, 1))
                              else sprintf("The probability that %s is %s: in about %s of all possible samples of size %s, %s.", cond, .f(v), .pct(v, 1), samples, cond))
    } else if (startsWith(k, "q_")) {
      a <- as.numeric(sub("q_", "", k))
      res <- c(res, sprintf("%s = %s", k, .f(v)))
      mean_txt <- c(mean_txt, sprintf("%s = %s: %s is at most %s with probability %s (%s of the values lie above it).", k, .f(v), X, .f(v), .f(a), .pct(1 - a, 1)))
    }
  }
  if (all(c("middle_lower", "middle_upper") %in% nm)) {
    lo <- vals[["middle_lower"]]; hi <- vals[["middle_upper"]]
    res <- c(res, sprintf("central interval [%s, %s]", .f(lo), .f(hi)))
    mean_txt <- c(mean_txt, sprintf("The central (most typical) values of %s lie between %s and %s, with the same probability in each tail.", X, .f(lo), .f(hi)))
  }
  c(.w("result", paste(res, collapse = "; ")), .w("meaning", mean_txt))
}

.dist_wording <- function(frame, tool, vals, X, samples = NULL, caveat = NULL)
  c(.w("frame", frame), .w("tool", tool), .prob_wording(vals, X, samples), .w("caveat", caveat))

#' @rdname probability
#' @export
prob_normal <- function(mean = 0, sd = NULL, below = NULL, above = NULL, between = NULL, quantile = NULL,
                        middle = NULL, var = NULL) {
  sd <- sd %||% (if (!is.null(var)) sqrt(var) else 1)
  if (sd <= 0) stop("sd must be positive.", call. = FALSE)
  E <- .prob_engine(.D_normal(mean, sd), below, above, between, quantile, middle, "X", "Normal")
  wd <- .dist_wording(sprintf("X is a normal random variable with mean %s and standard deviation %s: X ~ N(%s, %s^2).", .f(mean), .f(sd), .f(mean), .f(sd)),
                      "Standardise: Z = (X - mu) / sigma ~ N(0, 1); probabilities come from the distribution function (pnorm in R), percentiles from its inverse (qnorm).",
                      E$values, "X")
  .result("Normal probability", E$lines, wd, NULL, match.call(), values = E$values)
}

#' @rdname probability
#' @export
prob_t <- function(df, below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL) {
  E <- .prob_engine(.D_t(df), below, above, between, quantile, middle, "T", "Student")
  wd <- .dist_wording(sprintf("T follows a Student t distribution with %s degrees of freedom.", .f(df)),
                      "The t distribution is symmetric around 0 with heavier tails than N(0, 1) (closer to it as the df grow); probabilities from pt, quantiles from qt.",
                      E$values, "T")
  .result("Student t probability", E$lines, wd, NULL, match.call(), values = E$values)
}

#' @rdname probability
#' @export
prob_chisq <- function(df, below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL) {
  E <- .prob_engine(.D_chisq(df), below, above, between, quantile, middle, "X2", "")
  wd <- .dist_wording(sprintf("X2 follows a chi-square distribution with %s degrees of freedom.", .f(df)),
                      "The chi-square distribution takes only positive values and is right-skewed; probabilities from pchisq, quantiles from qchisq.",
                      E$values, "X2")
  .result("Chi-square probability", E$lines, wd, NULL, match.call(), values = E$values)
}

#' @rdname probability
#' @export
prob_unif <- function(min, max, below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL) {
  if (missing(min) || missing(max) || max <= min) stop("Give min = a and max = b with a < b.", call. = FALSE)
  a <- min; b <- max
  lines <- c(sprintf("X ~ Uniform(%s, %s): density f(x) = 1 / (b - a) = 1 / %s = %s for %s <= x <= %s",
                     .f(a), .f(b), .f(b - a), .f(1 / (b - a), 6), .f(a), .f(b)),
             sprintf("E(X) = (a + b) / 2 = %s     Var(X) = (b - a)^2 / 12 = %s     SD = %s     median = E(X)",
                     .f((a + b) / 2), .f((b - a)^2 / 12), .f(sqrt((b - a)^2 / 12))))
  vals <- NULL
  if (.has_query(below, above, between, quantile, middle)) {
    E <- .prob_engine(.D_unif(a, b), below, above, between, quantile, middle, "X", "")
    lines <- c(lines, "", "Probabilities = width of the interval x density:", E$lines[-1]); vals <- E$values
  }
  .result("Uniform distribution", lines,
          "With a Uniform distribution the probability of an interval depends only on its width (no concentration of probability anywhere in [a, b]).",
          NULL, match.call(), mean = (a + b) / 2, variance = (b - a)^2 / 12, values = vals)
}

# ---------- events (5.1) ----------

#' @rdname probability
#' @export
prob_events <- function(pA, pB, pAB = NULL, pA_given_B = NULL, pB_given_A = NULL, pAorB = NULL, independent = FALSE) {
  pA <- .prob_or_01(pA, "pA"); pB <- .prob_or_01(pB, "pB")
  how <- NULL
  if (!is.null(pAB)) { pAB <- .prob_or_01(pAB, "pAB"); how <- "given" }
  else if (!is.null(pA_given_B)) { pAB <- .prob_or_01(pA_given_B, "pA_given_B") * pB; how <- sprintf("P(A and B) = P(A | B) P(B) = %s x %s", .f(pA_given_B), .f(pB)) }
  else if (!is.null(pB_given_A)) { pAB <- .prob_or_01(pB_given_A, "pB_given_A") * pA; how <- sprintf("P(A and B) = P(B | A) P(A) = %s x %s", .f(pB_given_A), .f(pA)) }
  else if (!is.null(pAorB)) { pAB <- pA + pB - .prob_or_01(pAorB, "pAorB"); how <- sprintf("P(A and B) = P(A) + P(B) - P(A or B) = %s + %s - %s", .f(pA), .f(pB), .f(pAorB)) }
  else if (isTRUE(independent)) { pAB <- pA * pB; how <- sprintf("independent: P(A and B) = P(A) P(B) = %s x %s", .f(pA), .f(pB)) }
  else stop("Give P(A and B) as pAB =, or pA_given_B =, pB_given_A =, pAorB =, or independent = TRUE.", call. = FALSE)
  if (pAB > min(pA, pB) + 1e-12 || pA + pB - pAB > 1 + 1e-12 || pAB < -1e-12)
    stop("These probabilities are not consistent (P(A and B) must be <= P(A), P(B) and P(A or B) <= 1).", call. = FALSE)
  pU <- pA + pB - pAB
  indep <- abs(pAB - pA * pB) < 1e-9
  lines <- c(
    sprintf("P(A) = %s   P(B) = %s   P(A and B) = %s%s", .f(pA), .f(pB), .f(pAB), if (how != "given") paste0("   [", how, "]") else ""), "",
    sprintf("Complements:  P(A^c) = 1 - P(A) = %s     P(B^c) = 1 - P(B) = %s", .f(1 - pA), .f(1 - pB)),
    sprintf("Union:        P(A or B) = P(A) + P(B) - P(A and B) = %s + %s - %s = %s", .f(pA), .f(pB), .f(pAB), .f(pU)),
    sprintf("Neither:      P(A^c and B^c) = 1 - P(A or B) = %s", .f(1 - pU)),
    sprintf("Only A:       P(A and B^c) = P(A) - P(A and B) = %s     only B: P(A^c and B) = %s", .f(pA - pAB), .f(pB - pAB)),
    if (pB > 0) sprintf("Conditional:  P(A | B) = P(A and B) / P(B) = %s / %s = %s", .f(pAB), .f(pB), .f(pAB / pB)),
    if (pA > 0) sprintf("              P(B | A) = P(A and B) / P(A) = %s / %s = %s", .f(pAB), .f(pA), .f(pAB / pA)),
    if (pB < 1) sprintf("              P(A | B^c) = P(A and B^c) / P(B^c) = %s / %s = %s", .f(pA - pAB), .f(1 - pB), .f((pA - pAB) / (1 - pB))), "",
    sprintf("Independence: P(A) P(B) = %s x %s = %s %s P(A and B) = %s  ->  A and B are %s",
            .f(pA), .f(pB), .f(pA * pB), if (indep) "=" else "!=", .f(pAB), if (indep) "INDEPENDENT" else "NOT independent"),
    sprintf("Mutually exclusive (P(A and B) = 0): %s", if (abs(pAB) < 1e-12) "yes" else "no"))
  .result("Events and probability rules", lines,
          "Two events are independent when P(A and B) = P(A) P(B), i.e. P(A | B) = P(A): knowing that B occurred does not change the probability of A. Mutually exclusive events (that cannot occur together) are NOT independent unless one of them has probability 0.",
          NULL, match.call(), union = pU, intersection = pAB, A_given_B = if (pB > 0) pAB / pB, B_given_A = if (pA > 0) pAB / pA,
          independent = indep)
}

#' @rdname probability
#' @export
prob_bayes <- function(prior, likelihood, event = "B") {
  if (length(prior) != length(likelihood)) stop("prior and likelihood need one value per event of the partition.", call. = FALSE)
  if (all(prior >= 0) && abs(sum(prior) - 100) < 1e-6) prior <- prior / 100
  if (any(prior < 0) || abs(sum(prior) - 1) > 1e-6)
    stop("The prior probabilities of the partition must sum to 1 (the events must be exhaustive and mutually exclusive). They sum to ",
         .f(sum(prior)), ".", call. = FALSE)
  if (all(likelihood >= 0) && any(likelihood > 1) && all(likelihood <= 100)) likelihood <- likelihood / 100
  if (any(likelihood < 0 | likelihood > 1)) stop("likelihood values P(B | E_i) must be between 0 and 1.", call. = FALSE)
  labs <- names(prior) %||% paste0("E", seq_along(prior))
  joint <- prior * likelihood; pB <- sum(joint)
  post <- joint / pB
  postc <- prior * (1 - likelihood) / (1 - pB)
  tab <- data.frame(event = labs, prior = prior, likelihood = likelihood, joint = joint, posterior = post,
                    posterior_if_not = postc, check.names = FALSE)
  names(tab) <- c("E_i", "P(E_i)", sprintf("P(%s|E_i)", event), sprintf("P(%s,E_i)", event), sprintf("P(E_i|%s)", event),
                  sprintf("P(E_i|not %s)", event))
  terms <- sprintf("%s x %s", .f(likelihood), .f(prior))
  shown <- tab; for (j in 2:ncol(shown)) shown[[j]] <- .f(shown[[j]], 6)
  lines <- c(
    sprintf("Partition: %s (exhaustive and mutually exclusive)   Observed event: %s", paste(labs, collapse = ", "), event), "",
    .table_lines(shown), "",
    sprintf("Law of total probability: P(%s) = sum P(%s | E_i) P(E_i) = %s = %s",
            event, event, paste(terms, collapse = " + "), .f(pB, 6)),
    sprintf("P(not %s) = 1 - P(%s) = %s", event, event, .f(1 - pB, 6)),
    sprintf("Bayes' theorem: P(E_k | %s) = P(%s | E_k) P(E_k) / P(%s)", event, event, event),
    sprintf("   %s", sprintf("P(%s | %s) = %s / %s = %s", labs, event, .f(joint, 6), .f(pB, 6), .f(post, 6))),
    sprintf("   given NOT %s: P(E_k | not %s) = P(not %s | E_k) P(E_k) / P(not %s)", event, event, event, event),
    sprintf("   %s", sprintf("P(%s | not %s) = %s x %s / %s = %s", labs, event, .f(1 - likelihood), .f(prior), .f(1 - pB, 6), .f(postc, 6))))
  best <- labs[which.max(post)]
  wording <- c(
    sprintf("By the law of total probability P(%s) = %s. Bayes' theorem updates the prior probabilities P(E_i) with the evidence that %s occurred: the most likely event given %s is %s (posterior %s), which need not be the one with the highest P(%s | E_i) (%s) because the priors also matter.",
            event, .f(pB), event, event, best, .f(max(post)), event, labs[which.max(likelihood)]))
  .result("Total probability and Bayes' theorem", lines, wording, NULL, match.call(),
          p_event = pB, posterior = stats::setNames(post, labs), posterior_not = stats::setNames(postc, labs), table = tab)
}

# ---------- discrete r.v.s (5.2.1-5.2.2) ----------

.disc_quantile <- function(values, probs, alpha) values[which(cumsum(probs) >= alpha - 1e-9)[1]]

#' @rdname probability
#' @export
rv_discrete <- function(values, probs, at_most = NULL, at_least = NULL) {
  if (length(values) != length(probs)) stop("values and probs must have the same length.", call. = FALSE)
  if (all(probs >= 0) && abs(sum(probs) - 100) < 1e-6) probs <- probs / 100
  if (any(probs < 0) || abs(sum(probs) - 1) > 1e-6) stop("probs must be non-negative and sum to 1 (or 100%).", call. = FALSE)
  o <- order(values); values <- values[o]; probs <- probs[o]
  mu <- sum(values * probs); ex2 <- sum(values^2 * probs); v <- ex2 - mu^2
  tab <- data.frame(x = values, p_x = probs, x_p = values * probs, x2_p = values^2 * probs, F_x = cumsum(probs))
  q <- vapply(c(0.25, 0.5, 0.75), function(a) .disc_quantile(values, probs, a), numeric(1))
  lines <- c(.table_lines(.round_df(tab)), "",
             sprintf("E(X)   = sum x p(x)   = %s = %s", .expand_sum(values, probs), .f(mu)),
             sprintf("E(X^2) = sum x^2 p(x) = %s", .f(ex2)),
             sprintf("Var(X) = E(X^2) - E(X)^2 = %s - %s^2 = %s", .f(ex2), .f(mu), .f(v)),
             sprintf("SD(X)  = sqrt(Var(X)) = %s", .f(sqrt(v))),
             sprintf("Median = %s   Q1 = %s   Q3 = %s   (smallest x with F(x) >= 0.5 / 0.25 / 0.75)", .f(q[2]), .f(q[1]), .f(q[3])))
  vals <- list()
  if (!is.null(at_most)) { vals$at_most <- sum(probs[values <= at_most]); lines <- c(lines, sprintf("P(X <= %s) = F(%s) = %s", .f(at_most), .f(at_most), .f(vals$at_most))) }
  if (!is.null(at_least)) { vals$at_least <- sum(probs[values >= at_least]); lines <- c(lines, sprintf("P(X >= %s) = 1 - P(X < %s) = %s", .f(at_least), .f(at_least), .f(vals$at_least))) }
  .with_plot(function() plot(values, probs, type = "h", lwd = 6, col = "grey40", xlab = "x", ylab = "p(x)",
                             main = "Probability function", las = 1, ylim = c(0, max(probs) * 1.05)))
  .result("Discrete random variable", lines,
          c(.w("frame", sprintf("X is a discrete random variable with %d possible values and the given probabilities (they sum to 1).", length(values))),
            .w("tool", "E(X) = sum x p(x) (average of the values weighted by their probabilities); Var(X) = E(X^2) - E(X)^2; the median is the smallest x with F(x) >= 0.5."),
            .w("result", sprintf("E(X) = %s, Var(X) = %s, SD(X) = %s, median %s.", .f(mu), .f(v), .f(sqrt(v)), .f(q[2])),
               if (length(vals)) paste(c(if (!is.null(vals$at_most)) sprintf("P(X <= %s) = %s", .f(at_most), .f(vals$at_most)),
                                         if (!is.null(vals$at_least)) sprintf("P(X >= %s) = %s", .f(at_least), .f(vals$at_least))), collapse = "; ")),
            .w("meaning", sprintf("The expected value %s is the long-run average of X over many repetitions; the standard deviation %s measures the expected distance of X from it.",
                                  .f(mu), .f(sqrt(v))))),
          NULL, match.call(), mean = mu, variance = v, sd = sqrt(v), median = q[2], quartiles = q[c(1, 3)], table = tab, values = vals)
}

#' @rdname probability
#' @export
prob_binom <- function(size, prob, exactly = NULL, at_most = NULL, at_least = NULL, less_than = NULL, more_than = NULL,
                       between = NULL, quantile = NULL, normal = FALSE) {
  m <- size; p <- .prob_or_01(prob, "prob")
  if (m < 1 || m != round(m)) stop("size (number of trials m) must be a positive whole number.", call. = FALSE)
  mu <- m * p; v <- m * p * (1 - p)
  dist <- if (m == 1) sprintf("Bernoulli(%s)", .f(p)) else sprintf("Binomial(m = %d, p = %s)", m, .f(p))
  lines <- c(sprintf("X ~ %s: number of successes in %d independent trials, each with success probability %s", dist, m, .f(p)),
             sprintf("p(x) = C(m, x) p^x (1 - p)^(m - x),  C(m, x) = m! / (x! (m - x)!),  x = 0, 1, ..., %d", m),
             sprintf("E(X) = m p = %s     Var(X) = m p (1 - p) = %s     SD = %s     median = %s (qbinom(0.5, %d, %s))",
                     .f(mu), .f(v), .f(sqrt(v)), .f(qbinom(0.5, m, p)), m, .f(p)), "")
  vals <- list(); hl <- integer()
  nz <- function(nm, val, txt) { vals[[nm]] <<- val; lines <<- c(lines, txt) }
  for (x in exactly) {
    nz(sprintf("P(X = %s)", x), dbinom(x, m, p),
       sprintf("P(X = %s) = C(%d, %s) %s^%s %s^%s = %s x %s = %s   [dbinom(%s, %d, %s)]", x, m, x, .f(p), x, .f(1 - p), m - x,
               .f(choose(m, x)), .f(p^x * (1 - p)^(m - x), 8), .f(dbinom(x, m, p), 8), x, m, .f(p)))
    hl <- c(hl, x)
  }
  for (x in at_most) { nz(sprintf("P(X <= %s)", x), pbinom(x, m, p), sprintf("P(X <= %s) = F(%s) = %s   [pbinom(%s, %d, %s)]", x, x, .f(pbinom(x, m, p), 6), x, m, .f(p))); hl <- c(hl, 0:floor(x)) }
  for (x in less_than) { nz(sprintf("P(X < %s)", x), pbinom(ceiling(x) - 1, m, p), sprintf("P(X < %s) = P(X <= %s) = %s   [pbinom(%s, %d, %s)]", x, ceiling(x) - 1, .f(pbinom(ceiling(x) - 1, m, p), 6), ceiling(x) - 1, m, .f(p))); hl <- c(hl, seq_len(ceiling(x)) - 1) }
  for (x in more_than) { nz(sprintf("P(X > %s)", x), 1 - pbinom(x, m, p), sprintf("P(X > %s) = 1 - P(X <= %s) = %s   [1 - pbinom(%s, %d, %s)]", x, floor(x), .f(1 - pbinom(x, m, p), 6), floor(x), m, .f(p))); hl <- c(hl, (floor(x) + 1):m) }
  for (x in at_least) { nz(sprintf("P(X >= %s)", x), 1 - pbinom(ceiling(x) - 1, m, p), sprintf("P(X >= %s) = 1 - P(X <= %s) = %s   [1 - pbinom(%s, %d, %s)]", x, ceiling(x) - 1, .f(1 - pbinom(ceiling(x) - 1, m, p), 6), ceiling(x) - 1, m, .f(p))); hl <- c(hl, ceiling(x):m) }
  if (!is.null(between)) {
    a <- min(between); b <- max(between); val <- pbinom(b, m, p) - pbinom(ceiling(a) - 1, m, p)
    nz("between", val, sprintf("P(%s <= X <= %s) = F(%s) - F(%s) = %s", a, b, b, ceiling(a) - 1, .f(val, 6))); hl <- c(hl, ceiling(a):floor(b))
  }
  for (a in quantile) {
    a <- .prob(a, "quantile"); qv <- qbinom(a, m, p)
    nz(sprintf("q_%s", .f(a)), qv, sprintf("q_%s = smallest x with F(x) >= %s = %s   [qbinom(%s, %d, %s)]", .f(a), .f(a), qv, .f(a), m, .f(p)))
  }
  notes <- NULL
  if (normal) {
    sd0 <- sqrt(v)
    lines <- c(lines, "", sprintf("Normal approximation (CLT): X ~ approx. N(m p, m p (1 - p)) = N(%s, %s)   [np(1-p) = %s]", .f(mu), .f(v), .f(v)))
    for (x in at_most) lines <- c(lines, sprintf("  P(X <= %s) ~ pnorm(%s, %s, %s) = %s", x, x, .f(mu), .f(sd0), .f(pnorm(x, mu, sd0), 6)))
    for (x in more_than) lines <- c(lines, sprintf("  P(X > %s) ~ 1 - pnorm(%s, %s, %s) = %s", x, x, .f(mu), .f(sd0), .f(1 - pnorm(x, mu, sd0), 6)))
    for (a in quantile) lines <- c(lines, sprintf("  q_%s ~ qnorm(%s, %s, %s) = %s", .f(a), .f(a), .f(mu), .f(sd0), .f(qnorm(a, mu, sd0), 6)))
    if (v < 9) notes <- "m p (1 - p) is small: the normal approximation may be poor (thresholds of 5 to 9 are suggested)."
  }
  .with_plot(function() {
    xs <- 0:m; px <- dbinom(xs, m, p)
    keep <- px > 1e-4 * max(px) | xs %in% hl
    plot(xs[keep], px[keep], type = "h", lwd = 4, col = ifelse(xs[keep] %in% hl, "firebrick", "grey50"), xlab = "x", ylab = "p(x)",
         main = dist, las = 1, ylim = c(0, max(px) * 1.05))
  })
  .result(if (m == 1) "Bernoulli distribution" else "Binomial distribution", lines,
          "The Binomial model requires independent trials (draws with replacement, or a population large enough) with the same success probability p in every trial.",
          notes, match.call(), mean = mu, variance = v, values = vals)
}

# ---------- several r.v.s (5.3-5.4) ----------

#' @rdname probability
#' @export
rv_lincomb <- function(a, mu, sigma = NULL, var = NULL, rho = 0, cov = NULL, c = 0,
                       below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL) {
  k <- length(a)
  mu <- rep_len(mu, k)
  s <- if (!is.null(sigma)) rep_len(sigma, k) else if (!is.null(var)) sqrt(rep_len(var, k)) else stop("Give sigma = (SDs) or var = (variances).", call. = FALSE)
  if (!is.null(cov)) {
    S <- if (is.matrix(cov)) cov else { M <- matrix(cov, k, k); diag(M) <- s^2; M }
  } else {
    R <- if (is.matrix(rho)) rho else { M <- matrix(rho, k, k); diag(M) <- 1; M }
    S <- R * outer(s, s)
  }
  if (!all(dim(S) == k)) stop("The correlation / covariance matrix must be ", k, " x ", k, ".", call. = FALSE)
  m <- sum(a * mu) + c
  v <- as.numeric(t(a) %*% S %*% a)
  if (v < 0) stop("The variance came out negative; check the correlations.", call. = FALSE)
  nm <- paste0("X", seq_len(k))
  lin <- paste(sprintf("%s %s", .f(a), nm), collapse = " + ")
  lines <- c(sprintf("T = %s%s", lin, if (c != 0) sprintf(" %s %s", if (c < 0) "-" else "+", .f(abs(c))) else ""),
             sprintf("E(T) = sum a_i mu_i + c = %s%s = %s", paste(sprintf("%s x %s", .f(a), .f(mu)), collapse = " + "),
                     if (c != 0) sprintf(" + %s", .f(c)) else "", .f(m)))
  vt <- sprintf("%s^2 x %s", .f(a), .f(s^2))
  ct <- character()
  if (k > 1) for (i in 1:(k - 1)) for (j in (i + 1):k) if (S[i, j] != 0)
    ct <- c(ct, sprintf("2 x %s x %s x %s", .f(a[i]), .f(a[j]), .f(S[i, j])))
  if (k > 1) {
    covs <- character()
    for (i in 1:(k - 1)) for (j in (i + 1):k) if (S[i, j] != 0)
      covs <- c(covs, sprintf("Cov(X%d, X%d) = rho x sigma_%d x sigma_%d = %s x %s x %s = %s", i, j, i, j,
                              .f(S[i, j] / (s[i] * s[j])), .f(s[i]), .f(s[j]), .f(S[i, j])))
    lines <- c(lines, covs)
  }
  lines <- c(lines,
             sprintf("Var(T) = sum a_i^2 sigma_i^2 + 2 sum a_i a_j Cov(X_i, X_j) = %s = %s", paste(c(vt, ct), collapse = " + "), .f(v)),
             sprintf("SD(T) = sqrt(%s) = %s", .f(v), .f(sqrt(v))))
  if (k > 1 && all(S[upper.tri(S)] == 0)) lines <- c(lines, "(independent / uncorrelated variables: no covariance terms)")
  vals <- NULL
  if (.has_query(below, above, between, quantile, middle)) {
    E <- .prob_engine(.D_normal(m, sqrt(v)), below, above, between, quantile, middle, "T", "T ~")
    lines <- c(lines, "", "Assuming T is normal (the X_i jointly normal, e.g. independent normal variables):", E$lines[-1]); vals <- E$values
  }
  .result("Linear combination of random variables", lines,
          c(.dist_wording("T is a linear combination of the random variables, T = sum a_i X_i + c.",
                          "E(T) = sum a_i E(X_i) + c and Var(T) = sum a_i^2 Var(X_i) + 2 sum a_i a_j Cov(X_i, X_j) follow from the means, variances and covariances alone; probabilities and quantiles of T also need its distribution: if the variables are (jointly) normal, any linear combination is normal.",
                          vals, "T"),
            .w("result", sprintf("E(T) = %s, Var(T) = %s, SD(T) = %s.", .f(m), .f(v), .f(sqrt(v))))),
          NULL, match.call(), mean = m, variance = v, sd = sqrt(v), values = vals)
}

#' @rdname probability
#' @export
rv_linear <- function(a = 1, mu_x, sd_x = NULL, b = 0, mu_y = 0, sd_y = 0, c = 0, cov = NULL, rho = 0,
                      var_x = NULL, var_y = NULL, below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL) {
  if (missing(mu_x)) stop("Give mu_x = (and sd_x = or var_x =).", call. = FALSE)
  vx <- var_x %||% (if (is.null(sd_x)) stop("Give sd_x = or var_x =.", call. = FALSE) else sd_x^2)
  vy <- var_y %||% sd_y^2
  cxy <- cov %||% (rho * sqrt(vx) * sqrt(vy))
  mu <- a * mu_x + b * mu_y + c
  v <- a^2 * vx + b^2 * vy + 2 * a * b * cxy
  if (v < 0) stop("The variance came out negative; check the inputs.", call. = FALSE)
  sgn <- function(v, what = "") sprintf(" %s %s%s", if (v < 0) "-" else "+", .f(abs(v)), what)
  lines <- c(sprintf("T = %s X%s%s", .f(a), if (b != 0) sgn(b, " Y") else "", if (c != 0) sgn(c) else ""),
             if (b != 0 && is.null(cov)) sprintf("Cov(X, Y) = rho x SD(X) x SD(Y) = %s x %s x %s = %s", .f(rho), .f(sqrt(vx)), .f(sqrt(vy)), .f(cxy)),
             sprintf("E(T) = a E(X) + b E(Y) + c = %s x %s + %s x %s + %s = %s", .f(a), .f(mu_x), .f(b), .f(mu_y), .f(c), .f(mu)),
             sprintf("Var(T) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X,Y) = %s^2 x %s + %s^2 x %s + 2 x %s x %s x %s = %s",
                     .f(a), .f(vx), .f(b), .f(vy), .f(a), .f(b), .f(cxy), .f(v)),
             sprintf("SD(T) = sqrt(Var(T)) = %s%s", .f(sqrt(v)), if (b == 0) sprintf("   (= |a| SD(X) = %s x %s)", .f(abs(a)), .f(sqrt(vx))) else ""))
  vals <- NULL
  if (.has_query(below, above, between, quantile, middle)) {
    E <- .prob_engine(.D_normal(mu, sqrt(v)), below, above, between, quantile, middle, "T", "T ~")
    lines <- c(lines, "", "Assuming T is normal (X, Y jointly normal):", E$lines[-1]); vals <- E$values
  }
  .result("Linear combination of random variables", lines,
          c(.dist_wording("T = a X + b Y + c is a linear transformation / combination of random variables.",
                          "E(T) = a E(X) + b E(Y) + c and Var(T) = a^2 Var(X) + b^2 Var(Y) + 2ab Cov(X, Y) (an additive constant changes the mean, not the variance); with (jointly) normal variables T is normal.",
                          vals, "T"),
            .w("result", sprintf("E(T) = %s, Var(T) = %s, SD(T) = %s.", .f(mu), .f(v), .f(sqrt(v))))),
          NULL, match.call(), mean = mu, variance = v, sd = sqrt(v), covariance = cxy, values = vals)
}

#' @rdname probability
#' @export
rv_joint <- function(p) {
  m <- .as_count_table(p)
  if (is.null(m)) stop("Give the joint probability table as a matrix (values of X as row names, of Y as column names) or a count table.", call. = FALSE)
  tot <- sum(m)
  notes <- NULL
  if (abs(tot - 1) > 1e-6) { notes <- sprintf("The table summed to %s: it was divided by its total.", .f(tot)); m <- m / tot }
  dn <- names(dimnames(m)); xl <- if (!is.null(dn) && nzchar(dn[1])) dn[1] else "X"; yl <- if (!is.null(dn) && length(dn) > 1 && nzchar(dn[2])) dn[2] else "Y"
  px <- rowSums(m); py <- colSums(m)
  indep_err <- max(abs(m - outer(px, py)))
  lines <- c(sprintf("Joint probability function p(x, y) = P(%s = x, %s = y), with marginals:", xl, yl), .fmt_mat(m, 4, TRUE), "",
             sprintf("Conditional distributions of %s | %s = x (rows sum to 1): p(x, y) / p_X(x)", yl, xl), .fmt_mat(m / px, 4), "",
             sprintf("Conditional distributions of %s | %s = y (columns sum to 1): p(x, y) / p_Y(y)", xl, yl), .fmt_mat(t(t(m) / py), 4), "")
  xv <- suppressWarnings(as.numeric(rownames(m))); yv <- suppressWarnings(as.numeric(colnames(m)))
  res <- list()
  if (!anyNA(xv) && !anyNA(yv)) {
    mx <- sum(xv * px); my <- sum(yv * py)
    vx <- sum(xv^2 * px) - mx^2; vy <- sum(yv^2 * py) - my^2
    exy <- sum(outer(xv, yv) * m); cv <- exy - mx * my
    if (abs(cv) < 1e-12) cv <- 0
    r <- cv / sqrt(vx * vy)
    lines <- c(lines,
               sprintf("E(%s) = %s   Var(%s) = %s   SD = %s", xl, .f(mx), xl, .f(vx), .f(sqrt(vx))),
               sprintf("E(%s) = %s   Var(%s) = %s   SD = %s", yl, .f(my), yl, .f(vy), .f(sqrt(vy))),
               sprintf("E(%s %s) = sum sum x y p(x, y) = %s", xl, yl, .f(exy)),
               sprintf("Cov(%s, %s) = E(XY) - mu_X mu_Y = %s - %s x %s = %s", xl, yl, .f(exy), .f(mx), .f(my), .f(cv)),
               sprintf("Corr(%s, %s) = Cov / (sigma_X sigma_Y) = %s", xl, yl, .f(r)))
    res <- list(mean_x = mx, mean_y = my, var_x = vx, var_y = vy, exy = exy, covariance = cv, correlation = r)
  }
  indep <- indep_err < 1e-9
  lines <- c(lines, "", sprintf("Independence: p(x, y) = p_X(x) p_Y(y) for every pair?  %s",
                                if (indep) "YES - independent" else sprintf("NO - not independent (largest difference %s)", .f(indep_err, 6))))
  .result("Joint distribution of two discrete random variables", lines,
          c("X and Y are independent when every conditional distribution of Y | X equals the marginal distribution of Y (equivalently p(x, y) = p_X(x) p_Y(y) for all pairs). Independent variables are uncorrelated (Cov = 0); the converse does not hold in general (it does for jointly normal variables)."),
          notes, match.call(), joint = m, marginal_x = px, marginal_y = py, independent = indep, values = res)
}

#' @rdname probability
#' @export
rv_iid <- function(mu, sigma = NULL, n, stat = c("mean", "sum"), var = NULL,
                   below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL) {
  stat <- match.arg(stat)
  v1 <- var %||% (if (is.null(sigma)) stop("Give sigma = or var =.", call. = FALSE) else sigma^2)
  if (n < 1 || n != round(n)) stop("n must be a positive whole number.", call. = FALSE)
  if (stat == "mean") {
    m <- mu; v <- v1 / n
    lines <- c(sprintf("Xbar = mean of n = %s iid variables with mean %s and variance %s", n, .f(mu), .f(v1)),
               sprintf("E(Xbar) = mu = %s", .f(m)),
               sprintf("Var(Xbar) = sigma^2 / n = %s / %s = %s", .f(v1), n, .f(v)),
               sprintf("SD(Xbar) = sigma / sqrt(n) = %s", .f(sqrt(v))))
    nm <- "Xbar"
  } else {
    m <- n * mu; v <- n * v1
    lines <- c(sprintf("S = X_1 + ... + X_n: sum of n = %s iid variables with mean %s and variance %s", n, .f(mu), .f(v1)),
               sprintf("E(S) = n mu = %s x %s = %s", n, .f(mu), .f(m)),
               sprintf("Var(S) = n sigma^2 = %s x %s = %s", n, .f(v1), .f(v)),
               sprintf("SD(S) = sqrt(n) sigma = %s", .f(sqrt(v))))
    nm <- "S"
  }
  vals <- NULL; notes <- NULL
  if (.has_query(below, above, between, quantile, middle)) {
    sdt <- if (stat == "mean") sprintf("sqrt(%s/%s)", .f(v1, 6), n) else sprintf("sqrt(%s*%s)", n, .f(v1, 6))
    E <- .prob_engine(.D_normal(m, sqrt(v), sdt), below, above, between, quantile, middle, nm, paste(nm, "~"))
    lines <- c(lines, "", sprintf("%s is normal if the X_i are normal (exact); otherwise, for n large (typically n > 30), approximately normal by the Central Limit Theorem:", nm),
               E$lines[-1]); vals <- E$values
    if (n <= 30) notes <- sprintf("n = %d is not large: the probabilities are exact only if the population is normal (the CLT approximation may be poor).", n)
  }
  wd <- c(.dist_wording(if (stat == "mean") sprintf("Xbar is the mean of a random sample of n = %s iid variables with mean %s and variance %s.", n, .f(mu), .f(v1))
                        else sprintf("S is the sum of n = %s iid variables with mean %s and variance %s.", n, .f(mu), .f(v1)),
                        if (stat == "mean") "E(Xbar) = mu and Var(Xbar) = sigma^2 / n; Xbar is normal if the population is normal, and approximately normal for large n by the Central Limit Theorem."
                        else "E(S) = n mu and Var(S) = n sigma^2; S is normal if the population is normal, and approximately normal for large n by the Central Limit Theorem.",
                        vals, nm, samples = if (stat == "mean") n else NULL,
                        caveat = if (!is.null(notes)) notes),
          .w("result", sprintf("E(%s) = %s, SD(%s) = %s.", nm, .f(m), nm, .f(sqrt(v)))))
  .result(if (stat == "mean") "Sampling distribution of the sample mean" else "Sum of iid random variables",
          lines, wd, notes, match.call(), mean = m, variance = v, sd = sqrt(v), values = vals)
}

#' @rdname probability
#' @export
rv_prop <- function(p, n, below = NULL, above = NULL, between = NULL, quantile = NULL, middle = NULL) {
  p <- .prob(p, "p")
  se <- sqrt(p * (1 - p) / n)
  lines <- c(sprintf("p-hat from a sample of n = %s, population proportion p = %s", .f(n), .f(p)),
             sprintf("E(p-hat) = p = %s", .f(p)),
             sprintf("SD(p-hat) = sqrt(p (1 - p) / n) = sqrt(%s x %s / %s) = %s", .f(p), .f(1 - p), .f(n), .f(se)),
             sprintf("Check: n p = %s, n (1 - p) = %s (both should be large enough, e.g. >= 5-10, for the normal approximation)", .f(n * p), .f(n * (1 - p))))
  vals <- NULL
  if (.has_query(below, above, between, quantile, middle)) {
    E <- .prob_engine(.D_normal(p, se, sprintf("sqrt(%s*(1-%s)/%s)", .f(p, 6), .f(p, 6), .f(n))), below, above, between, quantile, middle, "p-hat", "p-hat ~")
    lines <- c(lines, "", "Normal approximation (CLT): p-hat ~ approx. N(p, p(1 - p)/n)", E$lines[-1]); vals <- E$values
  }
  wd <- c(.dist_wording(sprintf("p-hat is the proportion in a random sample of n = %s units from a population with proportion p = %s.", .f(n), .f(p)),
                        "E(p-hat) = p and SD(p-hat) = sqrt(p (1 - p) / n); for large n, p-hat is approximately normal by the Central Limit Theorem.",
                        vals, "p-hat", samples = n),
          .w("result", sprintf("E(p-hat) = %s, SD(p-hat) = %s.", .f(p), .f(se))))
  .result("Sampling distribution of a sample proportion", lines, wd, NULL, match.call(),
          mean = p, se = se, values = vals)
}
