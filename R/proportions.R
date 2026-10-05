# ------------------------------------------------------------
# Proportions: one proportion and two independent proportions.
# Data can be a logical vector, a 0/1 vector, a categorical vector
# + event, or the counts / sample proportions from the question.
# ------------------------------------------------------------

.one_prop <- function(x, event, count, n, phat, xlab) {
  if (!is.null(x)) {
    e <- .event(x, event, xlab)
    n <- length(e); count <- sum(e); phat <- count / n
    ev <- if (is.null(event) && attr(e, "event") == "TRUE") xlab else sprintf("%s = %s", xlab, attr(e, "event"))
    note <- .dropped_note(e)
    inputs <- sprintf("Event: %s   count x = %s   n = %s   p-hat = x/n = %s", ev, .f(count), .f(n), .f(phat))
  } else {
    if (is.null(n)) stop("Give the data (x = ...) or the summary numbers count = (or phat = ) and n =.", call. = FALSE)
    if (is.null(count) && is.null(phat)) stop("Give count = (number of successes) or phat = (sample proportion).", call. = FALSE)
    if (is.null(phat)) phat <- count / n else { phat <- .prob_or_01(phat, "phat"); count <- phat * n }
    ev <- "the event of interest"; note <- NULL
    inputs <- sprintf("Summary numbers   x = %s   n = %s   p-hat = %s", .f(count), .f(n), .f(phat))
  }
  list(count = count, n = n, phat = phat, ev = ev, note = note, inputs = inputs)
}

# Like .prob() but also allows exactly 0 or 1 (a sample proportion can be 0 or 1).
.prob_or_01 <- function(x, what) {
  if (x > 1 && x <= 100) x <- x / 100
  if (x < 0 || x > 1) stop(what, " must be between 0 and 1.", call. = FALSE)
  x
}

#' @rdname tests
#' @export
test_prop <- function(x = NULL, p0, event = NULL, alt = "two.sided", alpha = 0.05,
                      count = NULL, n = NULL, phat = NULL) {
  if (missing(p0)) stop("Give the null value: p0 = ...", call. = FALSE)
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha"); p0 <- .prob(p0, "p0")
  P <- .one_prop(x, event, count, n, phat, .label(substitute(x)))
  se0 <- sqrt(p0 * (1 - p0) / P$n)
  stat <- (P$phat - p0) / se0
  p <- .pval(stat, alt, "z"); crit <- .crit(alt, alpha, "z")
  notes <- c(P$note, if (P$n * p0 < 5 || P$n * (1 - p0) < 5)
    sprintf("n p0 = %s and n (1 - p0) = %s: at least one is below 5, so the normal approximation may be poor.",
            .f(P$n * p0), .f(P$n * (1 - p0))))
  lines <- c(
    sprintf("H0: p = %s      H1: p %s %s", .f(p0), .alt_sym(alt), .f(p0)),
    P$inputs,
    "Case: large-sample Z test for one proportion (SE computed under H0, using p0)",
    "",
    sprintf("SE0 = sqrt(p0 (1 - p0) / n) = sqrt(%s x %s / %s) = %s", .f(p0), .f(1 - p0), .f(P$n), .f(se0)),
    sprintf("Z = (p-hat - p0) / SE0 = (%s - %s) / %s = %s", .f(P$phat), .f(p0), .f(se0), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, "Z")),
    sprintf("  on the p-hat scale: %s", .reject_region(alt, p0 + crit * se0, "p-hat")),
    sprintf("p-value = %s = %s", .p_text(alt, "Z", stat), .fp(p)),
    "",
    .decision_lines(p, alpha))
  wording <- sprintf(
    "Let p denote the population proportion of %s. We test H0: p = %s against H1: p %s %s. The sample proportion is p-hat = %s/%s = %s. Under H0 the standard error is sqrt[p0(1 - p0)/n] = %s, giving Z = %s. %s to conclude that the population proportion is %s %s.",
    P$ev, .f(p0), .alt_sym(alt), .f(p0), .f(P$count), .f(P$n), .f(P$phat), .f(se0), .f(stat),
    .decision_words(p, alpha), .alt_words(alt), .f(p0))
  .plot_test(stat, alt, alpha, "z", main = "One-proportion test")
  .result("One-proportion test", lines, wording, notes, match.call(),
          statistic = stat, p_value = p, critical = crit, se = se0, estimate = P$phat, n = P$n,
          decision = if (p < alpha) "reject H0" else "fail to reject H0")
}

#' @rdname ci
#' @export
ci_prop <- function(x = NULL, event = NULL, conf = 0.95, count = NULL, n = NULL, phat = NULL) {
  conf <- .prob(conf, "conf")
  P <- .one_prop(x, event, count, n, phat, .label(substitute(x)))
  se <- sqrt(P$phat * (1 - P$phat) / P$n)
  cr <- .crit_ci("z", conf, NULL)
  b <- .ci_lines(sprintf("Point estimate: p-hat = %s", .f(P$phat)),
                 sprintf("SE = sqrt(p-hat (1 - p-hat) / n) = sqrt(%s x %s / %s) = %s", .f(P$phat), .f(1 - P$phat), .f(P$n), .f(se)),
                 cr$v, cr$txt, se, P$phat, conf, "p")
  lines <- c(P$inputs, "Case: large-sample Z interval (SE estimated with p-hat)", "", b$lines)
  .result("Confidence interval for a proportion", lines,
          .ci_wording(conf, sprintf("population proportion of %s", P$ev), b$ci), P$note, match.call(),
          estimate = P$phat, se = se, critical = cr$v, margin = b$me, ci = b$ci)
}

# ---------- two proportions ----------

.two_props <- function(x, y, event, group, levels, count1, n1, count2, n2, phat1, phat2, xlab, ylab) {
  note <- NULL
  if (!is.null(x)) {
    if (!is.null(group)) {
      xx <- .one_column(x, xlab); g <- .one_column(group, "group")
      ok <- !is.na(xx) & !is.na(g)
      sp <- .split2(xx[ok], g[ok], levels)
      e1 <- .event(sp$x1, event, xlab); e2 <- .event(sp$x2, event, xlab)
      labs <- sp$levels
    } else {
      if (is.null(y)) stop("Give the second sample y = ..., or the grouping variable group = ...", call. = FALSE)
      e1 <- .event(x, event, xlab); e2 <- .event(y, event, ylab); labs <- c(xlab, ylab)
    }
    note <- .dropped_note(e1, e2)
    ev <- if (is.null(event)) "the event" else sprintf("%s = %s", xlab, attr(e1, "event"))
    n1 <- length(e1); n2 <- length(e2); count1 <- sum(e1); count2 <- sum(e2)
  } else {
    if (is.null(n1) || is.null(n2)) stop("Give data, or n1 = and n2 = with count1/count2 (or phat1/phat2).", call. = FALSE)
    if (is.null(count1)) { .need(phat1, msg = "Give count1 = or phat1 =."); count1 <- .prob_or_01(phat1, "phat1") * n1 }
    if (is.null(count2)) { .need(phat2, msg = "Give count2 = or phat2 =."); count2 <- .prob_or_01(phat2, "phat2") * n2 }
    labs <- c("group 1", "group 2"); ev <- "the event"
  }
  p1 <- count1 / n1; p2 <- count2 / n2
  tab <- data.frame(group = labs, successes = c(count1, count2), n = c(n1, n2), p_hat = c(p1, p2))
  list(n1 = n1, n2 = n2, x1 = count1, x2 = count2, p1 = p1, p2 = p2, diff = p1 - p2, labs = labs, ev = ev,
       note = note, table = .table_lines(.round_df(tab)))
}

#' @rdname tests
#' @export
test_2props <- function(x = NULL, y = NULL, event = NULL, alt = "two.sided", alpha = 0.05,
                        group = NULL, levels = NULL,
                        count1 = NULL, n1 = NULL, count2 = NULL, n2 = NULL, phat1 = NULL, phat2 = NULL) {
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  P <- .two_props(x, y, event, group, levels, count1, n1, count2, n2, phat1, phat2,
                  .label(substitute(x)), .label(substitute(y)))
  pp <- (P$x1 + P$x2) / (P$n1 + P$n2)
  se <- sqrt(pp * (1 - pp) * (1 / P$n1 + 1 / P$n2))
  stat <- P$diff / se
  p <- .pval(stat, alt, "z"); crit <- .crit(alt, alpha, "z")
  lines <- c(
    sprintf("H0: p1 - p2 = 0      H1: p1 - p2 %s 0", .alt_sym(alt)),
    sprintf("Group 1 = %s,  Group 2 = %s   (independent samples)", P$labs[1], P$labs[2]),
    P$table,
    "",
    sprintf("Difference p1-hat - p2-hat = %s - %s = %s", .f(P$p1), .f(P$p2), .f(P$diff)),
    sprintf("Pooled p under H0 = (x1 + x2) / (n1 + n2) = (%s + %s) / (%s + %s) = %s",
            .f(P$x1), .f(P$x2), .f(P$n1), .f(P$n2), .f(pp)),
    sprintf("SE0 = sqrt(p (1 - p) (1/n1 + 1/n2)) = sqrt(%s x %s x (1/%s + 1/%s)) = %s",
            .f(pp), .f(1 - pp), .f(P$n1), .f(P$n2), .f(se)),
    sprintf("Z = (p1-hat - p2-hat) / SE0 = %s / %s = %s", .f(P$diff), .f(se), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, "Z")),
    sprintf("p-value = %s = %s", .p_text(alt, "Z", stat), .fp(p)),
    "",
    .decision_lines(p, alpha))
  wording <- sprintf(
    "Let p1 and p2 denote the population proportions of %s for %s and %s. We test H0: p1 - p2 = 0 against H1: p1 - p2 %s 0. The sample proportions are %s and %s. Under H0 the pooled proportion is %s, giving a standard error of %s and Z = %s. %s to conclude that the population proportion for %s is %s that for %s.",
    P$ev, P$labs[1], P$labs[2], .alt_sym(alt), .f(P$p1), .f(P$p2), .f(pp), .f(se), .f(stat),
    .decision_words(p, alpha), P$labs[1], .alt_words(alt), P$labs[2])
  .plot_test(stat, alt, alpha, "z", main = "Two-proportion test")
  .result("Two-proportion test", lines, wording, P$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = se, estimate = P$diff, pooled = pp,
          decision = if (p < alpha) "reject H0" else "fail to reject H0")
}

#' @rdname ci
#' @export
ci_2props <- function(x = NULL, y = NULL, event = NULL, conf = 0.95, group = NULL, levels = NULL,
                      count1 = NULL, n1 = NULL, count2 = NULL, n2 = NULL, phat1 = NULL, phat2 = NULL) {
  conf <- .prob(conf, "conf")
  P <- .two_props(x, y, event, group, levels, count1, n1, count2, n2, phat1, phat2,
                  .label(substitute(x)), .label(substitute(y)))
  se <- sqrt(P$p1 * (1 - P$p1) / P$n1 + P$p2 * (1 - P$p2) / P$n2)
  cr <- .crit_ci("z", conf, NULL)
  b <- .ci_lines(sprintf("Point estimate: p1-hat - p2-hat = %s - %s = %s", .f(P$p1), .f(P$p2), .f(P$diff)),
                 sprintf("SE = sqrt(p1(1-p1)/n1 + p2(1-p2)/n2) = sqrt(%s x %s/%s + %s x %s/%s) = %s",
                         .f(P$p1), .f(1 - P$p1), .f(P$n1), .f(P$p2), .f(1 - P$p2), .f(P$n2), .f(se)),
                 cr$v, cr$txt, se, P$diff, conf, "p1 - p2")
  lines <- c(sprintf("Group 1 = %s,  Group 2 = %s", P$labs[1], P$labs[2]), P$table,
             "Case: large-sample Z interval (unpooled SE: each p-hat separately)", "", b$lines)
  wording <- c(.ci_wording(conf, sprintf("difference between the population proportions (%s minus %s)", P$labs[1], P$labs[2]), b$ci),
               if (b$ci[1] > 0 || b$ci[2] < 0) "The interval does not contain 0, so the data indicate a difference between the two population proportions."
               else "The interval contains 0, so equal population proportions are plausible.")
  .result("Confidence interval for two proportions", lines, wording, P$note, match.call(),
          estimate = P$diff, se = se, critical = cr$v, margin = b$me, ci = b$ci)
}
