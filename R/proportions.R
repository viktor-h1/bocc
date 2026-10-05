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
  sx <- substitute(x)
  P <- .one_prop(x, event, count, n, phat, .label(sx))
  se0 <- sqrt(p0 * (1 - p0) / P$n)
  stat <- (P$phat - p0) / se0
  p <- .pval(stat, alt, "z"); crit <- .crit(alt, alpha, "z")
  notes <- c(P$note, if (P$n * p0 < 5 || P$n * (1 - p0) < 5)
    sprintf("n p0 = %s and n (1 - p0) = %s: at least one is below 5, so the normal approximation may be poor.",
            .f(P$n * p0), .f(P$n * (1 - p0))))
  tab <- data.frame(row.names = "", n = P$n, phat = P$phat, s_X = sqrt(p0 * (1 - p0)), se = se0, stat = stat,
                    `p-value` = .p_cells(p), check.names = FALSE)
  lines <- c(
    .hyp_lines("p", .f(p0), alt),
    "",
    "Test on the proportion   (s_X and se computed under H0, with p0, as in UBStats)",
    .ci_table_lines(tab),
    "",
    P$inputs,
    "Case: large-sample Z test for one proportion (SE computed under H0, using p0)",
    "",
    sprintf("SE0 = sqrt(p0 (1 - p0) / n) = sqrt(%s x %s / %s) = %s", .f(p0), .f(1 - p0), .f(P$n), .f(se0)),
    sprintf("Z = (p-hat - p0) / SE0 = (%s - %s) / %s = %s", .f(P$phat), .f(p0), .f(se0), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, "Z")),
    sprintf("  on the p-hat scale: %s", .reject_region(alt, p0 + crit * se0, "p-hat")),
    sprintf("p-value = %s = %s", .p_text(alt, "Z", stat), .fp(p)),
    "",
    .decision_lines(p, alpha),
    .p_reading(p))
  wording <- sprintf(
    "Let p denote the population proportion of %s. We test %s. The sample proportion is p-hat = %s/%s = %s. Under H0 (at the boundary value p0 = %s) the standard error is sqrt[p0(1 - p0)/n] = %s, giving Z = %s. %s to conclude that the population proportion is %s %s.",
    P$ev, .hyp_text("p", .f(p0), alt), .f(P$count), .f(P$n), .f(P$phat), .f(p0), .f(se0), .f(stat),
    .decision_words(p, alpha), .alt_words(alt), .f(p0))
  ub <- if (is.null(x)) .ub_raw_note
        else if (is.null(event)) .ub_call("TEST.prop", x = sx, p0 = p0, alternative = .ub_alt(alt))
        else if (length(event) == 1) .ub_call("TEST.prop", x = sx, success = event, p0 = p0, alternative = .ub_alt(alt))
        else .ub_call("TEST.prop", x = call("%in%", sx, event), p0 = p0, alternative = .ub_alt(alt))
  .plot_test(stat, alt, alpha, "z", main = "One-proportion test")
  .result("One-proportion test", lines, wording, notes, match.call(),
          statistic = stat, p_value = p, critical = crit, se = se0, estimate = P$phat, n = P$n,
          decision = if (p < alpha) "reject H0" else "fail to reject H0",
          cutoff = p0 + crit * se0, table = tab, ubstats = ub)
}

#' @rdname ci
#' @export
ci_prop <- function(x = NULL, event = NULL, conf = 0.95, count = NULL, n = NULL, phat = NULL) {
  conf <- .prob(conf, "conf")
  sx <- substitute(x)
  P <- .one_prop(x, event, count, n, phat, .label(sx))
  se <- sqrt(P$phat * (1 - P$phat) / P$n)
  cr <- .crit_ci("z", conf, NULL)
  b <- .ci_lines(sprintf("Point estimate: p-hat = %s", .f(P$phat)),
                 sprintf("se = sqrt(p-hat (1 - p-hat) / n) = sqrt(%s x %s / %s) = %s", .f(P$phat), .f(1 - P$phat), .f(P$n), .f(se)),
                 cr$v, cr$txt, se, P$phat, conf, "p")
  tab <- data.frame(row.names = "", n = P$n, phat = P$phat, s_X = sqrt(P$phat * (1 - P$phat)), se = se,
                    Lower = b$ci[1], Upper = b$ci[2])
  lines <- c(sprintf("Confidence interval for the proportion   (confidence level %s)", .f(conf)), .ci_table_lines(tab), "",
             P$inputs, "Case: large-sample normal approximation (CLT); the unknown p in the SE is replaced by p-hat", "", b$lines)
  ub <- if (is.null(x)) .ub_raw_note
        else if (is.null(event)) .ub_call("CI.prop", x = sx, conf.level = conf)
        else if (length(event) == 1) .ub_call("CI.prop", x = sx, success = event, conf.level = conf)
        else .ub_call("CI.prop", x = call("%in%", sx, event), conf.level = conf)
  .result("Confidence interval for a proportion", lines,
          c("For a large sample the sample proportion is approximately normal (Central Limit Theorem); since p is unknown, its standard error is estimated by sqrt(p-hat (1 - p-hat) / n).",
            .ci_wording(conf, sprintf("population proportion of %s", P$ev), b$ci)), P$note, match.call(),
          estimate = P$phat, se = se, critical = cr$v, margin = b$me, ci = b$ci, table = tab, ubstats = ub)
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
                        count1 = NULL, n1 = NULL, count2 = NULL, n2 = NULL, phat1 = NULL, phat2 = NULL, d0 = 0) {
  alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  sx <- substitute(x); sy <- substitute(y); sg <- substitute(group)
  P <- .two_props(x, y, event, group, levels, count1, n1, count2, n2, phat1, phat2, .label(sx), .label(sy))
  pooled <- d0 == 0
  if (pooled) {
    pp <- (P$x1 + P$x2) / (P$n1 + P$n2)
    se <- sqrt(pp * (1 - pp) * (1 / P$n1 + 1 / P$n2))
    se_steps <- c(
      sprintf("Pooled proportion under H0 (p_x = p_y): p0-hat = (x_x + x_y) / (n_x + n_y) = (%s + %s) / (%s + %s) = %s",
              .f(P$x1), .f(P$x2), .f(P$n1), .f(P$n2), .f(pp)),
      sprintf("se_0 = sqrt(p0-hat (1 - p0-hat) (1/n_x + 1/n_y)) = sqrt(%s x %s x (1/%s + 1/%s)) = %s",
              .f(pp), .f(1 - pp), .f(P$n1), .f(P$n2), .f(se)))
  } else {
    pp <- NULL
    se <- sqrt(P$p1 * (1 - P$p1) / P$n1 + P$p2 * (1 - P$p2) / P$n2)
    se_steps <- c(
      sprintf("d0 = %s is not 0, so p_x and p_y differ under H0: no pooling, each p-hat in its own term", .f(d0)),
      sprintf("se = sqrt(p_x-hat (1 - p_x-hat)/n_x + p_y-hat (1 - p_y-hat)/n_y) = sqrt(%s x %s/%s + %s x %s/%s) = %s",
              .f(P$p1), .f(1 - P$p1), .f(P$n1), .f(P$p2), .f(1 - P$p2), .f(P$n2), .f(se)))
  }
  stat <- (P$diff - d0) / se
  p <- .pval(stat, alt, "z"); crit <- .crit(alt, alpha, "z")
  tab <- data.frame(row.names = "", n_x = P$n1, n_y = P$n2, phat_x = P$p1, phat_y = P$p2, `phat_x-phat_y` = P$diff,
                    s_X = sqrt(P$p1 * (1 - P$p1)), s_Y = sqrt(P$p2 * (1 - P$p2)), se = se, stat = stat,
                    `p-value` = .p_cells(p), check.names = FALSE)
  if (pooled) names(tab)[names(tab) == "se"] <- "se_0"
  lines <- c(
    .hyp_lines("p_x - p_y", .f(d0), alt),
    "",
    sprintf("Test on p_x - p_y   (independent samples)   x = %s, y = %s", P$labs[1], P$labs[2]),
    .ci_table_lines(tab),
    "",
    sprintf("Difference p_x-hat - p_y-hat = %s - %s = %s", .f(P$p1), .f(P$p2), .f(P$diff)),
    se_steps,
    sprintf("Z = (p_x-hat - p_y-hat - d0) / %s = (%s - %s) / %s = %s", if (pooled) "se_0" else "se", .f(P$diff), .f(d0), .f(se), .f(stat)),
    sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, "Z")),
    sprintf("p-value = %s = %s", .p_text(alt, "Z", stat), .fp(p)),
    "",
    .decision_lines(p, alpha),
    .p_reading(p))
  wording <- sprintf(
    "Let p_x and p_y denote the population proportions of %s for %s and %s (independent samples). We test %s. The sample proportions are %s and %s. %s, giving Z = %s. %s to conclude that the population proportion for %s is %s that for %s%s.",
    P$ev, P$labs[1], P$labs[2], .hyp_text("p_x - p_y", .f(d0), alt), .f(P$p1), .f(P$p2),
    if (pooled) sprintf("Under H0 the two proportions are equal and estimated by the pooled proportion %s, so the standard error is %s", .f(pp), .f(se))
    else sprintf("Since d0 is not 0 the two proportions differ under H0, so each is estimated separately and the standard error is %s", .f(se)),
    .f(stat), .decision_words(p, alpha), P$labs[1], .alt_words(alt), P$labs[2],
    if (d0 != 0) sprintf(" by %s", .f(d0)) else "")
  succ <- if (!is.null(event) && length(event) == 1) event else NULL
  pd <- if (d0 != 0) d0
  ub <- if (is.null(x)) .ub_raw_note
        else if (!is.null(event) && length(event) > 1) "UBStats needs a single success category: build a TRUE/FALSE vector first, e.g. x %in% c(...)."
        else if (!is.null(group)) {
          if (.ub_by_ok(group, levels)) .ub_call("TEST.diffprop", x = sx, by = sg, success.x = succ, pdiff0 = pd, alternative = .ub_alt(alt))
          else .ub_call("TEST.diffprop", x = .ub_subset(sx, sg, P$labs[1]), y = .ub_subset(sx, sg, P$labs[2]),
                        success.x = succ, pdiff0 = pd, alternative = .ub_alt(alt))
        } else .ub_call("TEST.diffprop", x = sx, y = sy, success.x = succ, pdiff0 = pd, alternative = .ub_alt(alt))
  .plot_test(stat, alt, alpha, "z", main = "Two-proportion test")
  .result("Two-proportion test", lines, wording, P$note, match.call(),
          statistic = stat, p_value = p, critical = crit, se = se, estimate = P$diff, pooled = pp,
          decision = if (p < alpha) "reject H0" else "fail to reject H0", table = tab, ubstats = ub)
}

#' @rdname ci
#' @export
ci_2props <- function(x = NULL, y = NULL, event = NULL, conf = 0.95, group = NULL, levels = NULL,
                      count1 = NULL, n1 = NULL, count2 = NULL, n2 = NULL, phat1 = NULL, phat2 = NULL) {
  conf <- .prob(conf, "conf")
  sx <- substitute(x); sy <- substitute(y); sg <- substitute(group)
  P <- .two_props(x, y, event, group, levels, count1, n1, count2, n2, phat1, phat2, .label(sx), .label(sy))
  se <- sqrt(P$p1 * (1 - P$p1) / P$n1 + P$p2 * (1 - P$p2) / P$n2)
  cr <- .crit_ci("z", conf, NULL)
  b <- .ci_lines(sprintf("Point estimate: p_x-hat - p_y-hat = %s - %s = %s", .f(P$p1), .f(P$p2), .f(P$diff)),
                 sprintf("se = sqrt(p_x(1-p_x)/n_x + p_y(1-p_y)/n_y) = sqrt(%s x %s/%s + %s x %s/%s) = %s",
                         .f(P$p1), .f(1 - P$p1), .f(P$n1), .f(P$p2), .f(1 - P$p2), .f(P$n2), .f(se)),
                 cr$v, cr$txt, se, P$diff, conf, "p_x - p_y")
  tab <- data.frame(row.names = "", n_x = P$n1, n_y = P$n2, phat_x = P$p1, phat_y = P$p2, `phat_x-phat_y` = P$diff,
                    s_X = sqrt(P$p1 * (1 - P$p1)), s_Y = sqrt(P$p2 * (1 - P$p2)), se = se, Lower = b$ci[1], Upper = b$ci[2],
                    check.names = FALSE)
  lines <- c(sprintf("Confidence interval for p_x - p_y   (independent samples, confidence level %s)   x = %s, y = %s",
                     .f(conf), P$labs[1], P$labs[2]), .ci_table_lines(tab), "",
             "Case: large-sample normal approximation; unpooled se (each p-hat separately)", "", b$lines)
  wording <- c(.ci_wording(conf, sprintf("difference between the population proportions (%s minus %s)", P$labs[1], P$labs[2]), b$ci),
               if (b$ci[1] > 0 || b$ci[2] < 0) "The interval does not contain 0, so the data indicate a difference between the two population proportions."
               else "The interval contains 0, so equal population proportions are plausible: the data are compatible both with no difference and with a difference in either direction.")
  succ <- if (!is.null(event) && length(event) == 1) event else NULL
  ub <- if (is.null(x)) .ub_raw_note
        else if (!is.null(event) && length(event) > 1) "UBStats needs a single success category: build a TRUE/FALSE vector first, e.g. x %in% c(...)."
        else if (!is.null(group)) {
          if (.ub_by_ok(group, levels)) .ub_call("CI.diffprop", x = sx, by = sg, success.x = succ, conf.level = conf)
          else .ub_call("CI.diffprop", x = .ub_subset(sx, sg, P$labs[1]), y = .ub_subset(sx, sg, P$labs[2]),
                        success.x = succ, conf.level = conf)
        } else .ub_call("CI.diffprop", x = sx, y = sy, success.x = succ, conf.level = conf)
  .result("Confidence interval for two proportions", lines, wording, P$note, match.call(),
          estimate = P$diff, se = se, critical = cr$v, margin = b$me, ci = b$ci, table = tab, ubstats = ub)
}
