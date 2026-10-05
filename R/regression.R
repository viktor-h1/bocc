# ------------------------------------------------------------
# Linear regression: fit + tests, prediction, single-coefficient
# tests, diagnostics, model comparison.
# ------------------------------------------------------------

#' Linear regression
#'
#' * `reg_fit()`     fit `lm`: equation, coefficient t tests, global F test,
#'   R^2 / adjusted R^2, residual SE, interpretation of every coefficient.
#' * `reg_predict()` predicted value, confidence interval for the MEAN
#'   response and prediction interval for ONE new individual.
#' * `reg_test()`    t test of one coefficient against any value
#'   (H0: beta = value).
#' * `reg_effect()`  confidence interval for a coefficient and for the
#'   effect of a change of `change` units (e.g. 10 units: 10 x CI).
#' * `reg_check()`   diagnostics: residual plots, normal Q-Q, leverage,
#'   Cook's distance, largest standardised residuals, correlations among
#'   predictors and VIFs (multicollinearity).
#' * `reg_compare()` compare two models: R^2, adjusted R^2, residual SE and,
#'   for nested models, the partial F test.
#'
#' @param formula Model formula, e.g. `spend ~ age + premium`. Anything `lm`
#'   accepts works (`factor(x)`, `log(y)`, `I(x^2)`). Non-standard column
#'   names need backticks: `` `monthly spend` ~ age ``.
#' @param data Data frame holding the variables.
#' @param alpha Significance level.
#' @param model A fitted model: the result of `reg_fit()` or an `lm` object.
#' @param ... For `reg_predict()`: values of the predictors, e.g.
#'   `age = 40, premium = "Yes"`.
#' @param newdata Alternative to `...`: a data frame of predictor values.
#' @param value `reg_predict()`: an observed value to judge (is it unexpected
#'   for one unit with these characteristics? inside the prediction interval?).
#' @param change `reg_effect()`: number of units of the change in the
#'   explanatory variable (default 1).
#' @param conf Confidence level.
#' @param term Name of the coefficient as shown in the coefficient table.
#' @param value Null value of the coefficient (default 0).
#' @param alt Alternative: `"<"`, `">"` or `"!="`.
#' @param model1,model2 Two fitted models (smaller first for nested models).
#' @return An `sc_result`; `reg_fit()` results carry the `lm` object in `$model`.
#' @examples
#' fit <- reg_fit(mpg ~ wt + hp, data = mtcars)
#' reg_predict(fit, wt = 3, hp = 120)
#' reg_test(fit, "wt", value = -3)
#' reg_effect(fit, "hp", change = 10, conf = 0.90)
#' @name regression
NULL

.as_lm <- function(model) {
  if (inherits(model, "sc_result") && !is.null(model$model)) model <- model$model
  if (!inherits(model, "lm")) stop("model must be the result of reg_fit() or an lm() model.", call. = FALSE)
  model
}

# Factor dummies: coefficient name -> c(variable, level, baseline).
.dummy_info <- function(m) {
  xl <- m$xlevels; out <- list()
  for (v in names(xl)) for (lv in xl[[v]][-1]) {
    for (nm in c(paste0(v, lv), paste0("`", v, "`", lv))) out[[nm]] <- c(v, lv, xl[[v]][1])
  }
  out
}

# Coefficient names as the course writes them: I(EmplStatus = Stud) for dummies.
.coef_labels <- function(m) {
  di <- .dummy_info(m); nms <- names(coef(m))
  vapply(nms, function(nm) if (!is.null(di[[nm]])) sprintf("I(%s = %s)", di[[nm]][1], di[[nm]][2]) else nm, character(1))
}

# Significance of one coefficient at the usual levels.
.signif_txt <- function(p) {
  if (is.na(p)) return("p-value not available")
  if (p < 0.01) sprintf("p-value %s < 0.01: significant at any usual level", .fp(p))
  else if (p < 0.05) sprintf("p-value %s: significant at 5%% and 10%%, not at 1%%", .fp(p))
  else if (p < 0.10) sprintf("p-value %s: significant only at 10%%, not at 5%% or 1%%", .fp(p))
  else sprintf("p-value %s > 0.10: not significant at the usual levels", .fp(p))
}

# Plain-language reading of each coefficient.
.coef_meaning <- function(m) {
  co <- coef(m); nms <- names(co)
  y <- deparse(formula(m)[[2]])
  xl <- m$xlevels
  multi <- length(co) > 2
  hold <- if (multi) ", holding the other explanatory variables constant" else ""
  out <- character()
  for (nm in nms) {
    b <- co[[nm]]
    if (nm == "(Intercept)") {
      num <- setdiff(attr(terms(m), "term.labels"), names(xl))
      base <- if (length(xl)) paste(sprintf("%s = %s", names(xl), vapply(xl, `[`, "", 1)), collapse = " and ") else ""
      out <- c(out, if (!length(xl)) sprintf("Intercept %s: estimated mean of %s when all explanatory variables are 0 (meaningful only if 0 is a sensible value).", .f(b), y)
               else if (!length(num)) sprintf("Intercept %s: estimated mean of %s for units with %s (the baseline level%s, which have all dummies equal to 0).",
                                               .f(b), y, base, if (length(xl) > 1) "s" else "")
               else sprintf("Intercept %s: estimated mean of %s for units with %s (the baseline level%s) when %s %s 0 (meaningful only if 0 is a sensible value).",
                            .f(b), y, base, if (length(xl) > 1) "s" else "", paste(num, collapse = ", "), if (length(num) > 1) "are all" else "is"))
      next
    }
    fac <- NULL
    for (v in names(xl)) {
      for (lv in xl[[v]][-1]) if (identical(nm, paste0(v, lv)) || identical(nm, paste0("`", v, "`", lv))) fac <- c(v, lv, xl[[v]][1])
    }
    pv <- coef(summary(m))[nm, 4]
    if (!is.null(fac)) {
      out <- c(out, sprintf("%s %s: units with %s = %s have a %s that is on average %s %s than units with %s = %s (the baseline)%s. %s (H0: beta = 0, i.e. no difference from the baseline %s).",
                            nm, .f(b), fac[1], fac[2], y, .f(abs(b)), if (b < 0) "lower" else "higher",
                            fac[1], fac[3], hold, .signif_txt(pv), fac[3]))
    } else {
      out <- c(out, sprintf("%s %s: when %s increases by 1 unit, %s changes on average by %s (an average %s of %s)%s. %s.",
                            nm, .f(b), nm, y, .f(b), if (b < 0) "decrease" else "increase", .f(abs(b)), hold, .signif_txt(pv)))
    }
  }
  out
}

# Differences between non-baseline levels of the same factor (b_A - b_B).
.dummy_pairs <- function(m) {
  di <- .dummy_info(m); b <- coef(m); out <- character()
  vars <- unique(vapply(di, `[`, "", 1))
  for (v in vars) {
    nms <- intersect(names(b), names(di)[vapply(di, function(z) z[1] == v, logical(1))])
    if (length(nms) < 2) next
    pr <- utils::combn(nms, 2)
    d <- apply(pr, 2, function(z) sprintf("%s vs %s: %s - (%s) = %s", di[[z[1]]][2], di[[z[2]]][2], .f(b[[z[1]]]), .f(b[[z[2]]]), .f(b[[z[1]]] - b[[z[2]]])))
    out <- c(out, sprintf("%s: each dummy compares a level with the baseline %s, and its p-value tests only that difference. The estimated average difference between two non-baseline levels, holding the other variables constant, is the difference of their coefficients (%s); its significance cannot be read from this output (refit with a different baseline, e.g. relevel(factor(%s), ref = \"%s\")).",
                          v, di[[nms[1]]][3], paste(d, collapse = "; "), v, di[[nms[1]]][2]))
  }
  out
}

#' @rdname regression
#' @export
reg_fit <- function(formula, data, alpha = 0.05) {
  alpha <- .prob(alpha, "alpha")
  m <- lm(formula, data = data)
  s <- summary(m)
  co <- coef(s)
  fs <- s$fstatistic
  fp <- if (is.null(fs)) NA else pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
  y <- deparse(formula(m)[[2]])
  b <- coef(m); bl <- .coef_labels(m)
  eq <- paste0(y, "-hat = ", .f(b[1]),
               paste0(vapply(seq_along(b)[-1], function(i)
                 sprintf(" %s %s %s", if (b[i] < 0) "-" else "+", .f(abs(b[i])), bl[i]), character(1)), collapse = ""))
  ctab <- data.frame(term = rownames(co), estimate = co[, 1], std_error = co[, 2], t = co[, 3], p_value = co[, 4],
                     signif = ifelse(co[, 4] < alpha, "yes", "no"))
  rdf <- m$df.residual
  tcrit <- qt(1 - alpha / 2, rdf)
  lines <- c(
    sprintf("Model: %s   (n = %s, residual df = %s)", paste(deparse(formula(m)), collapse = " "), nobs(m), rdf), "",
    "Fitted equation:", paste0("  ", eq), "",
    sprintf("Coefficients (each t test: H0: beta_j = 0 vs H1: beta_j != 0; signif at alpha = %s; |t| critical = %s):", .f(alpha), .f(tcrit)),
    .table_lines(transform(.round_df(ctab), p_value = .p_cells(ctab$p_value))),
    sprintf("  t = estimate / std_error, e.g. %s: %s / %s = %s; p-value = 2 P(T(%s) > |t|)",
            rownames(co)[min(2, nrow(co))], .f(co[min(2, nrow(co)), 1]), .f(co[min(2, nrow(co)), 2]), .f(co[min(2, nrow(co)), 3]), rdf), "",
    sprintf("R-squared          = %s  (%s of the sample variability of %s is explained by the model)", .f(s$r.squared), .pct(s$r.squared), y),
    sprintf("Adjusted R-squared = %s  (use this to compare models with different numbers of predictors)", .f(s$adj.r.squared)),
    sprintf("Residual SE        = %s  (typical size of a residual, in units of %s)", .f(s$sigma), y))
  if (!is.null(fs)) {
    fcrit <- qf(1 - alpha, fs[2], fs[3])
    lines <- c(lines, "",
               "Global F test: H0: beta_1 = ... = beta_k = 0 (all slopes)   vs   H1: at least one beta_j != 0",
               sprintf("F = (SSR / k) / (SSE / (n - k - 1)) = (%s / %s) / (%s / %s) = %s",
                       .f(sum((fitted(m) - mean(model.response(model.frame(m))))^2)), fs[2], .f(sum(residuals(m)^2)), fs[3], .f(fs[1])),
               sprintf("F = %s on %s and %s df;  critical F = %s;  p-value = P(F(%s, %s) > %s) = %s", .f(fs[1]), fs[2], fs[3], .f(fcrit), fs[2], fs[3], .f(fs[1]), .fp(fp)),
               .r_pval_line(fs[1], "greater", "F", fs[2], fs[3]),
               .decision_lines(fp, alpha))
  }
  wording <- c(.coef_meaning(m),
               if (!is.null(fs)) sprintf("Global F test: F = %s with p-value %s, so the model is %s at alpha = %s: %s",
                                         .f(fs[1]), .fp(fp), if (fp < alpha) "globally significant" else "not globally significant", .f(alpha),
                                         if (fp < alpha) "at least one explanatory variable helps explain the response (this does not mean every coefficient is significant)."
                                         else "there is insufficient evidence that any explanatory variable helps explain the response."),
               sprintf("R-squared = %s: %s of the sample variability of %s is explained by the linear model with %s as explanatory variables.",
                       .f(s$r.squared), .pct(s$r.squared), y, paste(attr(terms(m), "term.labels"), collapse = ", ")),
               if (s$r.squared < 0.3) sprintf("Only %s of the variability is explained: the residual variability is high, so prediction intervals are wide and the model is not advisable for predicting individual values of %s (it can still describe significant average effects).",
                                              .pct(s$r.squared), y),
               .dummy_pairs(m),
               if (length(m$xlevels)) sprintf("The baseline of each factor is its FIRST level (%s): alphabetical order for a character variable unless the order is set with factor(x, levels = ...) or changed with relevel(factor(x), ref = ...).",
                                              paste(sprintf("%s = %s", names(m$xlevels), vapply(m$xlevels, `[`, "", 1)), collapse = ", ")))
  mf <- model.frame(m)
  .with_plot(function() {
    if (ncol(mf) == 2 && is.numeric(mf[[2]])) {
      plot(mf[[2]], mf[[1]], xlab = names(mf)[2], ylab = y, main = "Data and fitted line", pch = 19, col = "grey40", las = 1)
      abline(m, col = "firebrick", lwd = 2)
    } else {
      op <- par(mfrow = c(1, 2)); on.exit(par(op))
      plot(m, which = 1); plot(m, which = 2)
    }
  })
  .result("Linear regression", lines, wording, NULL, match.call(),
          model = m, coefficients = ctab, r_squared = s$r.squared, adj_r_squared = s$adj.r.squared,
          sigma = s$sigma, f = if (!is.null(fs)) unname(fs[1]) else NULL, f_p_value = fp)
}

#' @rdname regression
#' @export
reg_predict <- function(model, ..., newdata = NULL, conf = 0.95, value = NULL) {
  m <- .as_lm(model); conf <- .prob(conf, "conf")
  if (is.null(newdata)) {
    vals <- list(...)
    if (!length(vals)) stop("Give the predictor values, e.g. reg_predict(fit, age = 40, premium = \"Yes\").", call. = FALSE)
    newdata <- as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
  }
  pr <- predict(m, newdata, se.fit = TRUE)
  s <- summary(m)$sigma; rdf <- m$df.residual
  tq <- qt((1 + conf) / 2, rdf)
  se_pi <- sqrt(pr$se.fit^2 + s^2)
  out <- data.frame(newdata, fit = pr$fit, se_mean = pr$se.fit,
                    ci_lower = pr$fit - tq * pr$se.fit, ci_upper = pr$fit + tq * pr$se.fit,
                    se_individual = se_pi, pi_lower = pr$fit - tq * se_pi, pi_upper = pr$fit + tq * se_pi,
                    check.names = FALSE)
  lines <- c(sprintf("Model: %s", paste(deparse(formula(m)), collapse = " ")),
             sprintf("t critical = t(%s; df = %s) = %s,  residual SE s = %s", .f((1 + conf) / 2), rdf, .f(tq), .f(s)), "")
  for (i in seq_len(nrow(out))) {
    lines <- c(lines,
      sprintf("At %s:", paste(sprintf("%s = %s", names(newdata), vapply(newdata[i, , drop = TRUE], as.character, "")), collapse = ", ")),
      sprintf("  Predicted value y-hat = %s", .f(out$fit[i])),
      sprintf("  %s%% CI for the MEAN response:   y-hat +/- t x SE_mean = %s +/- %s x %s = [%s, %s]",
              .f(100 * conf, 2), .f(out$fit[i]), .f(tq), .f(out$se_mean[i]), .f(out$ci_lower[i]), .f(out$ci_upper[i])),
      sprintf("  %s%% PI for ONE new individual:  y-hat +/- t x sqrt(SE_mean^2 + s^2) = %s +/- %s x %s = [%s, %s]",
              .f(100 * conf, 2), .f(out$fit[i]), .f(tq), .f(se_pi[i]), .f(out$pi_lower[i]), .f(out$pi_upper[i])))
  }
  y <- deparse(formula(m)[[2]])
  wording <- c(
    sprintf("Point estimate: the predicted %s for units with these characteristics is %s. The %s%% confidence interval [%s, %s] estimates the AVERAGE %s of all units with these characteristics; the %s%% prediction interval [%s, %s] is for the %s of ONE new unit.",
            y, .f(out$fit[1]), .f(100 * conf, 2), .f(out$ci_lower[1]), .f(out$ci_upper[1]), y, .f(100 * conf, 2), .f(out$pi_lower[1]), .f(out$pi_upper[1]), y),
    "The prediction interval is wider than the confidence interval because, besides the uncertainty in estimating the mean response, it includes the natural variability of individual observations around that mean (s^2).")
  if (length(value)) {
    v <- value[1]; i <- 1
    in_ci <- v >= out$ci_lower[i] && v <= out$ci_upper[i]; in_pi <- v >= out$pi_lower[i] && v <= out$pi_upper[i]
    lines <- c(lines, "", sprintf("Value %s: %s the CI for the mean, %s the PI for one individual.", .f(v),
                                  if (in_ci) "inside" else "outside", if (in_pi) "inside" else "outside"))
    wording <- c(wording, sprintf("A value of %s for ONE unit with these characteristics is %s: it lies %s the %s%% prediction interval [%s, %s]%s.",
      .f(v), if (in_pi) "not unexpected or anomalous" else "unexpected (anomalous)", if (in_pi) "inside" else "outside",
      .f(100 * conf, 2), .f(out$pi_lower[i]), .f(out$pi_upper[i]),
      if (!in_ci && in_pi) sprintf(" (it is outside the confidence interval for the MEAN, [%s, %s], but that interval refers to the average of all such units, not to a single one)", .f(out$ci_lower[i]), .f(out$ci_upper[i])) else ""))
  }
  # extrapolation: numeric predictor values outside the observed range
  mf <- model.frame(m); ext <- character()
  for (v in intersect(names(newdata), names(mf))) if (is.numeric(mf[[v]]) && is.numeric(newdata[[v]])) {
    r <- range(mf[[v]]); bad <- newdata[[v]] < r[1] | newdata[[v]] > r[2]
    if (any(bad)) ext <- c(ext, sprintf("%s = %s is outside the observed range [%s, %s]", v, paste(.f(newdata[[v]][bad]), collapse = ", "), .f(r[1]), .f(r[2])))
  }
  notes <- "Check the diagnostics (reg_check) before relying on intervals."
  if (length(ext)) {
    notes <- c(sprintf("EXTRAPOLATION: %s.", paste(ext, collapse = "; ")), notes)
    wording <- c(wording, sprintf("The estimate is not reliable: %s, so we are predicting for units whose characteristics are not homogeneous with those of the sample used to estimate the model (extrapolation); we do not know whether the linear relation still holds there.",
                                  paste(ext, collapse = "; ")))
  }
  .result("Regression prediction", lines, wording, notes, match.call(), predictions = out, extrapolation = length(ext) > 0)
}

#' @rdname regression
#' @export
reg_test <- function(model, term, value = 0, alt = "two.sided", alpha = 0.05) {
  m <- .as_lm(model); alt <- .alt(alt); alpha <- .prob(alpha, "alpha")
  co <- coef(summary(m))
  if (!term %in% rownames(co)) stop("term must be one of: ", paste(rownames(co), collapse = ", "), call. = FALSE)
  b <- co[term, 1]; se <- co[term, 2]; df <- m$df.residual
  stat <- (b - value) / se
  p <- .pval(stat, alt, "t", df); crit <- .crit(alt, alpha, "t", df)
  lines <- c(sprintf("H0: beta(%s) = %s      H1: beta(%s) %s %s", term, .f(value), term, .alt_sym(alt), .f(value)),
             sprintf("Estimate b = %s   SE(b) = %s   residual df = %s", .f(b), .f(se), df), "",
             sprintf("t = (b - %s) / SE(b) = (%s - %s) / %s = %s", .f(value), .f(b), .f(value), .f(se), .f(stat)),
             sprintf("Critical value: %s  ->  %s", paste(.f(crit), collapse = " and "), .reject_region(alt, crit, "t")),
             sprintf("p-value = %s = %s", .p_text(alt, sprintf("t(%s)", df), stat), .fp(p)),
             .r_pval_line(stat, alt, "t", df), "",
             .decision_lines(p, alpha))
  wording <- sprintf("We test H0: beta = %s for %s against H1: beta %s %s using t = (b - %s)/SE(b) = %s with %s residual degrees of freedom. %s that the coefficient of %s is %s %s.",
                     .f(value), term, .alt_sym(alt), .f(value), .f(value), .f(stat), df, .decision_words(p, alpha),
                     term, .alt_words(alt), .f(value))
  .plot_test(stat, alt, alpha, "t", df, main = paste("Test of", term))
  .result("Regression coefficient t test", lines, wording, NULL, match.call(),
          statistic = stat, p_value = p, critical = crit, df = df,
          decision = if (p < alpha) "reject H0" else "fail to reject H0")
}

#' @rdname regression
#' @export
reg_effect <- function(model, term, change = 1, conf = 0.95) {
  m <- .as_lm(model); conf <- .prob(conf, "conf")
  co <- coef(summary(m))
  if (!term %in% rownames(co)) stop("term must be one of: ", paste(rownames(co), collapse = ", "), call. = FALSE)
  b <- co[term, 1]; se <- co[term, 2]; df <- m$df.residual
  tq <- qt((1 + conf) / 2, df)
  ci <- b + c(-1, 1) * tq * se
  eff <- change * b; eci <- sort(change * ci)
  y <- deparse(formula(m)[[2]])
  di <- .dummy_info(m)[[term]]
  hold <- if (length(coef(m)) > 2) ", keeping the other explanatory variables constant" else ""
  lines <- c(sprintf("Coefficient of %s: b = %s,  SE(b) = %s,  residual df = %s", term, .f(b), .f(se), df),
             sprintf("%s%% CI for beta: b +/- t(%s; %s) x SE = %s +/- %s x %s = [%s, %s]",
                     .f(100 * conf, 2), .f((1 + conf) / 2), df, .f(b), .f(tq), .f(se), .f(ci[1]), .f(ci[2])),
             sprintf("  in R: confint(mod, \"%s\", level = %s)", term, .f(conf)))
  chg_txt <- if (change < 0) sprintf("a decrease of %s unit%s", .f(-change), if (change == -1) "" else "s")
             else sprintf("an increase of %s unit%s", .f(change), if (change == 1) "" else "s")
  if (change != 1) lines <- c(lines, "",
             sprintf("Change of %s units in %s (%s): expected change in %s = %s x b = %s", .f(change), term, chg_txt, y, .f(change), .f(eff)),
             sprintf("%s%% CI = %s x [%s, %s] = [%s, %s]", .f(100 * conf, 2), .f(change), .f(ci[1]), .f(ci[2]), .f(eci[1]), .f(eci[2])))
  wording <- if (!is.null(di)) sprintf("We are %s%% confident that, on average%s, %s for units with %s = %s differs from that of the baseline %s = %s by a value between %s and %s.",
                                         .f(100 * conf, 2), hold, y, di[1], di[2], di[1], di[3], .f(ci[1]), .f(ci[2]))
             else sprintf("We are %s%% confident that%s, %s in %s is associated with an average change in %s between %s and %s%s.",
                          .f(100 * conf, 2), hold, chg_txt, term, y, .f(eci[1]), .f(eci[2]),
                          if (eci[2] < 0) sprintf(" (an average decrease between %s and %s)", .f(-eci[2]), .f(-eci[1]))
                          else if (eci[1] > 0) sprintf(" (an average increase between %s and %s)", .f(eci[1]), .f(eci[2])) else "")
  wording <- c(wording, if (ci[1] > 0 || ci[2] < 0) sprintf("The interval does not contain 0, so the coefficient is significantly different from 0 at alpha = %s.", .f(1 - conf))
               else sprintf("The interval contains 0, so the coefficient is not significantly different from 0 at alpha = %s.", .f(1 - conf)),
               if (length(coef(m)) > 2) "The effect is quantified for given values of the other explanatory variables: nothing has to be specified about them.")
  .result(sprintf("Effect of %s (coefficient interval)", term), lines, wording, NULL, match.call(),
          estimate = b, se = se, ci = ci, change = change, effect = eff, effect_ci = eci, df = df)
}

.vif <- function(m) {
  X <- model.matrix(m)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (ncol(X) < 2) return(NULL)
  v <- vapply(seq_len(ncol(X)), function(j) {
    r2 <- summary(lm(X[, j] ~ X[, -j]))$r.squared
    if (r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
  stats::setNames(v, colnames(X))
}

#' @rdname regression
#' @export
reg_check <- function(model) {
  m <- .as_lm(model)
  rs <- rstandard(m); lev <- hatvalues(m); cook <- cooks.distance(m)
  n <- nobs(m); p <- length(coef(m))
  tab <- data.frame(obs = names(rs), std_residual = rs, leverage = lev, cooks_d = cook)
  tab <- tab[order(abs(tab$std_residual), decreasing = TRUE), , drop = FALSE]
  lines <- c("Plots: residuals vs fitted (which = 1), normal Q-Q (which = 2), scale-location (which = 3), histogram of standardised residuals.", "",
             "Largest standardised residuals:", .table_lines(.round_df(head(tab, 8))), "",
             sprintf("Flags: |std residual| > 2: %d obs;  leverage > 2p/n = %s: %d obs;  Cook's D > 4/n = %s: %d obs",
                     sum(abs(rs) > 2), .f(2 * p / n), sum(lev > 2 * p / n), .f(4 / n), sum(cook > 4 / n)))
  notes <- NULL
  X <- model.matrix(m)[, -1, drop = FALSE]
  if (ncol(X) >= 2) {
    cm <- cor(X)
    lines <- c(lines, "", "Correlations among explanatory variables:", .table_lines(round(cm, 3), row.names = TRUE))
    up <- abs(cm); up[lower.tri(up, diag = TRUE)] <- NA
    if (any(up >= 0.7, na.rm = TRUE)) notes <- c(notes, "At least one |correlation| >= 0.7 between explanatory variables: warning sign of multicollinearity.")
    v <- .vif(m)
    lines <- c(lines, "", "Variance inflation factors VIF = 1 / (1 - R_j^2):",
               paste0("  ", paste(sprintf("%s = %s", names(v), .f(v, 2)), collapse = "   ")))
    if (any(v > 5)) notes <- c(notes, "A VIF above 5 (or 10) indicates strong multicollinearity.")
  }
  sk <- mean((rs - mean(rs))^3) / sd(rs)^3
  fit <- fitted(m); thirds <- cut(rank(fit, ties.method = "first"), 3, labels = FALSE)
  sds <- tapply(rs, thirds, sd)
  lines <- c(lines, "",
    "Strong assumptions of the linear model Y_i = beta_0 + beta_1 x_1i + ... + beta_k x_ki + eps_i:",
    "  1. E(eps_i) = 0                 (the model is correctly specified: linear in the betas)",
    "  2. Var(eps_i) = sigma^2         (homoscedasticity)",
    "  3. Cor(eps_i, eps_j) = 0, i != j (uncorrelated errors)",
    "  4. eps_i ~ Normal               (together: eps_i iid N(0, sigma^2))", "",
    "How to check them on the residuals:",
    "  Homoscedasticity: Var(eps_i) = sigma^2 for every i (constant, not depending on the explanatory variables)",
    "    tools: residuals vs fitted  plot(mod, which = 1);  sqrt(|standardised residuals|) vs fitted  plot(mod, which = 3)",
    sprintf("    rough check: SD of standardised residuals in the lower / middle / upper third of fitted values = %s",
            paste(.f(sds, 2), collapse = " / ")),
    "  Normality: eps_i ~ N(0, sigma^2)",
    "    tools: normal Q-Q plot  plot(mod, which = 2);  histogram of standardised residuals  hist(rstandard(mod))",
    sprintf("    rough check: skewness of standardised residuals = %s; %s of them beyond +/-2 (about 5%% expected under normality)",
            .f(sk, 2), .pct(mean(abs(rs) > 2), 1)),
    "  Linearity / independence: no pattern (curve) in residuals vs fitted.")
  .with_plot(function() {
    op <- par(mfrow = c(2, 2)); on.exit(par(op))
    plot(m, which = c(1, 2, 3))
    hist(rs, main = "Histogram of standardised residuals", xlab = "standardised residuals", col = "grey85", border = "white", freq = FALSE)
  })
  wording <- c(
    "The strong assumptions of the linear regression model concern the errors: (1) E(eps_i) = 0, (2) Var(eps_i) = sigma^2 (homoscedasticity), (3) Cor(eps_i, eps_j) = 0 for i != j, (4) normally distributed errors; together, the eps_i are iid N(0, sigma^2). They are checked on the residuals, the observable counterpart of the errors.",
    sprintf("Homoscedasticity (constant error variance, Var(eps_i) = sigma^2) is assessed with the residuals-vs-fitted plot, plot(mod, which = 1), or the scale-location plot, plot(mod, which = 3): a random band of constant width around 0 supports it, a funnel shape (spread growing with the fitted values) indicates a violation. Here the spread of the standardised residuals across low / middle / high fitted values is %s.",
            paste(.f(sds, 2), collapse = " / ")),
    sprintf("Normality of the errors is assessed with the normal Q-Q plot, plot(mod, which = 2) (points close to the line; deviations in the tails indicate heavier or lighter tails), and the histogram of the standardised residuals (approximately symmetric and bell-shaped). Here the skewness of the standardised residuals is %s (%s).",
            .f(sk, 2), if (abs(sk) < 0.3) "close to symmetric" else if (sk > 0) "right-skewed: a mild or marked deviation from normality, check the plots" else "left-skewed: a mild or marked deviation from normality, check the plots"),
    "Leverage / Cook's distance: observations with high leverage and large residuals can be influential; check whether the conclusions change without them.",
    "Multicollinearity signs: strong correlations between explanatory variables, a significant global F test with weak individual t tests, unexpected signs, or coefficients that change a lot when a variable is added.")
  .result("Regression diagnostics", lines, wording, notes, match.call(), influence = tab)
}

#' @rdname regression
#' @export
reg_compare <- function(model1, model2, alpha = 0.05) {
  m1 <- .as_lm(model1); m2 <- .as_lm(model2); alpha <- .prob(alpha, "alpha")
  s1 <- summary(m1); s2 <- summary(m2)
  tab <- data.frame(model = c("model 1", "model 2"),
                    formula = c(paste(deparse(formula(m1)), collapse = " "), paste(deparse(formula(m2)), collapse = " ")),
                    k = c(length(coef(m1)) - 1, length(coef(m2)) - 1),
                    n = c(nobs(m1), nobs(m2)),
                    R2 = c(s1$r.squared, s2$r.squared), adj_R2 = c(s1$adj.r.squared, s2$adj.r.squared),
                    resid_SE = c(s1$sigma, s2$sigma))
  lines <- c(.table_lines(.round_df(tab)))
  better <- if (s2$adj.r.squared > s1$adj.r.squared) "model 2" else "model 1"
  wording <- sprintf("R-squared never decreases when variables are added, so compare models with different numbers of predictors using adjusted R-squared: %s has the higher adjusted R-squared (%s vs %s).",
                     better, .f(max(tab$adj_R2)), .f(min(tab$adj_R2)))
  t1 <- attr(terms(m1), "term.labels"); t2 <- attr(terms(m2), "term.labels")
  nested <- nobs(m1) == nobs(m2) && (all(t1 %in% t2) || all(t2 %in% t1)) && !setequal(t1, t2)
  fp <- NULL
  if (nested) {
    small <- if (length(t1) < length(t2)) m1 else m2; big <- if (length(t1) < length(t2)) m2 else m1
    a <- anova(small, big)
    fstat <- a$F[2]; fp <- a$`Pr(>F)`[2]
    lines <- c(lines, "", "Nested models: partial F test",
               sprintf("H0: the extra coefficients (%s) are all 0", paste(setdiff(attr(terms(big), "term.labels"), attr(terms(small), "term.labels")), collapse = ", ")),
               sprintf("F = %s on %s and %s df,  p-value = %s", .f(fstat), a$Df[2], a$Res.Df[2], .fp(fp)),
               .decision_lines(fp, alpha))
    wording <- c(wording, sprintf("The partial F test %s that the added variables improve the model (p-value %s).",
                                  if (fp < alpha) "shows" else "does not show", .fp(fp)))
  }
  # significance of common coefficients in the two models
  c1 <- coef(s1); c2 <- coef(s2); common <- setdiff(intersect(rownames(c1), rownames(c2)), "(Intercept)")
  if (length(common)) {
    ct <- data.frame(term = common, b_model1 = c1[common, 1], p_model1 = c1[common, 4], b_model2 = c2[common, 1], p_model2 = c2[common, 4])
    lines <- c(lines, "", "Coefficients in both models:",
               .table_lines(transform(.round_df(ct), p_model1 = .p_cells(ct$p_model1), p_model2 = .p_cells(ct$p_model2))))
    ch <- common[(c1[common, 4] < alpha) != (c2[common, 4] < alpha)]
    if (length(ch)) {
      big <- if (nrow(c2) >= nrow(c1)) m2 else m1
      X <- model.matrix(big)[, -1, drop = FALSE]
      added <- setdiff(colnames(X), if (identical(big, m2)) rownames(c1) else rownames(c2))
      dib <- .dummy_info(big)
      for (tm in ch) {
        cors <- if (length(added) && tm %in% colnames(X)) sapply(added, function(a) stats::cor(X[, tm], X[, a])) else numeric()
        rtxt <- paste(sprintf("r(%s, %s) = %s", tm, names(cors), .f(cors, 2)), collapse = ", ")
        why <- if (!length(cors)) "Its estimated contribution depends on which other explanatory variables are held constant."
               else if (!is.null(dib[[tm]])) sprintf("The groups %s = %s and %s = %s (baseline) differ in the added variable(s) (%s): part of the average difference seen in the smaller model is due to them; for given values of them the difference between the groups is smaller.",
                                                    dib[[tm]][1], dib[[tm]][2], dib[[tm]][1], dib[[tm]][3], rtxt)
               else sprintf("It is correlated with the added variable(s) (%s): in the smaller model %s also captured part of their effect; once they are included its own contribution, for given values of them, is smaller (a situation close to multicollinearity).",
                            rtxt, tm)
        wording <- c(wording, sprintf("%s is %s in model 1 (p = %s) but %s in model 2 (p = %s). %s",
          tm, if (c1[tm, 4] < alpha) "significant" else "not significant", .fp(c1[tm, 4]),
          if (c2[tm, 4] < alpha) "significant" else "not significant", .fp(c2[tm, 4]), why))
      }
    }
  }
  if (nested && abs(length(t1) - length(t2)) == 1)
    wording <- c(wording, "With a single added variable the partial F test is equivalent to that variable's t test in the larger model (F = t^2, same p-value).")
  .result("Comparing two regression models", lines, wording, NULL, match.call(), table = tab, f_p_value = fp)
}
