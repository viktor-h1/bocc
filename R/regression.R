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
#' @name regression
NULL

.as_lm <- function(model) {
  if (inherits(model, "sc_result") && !is.null(model$model)) model <- model$model
  if (!inherits(model, "lm")) stop("model must be the result of reg_fit() or an lm() model.", call. = FALSE)
  model
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
      out <- c(out, sprintf("Intercept %s: estimated mean of %s when all explanatory variables are 0%s (meaningful only if 0 is a sensible value).",
                            .f(b), y, if (length(xl)) " and every factor is at its baseline level" else ""))
      next
    }
    fac <- NULL
    for (v in names(xl)) {
      for (lv in xl[[v]][-1]) if (identical(nm, paste0(v, lv)) || identical(nm, paste0("`", v, "`", lv))) fac <- c(v, lv, xl[[v]][1])
    }
    if (!is.null(fac)) {
      out <- c(out, sprintf("%s %s: units with %s = %s have a fitted %s that is on average %s %s than units with %s = %s (the baseline)%s.",
                            nm, .f(b), fac[1], fac[2], y, .f(abs(b)), if (b < 0) "lower" else "higher",
                            fac[1], fac[3], hold))
    } else {
      out <- c(out, sprintf("%s %s: when %s increases by 1 unit, the fitted %s changes on average by %s%s.",
                            nm, .f(b), nm, y, .f(b), hold))
    }
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
  b <- coef(m)
  eq <- paste0(y, "-hat = ", .f(b[1]),
               paste0(vapply(seq_along(b)[-1], function(i)
                 sprintf(" %s %s %s", if (b[i] < 0) "-" else "+", .f(abs(b[i])), names(b)[i]), character(1)), collapse = ""))
  ctab <- data.frame(term = rownames(co), estimate = co[, 1], std_error = co[, 2], t = co[, 3], p_value = co[, 4],
                     signif = ifelse(co[, 4] < alpha, "yes", "no"))
  rdf <- m$df.residual
  tcrit <- qt(1 - alpha / 2, rdf)
  lines <- c(
    sprintf("Model: %s   (n = %s, residual df = %s)", paste(deparse(formula(m)), collapse = " "), nobs(m), rdf), "",
    "Fitted equation:", paste0("  ", eq), "",
    sprintf("Coefficients (each t test: H0: beta_j = 0 vs H1: beta_j != 0; signif at alpha = %s; |t| critical = %s):", .f(alpha), .f(tcrit)),
    .table_lines(.round_df(ctab)), "",
    sprintf("R-squared          = %s  (%s of the sample variability of %s is explained by the model)", .f(s$r.squared), .pct(s$r.squared), y),
    sprintf("Adjusted R-squared = %s  (use this to compare models with different numbers of predictors)", .f(s$adj.r.squared)),
    sprintf("Residual SE        = %s  (typical size of a residual, in units of %s)", .f(s$sigma), y))
  if (!is.null(fs)) {
    fcrit <- qf(1 - alpha, fs[2], fs[3])
    lines <- c(lines, "",
               "Global F test: H0: all slope coefficients = 0   vs   H1: at least one slope != 0",
               sprintf("F = %s on %s and %s df;  critical F = %s;  p-value = %s", .f(fs[1]), fs[2], fs[3], .f(fcrit), .fp(fp)),
               .decision_lines(fp, alpha))
  }
  wording <- c(.coef_meaning(m),
               if (!is.null(fs)) sprintf("Global F test: F = %s with p-value %s, so the model is %s at alpha = %s: %s",
                                         .f(fs[1]), .fp(fp), if (fp < alpha) "globally significant" else "not globally significant", .f(alpha),
                                         if (fp < alpha) "at least one explanatory variable helps explain the response (this does not mean every coefficient is significant)."
                                         else "there is insufficient evidence that any explanatory variable helps explain the response."),
               sprintf("R-squared = %s: %s of the sample variability of %s is explained by the fitted model.", .f(s$r.squared), .pct(s$r.squared), y))
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
reg_predict <- function(model, ..., newdata = NULL, conf = 0.95) {
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
  wording <- "The confidence interval estimates the AVERAGE response of all units with these characteristics; the prediction interval covers the response of ONE new unit. The prediction interval is wider because it adds the variability of an individual outcome around the mean (s^2) to the uncertainty in the estimated mean."
  .result("Regression prediction", lines, wording,
          "Check the diagnostics (reg_check) before relying on intervals; avoid extrapolating outside the observed range.",
          match.call(), predictions = out)
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
             sprintf("p-value = %s = %s", .p_text(alt, sprintf("t(%s)", df), stat), .fp(p)), "",
             .decision_lines(p, alpha))
  wording <- sprintf("We test H0: beta = %s for %s against H1: beta %s %s using t = (b - %s)/SE(b) = %s with %s residual degrees of freedom. %s that the coefficient of %s is %s %s.",
                     .f(value), term, .alt_sym(alt), .f(value), .f(value), .f(stat), df, .decision_words(p, alpha),
                     term, .alt_words(alt), .f(value))
  .plot_test(stat, alt, alpha, "t", df, main = paste("Test of", term))
  .result("Regression coefficient t test", lines, wording, NULL, match.call(),
          statistic = stat, p_value = p, critical = crit, df = df,
          decision = if (p < alpha) "reject H0" else "fail to reject H0")
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
  lines <- c("Plots: residuals vs fitted, normal Q-Q, Cook's distance, residuals vs leverage.", "",
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
  .with_plot(function() {
    op <- par(mfrow = c(2, 2)); on.exit(par(op))
    plot(m, which = c(1, 2, 4, 5))
  })
  wording <- c(
    "Residuals vs fitted: look for a random cloud around 0 with constant spread (linearity, homoskedasticity); a funnel shape suggests heteroskedasticity, a curve suggests non-linearity.",
    "Normal Q-Q: points close to the line support approximately normal errors.",
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
  .result("Comparing two regression models", lines, wording, NULL, match.call(), table = tab, f_p_value = fp)
}
