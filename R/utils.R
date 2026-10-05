# ------------------------------------------------------------
# Small shared helpers: options, number formatting, argument
# normalisation, p-values / critical values, scripted input.
# ------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

.opt <- function(name, default) getOption(paste0("statcram.", name), default)

.digits <- function() .opt("digits", 4)

.plots_on <- function() isTRUE(.opt("plot", TRUE))

# Format numbers for display: fixed decimals, trailing zeros dropped,
# tiny non-zero values in scientific notation.
.f <- function(x, d = .digits()) {
  if (is.null(x) || !length(x)) return("NA")
  vapply(x, function(v) {
    if (is.na(v)) return("NA")
    if (is.infinite(v)) return(if (v > 0) "Inf" else "-Inf")
    if (v != 0 && abs(v) < 10^(-d)) return(formatC(v, digits = 3, format = "e"))
    out <- formatC(round(v, d), format = "f", digits = d, drop0trailing = TRUE)
    if (out == "-0") "0" else out
  }, character(1))
}

# p-values: more precision, scientific when very small.
.fp <- function(p) {
  vapply(p, function(v) {
    if (is.na(v)) return("NA")
    if (v < 2.2e-16) "< 2.2e-16" else if (v < 1e-4) formatC(v, digits = 3, format = "e") else formatC(v, digits = 4, format = "f")
  }, character(1))
}

.pct <- function(x, d = 2) paste0(.f(100 * x, d), "%")

# Accept 0.95 or 95 (or "95%"); always return a probability in (0, 1).
.prob <- function(x, what = "value") {
  if (is.character(x)) x <- as.numeric(sub("%", "", x, fixed = TRUE))
  if (is.null(x) || length(x) != 1 || is.na(x)) stop(what, " must be a single number.", call. = FALSE)
  if (x >= 1 && x < 100) x <- x / 100
  if (x <= 0 || x >= 1) stop(what, " must be between 0 and 1 (or 0 and 100 as a percentage).", call. = FALSE)
  x
}

# Normalise the alternative hypothesis. Accepts symbols and words.
.alt <- function(alt) {
  if (is.null(alt) || !length(alt)) stop("Give alt = \"<\", \">\" or \"!=\".", call. = FALSE)
  a <- tolower(gsub("\\s+", "", as.character(alt[1])))
  if (a %in% c("two.sided", "twosided", "two-sided", "two", "2", "!=", "<>", "=/=", "ne",
               "different", "differs", "neq", "two.tailed", "twotailed"))
    return("two.sided")
  if (a %in% c("less", "<", "l", "lower", "lt", "smaller", "left", "decrease", "less.than"))
    return("less")
  if (a %in% c("greater", ">", "g", "higher", "gt", "larger", "more", "right", "increase", "greater.than"))
    return("greater")
  stop("alt must be \"<\", \">\" or \"!=\" (or less / greater / two.sided).", call. = FALSE)
}

.alt_sym <- function(alt) switch(alt, less = "<", greater = ">", two.sided = "!=")

# Two-sided p-value etc. for z / t / chisq / F statistics.
.pval <- function(stat, alt, dist = c("z", "t"), df = NULL) {
  dist <- match.arg(dist)
  cdf <- if (dist == "z") function(q) pnorm(q) else function(q) pt(q, df)
  switch(alt,
    less = cdf(stat),
    greater = 1 - cdf(stat),
    two.sided = 2 * (1 - cdf(abs(stat))))
}

.crit <- function(alt, alpha, dist = c("z", "t"), df = NULL) {
  dist <- match.arg(dist)
  qf <- if (dist == "z") function(p) qnorm(p) else function(p) qt(p, df)
  switch(alt,
    less = qf(alpha),
    greater = qf(1 - alpha),
    two.sided = c(-qf(1 - alpha / 2), qf(1 - alpha / 2)))
}

.dist_label <- function(dist, df = NULL) {
  if (dist == "z") "Z" else sprintf("t(%s)", .f(df, 2))
}

# Text for the critical value and rejection region.
.reject_region <- function(alt, crit, stat_name = "stat") {
  switch(alt,
    less = sprintf("reject H0 if %s < %s", stat_name, .f(crit)),
    greater = sprintf("reject H0 if %s > %s", stat_name, .f(crit)),
    two.sided = sprintf("reject H0 if %s < %s or %s > %s", stat_name, .f(crit[1]), stat_name, .f(crit[2])))
}

.p_text <- function(alt, stat_name, stat) {
  switch(alt,
    less = sprintf("P(%s < %s)", stat_name, .f(stat)),
    greater = sprintf("P(%s > %s)", stat_name, .f(stat)),
    two.sided = sprintf("2 * P(%s > |%s|)", stat_name, .f(stat)))
}

.decision_lines <- function(p, alpha) {
  rej <- p < alpha
  c(sprintf("p-value = %s %s alpha = %s  ->  %s",
            .fp(p), if (rej) "<" else ">=", .f(alpha), if (rej) "REJECT H0" else "FAIL TO REJECT H0"))
}

.decision_words <- function(p, alpha) {
  rej <- p < alpha
  sprintf("Since the p-value (%s) is %s the significance level alpha = %s, we %s. There is %s empirical evidence",
          .fp(p), if (rej) "below" else "not below", .f(alpha),
          if (rej) "reject H0" else "fail to reject H0 (we do not 'accept' H0)",
          if (rej) "sufficient" else "insufficient")
}

# Readable label for an argument expression: df$spend -> "spend".
.label <- function(expr, default = "x") {
  if (is.null(expr)) return(default)
  if (is.call(expr)) {
    fn <- as.character(expr[[1]])[1]
    if (fn == "$" && length(expr) == 3) return(as.character(expr[[3]]))
    if (fn == "[[" && length(expr) == 3 && is.character(expr[[3]])) return(expr[[3]])
  }
  out <- paste(deparse(expr, width.cutoff = 60L), collapse = " ")
  if (nchar(out) > 40) out <- paste0(substr(out, 1, 37), "...")
  out
}

# Text of the call used to produce a result, for the "Re-run" line.
.call_text <- function(cl) {
  if (is.null(cl)) return(NULL)
  txt <- paste(deparse(cl, width.cutoff = 500L), collapse = " ")
  txt <- gsub("\\s+", " ", txt)
  sub("^statcram::", "", txt)
}

.rule <- function(text = "", width = 70, char = "-") {
  n <- max(3, width - nchar(text))
  paste0(text, strrep(char, n))
}

.width <- function() min(max(getOption("width", 80), 50), 90)

# Read one line of input. Tests (and scripted demos) can queue answers in
# options(statcram.input = c("4", "1", ...)); otherwise readline() is used.
.read_line <- function(prompt = "") {
  q <- getOption("statcram.input")
  if (!is.null(q)) {
    if (!length(q)) stop("statcram: scripted input exhausted.", call. = FALSE)
    ans <- as.character(q[[1]])
    options(statcram.input = q[-1])
    cat(prompt, ans, "\n", sep = "")
    return(ans)
  }
  if (!interactive())
    stop("statcram menus need an interactive R session (RStudio console).", call. = FALSE)
  readline(prompt)
}

# Run plotting code safely: never let a plotting problem kill a result.
.with_plot <- function(expr_fun) {
  if (!.plots_on()) return(invisible(NULL))
  tryCatch(expr_fun(), error = function(e) message("(plot skipped: ", conditionMessage(e), ")"))
  invisible(NULL)
}

.need <- function(..., msg) {
  vals <- list(...)
  if (any(vapply(vals, is.null, logical(1)))) stop(msg, call. = FALSE)
}

# ---------- UBStats equivalents (course package) ----------

.ub_raw_note <- "UBStats functions need the raw data: with summary numbers, use the formulas above."

# Build "FUN(arg = value, ...)" from language objects / constants, dropping NULLs.
.ub_call <- function(fn, ...) {
  args <- list(...)
  args <- args[!vapply(args, is.null, logical(1))]
  if (!length(args)) return(paste0(fn, "()"))
  txt <- vapply(names(args), function(nm) {
    v <- args[[nm]]
    val <- if (is.language(v)) paste(deparse(v, width.cutoff = 500L), collapse = " ")
           else paste(deparse(v, width.cutoff = 500L), collapse = " ")
    paste(nm, "=", val)
  }, character(1))
  paste0(fn, "(", paste(txt, collapse = ", "), ")")
}

# Expression selecting one group: x[group == "level"].
.ub_subset <- function(x_expr, g_expr, level) call("[", x_expr, call("==", g_expr, level))

# UBStats "by" uses the standard order of the grouping variable (factor levels,
# else alphabetical / numeric); TRUE when the requested levels match it.
.ub_by_ok <- function(group, levels) {
  if (is.null(group)) return(FALSE)
  g <- if (is.data.frame(group)) group[[1]] else group
  lv <- .cats(g[!is.na(g)])
  length(lv) == 2 && (is.null(levels) || identical(as.character(levels), lv))
}

# ---------- hypothesis-test helpers (book chapter 7) ----------

# H0 always contains the equality; with a one-sided H1 it is composite and the
# boundary value (the H0 value closest to H1) gives the critical value and p-value.
.hyp_lines <- function(param, null_txt, alt) {
  h0 <- switch(alt, less = sprintf("%s >= %s  (or %s = %s)", param, null_txt, param, null_txt),
               greater = sprintf("%s <= %s  (or %s = %s)", param, null_txt, param, null_txt),
               two.sided = sprintf("%s = %s", param, null_txt))
  c(sprintf("H0: %s      H1: %s %s %s", h0, param, .alt_sym(alt), null_txt),
    if (alt != "two.sided")
      sprintf("    (critical value and p-value use the boundary value %s = %s, the H0 value closest to H1)", param, null_txt))
}

.hyp_text <- function(param, null_txt, alt) {
  h0 <- switch(alt, less = ">=", greater = "<=", two.sided = "=")
  sprintf("H0: %s %s %s against H1: %s %s %s", param, h0, null_txt, param, .alt_sym(alt), null_txt)
}

# p-value as the smallest alpha at which H0 is rejected.
.p_reading <- function(p) {
  a <- c(0.1, 0.05, 0.025, 0.01, 0.001)
  rej <- a[p < a]; acc <- a[p >= a]
  sprintf("Reading: the p-value is the smallest alpha at which H0 is rejected -> %s%s%s.",
          if (length(rej)) paste0("rejected at alpha = ", paste(.f(rej), collapse = ", ")) else "",
          if (length(rej) && length(acc)) "; " else "",
          if (length(acc)) paste0("not rejected at alpha = ", paste(.f(acc), collapse = ", ")) else "")
}

# p-values in UBStats-style tables.
.p_cells <- function(p) ifelse(p < 1e-4, "<0.0001", formatC(round(p, 4), format = "f", digits = 4, drop0trailing = TRUE))

# UBStats-style rows for a mean-type test: Normal.Approx and Student-t.
.test_rows <- function(est, se, null, alt, df) {
  stat <- (est - null) / se
  data.frame(row.names = c("Normal.Approx", "Student-t"), stat = stat,
             p = c(.pval(stat, alt, "z"), .pval(stat, alt, "t", df)))
}

# UBStats alternative argument (omitted when two-sided, the default).
.ub_alt <- function(alt) if (alt == "two.sided") NULL else alt

# Extra wording when the z and t versions lead to different decisions.
.zt_disagree <- function(p_z, p_t, alpha) {
  if (is.null(p_z) || is.null(p_t) || (p_z < alpha) == (p_t < alpha)) return(NULL)
  sprintf("At alpha = %s the normal approximation (p = %s) and the Student t version (p = %s) lead to different decisions: the t test is the more conservative one (heavier tails), so with a normal population or a moderate n use t.",
          .f(alpha), .fp(p_z), .fp(p_t))
}
