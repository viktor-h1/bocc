# ------------------------------------------------------------
# Forms of the sc_app() panel: one entry per procedure of the first
# partial (descriptive statistics, random variables and the CLT,
# estimation of one mean / one proportion). Each field becomes a
# standard shiny input; .app_call() turns the filled-in values into the
# same call the menu builds with .mk().
# ------------------------------------------------------------

# One form field.
#   type: data (a vector or expression; kind = numeric / categorical / any),
#         dataframe, num, nums, vals (numbers or category names), levels
#         (categories of another field, src), choice, check, pick (choose
#         which argument gets the typed value)
#   arg:  argument name in the call (NA = helper field, not passed)
#   when: list(field = value) - shown and used only in that case
.fld <- function(id, label, type, arg = id, when = NULL, required = FALSE, default = NULL, hint = NULL,
                 kind = "any", choices = NULL, src = NULL, multiple = TRUE, skip = NULL) {
  list(id = id, label = label, type = type, arg = arg, when = when, required = required, default = default,
       hint = hint, kind = kind, choices = choices, src = src, multiple = multiple, skip = skip)
}

.src_fld <- function(choices) .fld("source", "Data", "choice", arg = NA, choices = choices, default = names(choices)[1])
.q_fld <- function(choices = c(below = "P(X < a)", above = "P(X > a)", between = "P(a < X < b)",
                               quantile = "quantile", middle = "central interval")) {
  .fld("q", "What is asked", "pick", choices = choices, default = "below",
       hint = "a  (between: a b;  quantile: the probability, e.g. 0.95)")
}
.conf_fld <- function() .fld("conf", "Confidence level", "num", default = "0.95", skip = "0.95")

.app_specs <- list(
  # ---- Describe data ----
  desc_vars = list(topic = "Describe data", label = "Variable types in a data frame", fn = "desc_vars", fields = list(
    .fld("data", "Data frame", "dataframe", required = TRUE))),
  desc_freq = list(topic = "Describe data", label = "Frequency table (few distinct values)", fn = "desc_freq", fields = list(
    .src_fld(c(raw = "Raw data", table = "Typed frequency table")),
    .fld("x", "Variable", "data", when = list(source = "raw"), required = TRUE),
    .fld("tvals", "Values / categories", "vals", arg = NA, when = list(source = "table"), required = TRUE, hint = "e.g. 1 2 3 4 5 6"),
    .fld("tcounts", "Counts", "nums", arg = NA, when = list(source = "table"), required = TRUE, hint = "e.g. 5401 7340 8238 2561 0 700"),
    .fld("order", "Order of the categories (ordinal: click them from lowest to highest)", "levels", src = "x", when = list(source = "raw")),
    .fld("at_most", "Share at most", "vals", hint = "a value or a category"),
    .fld("at_least", "Share at least", "vals"),
    .fld("event", "Combined share of these categories", "levels", src = "x", when = list(source = "raw")),
    .fld("compare", "Compare with another distribution (optional)", "data"),
    .fld("plot", "Plot", "choice", choices = c(auto = "auto", bars = "bars", pie = "pie", spike = "spike", cum = "cumulative"),
         default = "auto"))),
  desc_classes = list(topic = "Describe data", label = "Classes: densities, histogram, ogive", fn = "desc_classes", fields = list(
    .src_fld(c(raw = "Raw data", table = "Class table", density = "Densities read off a histogram")),
    .fld("x", "Numerical variable", "data", kind = "numeric", when = list(source = "raw"), required = TRUE),
    .fld("breaks", "Classes", "nums", required = TRUE, hint = "K equal classes (e.g. 10) or the limits (0 10 20 30 60 90 150)"),
    .fld("ftype", "The table gives", "choice", arg = NA, when = list(source = "table"), choices = c(freq = "counts", prop = "proportions / %"),
         default = "freq"),
    .fld("fvals", "One value per class", "nums", arg = NA, when = list(source = "table"), required = TRUE),
    .fld("density", "Densities, one per class (NA for the one to obtain as 1 - the others)", "nums", when = list(source = "density"), required = TRUE),
    .fld("n", "n (if only proportions are given)", "num"),
    .fld("at_most", "Share at most", "nums", hint = "one or more values"),
    .fld("at_least", "Share at least", "nums"),
    .fld("between", "Share between", "nums", hint = "a b"),
    .fld("split", "Split into two subgroups at", "num", hint = "a class limit"),
    .fld("plot", "Plot", "choice", choices = c(both = "histogram + ogive", hist = "histogram", ogive = "ogive"), default = "both"))),
  desc_summary = list(topic = "Describe data", label = "Summary of one numerical variable", fn = "desc_summary", fields = list(
    .fld("x", "Numerical variable", "data", kind = "numeric", required = TRUE),
    .fld("probs", "Percentiles", "nums", hint = "default 0.05 0.10 0.90 0.95 0.99"),
    .fld("value", "Is this value extreme?", "nums"),
    .fld("at_most", "Exact share at most", "nums"),
    .fld("below", "Exact share below", "nums"),
    .fld("at_least", "Exact share at least", "nums"),
    .fld("between", "Exact share between (a <= X < b)", "nums", hint = "a b"),
    .fld("population", "The data are the whole population (divisor N)", "check", default = FALSE))),
  desc_compare = list(topic = "Describe data", label = "Numerical variable across groups", fn = "desc_compare", fields = list(
    .fld("x", "Numerical variable", "data", kind = "numeric", required = TRUE),
    .fld("group", "Groups", "data", kind = "categorical", required = TRUE),
    .fld("group2", "Second grouping variable (optional)", "data", kind = "categorical"),
    .fld("value", "Is this value extreme within a group?", "nums"),
    .fld("plot", "Plot", "choice", choices = c(boxplot = "boxplots", hist = "histograms"), default = "boxplot"))),
  desc_cv = list(topic = "Describe data", label = "Dispersion: SD vs coefficient of variation", fn = "desc_cv", fields = list(
    .fld("x1", "Variable (raw data)", "data", arg = "", kind = "numeric"),
    .fld("x2", "Another variable (raw data)", "data", arg = "", kind = "numeric"),
    .fld("mean", "Means given in the question", "nums", hint = "e.g. 4.875 (or several)"),
    .fld("sd", "Their standard deviations", "nums"),
    .fld("var", "... or their variances", "nums"),
    .fld("names", "Names for those variables", "vals", hint = "e.g. classes"))),
  desc_prop = list(topic = "Describe data", label = "Sample proportion and its standard error", fn = "desc_prop", fields = list(
    .src_fld(c(raw = "Raw data", summary = "Count and n")),
    .fld("x", "Categorical variable", "data", kind = "categorical", when = list(source = "raw"), required = TRUE),
    .fld("event", "Category of interest", "levels", src = "x", multiple = FALSE, when = list(source = "raw")),
    .fld("count", "Number of units with the characteristic", "num", when = list(source = "summary"), required = TRUE),
    .fld("n", "n", "num", when = list(source = "summary"), required = TRUE))),
  desc_crosstab = list(topic = "Describe data", label = "Two variables with few values (cross table)", fn = "desc_crosstab", fields = list(
    .fld("x", "Rows: X (groups)", "data", required = TRUE),
    .fld("y", "Columns: Y (response)", "data", required = TRUE),
    .fld("order_y", "Order of the Y categories (ordinal)", "levels", src = "y"),
    .fld("y_event", "Share of these Y categories in each group", "levels", src = "y"),
    .fld("plot", "Bar chart", "choice", choices = c(stacked = "stacked: Y | X", beside = "side by side: joint"), default = "stacked"))),
  desc_cor = list(topic = "Describe data", label = "Two numerical variables: covariance, correlation", fn = "desc_cor", fields = list(
    .fld("x", "X", "data", kind = "numeric", required = TRUE),
    .fld("y", "Y", "data", kind = "numeric", required = TRUE),
    .fld("population", "Population formulas (divisor N)", "check", default = FALSE))),
  # ---- Random variables and the CLT ----
  rv_discrete = list(topic = "Random variables and the CLT", label = "Discrete random variable: E(X), Var(X), F(x)", fn = "rv_discrete", fields = list(
    .fld("values", "Values", "nums", required = TRUE),
    .fld("probs", "Probabilities", "nums", required = TRUE),
    .fld("at_most", "P(X <= x) for x =", "num"),
    .fld("at_least", "P(X >= x) for x =", "num"))),
  prob_normal = list(topic = "Random variables and the CLT", label = "Normal: probabilities and percentiles", fn = "prob_normal", fields = list(
    .fld("mean", "Mean", "num", default = "0", required = TRUE),
    .fld("sd", "Standard deviation", "num", default = "1", required = TRUE, hint = "e.g. 2 or sqrt(380/80)"),
    .q_fld())),
  rv_lincomb = list(topic = "Random variables and the CLT", label = "Linear combination a1 X1 + ... + c", fn = "rv_lincomb", fields = list(
    .fld("a", "Coefficients a", "nums", required = TRUE),
    .fld("mu", "Means", "nums", required = TRUE),
    .fld("sigma", "Standard deviations", "nums", required = TRUE),
    .fld("rho", "Correlation (two variables)", "num"),
    .fld("c", "Constant c", "num"),
    .q_fld())),
  rv_iid = list(topic = "Random variables and the CLT", label = "Sum or mean of n iid variables (CLT)", fn = "rv_iid", fields = list(
    .fld("mu", "Mean of one X", "num", required = TRUE),
    .fld("sigma", "SD of one X", "num", required = TRUE, hint = "e.g. sqrt(380)"),
    .fld("n", "n", "num", required = TRUE),
    .fld("stat", "Statistic", "choice", choices = c(mean = "sample mean", sum = "sum"), default = "mean"),
    .q_fld())),
  rv_prop = list(topic = "Random variables and the CLT", label = "Sample proportion p-hat (CLT)", fn = "rv_prop", fields = list(
    .fld("p", "Population proportion p", "num", required = TRUE),
    .fld("n", "n", "num", required = TRUE),
    .q_fld())),
  # ---- Estimation: one mean, one proportion ----
  est_mean = list(topic = "Estimation (one mean, one proportion)", label = "Point estimate of a mean + standard error", fn = "est_mean", fields = list(
    .src_fld(c(raw = "Raw data", summary = "Summary numbers")),
    .fld("x", "Numerical variable", "data", kind = "numeric", when = list(source = "raw"), required = TRUE),
    .fld("group", "Estimate in each group of (optional)", "data", kind = "categorical", when = list(source = "raw")),
    .fld("levels", "Only these groups (optional)", "levels", src = "group", when = list(source = "raw")),
    .fld("xbar", "Sample mean", "num", when = list(source = "summary"), required = TRUE),
    .fld("s", "Sample SD s", "num", when = list(source = "summary")),
    .fld("sigma", "Population SD sigma (if known)", "num"),
    .fld("n", "n", "num", when = list(source = "summary"), required = TRUE))),
  ci_mean = list(topic = "Estimation (one mean, one proportion)", label = "Confidence interval for a mean", fn = "ci_mean", fields = list(
    .src_fld(c(raw = "Raw data", summary = "Summary numbers")),
    .fld("x", "Numerical variable", "data", kind = "numeric", when = list(source = "raw"), required = TRUE),
    .fld("xbar", "Sample mean", "num", when = list(source = "summary"), required = TRUE),
    .fld("s", "Sample SD s", "num", when = list(source = "summary")),
    .fld("sigma", "Population SD sigma (if known)", "num"),
    .fld("n", "n", "num", when = list(source = "summary"), required = TRUE),
    .conf_fld(),
    .fld("method", "sigma unknown", "choice", choices = c(t = "t (normal population)", z = "z (large n, CLT)"), default = "t"))),
  ci_prop = list(topic = "Estimation (one mean, one proportion)", label = "Confidence interval for a proportion", fn = "ci_prop", fields = list(
    .src_fld(c(raw = "Raw data", summary = "Count and n")),
    .fld("x", "Categorical variable", "data", kind = "categorical", when = list(source = "raw"), required = TRUE),
    .fld("event", "Category of interest", "levels", src = "x", multiple = FALSE, when = list(source = "raw")),
    .fld("count", "Count", "num", when = list(source = "summary"), required = TRUE),
    .fld("n", "n", "num", when = list(source = "summary"), required = TRUE),
    .conf_fld())),
  n_mean = list(topic = "Estimation (one mean, one proportion)", label = "Sample size for a mean", fn = "n_mean", fields = list(
    .fld("target", "Requirement", "pick", choices = c(margin = "margin of error", width = "width", se = "standard error"), default = "margin"),
    .fld("sigma", "Population SD sigma", "num", required = TRUE),
    .conf_fld())),
  n_prop = list(topic = "Estimation (one mean, one proportion)", label = "Sample size for a proportion", fn = "n_prop", fields = list(
    .fld("target", "Requirement", "pick", choices = c(margin = "margin of error", width = "width", se = "standard error"), default = "margin"),
    .fld("p", "Guess of p", "num", default = "0.5", skip = "0.5"),
    .conf_fld()))
)

# "10 25 30" / "0.1; 0.2" -> "c(10, 25, 30)"; a single number stays as it is;
# anything else (sqrt(380/80), mean(x), c(1, 2)) is kept as an R expression.
.app_nums <- function(txt) {
  txt <- trimws(txt)
  tok <- strsplit(txt, "[[:space:];,]+")[[1]]
  tok <- tok[nzchar(tok)]
  isnum <- grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?%?$", tok) | tok == "NA"
  if (length(tok) && all(isnum)) {
    tok <- ifelse(grepl("%$", tok), as.character(as.numeric(sub("%$", "", tok)) / 100), tok)
    return(if (length(tok) == 1) tok else paste0("c(", paste(tok, collapse = ", "), ")"))
  }
  txt
}

# Numbers, or category names (quoted).
.app_vals <- function(txt) {
  tok <- strsplit(trimws(txt), "[[:space:];,]+")[[1]]
  tok <- tok[nzchar(tok)]
  if (!length(tok)) return(NULL)
  if (all(!is.na(suppressWarnings(as.numeric(tok))))) return(.code(.app_nums(txt)))
  if (length(tok) == 1) tok else tok
}

.app_empty <- function(v) is.null(v) || !length(v) || (length(v) == 1 && (is.na(v) || !nzchar(trimws(as.character(v)))))

# Build the call for one procedure from the form values (a list keyed by field id).
# Returns a call, or a character message saying what is missing.
.app_call <- function(spec, v) {
  active <- function(f) is.null(f$when) || identical(as.character(v[[names(f$when)[1]]] %||% ""), f$when[[1]])
  args <- list()
  for (f in spec$fields) {
    if (!active(f)) next
    val <- v[[f$id]]
    if (f$type == "pick") {
      txt <- v[[paste0(f$id, "_value")]]
      if (.app_empty(txt)) {
        if (f$required || f$id %in% c("target")) return(sprintf("Type the value for: %s.", f$label))
        next
      }
      args[[val %||% f$default]] <- .code(.app_nums(txt))
      next
    }
    if (.app_empty(val)) {
      if (f$required) return(sprintf("Fill in: %s.", f$label))
      next
    }
    a <- switch(f$type,
      data = , dataframe = .code(trimws(val)),
      num = , nums = if (!is.null(f$skip) && identical(trimws(val), f$skip)) NULL else .code(.app_nums(val)),
      vals = .app_vals(val),
      levels = as.character(val),
      choice = if (identical(val, f$default)) NULL else val,
      check = if (isTRUE(val) == isTRUE(f$default)) NULL else isTRUE(val),
      NULL)
    if (is.null(a) || is.na(f$arg)) next
    if (identical(f$arg, "")) args[[length(args) + 1]] <- a else args[[f$arg]] <- a
  }
  # inputs that combine several fields
  if (identical(spec$fn, "desc_freq") && identical(v$source, "table")) {
    vals <- strsplit(trimws(v$tvals), "[[:space:];,]+")[[1]]; cnt <- eval(str2lang(.app_nums(v$tcounts)))
    if (length(vals) != length(cnt)) return("Give one count per value.")
    args <- c(list(x = .code(sprintf("c(%s)", paste(sprintf("\"%s\" = %s", vals, cnt), collapse = ", ")))), args)
  }
  if (identical(spec$fn, "desc_classes") && identical(v$source, "table"))
    args[[v$ftype %||% "freq"]] <- .code(.app_nums(v$fvals))
  if (is.null(names(args))) names(args) <- rep("", length(args))
  .mk(spec$fn, args)
}

# Categories of the variable chosen in another field (for level pickers).
.app_levels <- function(expr) {
  if (.app_empty(expr)) return(character())
  x <- tryCatch(.eval_expr(expr), error = function(e) NULL)
  if (is.null(x)) return(character())
  fr <- .as_freq(x)
  if (!is.null(fr)) return(names(fr))
  x <- .one_column(x, "x")
  if (is.numeric(x) && length(unique(x)) > 30) return(character())
  .cats(x[!is.na(x)])
}
