# ------------------------------------------------------------
# Prompt helpers used by the sc() menu. Every prompt understands
#   b = back to the list of procedures, m = main menu, q = quit.
# Variable pickers re-scan the workspace every time they are shown.
# ------------------------------------------------------------

.signal <- function(cls) stop(structure(class = c(cls, "sc_nav", "error", "condition"),
                                        list(message = cls, call = NULL)))

.mq <- function(prompt, default = NULL, end = ": ") {
  shown <- if (!is.null(default) && nzchar(default)) paste0(" [", default, "]") else ""
  a <- trimws(.read_line(paste0(prompt, shown, end)))
  switch(tolower(a), b = .signal("sc_back"), m = .signal("sc_main"), q = , quit = .signal("sc_quit"))
  if (!nzchar(a) && !is.null(default)) return(as.character(default))
  a
}

# Numbered choice; returns the index.
.ask_choice <- function(title, labels, default = NULL) {
  cat(title, "\n", sep = "")
  for (i in seq_along(labels)) cat(sprintf(" %2d  %s\n", i, labels[i]))
  repeat {
    a <- .mq("Choose a number", default)
    v <- suppressWarnings(as.integer(a))
    if (!is.na(v) && v >= 1 && v <= length(labels)) return(v)
    cat("  Type one of the numbers above (b = back, m = main menu, q = quit).\n")
  }
}

# A single number; accepts "3,5", "12%", or any R expression (sqrt(25), mean(x)).
.ask_num <- function(prompt, default = NULL, allow_empty = FALSE) {
  repeat {
    a <- .mq(prompt, if (is.null(default)) NULL else as.character(default))
    if (!nzchar(a)) {
      if (allow_empty) return(NULL)
      cat("  Please type a number.\n"); next
    }
    a2 <- sub("%$", "", a)
    if (grepl("^[-+]?[0-9]*,[0-9]+$", a2)) a2 <- sub(",", ".", a2, fixed = TRUE)
    v <- tryCatch(eval(str2lang(a2), .GlobalEnv), error = function(e) NULL)
    if (is.numeric(v) && length(v) == 1 && !is.na(v)) return(as.numeric(v))
    cat("  That is not a single number. Examples: 52.3   3,5   sqrt(25)   mean(x)\n")
  }
}

# Several numbers: "10 25 30", "0,1; 0,2", or an R expression like c(1, 2) or seq(10, 60, 5).
.ask_nums <- function(prompt, n = NULL, allow_empty = FALSE) {
  repeat {
    a <- .mq(prompt)
    if (!nzchar(a)) { if (allow_empty) return(NULL); cat("  Please type the numbers.\n"); next }
    v <- if (grepl("[A-Za-z(]", a)) tryCatch(eval(str2lang(a), .GlobalEnv), error = function(e) NULL)
         else suppressWarnings(as.numeric(vapply(.split_values(a), .clean_value, "")))
    if (is.numeric(v) && length(v) && !anyNA(v) && (is.null(n) || length(v) == n)) return(as.numeric(v))
    cat(sprintf("  Type %s numbers separated by spaces (decimal point or comma), e.g. 10 25 30\n",
                if (is.null(n)) "the" else n))
  }
}

.ask_prob <- function(prompt, default) {
  repeat {
    v <- .ask_num(prompt, default)
    out <- tryCatch(.prob(v, prompt), error = function(e) NULL)
    if (!is.null(out)) return(out)
    cat("  Type a value between 0 and 1, or a percentage such as 95.\n")
  }
}

.ask_yes <- function(prompt, default = FALSE) {
  a <- tolower(.mq(paste0(prompt, if (default) " [Y/n]" else " [y/N]"), ""))
  if (!nzchar(a)) return(default)
  a %in% c("y", "yes", "s", "si")
}

.ask_alt <- function(param, null_txt) {
  i <- .ask_choice(sprintf("Alternative hypothesis H1:"),
                   c(sprintf("%s < %s   (lower / less / decrease)", param, null_txt),
                     sprintf("%s > %s   (higher / greater / increase)", param, null_txt),
                     sprintf("%s != %s  (different / changed / two-sided)", param, null_txt)))
  c("<", ">", "!=")[i]
}

.eval_expr <- function(txt) eval(str2lang(txt), .GlobalEnv)

# Pick data from what is loaded right now, or type any R expression.
# want: "vector" (numeric or categorical vectors) or "table" (count tables / frequency lists / class tables).
# Returns list(expr = "<R code>") or list(summary = TRUE).
.ask_data <- function(what, want = c("vector", "table", "both"), allow_summary = FALSE, hint = NULL) {
  want <- match.arg(want)
  repeat {
    lo <- .live_objects()
    if (want == "vector") lo <- lo[lo$kind != "table", , drop = FALSE]
    if (want == "table") lo <- lo[lo$kind == "table", , drop = FALSE]
    if (nrow(lo) > 40 && length(unique(lo$group)) > 1) {
      groups <- unique(lo$group)
      g <- .ask_choice(sprintf("Where is %s?", what), groups)
      lo <- lo[lo$group == groups[g], , drop = FALSE]
    }
    cat(sprintf("\nChoose %s:%s\n", what, if (!is.null(hint)) paste0("  (", hint, ")") else ""))
    if (!nrow(lo)) cat("  (nothing suitable is loaded yet)\n")
    w <- if (nrow(lo)) max(nchar(lo$expr)) else 0
    for (g in unique(lo$group)) {
      cat("  ", g, "\n", sep = "")
      for (i in which(lo$group == g)) cat(sprintf("   %3d  %s   %s\n", i, formatC(lo$expr[i], width = -w), lo$tag[i]))
    }
    cat("  Or type any R expression, e.g. ",
        if (want == "table") "matrix(c(20, 30, 25, 25), nrow = 2)" else "df$after - df$before   or   c(12, 15, 9)", "\n", sep = "")
    cat("  t = type a table from paper first", if (allow_summary) "     s = enter summary numbers instead", "\n", sep = "")
    a <- .mq(">", end = " ")
    if (!nzchar(a)) next
    if (allow_summary && tolower(a) == "s") return(list(summary = TRUE))
    if (tolower(a) == "t") { sc_table(); next }
    v <- suppressWarnings(as.integer(a))
    if (!is.na(v) && as.character(v) == a && v >= 1 && v <= nrow(lo)) return(list(expr = lo$expr[v], summary = FALSE))
    val <- tryCatch(.eval_expr(a), error = function(e) e)
    if (inherits(val, "error")) { cat("  R could not use that: ", conditionMessage(val), "\n", sep = ""); next }
    if (is.null(val) || is.function(val)) { cat("  That is not data.\n"); next }
    return(list(expr = a, summary = FALSE))
  }
}

# Category (event) of a categorical vector; NULL when x is logical or 0/1.
.ask_event <- function(expr, prompt = "Which category is the event of interest (the 'success')?") {
  x <- .eval_expr(expr)
  if (is.data.frame(x)) x <- x[[1]]
  x <- x[!is.na(x)]
  if (is.logical(x) || (is.numeric(x) && all(x %in% c(0, 1)))) return(NULL)
  lv <- .cats(x)
  if (length(lv) > 30) { cat("  (", length(lv), " categories; type the category name)\n", sep = ""); return(.mq("Category")) }
  lv[.ask_choice(prompt, lv)]
}

.ask_two_levels <- function(expr) {
  g <- .eval_expr(expr)
  if (is.data.frame(g)) g <- g[[1]]
  lv <- .cats(g)
  if (length(lv) < 2) stop("The grouping variable has fewer than two categories.", call. = FALSE)
  if (length(lv) == 2) { cat(sprintf("Groups: 1 = %s, 2 = %s\n", lv[1], lv[2])); return(lv) }
  a <- .ask_choice("Group 1:", lv)
  rest <- lv[-a]
  b <- .ask_choice("Group 2:", rest)
  c(lv[a], rest[b])
}

# Build a call fn(arg = value, ...) from a named list. Character values marked
# with I() are inserted as R code (expressions), everything else as constants.
.mk <- function(fn, args) {
  args <- args[!vapply(args, is.null, logical(1))]
  args <- lapply(args, function(v) if (inherits(v, "AsIs")) str2lang(as.character(v)) else v)
  as.call(c(as.name(fn), args))
}

.code <- function(txt) I(txt)

# Evaluate a call built by the menu: the function comes from statcram, the
# data from the global environment; the result is printed.
.run_call <- function(cl, assign_to = NULL) {
  attr(cl, "assign_to") <- NULL
  ns <- environment(.run_call)
  env <- new.env(parent = .GlobalEnv)
  fn <- as.character(cl[[1]])
  assign(fn, get(fn, envir = ns), envir = env)
  res <- eval(cl, env)
  if (!is.null(assign_to)) {
    assign(assign_to, res, envir = .GlobalEnv)
    if (inherits(res, "sc_result")) res$call <- paste(assign_to, "<-", res$call)
  }
  if (inherits(res, "sc_result")) print(res)
  invisible(res)
}
