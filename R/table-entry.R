# ------------------------------------------------------------
# sc_table(): rebuild a table printed on paper as a data frame.
# kind -> dimensions -> labels -> numbered placeholder grid ->
# fill each placeholder -> review / fix -> save (+ R code to recreate).
# ------------------------------------------------------------

.abort <- function() stop(structure(class = c("sc_abort", "error", "condition"),
                                    list(message = "cancelled", call = NULL)))

# Ask a question; "q" cancels the whole table entry.
.tq <- function(prompt, default = NULL, end = ": ") {
  shown <- if (!is.null(default) && nzchar(default)) paste0(" [", default, "]") else ""
  ans <- trimws(.read_line(paste0(prompt, shown, end)))
  if (tolower(ans) %in% c("q", "quit", "cancel")) .abort()
  if (!nzchar(ans) && !is.null(default)) return(as.character(default))
  ans
}

.tq_int <- function(prompt, default = NULL, min = 1) {
  repeat {
    a <- .tq(prompt, default)
    v <- suppressWarnings(as.integer(a))
    if (!is.na(v) && v >= min && as.character(v) == a) return(v)
    cat("  Please type a whole number", if (min > 0) sprintf(" (at least %d)", min), ".\n", sep = "")
  }
}

.tq_yes <- function(prompt, default = TRUE) {
  a <- tolower(.tq(paste0(prompt, if (default) " [Y/n]" else " [y/N]"), ""))
  if (!nzchar(a)) return(default)
  a %in% c("y", "yes", "s", "si")
}

# Split a typed line into values: spaces or ";" separate; quotes keep text with spaces.
.split_values <- function(line) {
  line <- gsub(";", " ", line, fixed = TRUE)
  out <- tryCatch(scan(text = line, what = "character", quiet = TRUE, quote = "\"'"),
                  error = function(e) strsplit(trimws(line), "\\s+")[[1]])
  out[nzchar(out)]
}

# Clean one typed value: "3,5" -> "3.5", "12%" -> "12", "-"/"NA" -> NA.
.clean_value <- function(tok) {
  tok <- trimws(tok)
  if (tok %in% c("NA", "na", "-", ".", "?", "")) return(NA_character_)
  tok <- sub("%$", "", tok)
  if (grepl("^[-+]?[0-9]*,[0-9]+$", tok)) tok <- sub(",", ".", tok, fixed = TRUE)
  tok
}

.is_numberish <- function(v) {
  v <- v[!is.na(v)]
  !length(v) || all(!is.na(suppressWarnings(as.numeric(v))))
}

.fmt_bound <- function(x) as.character(signif(x, 10))

# Labels: Enter = defaults; k labels typed; or "c" -> class boundaries.
.tq_labels <- function(what, k, prefix, allow_classes = TRUE) {
  def <- paste0(prefix, seq_len(k))
  repeat {
    a <- .tq(sprintf("%s labels: Enter = %s%s, or type %d labels%s", what, paste(head(def, 3), collapse = " "),
                     if (k > 3) " ..." else "", k, if (allow_classes) ", or c = class boundaries" else ""), "")
    if (!nzchar(a)) return(list(labels = def))
    if (allow_classes && tolower(a) == "c") {
      b <- .tq_bounds(k)
      return(list(labels = paste0(.fmt_bound(head(b, -1)), "-", .fmt_bound(tail(b, -1))),
                  lower = head(b, -1), upper = tail(b, -1)))
    }
    lab <- .split_values(a)
    if (length(lab) == k) return(list(labels = lab))
    cat(sprintf("  I counted %d labels but need %d. (Use quotes for labels with spaces: \"New York\")\n", length(lab), k))
  }
}

.tq_bounds <- function(k) {
  repeat {
    a <- .tq(sprintf("Type the %d class boundaries in order, e.g. 10 25 30 ... (open last class: type an assumed upper limit)", k + 1))
    b <- suppressWarnings(as.numeric(vapply(.split_values(a), .clean_value, "")))
    if (length(b) == k + 1 && !anyNA(b) && all(diff(b) > 0)) return(b)
    cat(sprintf("  Need %d increasing numbers (got %d).\n", k + 1, length(b)))
  }
}

# ---------- the table state ----------

.tbl_new <- function(kind, rows, cols, order = "row") {
  nr <- length(rows); nc <- length(cols)
  idx <- if (order == "row") matrix(seq_len(nr * nc), nr, nc, byrow = TRUE) else matrix(seq_len(nr * nc), nr, nc)
  list(kind = kind, rows = rows, cols = cols, vals = matrix(NA_character_, nr, nc), idx = idx,
       filled = matrix(FALSE, nr, nc))
}

.cell_pos <- function(T, k) which(T$idx == k, arr.ind = TRUE)[1, ]

.cell_name <- function(T, k) {
  pos <- .cell_pos(T, k); i <- pos[1]; j <- pos[2]
  switch(T$kind,
    data = sprintf("row %d, %s", i, T$cols[j]),
    counts = sprintf("%s / %s", T$rows[i], T$cols[j]),
    classes = if (length(T$cols) > 1) sprintf("class %s, %s", T$rows[i], T$cols[j]) else sprintf("class %s", T$rows[i]),
    freq = T$rows[i])
}

.tbl_show <- function(T, numbers = TRUE) {
  disp <- matrix("", nrow(T$vals), ncol(T$vals))
  for (i in seq_len(nrow(disp))) for (j in seq_len(ncol(disp))) {
    k <- T$idx[i, j]
    disp[i, j] <- if (!T$filled[i, j]) sprintf("[%d]", k)
                  else if (numbers) sprintf("[%d] %s", k, if (is.na(T$vals[i, j])) "NA" else T$vals[i, j])
                  else if (is.na(T$vals[i, j])) "NA" else T$vals[i, j]
  }
  rl <- if (T$kind == "data") as.character(seq_len(nrow(disp))) else T$rows
  head_row <- if (T$kind == "data") "" else T$rowvar %||% ""
  full <- rbind(c(head_row, T$cols), cbind(rl, disp))
  w <- apply(full, 2, function(col) max(nchar(col)))
  cat("\n")
  for (i in seq_len(nrow(full))) {
    cat("  ", formatC(full[i, 1], width = -w[1]), sep = "")
    for (j in 2:ncol(full)) cat("   ", formatC(full[i, j], width = w[j]), sep = "")
    cat("\n")
  }
  if (!is.null(T$colvar) && nzchar(T$colvar) && T$kind == "counts") cat("  (columns: ", T$colvar, ")\n", sep = "")
  cat("\n")
}

.tbl_fill <- function(T, from = 1) {
  N <- length(T$vals)
  cat("Fill the placeholders. Type one value, or several separated by spaces to fill the next ones.\n",
      "b = back one cell,  NA = missing,  q = cancel.\n", sep = "")
  k <- from
  while (k <= N) {
    a <- .tq(sprintf("[%d/%d] %s", k, N, .cell_name(T, k)), end = " = ")
    if (tolower(a) == "b") { k <- max(1, k - 1); next }
    if (!nzchar(a)) { cat("  Type a value (or NA if it is missing).\n"); next }
    toks <- .split_values(a)
    for (tok in toks) {
      if (k > N) { cat("  (extra values ignored: the table is full)\n"); break }
      pos <- .cell_pos(T, k)
      T$vals[pos[1], pos[2]] <- .clean_value(tok); T$filled[pos[1], pos[2]] <- TRUE
      k <- k + 1
    }
    if (length(toks) > 1 && k <= N) .tbl_show(T)
  }
  T
}

.tbl_problems <- function(T) {
  if (T$kind == "data") return(character())
  bad <- which(!is.na(T$vals) & is.na(suppressWarnings(as.numeric(T$vals))))
  probs <- vapply(bad, function(b) {
    i <- row(T$vals)[b]; j <- col(T$vals)[b]
    sprintf("cell [%d] '%s' is not a number", T$idx[i, j], T$vals[i, j])
  }, character(1))
  if (T$kind %in% c("counts", "freq") && anyNA(T$vals)) probs <- c(probs, "counts cannot be missing (NA)")
  probs
}

.tbl_review <- function(T) {
  repeat {
    .tbl_show(T)
    pr <- .tbl_problems(T)
    if (length(pr)) cat("  Fix: ", paste(pr, collapse = "; "), "\n", sep = "")
    a <- .tq("Enter = done,  4 or 4=27 = fix a cell,  n = rename labels,  q = cancel", "")
    if (!nzchar(a)) { if (!length(pr)) return(T) else next }
    if (tolower(a) == "n") { T <- .tbl_relabel(T); next }
    m <- regmatches(a, regexec("^([0-9]+)\\s*(=\\s*(.*))?$", a))[[1]]
    if (!length(m)) { cat("  Type a cell number (e.g. 4), 4=27, n, or press Enter.\n"); next }
    k <- as.integer(m[2])
    if (k < 1 || k > length(T$vals)) { cat("  There is no cell [", k, "].\n", sep = ""); next }
    v <- if (nzchar(m[3])) m[4] else .tq(sprintf("[%d] %s", k, .cell_name(T, k)), end = " = ")
    pos <- .cell_pos(T, k)
    T$vals[pos[1], pos[2]] <- .clean_value(v); T$filled[pos[1], pos[2]] <- TRUE
  }
}

.tbl_relabel <- function(T) {
  if (T$kind == "data") {
    T$cols <- .tq_labels("Column (variable)", length(T$cols), "x", FALSE)$labels
  } else if (T$kind == "classes") {
    if (length(T$cols) > 1) T$cols <- .tq_labels("Value column", length(T$cols), "v", FALSE)$labels
    b <- .tq_bounds(length(T$rows)); T$lower <- head(b, -1); T$upper <- tail(b, -1)
    T$rows <- paste0(.fmt_bound(T$lower), "-", .fmt_bound(T$upper))
  } else {
    T$rowvar <- .tq("Row variable name", T$rowvar)
    T$rows <- .tq_labels("Row", length(T$rows), "r")$labels
    if (T$kind == "counts") {
      T$colvar <- .tq("Column variable name", T$colvar)
      T$cols <- .tq_labels("Column", length(T$cols), "c")$labels
    }
  }
  T
}

# ---------- building the data frame ----------

.tbl_to_df <- function(T) {
  num <- function(v) as.numeric(v)
  if (T$kind == "data") {
    df <- as.data.frame(lapply(seq_along(T$cols), function(j) {
      v <- T$vals[, j]
      if (.is_numberish(v)) num(v) else v
    }), stringsAsFactors = FALSE)
    names(df) <- T$cols
  } else if (T$kind == "counts") {
    df <- data.frame(T$rows, stringsAsFactors = FALSE)
    names(df) <- T$rowvar
    for (j in seq_along(T$cols)) df[[T$cols[j]]] <- num(T$vals[, j])
    attr(df, "statcram_colvar") <- T$colvar
  } else if (T$kind == "classes") {
    df <- data.frame(class = T$rows, lower = T$lower, upper = T$upper, stringsAsFactors = FALSE)
    for (j in seq_along(T$cols)) df[[T$cols[j]]] <- num(T$vals[, j])
    attr(df, "statcram_values") <- T$value_type
  } else {
    df <- data.frame(T$rows, num(T$vals[, 1]), stringsAsFactors = FALSE)
    names(df) <- c(T$rowvar, T$cols[1])
    attr(df, "statcram_values") <- T$value_type
  }
  attr(df, "statcram_kind") <- T$kind
  df
}

.tbl_from_df <- function(df) {
  kind <- .kind_attr(df)
  if (is.na(kind)) kind <- if (all(c("lower", "upper") %in% names(df))) "classes" else "data"
  if (kind == "data") {
    T <- .tbl_new("data", as.character(seq_len(nrow(df))), names(df))
    T$vals <- matrix(vapply(df, function(v) ifelse(is.na(v), NA_character_, as.character(v)), character(nrow(df))), nrow(df))
  } else if (kind == "classes") {
    vc <- setdiff(names(df), c("class", "lower", "upper"))
    T <- .tbl_new("classes", paste0(.fmt_bound(df$lower), "-", .fmt_bound(df$upper)), vc)
    T$lower <- df$lower; T$upper <- df$upper; T$value_type <- attr(df, "statcram_values") %||% "freq"
    T$vals <- matrix(as.character(unlist(df[vc])), nrow(df))
  } else {
    T <- .tbl_new(kind, as.character(df[[1]]), names(df)[-1])
    T$rowvar <- names(df)[1]; T$colvar <- attr(df, "statcram_colvar") %||% "col"
    T$value_type <- attr(df, "statcram_values") %||% "count"
    T$vals <- matrix(as.character(unlist(df[-1])), nrow(df))
  }
  T$filled[] <- TRUE
  T
}

# One row per unit, for count tables / frequency lists with whole-number counts.
.tbl_long <- function(T, df) {
  cnt <- suppressWarnings(as.numeric(T$vals))
  if (anyNA(cnt) || any(cnt < 0) || any(cnt != round(cnt))) return(NULL)
  if (T$kind == "counts") {
    grid <- expand.grid(i = seq_along(T$rows), j = seq_along(T$cols))
    n <- mapply(function(i, j) as.numeric(T$vals[i, j]), grid$i, grid$j)
    out <- data.frame(factor(rep(T$rows[grid$i], n), levels = T$rows),
                      factor(rep(T$cols[grid$j], n), levels = T$cols))
    names(out) <- make.unique(c(T$rowvar, T$colvar))
  } else if (T$kind == "freq") {
    out <- data.frame(factor(rep(T$rows, as.numeric(T$vals[, 1])), levels = T$rows))
    names(out) <- T$rowvar
  } else return(NULL)
  out
}

.vec_code <- function(v) {
  if (is.numeric(v)) paste0("c(", paste(ifelse(is.na(v), "NA", as.character(v)), collapse = ", "), ")")
  else paste0("c(", paste(ifelse(is.na(v), "NA", paste0("\"", gsub("\"", "\\\\\"", v), "\"")), collapse = ", "), ")")
}

.df_code <- function(name, df) {
  nm <- names(df)
  safe <- vapply(nm, function(z) if (.syntactic(z)) z else paste0("`", z, "`"), "")
  cols <- vapply(seq_along(nm), function(j) sprintf("  %s = %s", safe[j], .vec_code(df[[j]])), "")
  extra <- if (!all(vapply(nm, .syntactic, logical(1)))) ",\n  check.names = FALSE" else ""
  paste0(name, " <- data.frame(\n", paste(cols, collapse = ",\n"), extra, "\n)")
}

.next_name <- function() {
  i <- 1
  while (exists(paste0("tab", i), envir = .GlobalEnv, inherits = FALSE)) i <- i + 1
  paste0("tab", i)
}

.tbl_save <- function(T, default_name) {
  repeat {
    name <- .tq("Save as (object name)", default_name)
    if (!.syntactic(name)) { cat("  Use a simple R name: letters, digits, . or _ (no spaces), e.g. tab1.\n"); next }
    if (exists(name, envir = .GlobalEnv, inherits = FALSE) && name != default_name &&
        !.tq_yes(sprintf("'%s' already exists. Overwrite?", name), FALSE)) next
    break
  }
  df <- .tbl_to_df(T)
  assign(name, df, envir = .GlobalEnv)
  long <- NULL
  if (T$kind %in% c("counts", "freq") && identical(T$value_type, "count")) {
    lg <- .tbl_long(T, df)
    if (!is.null(lg) && .tq_yes(sprintf("Also create %s_long with one row per unit (%d rows), so every raw-data function can use it?",
                                        name, nrow(lg)), TRUE)) {
      long <- paste0(name, "_long"); assign(long, lg, envir = .GlobalEnv)
    }
  }
  cat("\nSaved '", name, "' (", .kind_tag(df), ")", if (!is.null(long)) paste0(" and '", long, "'"), " in your workspace.\n", sep = "")
  cat("\nR code that recreates it (paste into your script if you want a record):\n")
  cat(.df_code(name, df), "\n", sep = "")
  cat("\nNext steps:\n")
  nx <- switch(T$kind,
    data = c(sprintf("use %s$<column> in any function, e.g. desc_summary(%s$%s)", name, name, if (.syntactic(T$cols[1])) T$cols[1] else paste0("`", T$cols[1], "`")),
             "sc() lists every column; sc_data() shows what is loaded"),
    counts = c(sprintf("chisq_indep(%s)        chi-square test of independence   [sc(6,2)]", name),
               sprintf("desc_crosstab(%s)      row / column percentages            [sc(1,6)]", name),
               if (!is.null(long)) sprintf("test_2props(%s$%s == \"%s\", group = %s$%s, levels = c(\"%s\", \"%s\"))",
                                           long, names(get(long, envir = .GlobalEnv))[2], T$cols[1], long,
                                           names(get(long, envir = .GlobalEnv))[1], T$rows[1], T$rows[min(2, length(T$rows))])),
    classes = c(sprintf("desc_classes(%s)       approx. mean / quantiles / modal class   [sc(1,4)]", name),
                sprintf("desc_classes(%s, below = <value>)   share below a value", name)),
    freq = c(sprintf("chisq_gof(%s, p = c(...))   goodness of fit (equal shares if p omitted)   [sc(6,1)]", name),
             sprintf("desc_freq(%s)              proportions and their SEs                     [sc(1,5)]", name),
             sprintf("desc_prop(%s, event = \"%s\")  one category's proportion + SE", name, T$rows[1])))
  cat(paste0("  ", nx, collapse = "\n"), "\n", sep = "")
  invisible(df)
}

#' Enter a table from paper
#'
#' A guided way to rebuild a table printed in the exam paper as a data frame.
#' It asks for the kind of table and its dimensions, shows a grid of
#' numbered placeholders `[1] [2] ...`, then asks for the value of each
#' placeholder. You can type several values at once (they fill the next
#' placeholders), go back with `b`, and fix any cell at the end with
#' `4=27`. Category labels can be typed, left as defaults, or generated from
#' class boundaries (type `c`).
#'
#' Kinds of table:
#' 1. **Data table** - each row is one unit (firm, person, month...), each
#'    column a variable. Fill row by row or column by column.
#' 2. **Count table** - counts for row categories x column categories
#'    (cross-tabulation). Saved in paper layout; optionally also as
#'    `<name>_long` with one row per unit.
#' 3. **Class table** - class intervals (e.g. 10-25, 25-30) with frequencies
#'    or percentages, for [desc_classes()].
#' 4. **Frequency list** - one categorical variable: category + count (or %).
#'
#' The table is saved in your workspace as a data frame, the R code that
#' recreates it is printed, and the next steps are suggested (e.g.
#' `chisq_indep(tab1)`). [chisq_indep()], [chisq_gof()], [desc_classes()],
#' [desc_freq()], [desc_prop()] and [desc_crosstab()] accept these tables
#' directly.
#'
#' @param edit An existing table (made by `sc_table()` or any data frame) to
#'   reopen and correct.
#' @return The new data frame, invisibly (it is also assigned in the global
#'   environment under the name you choose).
#' @examples
#' \dontrun{
#' sc_table()              # build a new table step by step
#' sc_table(edit = tab1)   # fix values in an existing one
#' }
#' @export
sc_table <- function(edit = NULL) {
  default_name <- if (!is.null(edit)) deparse(substitute(edit)) else .next_name()
  tryCatch({
    if (!is.null(edit)) {
      if (!is.data.frame(edit)) stop("edit must be a data frame (a table made with sc_table()).", call. = FALSE)
      T <- .tbl_from_df(edit)
      cat("\nEditing '", default_name, "'.\n", sep = "")
    } else {
      cat("\nENTER A TABLE FROM PAPER   (q = cancel at any prompt)\n",
          " 1  Data table      - each row is one unit (firm, person...), columns are variables\n",
          " 2  Count table     - counts for row categories x column categories (cross-tab)\n",
          " 3  Class table     - classes like 10-25, 25-30 with frequencies or %\n",
          " 4  Frequency list  - one categorical variable: categories with counts (or %)\n", sep = "")
      repeat {
        k <- .tq("What kind of table?")
        if (k %in% as.character(1:4)) break
        cat("  Type 1, 2, 3 or 4.\n")
      }
      T <- switch(k, "1" = .tbl_setup_data(), "2" = .tbl_setup_counts(), "3" = .tbl_setup_classes(), "4" = .tbl_setup_freq())
      .tbl_show(T)
      T <- .tbl_fill(T)
    }
    T <- .tbl_review(T)
    .tbl_save(T, default_name)
  }, sc_abort = function(e) {
    cat("Table entry cancelled. Nothing was saved.\n")
    invisible(NULL)
  })
}

.tbl_setup_data <- function() {
  nr <- .tq_int("How many rows (units)?")
  nc <- .tq_int("How many columns (variables)?")
  cols <- .tq_labels("Column (variable)", nc, "x", FALSE)$labels
  cols <- make.unique(cols)
  ord <- if (nc > 1 && nr > 1) {
    a <- .tq("Fill order: 1 = row by row, 2 = column by column (one variable at a time)", "1")
    if (a == "2") "col" else "row"
  } else "row"
  .tbl_new("data", as.character(seq_len(nr)), cols, ord)
}

.tbl_setup_counts <- function() {
  nr <- .tq_int("How many rows (categories of the row variable)?", min = 2)
  nc <- .tq_int("How many columns (categories of the column variable)?", min = 2)
  rowvar <- .tq("Row variable name", "row")
  rl <- .tq_labels("Row", nr, "r")
  colvar <- .tq("Column variable name", "col")
  cl <- .tq_labels("Column", nc, "c")
  T <- .tbl_new("counts", make.unique(rl$labels), make.unique(cl$labels))
  T$rowvar <- if (.syntactic(rowvar)) rowvar else make.names(rowvar)
  T$colvar <- if (.syntactic(colvar)) colvar else make.names(colvar)
  if (T$colvar == T$rowvar) T$colvar <- paste0(T$colvar, "2")
  T$value_type <- if (.tq("Cells are: 1 = counts, 2 = percentages / proportions", "1") == "2") "percent" else "count"
  T
}

.tbl_setup_classes <- function() {
  k <- .tq_int("How many classes?")
  b <- .tq_bounds(k)
  vt <- if (.tq("Values are: 1 = frequencies (counts), 2 = percentages / proportions", "1") == "2") "percent" else "freq"
  nv <- .tq_int("How many value columns (e.g. 2 for men and women)?", 1)
  cols <- if (nv == 1) vt else make.unique(.tq_labels("Value column", nv, "v", FALSE)$labels)
  T <- .tbl_new("classes", paste0(.fmt_bound(head(b, -1)), "-", .fmt_bound(tail(b, -1))), cols)
  T$lower <- head(b, -1); T$upper <- tail(b, -1); T$value_type <- vt
  T
}

.tbl_setup_freq <- function() {
  k <- .tq_int("How many categories?", min = 2)
  var <- .tq("Variable name", "category")
  rl <- .tq_labels("Category", k, "c")
  vt <- if (.tq("Values are: 1 = counts, 2 = percentages / proportions", "1") == "2") "percent" else "count"
  T <- .tbl_new("freq", make.unique(rl$labels), if (vt == "count") "count" else "percent")
  T$rowvar <- if (.syntactic(var)) var else make.names(var)
  T$value_type <- vt
  T
}
