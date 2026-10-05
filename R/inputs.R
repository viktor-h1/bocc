# ------------------------------------------------------------
# Turning whatever the user passes (a column, a typed vector, an
# expression, a table typed from paper) into clean values.
# Nothing here is cached: every call looks at the data as it is now.
# ------------------------------------------------------------

.one_column <- function(x, what) {
  if (is.data.frame(x)) {
    if (ncol(x) != 1)
      stop(what, " is a data frame with ", ncol(x), " columns; pick one column, e.g. df$col.", call. = FALSE)
    x <- x[[1]]
  }
  if (is.matrix(x) && min(dim(x)) == 1) x <- as.vector(x)
  x
}

# Numeric values with missing values removed. attr "dropped" = number of NAs removed.
.num <- function(x, what = "x") {
  if (is.null(x)) stop("No data given for ", what, ".", call. = FALSE)
  x <- .one_column(x, what)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    y <- suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
    if (any(is.na(y) & !is.na(x) & nzchar(x)))
      stop(what, " contains text values (", paste(head(unique(x[is.na(y) & !is.na(x)]), 3), collapse = ", "),
           "); a numeric variable is needed here.", call. = FALSE)
    x <- y
  }
  if (is.logical(x)) x <- as.numeric(x)
  if (!is.numeric(x)) stop(what, " must contain numbers (got ", class(x)[1], ").", call. = FALSE)
  x <- as.numeric(x)
  bad <- is.na(x)
  out <- x[!bad]
  attr(out, "dropped") <- sum(bad)
  if (!length(out)) stop(what, " has no non-missing values.", call. = FALSE)
  out
}

.dropped_note <- function(...) {
  vals <- list(...)
  k <- sum(vapply(vals, function(v) attr(v, "dropped") %||% 0L, numeric(1)))
  if (k > 0) sprintf("%d missing value(s) were removed before computing.", as.integer(k)) else NULL
}

# Categories of a vector, in a sensible order.
# Numbers in numeric order; interval labels ("[0,50)", "10-20") by their lower
# limit; other text alphabetically; factors keep their level order.
.cats <- function(x) {
  if (is.factor(x)) return(levels(droplevels(x)))
  u <- unique(x[!is.na(x)])
  if (is.numeric(u)) return(as.character(sort(u)))
  u <- as.character(u)
  iv <- .parse_intervals(u)
  if (!is.null(iv)) return(iv$label)
  sort(u)
}

# Parse class labels such as "[0,50)", "[10, 20]", "(5;10]" or "10-20".
# Returns data.frame(label, lower, upper) sorted by lower limit, or NULL if
# any label is not an interval.
.parse_intervals <- function(labels) {
  labels <- unique(as.character(labels[!is.na(labels)]))
  if (!length(labels)) return(NULL)
  num <- "\\s*(-?[0-9]+(?:[.][0-9]+)?)\\s*"
  br <- regmatches(labels, regexec(paste0("^\\s*[\\[(]", num, "[,;]", num, "[\\])]\\s*$"), labels, perl = TRUE))
  dash <- regmatches(labels, regexec(paste0("^", num, "-", num, "$"), labels, perl = TRUE))
  lo <- hi <- rep(NA_real_, length(labels))
  for (i in seq_along(labels)) {
    m <- if (length(br[[i]]) == 3) br[[i]] else if (length(dash[[i]]) == 3) dash[[i]] else NULL
    if (is.null(m)) return(NULL)
    lo[i] <- as.numeric(m[2]); hi[i] <- as.numeric(m[3])
  }
  if (any(hi <= lo)) return(NULL)
  o <- order(lo, hi)
  data.frame(label = labels[o], lower = lo[o], upper = hi[o], stringsAsFactors = FALSE)
}

# Event indicator for proportions. Accepts:
#   logical vector (TRUE = event), 0/1 vector (1 = event),
#   factor/character + event = "level" (or several levels).
# Returns a logical vector without NAs, with attr "event" (text) and "dropped".
.event <- function(x, event = NULL, what = "x") {
  if (is.null(x)) stop("No data given for ", what, ".", call. = FALSE)
  x <- .one_column(x, what)
  bad <- is.na(x)
  x <- x[!bad]
  if (!length(x)) stop(what, " has no non-missing values.", call. = FALSE)
  if (is.logical(x) && is.null(event)) {
    y <- x; lab <- "TRUE"
  } else if (is.numeric(x) && is.null(event) && all(x %in% c(0, 1))) {
    y <- x == 1; lab <- "1"
  } else {
    if (is.null(event)) {
      stop("Say which category counts as the event, e.g. event = \"", .cats(x)[1], "\". Categories of ", what, ": ",
           paste(head(.cats(x), 12), collapse = ", "), call. = FALSE)
    }
    ev <- as.character(event)
    vals <- as.character(x)
    miss <- setdiff(ev, unique(vals))
    if (length(miss))
      stop("Event '", paste(miss, collapse = ", "), "' is not a category of ", what, ". Categories: ",
           paste(head(.cats(x), 12), collapse = ", "), call. = FALSE)
    y <- vals %in% ev
    lab <- paste(ev, collapse = " or ")
  }
  attr(y, "event") <- lab
  attr(y, "dropped") <- sum(bad)
  y
}

# Split an outcome by a grouping vector into two samples (levels[1], levels[2]).
.split2 <- function(x, group, levels = NULL, what = "group") {
  x <- .one_column(x, "x"); group <- .one_column(group, what)
  if (length(x) != length(group)) stop("x and group must have the same length.", call. = FALSE)
  g <- as.character(group)
  lv <- if (is.null(levels)) .cats(group) else as.character(levels)
  if (is.null(levels) && length(lv) != 2)
    stop(what, " has ", length(lv), " categories (", paste(head(lv, 12), collapse = ", "),
         "); choose two with levels = c(\"A\", \"B\").", call. = FALSE)
  if (length(lv) != 2) stop("levels must name exactly two groups.", call. = FALSE)
  miss <- setdiff(lv, unique(g))
  if (length(miss)) stop("Group '", paste(miss, collapse = ", "), "' not found in ", what, ".", call. = FALSE)
  list(x1 = x[!is.na(g) & g == lv[1]], x2 = x[!is.na(g) & g == lv[2]], levels = lv)
}

# ---------- tables typed from paper / count tables ----------

.kind_attr <- function(x) attr(x, "statcram_kind") %||% NA_character_

.is_count_matrix <- function(x) {
  (is.matrix(x) || inherits(x, "table")) && is.numeric(x) && length(dim(x)) == 2
}

# Interpret x as a two-way table of counts (or return NULL if it is not one).
.as_count_table <- function(x) {
  if (inherits(x, "table") && length(dim(x)) == 2) return(unclass(x) + 0)
  if (.is_count_matrix(x)) {
    m <- x + 0
    if (is.null(rownames(m))) rownames(m) <- paste0("r", seq_len(nrow(m)))
    if (is.null(colnames(m))) colnames(m) <- paste0("c", seq_len(ncol(m)))
    return(m)
  }
  if (is.data.frame(x) && ncol(x) >= 3) {
    first <- x[[1]]
    rest <- x[-1]
    if ((is.character(first) || is.factor(first)) && all(vapply(rest, is.numeric, logical(1)))) {
      m <- as.matrix(rest)
      rownames(m) <- as.character(first)
      dimnames(m) <- stats::setNames(list(as.character(first), names(rest)),
                                     c(names(x)[1], attr(x, "statcram_colvar") %||% ""))
      return(m)
    }
  }
  NULL
}

# Interpret x as a one-way frequency list: named counts. NULL if it is not one.
.as_freq <- function(x) {
  if (inherits(x, "table") && length(dim(x)) == 1) return(stats::setNames(as.numeric(x), names(x)))
  if (is.numeric(x) && !is.null(names(x)) && is.null(dim(x))) return(x)
  if (is.data.frame(x) && ncol(x) == 2 && (is.character(x[[1]]) || is.factor(x[[1]])) && is.numeric(x[[2]]))
    return(stats::setNames(as.numeric(x[[2]]), as.character(x[[1]])))
  NULL
}

# ---------- what is loaded right now ----------

.kind_tag <- function(x) {
  k <- .kind_attr(x)
  if (is.data.frame(x)) {
    if (!is.na(k) && k == "counts") return(sprintf("count table %dx%d", nrow(x), ncol(x) - 1))
    if (!is.na(k) && k == "classes") return(sprintf("class table, %d classes", nrow(x)))
    if (!is.na(k) && k == "freq") return(sprintf("frequency list, %d categories", nrow(x)))
    return(sprintf("data frame %d rows x %d cols", nrow(x), ncol(x)))
  }
  if (.is_count_matrix(x)) return(sprintf("table %dx%d", nrow(x), ncol(x)))
  if (inherits(x, "table")) return(sprintf("table (%d cells)", length(x)))
  n <- sum(!is.na(x))
  if (is.logical(x)) return(sprintf("TRUE/FALSE, n=%d", n))
  if (is.numeric(x)) {
    u <- unique(x[!is.na(x)])
    if (length(u) <= 2 && all(u %in% c(0, 1))) return(sprintf("0/1, n=%d", n))
    if (length(u) <= 10) return(sprintf("numeric (%d distinct values), n=%d", length(u), n))
    return(sprintf("numeric, n=%d", n))
  }
  if (is.factor(x) || is.character(x)) {
    iv <- .parse_intervals(x)
    if (!is.null(iv) && nrow(iv) > 1)
      return(sprintf("measured in classes: %s%s (n=%d)", paste(head(iv$label, 3), collapse = ", "),
                     if (nrow(iv) > 3) ", ..." else "", n))
    lv <- .cats(x)
    s <- paste(head(lv, 4), collapse = ", ")
    if (length(lv) > 4) s <- paste0(s, ", ...")
    return(sprintf("categorical: %s (n=%d)", s, n))
  }
  class(x)[1]
}

.kind_class <- function(x) {
  if (is.data.frame(x) || is.matrix(x) || inherits(x, "table")) return("table")
  if (is.logical(x)) return("event")
  if (is.numeric(x)) {
    u <- unique(x[!is.na(x)])
    if (length(u) <= 2 && all(u %in% c(0, 1))) return("binary")
    return("numeric")
  }
  if (is.factor(x) || is.character(x)) return("categorical")
  "other"
}

.syntactic <- function(nm) identical(make.names(nm), nm)

.col_expr <- function(df_name, col) {
  if (.syntactic(col)) paste0(df_name, "$", col) else paste0(df_name, "$`", col, "`")
}

# Every usable object in the global environment, re-scanned on each call.
# Returns a data.frame: expr (R code), tag, kind, group (data frame it belongs to).
.live_objects <- function(env = .GlobalEnv) {
  nms <- sort(ls(envir = env))
  rows <- list()
  add <- function(expr, obj, group) {
    rows[[length(rows) + 1]] <<- data.frame(expr = expr, tag = .kind_tag(obj), kind = .kind_class(obj),
                                            group = group, stringsAsFactors = FALSE)
  }
  for (nm in nms) {
    obj <- tryCatch(get(nm, envir = env), error = function(e) NULL)
    if (is.null(obj) || is.function(obj) || is.environment(obj)) next
    if (is.data.frame(obj)) {
      k <- .kind_attr(obj)
      if (!is.na(k) && k != "data") {
        add(nm, obj, nm)
      } else {
        for (col in names(obj)) {
          v <- obj[[col]]
          if (is.atomic(v) && is.null(dim(v))) add(.col_expr(nm, col), v, nm)
        }
      }
    } else if (.is_count_matrix(obj) || inherits(obj, "table")) {
      add(nm, obj, "(tables)")
    } else if (is.atomic(obj) && is.null(dim(obj)) && length(obj) > 1 &&
               (is.numeric(obj) || is.logical(obj) || is.character(obj) || is.factor(obj))) {
      add(nm, obj, "(vectors)")
    }
  }
  if (!length(rows)) return(data.frame(expr = character(), tag = character(), kind = character(),
                                       group = character(), stringsAsFactors = FALSE))
  do.call(rbind, rows)
}

#' Show everything that is loaded right now
#'
#' Lists every data frame (with each column's type), every standalone vector,
#' and every table in your workspace. It re-scans every time, so variables
#' you have just created show up immediately. Nothing is hidden because of
#' its type: a 0/1 column or an integer column with few values is listed
#' with a tag and can be used either way.
#'
#' @return Invisibly, a data frame with one row per usable object
#'   (`expr` = the R code to use it, `tag` = its type).
#' @examples
#' df <- data.frame(spend = c(10, 12, 9), vip = c(1, 0, 1))
#' sc_data()
#' @export
sc_data <- function() {
  lo <- .live_objects()
  cat("\nLOADED NOW (", format(Sys.time(), "%H:%M:%S"), ")\n", sep = "")
  if (!nrow(lo)) {
    cat("Nothing usable is loaded. Try load(\"EXAM.RData\"), or type data in with sc_table().\n")
    return(invisible(lo))
  }
  for (g in unique(lo$group)) {
    cat("\n", g, "\n", sep = "")
    sub <- lo[lo$group == g, , drop = FALSE]
    w <- max(nchar(sub$expr))
    for (i in seq_len(nrow(sub))) cat("  ", formatC(sub$expr[i], width = -w), "  ", sub$tag[i], "\n", sep = "")
  }
  cat("\nUse any of these in a function, e.g. test_mean(", lo$expr[1], ", mu0 = ...), or pick them in sc().\n", sep = "")
  invisible(lo)
}
