# ------------------------------------------------------------
# Phrasing game: the student writes the answer for a situation from the
# phrasing bank and gets a score against the bank's canon sentence, with
# missing crucial verbs and missing negations flagged. Word matching only
# (no AI, offline): a correct paraphrase can score lower than the canon
# wording, and a list of keywords can score high. Study use only.
# ------------------------------------------------------------

# Light stemmer for comparing answers: estimate / estimates / estimated /
# estimating -> "estimat"; vary / varies -> "vari"; class / classes -> "class".
.gm_stem <- function(w) {
  w <- ifelse(nchar(w) > 6, sub("ly$", "", w), w)
  w <- sub("(ies|ied)$", "y", w)
  w <- ifelse(nchar(w) > 5, sub("(ing|ed)$", "", w), w)
  w <- ifelse(nchar(w) > 3, sub("([^s])s$", "\\1", w), w)
  w <- sub("e$", "", w)
  w <- sub("y$", "i", w)
  gsub("iz", "is", w, fixed = TRUE)
}

# British / American spellings and common paraphrases, mapped to the bank's
# word before stemming (so "built" counts for "constructed").
.gm_spell <- c(center = "centre", centers = "centres", centered = "centred", behavior = "behaviour",
               modeled = "modelled", modeling = "modelling", favor = "favour", color = "colour",
               built = "constructed", build = "construct", builds = "constructs", computed = "calculated",
               compute = "calculate", computes = "calculates", approx = "approximately", roughly = "approximately",
               about = "approximately", around = "approximately", include = "contain", includes = "contains",
               included = "contained", capture = "contain", captures = "contains", captured = "contained",
               change = "vary", changes = "varies", changed = "varied", constant = "fixed", average = "mean",
               averages = "means", percentage = "percent", percentages = "percent", repeatedly = "repeated")

.gm_stop <- c("the", "a", "an", "and", "or", "of", "in", "on", "at", "to", "for", "from", "by", "with", "as", "is",
              "are", "was", "were", "be", "been", "being", "it", "its", "this", "that", "these", "those", "there",
              "their", "which", "who", "whom", "whose", "what", "when", "where", "while", "than", "then", "thus",
              "so", "such", "we", "our", "us", "i", "my", "you", "your", "has", "have", "had", "do", "does", "did",
              "can", "could", "may", "might", "will", "would", "should", "must", "shall", "also", "only", "each",
              "any", "all", "both", "either", "one", "into", "onto", "over", "under", "about", "between", "per",
              "very", "more", "most", "same", "other", "if", "because", "since", "therefore", "hence", "here",
              "gives", "give", "given", "using", "use", "used", "uses", "takes", "take", "make", "makes",
              "not", "no", "cannot", "never", "neither", "nor", "without", "none", "don", "doesn", "isn", "aren",
              "wasn", "weren", "won", "didn", "but", "across", "however", "whereas", "way", "times")

# Verbs whose absence usually changes or weakens the claim. Stems are
# compared, so any tense or person counts ("estimates", "estimated").
.gm_verbs <- c("estimate", "measure", "exceed", "contain", "cover", "conclude", "indicate", "suggest", "imply",
               "cause", "depend", "differ", "increase", "decrease", "reduce", "halve", "double", "quadruple",
               "overlap", "standardise", "approximate", "assume", "compare", "represent", "describe", "interpret",
               "justify", "recover", "equal", "lie", "fall", "vary", "capture", "include", "exclude", "divide",
               "multiply", "subtract", "shift", "concentrate", "converge", "tend", "follow", "distribute",
               "select", "replace", "generalise", "infer", "explain", "quantify", "identify", "classify",
               "satisfy", "fail", "violate", "require", "guarantee", "ensure", "observe", "realise", "reach",
               "occur", "provide", "leave", "summarise", "fit", "predict", "determine", "calculate", "compute",
               "obtain", "derive", "solve", "evaluate", "combine", "add", "widen", "narrow", "shrink", "consist",
               "belong", "refer", "correspond", "establish", "prove", "confirm", "verify", "check", "exist",
               "remain", "become", "affect", "influence", "dominate", "outweigh", "reverse", "account", "allow")

.gm_neg <- "(n't|n\u2019t|\\bnot\\b|\\bno\\b|\\bcannot\\b|\\bnever\\b|\\bneither\\b|\\bnor\\b|\\bwithout\\b|\\bnone\\b)"

# Words of a text (placeholders in [brackets] removed), normalised and stemmed.
.gm_words <- function(txt) {
  txt <- gsub("\\[[^]]*\\]", " ", txt)
  txt <- gsub("%", " percent ", txt, fixed = TRUE)
  w <- strsplit(.ab_norm(txt), "[^a-z0-9]+")[[1]]
  w <- w[nzchar(w)]
  w <- ifelse(w %in% names(.gm_spell), .gm_spell[w], w)
  keep <- !(w %in% .gm_stop) & (nchar(w) >= 3 | w %in% c("se", "sd", "cv", "ci", "me", "df", "mu", "x2"))
  data.frame(word = w[keep], stem = .gm_stem(w[keep]), stringsAsFactors = FALSE)
}

.gm_negations <- function(txt) {
  m <- gregexpr(.gm_neg, tolower(gsub("\\[[^]]*\\]", " ", txt)), perl = TRUE)[[1]]
  sum(m > 0)
}

# The canon text of a round: one labelled variant ("NOMINAL: ...") or the
# whole template.
.gm_items <- function(rows) {
  cs <- .pb()$cases
  do.call(rbind, lapply(rows, function(i) {
    w <- strsplit(cs$write[i], "\n", fixed = TRUE)[[1]]
    lab <- regmatches(w, regexec("^([A-Z][A-Z0-9 /-]*[A-Z0-9]): (.*)$", w))
    if (length(w) > 1 && all(lengths(lab) == 3))
      data.frame(row = i, id = cs$id[i], variant = vapply(lab, `[`, "", 2), canon = vapply(lab, `[`, "", 3),
                 stringsAsFactors = FALSE)
    else data.frame(row = i, id = cs$id[i], variant = "", canon = paste(w, collapse = " "), stringsAsFactors = FALSE)
  }))
}

.gm_grade <- function(s) if (s >= 85) "Exam-ready" else if (s >= 70) "Good - tighten the wording" else
  if (s >= 50) "Partial - key ideas missing" else "Revise this case"

#' Score your own wording against the phrasing bank (study use only)
#'
#' `sc_phrasing_game()` is a quiz on your phrasing bank: it shows the
#' situation of a case (its "use when" line), you type the answer you would
#' write, and it scores it against the bank's sentence ("canon"), lists the
#' key terms you missed, and **flags every crucial verb you left out** (such
#' as *estimates*, *contains*, *exceeds*, *does not imply*) and a missing
#' negation. Then it shows the canon sentence, what to avoid and your
#' error-log entry for the case. `sc_phrasing_score()` scores one answer
#' without the game, e.g. a longer answer written in the editor.
#'
#' **How the score works.** It is word matching, done offline: 55% key-term
#' coverage (80% of the key terms earns the full 55), 35% crucial verbs, 10%
#' negation (when the canon has no crucial verb: 85% terms, 15% negation).
#' Words inside the canon's `[placeholders]` are not required; tense, plural,
#' British/American spelling and a few common paraphrases ("built" for
#' "constructed", "about" for "approximately") do not matter. It does not understand meaning: a correct paraphrase with other
#' words scores lower, and a list of keywords scores high. Use it to check
#' that the crucial wording is there, then read the canon sentence.
#'
#' **Study use only.** The 2026/27 rules forbid prepared R scripts and AI
#' tools at the exam; use this to learn the wording beforehand.
#'
#' @param n Number of rounds.
#' @param section,query Restrict the rounds to a section (`"6I"`, `5`, ...)
#'   or to the cases matching a search (see [sc_phrasing()]).
#' @param id Play these case ids, in order.
#' @param review If `TRUE`, replay the rounds you scored below 70 in this
#'   R session.
#' @param shuffle Random order (default) or bank order.
#' @return `sc_phrasing_game()`: invisibly, a data frame of the rounds
#'   (id, variant, score, hint, crucial verbs missed).
#' @examples
#' sc_phrasing_score("6I.13", "t is wider because sigma is estimated")
#' \dontrun{
#' sc_phrasing_game()                    # 10 random cases
#' sc_phrasing_game(n = 5, section = "6I")
#' sc_phrasing_game(review = TRUE)       # replay what you scored < 70
#' }
#' @export
sc_phrasing_game <- function(n = 10, section = NULL, query = NULL, id = NULL, review = FALSE, shuffle = TRUE) {
  if (!is.numeric(n) || length(n) != 1 || n < 1) stop("n must be a positive number of rounds.", call. = FALSE)
  if (review) {
    h <- .gm_history()
    weak <- h[h$score < 70, , drop = FALSE]
    weak <- weak[!duplicated(paste(weak$id, weak$variant), fromLast = TRUE), , drop = FALSE]
    if (!nrow(weak)) { cat("Nothing to review: no round scored below 70 in this session.\n"); return(invisible(NULL)) }
    items <- .gm_items(unique(match(weak$id, .pb()$cases$id)))
    items <- items[paste(items$id, items$variant) %in% paste(weak$id, weak$variant), , drop = FALSE]
  } else {
    rows <- if (!is.null(id)) unlist(lapply(id, .pb_find)) else sc_phrasing(query = query, section = section, list = TRUE)$id
    if (is.null(id)) rows <- match(rows, .pb()$cases$id)
    if (!length(rows)) { cat("No case matches.\n"); return(invisible(NULL)) }
    items <- .gm_items(rows)
  }
  if (shuffle && is.null(id) && nrow(items) > 1) items <- items[sample(nrow(items)), , drop = FALSE]
  items <- items[seq_len(min(n, nrow(items))), , drop = FALSE]
  cs <- .pb()$cases
  .ab_cat(sprintf("PHRASING GAME - %d round%s. Type the answer you would write in the exam, then Enter.",
                  nrow(items), if (nrow(items) == 1) "" else "s"),
          "At the answer prompt: h = hint (-10 points), Enter = skip and show the answer, q = stop.",
          "Scoring is word matching against your bank's sentence: crucial verbs and negations are flagged.",
          "(Study use only.)")
  res <- data.frame(id = character(0), variant = character(0), score = numeric(0), hint = logical(0),
                    missed_verbs = character(0), stringsAsFactors = FALSE)
  total <- 0; streak <- 0; best <- 0
  for (k in seq_len(nrow(items))) {
    it <- items[k, ]; i <- it$row
    .ab_cat("", sprintf("==== ROUND %d/%d   %s  %s ====", k, nrow(items), it$id, cs$title[i]),
            .ab_wrap(.pb_md(cs$use_when[i]), "SITUATION"),
            if (nzchar(it$variant)) .ab_wrap(sprintf("write the %s version", it$variant), "CASE"))
    hint <- FALSE
    repeat {
      a <- trimws(.read_line("Your answer: "))
      if (tolower(a) == "h" && !hint) {
        hint <- TRUE
        sc <- .gm_score(it$canon, "")
        cw <- c(sc$verbs, sc$qualifiers)
        .ab_cat(.ab_wrap(sprintf("%d key terms; crucial words start with: %s%s", length(sc$terms),
                                 if (length(cw)) paste(paste0(substr(cw, 1, 3), "..."), collapse = ", ") else "(none)",
                                 if (sc$neg_canon) "; the answer contains a negation" else ""), "HINT"))
        next
      }
      break
    }
    if (tolower(a) == "q") break
    sc <- .gm_score(it$canon, a)
    pts <- if (!nzchar(a)) 0 else max(0, sc$score - if (hint) 10 else 0)
    clean <- nzchar(a) && sc$score >= 85 && !length(sc$missed_verbs) && !length(sc$missed_qual) && sc$neg_ok
    if (clean) pts <- pts + 10
    streak <- if (nzchar(a) && sc$score >= 85) streak + 1 else 0; best <- max(best, streak)
    total <- total + pts
    .ab_cat(if (nzchar(a)) .gm_feedback(sc, hint, clean) else "Skipped.", "",
            "CANON ANSWER (your bank)", .ab_wrap(.pb_md(it$canon), "", indent = 2),
            if (nzchar(cs$avoid[i])) .ab_wrap(.pb_md(cs$avoid[i]), "AVOID"),
            .gm_errorlog_lines(it$id),
            sprintf("Points this round: %d   Total: %d   Streak: %d", round(pts), round(total), streak))
    .gm_record(it$id, it$variant, if (nzchar(a)) sc$score else 0)
    res[nrow(res) + 1, ] <- list(it$id, it$variant, if (nzchar(a)) sc$score else 0, hint,
                                 paste(c(sc$missed_verbs, sc$missed_qual), collapse = " "))
    if (k < nrow(items) && tolower(trimws(.read_line("Enter = next round, q = stop: "))) == "q") break
  }
  if (nrow(res)) {
    avg <- mean(res$score)
    weak <- res$id[res$score < 70]
    .ab_cat("", "==== RESULT ====",
            sprintf("Rounds: %d   Points: %d   Average score: %.0f/100   Best streak (>= 85): %d", nrow(res), round(total), avg, best),
            sprintf("Rank: %s", .gm_grade(avg)),
            if (any(nzchar(res$missed_verbs)))
              .ab_wrap(paste(unique(unlist(strsplit(res$missed_verbs[nzchar(res$missed_verbs)], " "))), collapse = ", "), "VERBS MISSED"),
            if (length(weak)) .ab_wrap(paste(unique(weak), collapse = ", "), "REVIEW") ,
            if (length(weak)) "Replay them: sc_phrasing_game(review = TRUE)")
  }
  invisible(res)
}

# Compare an answer with the canon text.
.gm_score <- function(canon, answer) {
  cw <- .gm_words(canon); aw <- .gm_words(answer)
  vstem <- .gm_stem(.gm_verbs)
  is_v <- cw$stem %in% vstem
  verbs <- cw$word[is_v][!duplicated(cw$stem[is_v])]
  vstems <- cw$stem[is_v][!duplicated(cw$stem[is_v])]
  tk <- !is_v & !duplicated(cw$stem)
  terms <- cw$word[tk]; tstems <- cw$stem[tk]
  have_t <- tstems %in% aw$stem; have_v <- vstems %in% aw$stem
  neg_canon <- .gm_negations(canon) > 0
  neg_ok <- !neg_canon || .gm_negations(answer) > 0
  # 80% of the key terms earns the full term credit (a fair paraphrase drops a few words)
  tr <- if (length(tstems)) min(1, mean(have_t) / 0.8) else 1
  score <- if (length(vstems)) 100 * (0.55 * tr + 0.35 * mean(have_v) + 0.10 * neg_ok) else 100 * (0.85 * tr + 0.15 * neg_ok)
  # adverbs such as "approximately" share a stem with a crucial verb: report them as qualifiers
  qual <- grepl("ly$", verbs)
  list(score = round(score), terms = terms, missed_terms = terms[!have_t], verbs = verbs[!qual],
       missed_verbs = verbs[!have_v & !qual], qualifiers = verbs[qual], missed_qual = verbs[!have_v & qual],
       neg_canon = neg_canon, neg_ok = neg_ok, term_recall = tr)
}

.gm_feedback <- function(sc, hint = FALSE, clean = FALSE) {
  c(sprintf("SCORE %d/100   %s%s%s", sc$score, .gm_grade(sc$score), if (hint) "   (hint used: -10 points)" else "",
            if (clean) "   +10 clean answer" else ""),
    .ab_wrap(sprintf("%d/%d%s", length(sc$terms) - length(sc$missed_terms), length(sc$terms),
                     if (length(sc$missed_terms)) paste("  missing:", paste(sc$missed_terms, collapse = ", ")) else "  all there"),
             "KEY TERMS"),
    if (length(sc$verbs) && !length(sc$missed_verbs)) .ab_wrap(paste(sc$verbs, collapse = ", "), "CRUCIAL VERBS") ,
    if (length(sc$missed_verbs)) .ab_wrap(sprintf("%s   (the canon answer uses %s: without %s the claim changes or is incomplete)",
                                                paste(sc$missed_verbs, collapse = ", "), if (length(sc$missed_verbs) > 1) "them" else "it",
                                                if (length(sc$missed_verbs) > 1) "them" else "it"), "! MISSING VERB"),
    if (length(sc$missed_qual)) .ab_wrap(sprintf("%s   (the claim holds only %s)", paste(sc$missed_qual, collapse = ", "),
                                               if (any(grepl("^approx", sc$missed_qual))) "approximately: say so" else "in that way"),
                                       "! QUALIFIER"),
    if (!sc$neg_ok) .ab_wrap("the canon answer says what does NOT hold (not / cannot / no ...); your answer has no negation, so it may state the opposite.",
                             "! NEGATION"))
}

.gm_errorlog_lines <- function(id) {
  mk <- .pb()$mistakes
  mk <- mk[vapply(strsplit(mk$ids, " ", fixed = TRUE), function(v) id %in% v, logical(1)), , drop = FALSE]
  if (!nrow(mk)) return(NULL)
  unlist(lapply(seq_len(nrow(mk)), function(r) .ab_wrap(paste(.pb_md(mk$title[r]), .pb_md(mk$text[r])), "YOUR ERROR LOG")))
}

.gm_history <- function() {
  h <- .pb_cache$game
  if (is.null(h)) data.frame(id = character(0), variant = character(0), score = numeric(0), stringsAsFactors = FALSE) else h
}
.gm_record <- function(id, variant, score) {
  h <- .gm_history(); h[nrow(h) + 1, ] <- list(id, variant, score); .pb_cache$game <- h; invisible(h)
}

#' @rdname sc_phrasing_game
#' @param answer Your answer, as one string (several lines are pasted together).
#' @param variant For cases with labelled alternatives (e.g. 1.02 has
#'   `"NOMINAL"` and `"ORDINAL"`), the one you answered. If `NULL`, the
#'   best-matching alternative is used.
#' @return `sc_phrasing_score()`: invisibly, a list with `score`, `missed_terms`,
#'   `missed_verbs`, `neg_ok` and the `canon` text.
#' @export
sc_phrasing_score <- function(id, answer, variant = NULL) {
  i <- .pb_find(id)
  if (!length(i)) stop(sprintf("No case with id \"%s\".", id), call. = FALSE)
  answer <- paste(answer, collapse = " ")
  it <- .gm_items(i)
  if (!is.null(variant)) {
    it <- it[toupper(it$variant) == toupper(variant), , drop = FALSE]
    if (!nrow(it)) stop(sprintf("Case %s has no variant \"%s\". Variants: %s", id, variant,
                                paste(.gm_items(i)$variant, collapse = ", ")), call. = FALSE)
  }
  scs <- lapply(it$canon, .gm_score, answer = answer)
  b <- which.max(vapply(scs, `[[`, 0, "score"))
  sc <- scs[[b]]
  .ab_cat(sprintf("%s  %s%s", .pb()$cases$id[i], .pb()$cases$title[i], if (nzchar(it$variant[b])) sprintf("  [%s]", it$variant[b]) else ""),
          .gm_feedback(sc), "", "CANON ANSWER (your bank)", .ab_wrap(.pb_md(it$canon[b]), "", indent = 2))
  invisible(c(sc, list(canon = it$canon[b], variant = it$variant[b])))
}
