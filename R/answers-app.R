# ------------------------------------------------------------
# sc_answers_app(): a window to browse, search, practise and fill the study
# answer bank (R/answers.R). Left: search, START HERE group, chapter, weak
# spots, categories with counts. Middle: matching cards. Right: tabs with the
# card (use when / answer / formulas / don't use when / source / past papers /
# other cases), a fill form with Copy, Reset and Print to console, and the
# verbatim past-paper questions. Study use only.
# ------------------------------------------------------------

#' Answer template browser (study use only)
#'
#' Opens a window to find and study the cards of [sc_answers()]: start from
#' the wording of the question (START HERE), a chapter, the weak spots or a
#' search; choose a card; read when it applies and when it does not, compare
#' it with the other cases of the same kind, then fill its blanks and copy the
#' text or print it to the console. Practice mode hides the answer until you
#' ask, then lets you mark the card "remembered" or "needs review" (kept for
#' this R session). The "Past-paper questions" tab shows the verbatim
#' questions and official solution excerpts, labelled separately. The cards
#' are authored study templates, not official solutions; the window is for
#' studying only: the course rules forbid prepared scripts and AI tools at the
#' exam. Needs the shiny and miniUI packages; [sc_answers()] works without
#' them.
#'
#' @return Invisibly, the filled text printed when you press "Print to
#'   console" (or `NULL`).
#' @examples
#' if (interactive()) sc_answers_app()
#' @export
sc_answers_app <- function() {
  for (p in c("shiny", "miniUI"))
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("sc_answers_app() needs the %s package: install.packages(c(\"shiny\", \"miniUI\")). sc_answers() works without it.", p), call. = FALSE)
  viewer <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    shiny::dialogViewer("Answer template browser (study use only)", width = 1200, height = 800) else shiny::browserViewer()
  res <- shiny::runGadget(.ab_app_ui(), .ab_app_server, viewer = viewer, stopOnCancel = FALSE)
  if (!is.list(res)) return(invisible(NULL))
  cat("\n")
  invisible(sc_answers_fill(res$id, res$values))
}

.ab_app_css <- "
body { font-size: 13px; }
.sc-note { font-size: 12px; color: #6b4e00; background: #fff8e1; border: 1px solid #f0d58c; padding: 4px 8px; margin-bottom: 6px; }
.sc-wrap { display: flex; gap: 12px; height: calc(100% - 34px); }
.sc-left { flex: 3; min-width: 210px; overflow-y: auto; }
.sc-mid { flex: 4; min-width: 230px; overflow-y: auto; }
.sc-right { flex: 6; min-width: 300px; overflow-y: auto; padding-right: 6px; }
.sc-mid select { font-family: inherit; font-size: 13px; }
.sc-lab { font-weight: bold; font-size: 11px; letter-spacing: .04em; color: #444; margin-top: 10px; text-transform: uppercase; }
.sc-use { border-left: 4px solid #2e7d32; background: #f1f8f1; padding: 4px 8px; }
.sc-avoid { border-left: 3px solid #c62828; padding-left: 8px; }
.sc-ans { background: #f6f8fa; border: 1px solid #ddd; padding: 8px; white-space: pre-wrap; }
.sc-meta { color: #555; font-size: 12px; }
.sc-weak { color: #b00020; font-weight: bold; font-size: 12px; }
.sc-todo { color: #b00020; font-weight: bold; }
.sc-q { border-left: 3px solid #1565c0; padding-left: 8px; margin: 6px 0; }
.sc-quote { border-left: 3px solid #6a1b9a; padding-left: 8px; margin: 6px 0; font-style: italic; }
.sc-sib { margin: 2px 0; }
#filled { font-family: inherit; width: 100%; }
.form-group { margin-bottom: 8px; }
@media (max-width: 820px) { .sc-wrap { flex-direction: column; height: auto; } .sc-left, .sc-mid, .sc-right { overflow: visible; } }
"

.ab_app_ui <- function() {
  b <- .ab()
  miniUI::miniPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(.ab_app_css))),
    miniUI::gadgetTitleBar("Answer template browser",
                           left = miniUI::miniTitleBarButton("close", "Close"),
                           right = miniUI::miniTitleBarButton("done", "Print to console", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::div(class = "sc-note", "Study use only: authored exam-style templates, not official solutions. The 2026/27 rules forbid prepared R scripts and AI tools at the exam; consult the current exam rules."),
      shiny::div(class = "sc-wrap",
        shiny::div(class = "sc-left",
          shiny::tagAppendAttributes(shiny::textInput("q", "Search", width = "100%",
                                                      placeholder = "e.g. among those who, Bayes, SE, qnorm"),
                                     autofocus = NA),
          shiny::selectInput("group", "Start here: what does the question ask?", width = "100%", selectize = FALSE,
                             choices = c("Any" = "", stats::setNames(b$groups$id, paste0(b$groups$label, ifelse(b$groups$weak, "  (weak spot)", ""))))),
          shiny::selectInput("chapter", "Chapter", width = "100%", selectize = FALSE,
                             choices = c("All chapters" = "", stats::setNames(b$chapters$number, sprintf("Ch. %d  %s", b$chapters$number, b$chapters$title)))),
          shiny::checkboxInput("weak", "Weak spots only (critical and high priority)", FALSE),
          shiny::checkboxInput("review", "Only cards I marked \"needs review\"", FALSE),
          shiny::uiOutput("cats")),
        shiny::div(class = "sc-mid", shiny::uiOutput("cases")),
        shiny::div(class = "sc-right",
          shiny::tabsetPanel(id = "tab",
            shiny::tabPanel("Card",
              shiny::checkboxInput("recall", "Practice mode: hide the answer until I ask", FALSE),
              shiny::uiOutput("card"),
              shiny::uiOutput("fillform"),
              shiny::div(class = "sc-lab", "Your answer (selectable text)"),
              shiny::tags$textarea(id = "filled", rows = 7, readonly = NA, onfocus = "this.select()"),
              shiny::uiOutput("todo"),
              shiny::div(style = "margin-top: 6px;",
                shiny::tags$button(type = "button", class = "btn btn-default btn-sm", id = "copybtn",
                  onclick = paste0("var t=document.getElementById('filled');",
                                   "var ok=function(){document.getElementById('copymsg').textContent='Copied.';};",
                                   "var no=function(){t.focus();t.select();document.getElementById('copymsg').textContent='Text selected: press Ctrl+C / Cmd+C.';};",
                                   "if(navigator.clipboard){navigator.clipboard.writeText(t.value).then(ok,no);}",
                                   "else{t.select();try{document.execCommand('copy')?ok():no();}catch(e){no();}}"),
                  "Copy answer"),
                shiny::actionButton("reset", "Reset blanks", class = "btn-sm"),
                shiny::actionButton("nextcard", "Next card", class = "btn-sm"),
                shiny::span(id = "copymsg", class = "sc-meta", style = "margin-left: 8px;"))),
            shiny::tabPanel("Past-paper questions",
              shiny::selectInput("qsource", "Source", width = "100%", selectize = FALSE,
                                 choices = c("Questions of the selected card's kind" = "",
                                             stats::setNames(b$sources$key[b$sources$past_paper], b$sources$label[b$sources$past_paper]))),
              shiny::uiOutput("questions")))))))
}

.ab_app_server <- function(input, output, session) {
  bank <- .ab(); tpl <- bank$templates; cats <- bank$categories
  key <- paste(tpl$chapter, tpl$category, sep = ".")
  shown <- shiny::reactiveVal(FALSE)
  ratings <- shiny::reactiveVal(.ab_ratings())

  # cards matching the search and the filters, best first
  found <- shiny::reactive({
    q <- input$q %||% ""
    rows <- if (nzchar(trimws(q))) .ab_search(q)$rows else seq_len(nrow(tpl))
    keep <- rep(TRUE, nrow(tpl))
    if (nzchar(input$group %||% "")) keep <- keep & key %in% .ab_group_keys(input$group)$keys
    if (nzchar(input$chapter %||% "")) keep <- keep & tpl$chapter == as.integer(input$chapter)
    if (isTRUE(input$weak)) keep <- keep & tpl$priority %in% c("critical", "high")
    if (isTRUE(input$review)) keep <- keep & tpl$id %in% names(ratings())[ratings() == "needs review"]
    rows[keep[rows]]
  })
  output$cats <- shiny::renderUI({
    rows <- found()
    cnt <- table(factor(key[rows], levels = cats$key))
    show <- cats$key[cnt > 0]
    sel <- shiny::isolate(input$cat) %||% "all"
    if (!sel %in% c("all", show)) sel <- "all"
    shiny::radioButtons("cat", "Kind of question", width = "100%", selected = sel,
                        choices = stats::setNames(c("all", show),
                                                  c(sprintf("All (%d)", length(rows)),
                                                    sprintf("Ch.%d %s (%d)", cats$chapter[match(show, cats$key)],
                                                            cats$label[match(show, cats$key)], as.integer(cnt[show])))))
  })
  visible <- shiny::reactive({
    rows <- found(); cat <- input$cat %||% "all"
    if (cat != "all") rows <- rows[key[rows] == cat]
    rows
  })
  output$cases <- shiny::renderUI({
    rows <- visible()
    if (!length(rows))
      return(shiny::div(shiny::div(class = "sc-lab", "Cards"),
                        shiny::p("No card matches. Try a shorter word (skew, density, Bayes, standard error, qnorm) or clear a filter.")))
    keep <- shiny::isolate(input$case)
    lab <- sprintf("%s%s  -  %s", ifelse(tpl$priority[rows] == "critical", "! ", ""), tpl$title[rows], tpl$id[rows])
    shiny::selectInput("case", sprintf("Cards (%d)%s", length(rows), if (nzchar(trimws(input$q %||% ""))) ", best match first" else ""),
                       choices = stats::setNames(tpl$id[rows], lab),
                       selected = if (!is.null(keep) && keep %in% tpl$id[rows]) keep else tpl$id[rows][1],
                       selectize = FALSE, size = min(24, max(8, length(rows))), width = "100%")
  })
  current <- shiny::reactive({
    id <- input$case
    if (is.null(id) || !id %in% tpl$id[visible()]) return(NULL)
    match(id, tpl$id)
  })
  shiny::observeEvent(current(), shown(FALSE), ignoreNULL = FALSE)
  shiny::observeEvent(input$showans, shown(TRUE))
  shiny::observeEvent(input$nextcard, {
    ids <- tpl$id[visible()]; i <- match(input$case, ids)
    if (length(ids)) shiny::updateSelectInput(session, "case", selected = ids[if (is.na(i) || i == length(ids)) 1 else i + 1])
  })
  rate <- function(r) { i <- current(); if (!is.null(i)) { .ab_rate(tpl$id[i], r); ratings(.ab_ratings()) } }
  shiny::observeEvent(input$rate_ok, rate("remembered"))
  shiny::observeEvent(input$rate_review, rate("needs review"))

  parts <- shiny::reactive({
    i <- current(); if (is.null(i)) return(character(0))
    unique(trimws(unlist(strsplit(tpl$placeholders[[i]], ",", fixed = TRUE))))
  })
  values <- shiny::reactive({
    p <- parts()
    stats::setNames(lapply(seq_along(p), function(k) input[[paste0("ph_", k)]] %||% ""), p)
  })
  hidden <- shiny::reactive(isTRUE(input$recall) && !shown())

  output$card <- shiny::renderUI({
    i <- current()
    if (is.null(i)) return(shiny::p("Choose a card."))
    t <- tpl[i, ]
    src <- t$source_refs[[1]]; fm <- t$formulas[[1]]
    clab <- cats$label[cats$key == key[i]]
    sib <- which(key == key[i] & tpl$id != t$id)
    qs <- bank$questions[vapply(bank$questions$cards, function(cc) t$id %in% cc, TRUE), ]
    r <- ratings()[t$id]
    shiny::tagList(
      shiny::h4(t$title, style = "margin-top: 0;"),
      shiny::div(class = "sc-meta", sprintf("%s  |  Ch. %d  |  %s", t$id, t$chapter, clab)),
      if (t$priority != "normal") shiny::div(class = "sc-weak", .ab_pri_label(t$priority)),
      if (!is.na(r)) shiny::div(class = "sc-meta", paste("Your rating this session:", r)),
      shiny::div(class = "sc-lab", "Use this when"), shiny::div(class = "sc-use", t$use_when),
      shiny::div(class = "sc-lab", "Ready-to-practise answer"),
      if (hidden()) shiny::actionButton("showans", "Show answer", class = "btn-sm")
      else shiny::tagList(shiny::div(class = "sc-ans", t$answer_template),
                          if (isTRUE(input$recall)) shiny::div(style = "margin-top: 4px;",
                            shiny::actionButton("rate_ok", "Remembered", class = "btn-sm"),
                            shiny::actionButton("rate_review", "Needs review", class = "btn-sm"))),
      if (length(fm)) shiny::tagList(shiny::div(class = "sc-lab", "Formulas (book notation; R for preparation only)"),
                                     shiny::div(paste(fm, collapse = "   ;   "))),
      if (nzchar(t$avoid_when)) shiny::tagList(shiny::div(class = "sc-lab", "Don't use when / common mistake"),
                                               shiny::div(class = "sc-avoid", t$avoid_when)),
      shiny::div(class = "sc-lab", "Source status"),
      shiny::div(class = "sc-meta", paste(.ab_status(t$provenance),
                                          if (t$source_support %in% names(.ab_support)) .ab_support[[t$source_support]],
                                          if (t$corrected) "Wording corrected in statcram.",
                                          if (length(src)) paste0("Sources: ", paste(.ab_src_label(src), collapse = "; "), "."))),
      if (nrow(qs)) shiny::tagList(shiny::div(class = "sc-lab", sprintf("Past-paper questions of this kind (%d, verbatim)", nrow(qs))),
                                   lapply(seq_len(min(2, nrow(qs))), function(k)
                                     shiny::div(class = "sc-q", sprintf("%s, line %d: \"%s\"", .ab_src_label(qs$source[k]), qs$line[k], .ab_plain(qs$text[k])))),
                                   if (nrow(qs) > 2) shiny::div(class = "sc-meta", "More in the \"Past-paper questions\" tab.")),
      if (length(sib)) shiny::tagList(shiny::div(class = "sc-lab", "Other cases of this kind: check you have the right one"),
                                      lapply(utils::head(sib, 12), function(s) shiny::div(class = "sc-sib",
                                        shiny::strong(tpl$title[s]), sprintf(" - use when: %s", tpl$use_when[s])))))
  })
  output$fillform <- shiny::renderUI({
    p <- parts()
    if (!length(p)) return(shiny::div(class = "sc-lab", "No blanks to fill in this template"))
    shiny::tagList(shiny::div(class = "sc-lab", "Fill the blanks"),
                   lapply(seq_along(p), function(k) shiny::textInput(paste0("ph_", k), sprintf("[%s]", p[k]), width = "100%")))
  })
  shiny::observe({
    i <- current()
    txt <- if (is.null(i) || hidden()) "" else .ab_fill_text(tpl$answer_template[i], values())
    shiny::updateTextAreaInput(session, "filled", value = txt)
  })
  output$todo <- shiny::renderUI({
    i <- current(); if (is.null(i)) return(NULL)
    unf <- .ab_unfilled(i, values())
    if (length(unf)) shiny::div(class = "sc-todo", sprintf("Still to fill: %s", paste(sprintf("[%s]", unf), collapse = "  ")))
    else shiny::div(class = "sc-meta", "All blanks filled. Check that the card's conditions hold for your data.")
  })
  output$questions <- shiny::renderUI({
    src <- input$qsource %||% ""
    if (nzchar(src)) {
      qs <- bank$questions[bank$questions$source == src, ]; oq <- bank$quotes[bank$quotes$source == src, ]
    } else {
      i <- current(); if (is.null(i)) return(shiny::p("Choose a card or a source."))
      qs <- bank$questions[vapply(bank$questions$cards, function(cc) tpl$id[i] %in% cc, TRUE), ]; oq <- bank$quotes[0, ]
    }
    shiny::tagList(
      shiny::p(class = "sc-meta", "Questions and official solution excerpts are quoted word for word from your transcription of the past papers (Markdown emphasis removed). Some need the exam's context, graph or data file."),
      shiny::div(class = "sc-lab", sprintf("Verbatim questions (%d)", nrow(qs))),
      if (!nrow(qs)) shiny::p("None linked to this card."),
      lapply(seq_len(nrow(qs)), function(k) shiny::div(class = "sc-q",
        shiny::div(class = "sc-meta", sprintf("%s  |  %s, line %d  |  %s", qs$id[k], .ab_src_label(qs$source[k]), qs$line[k], qs$pattern[k])),
        .ab_plain(qs$text[k]),
        shiny::div(class = "sc-meta", paste("Practise with:", paste(qs$cards[[k]], collapse = ", "))))),
      if (nrow(oq)) shiny::tagList(shiny::div(class = "sc-lab", sprintf("Official solution excerpts (%d, verbatim)", nrow(oq))),
                                   lapply(seq_len(nrow(oq)), function(k) shiny::div(class = "sc-quote",
                                     shiny::div(class = "sc-meta", sprintf("line %d", oq$line[k])), .ab_plain(oq$text[k])))))
  })
  shiny::observeEvent(input$reset, for (k in seq_along(parts())) shiny::updateTextInput(session, paste0("ph_", k), value = ""))
  shiny::observeEvent(input$done, {
    i <- current()
    shiny::stopApp(if (is.null(i)) NULL else list(id = tpl$id[i], values = Filter(nzchar, values())))
  })
  shiny::observeEvent(input$close, shiny::stopApp(NULL))
}
