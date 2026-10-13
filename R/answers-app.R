# ------------------------------------------------------------
# sc_answers_app(): a window to browse, search and fill the study answer
# bank (R/answers.R). Left: search, scope and categories with counts.
# Middle: the matching cases. Right: the case card (use when / do not use
# when / answer / formulas / source) and a form to fill its blanks, with
# Copy, Reset and Print to console. Study use only.
# ------------------------------------------------------------

#' Answer template browser (study use only)
#'
#' Opens a window to find and study the answer templates of [sc_answers()]:
#' type words in the search box (or pick a category), choose a case, read
#' when it applies and when it does not, then fill its blanks and copy the
#' text or print it to the console. "Recall mode" hides the answer until you
#' ask for it, to test yourself. The templates are authored study examples,
#' not official solutions, and the window is for studying only: the course
#' rules forbid prepared scripts and AI tools at the exam. Needs the shiny
#' and miniUI packages; [sc_answers()] works without them.
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
    shiny::dialogViewer("Answer template browser (study use only)", width = 1150, height = 780) else shiny::browserViewer()
  res <- shiny::runGadget(.ab_app_ui(), .ab_app_server, viewer = viewer, stopOnCancel = FALSE)
  if (!is.list(res)) return(invisible(NULL))
  cat("\n")
  invisible(sc_answers_fill(res$id, res$values))
}

.ab_app_css <- "
body { font-size: 13px; }
.sc-note { font-size: 12px; color: #6b4e00; background: #fff8e1; border: 1px solid #f0d58c; padding: 4px 8px; margin-bottom: 6px; }
.sc-wrap { display: flex; gap: 12px; height: calc(100% - 34px); }
.sc-left { flex: 3; min-width: 200px; overflow-y: auto; }
.sc-mid { flex: 4; min-width: 220px; overflow-y: auto; }
.sc-right { flex: 6; min-width: 280px; overflow-y: auto; padding-right: 6px; }
.sc-mid select { font-family: inherit; font-size: 13px; }
.sc-lab { font-weight: bold; font-size: 11px; letter-spacing: .04em; color: #444; margin-top: 10px; text-transform: uppercase; }
.sc-use { border-left: 3px solid #2e7d32; padding-left: 8px; }
.sc-avoid { border-left: 3px solid #c62828; padding-left: 8px; }
.sc-ans { background: #f6f8fa; border: 1px solid #ddd; padding: 8px; white-space: pre-wrap; }
.sc-meta { color: #555; font-size: 12px; }
.sc-todo { color: #b00020; font-weight: bold; }
#filled { font-family: inherit; width: 100%; }
@media (max-width: 820px) { .sc-wrap { flex-direction: column; height: auto; } .sc-left, .sc-mid, .sc-right { overflow: visible; } }
"

.ab_app_ui <- function() {
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
                                                      placeholder = "e.g. unequal widths, among those, skew"),
                                     autofocus = NA),
          shiny::radioButtons("scope", NULL, c("All" = "all", "Ch. 2 core" = "core", "Cross-chapter" = "crossover"), inline = TRUE),
          shiny::uiOutput("cats")),
        shiny::div(class = "sc-mid", shiny::uiOutput("cases")),
        shiny::div(class = "sc-right",
          shiny::checkboxInput("recall", "Recall mode: hide the answer until I ask", FALSE),
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
            shiny::span(id = "copymsg", class = "sc-meta", style = "margin-left: 8px;")))))
  )
}

.ab_app_server <- function(input, output, session) {
  bank <- .ab(); tpl <- bank$templates; secs <- bank$sections
  shown <- shiny::reactiveVal(FALSE)

  # rows matching the search and the scope, best first
  found <- shiny::reactive({
    q <- input$q %||% ""
    rows <- if (nzchar(trimws(q))) .ab_search(q)$rows else seq_len(nrow(tpl))
    rows[.ab_scope_rows(input$scope %||% "all")[rows]]
  })
  output$cats <- shiny::renderUI({
    rows <- found()
    cnt <- as.integer(table(factor(tpl$category[rows], levels = secs$id)))
    sel <- shiny::isolate(input$cat) %||% "all"
    shiny::radioButtons("cat", "Category", width = "100%", selected = sel,
                        choices = stats::setNames(c("all", secs$id),
                                                  c(sprintf("All categories (%d)", length(rows)),
                                                    sprintf("%s (%d)", secs$title, cnt))))
  })
  visible <- shiny::reactive({
    rows <- found(); cat <- input$cat %||% "all"
    if (cat != "all") rows <- rows[tpl$category[rows] == cat]
    rows
  })
  output$cases <- shiny::renderUI({
    rows <- visible()
    if (!length(rows))
      return(shiny::div(shiny::div(class = "sc-lab", "Cases"),
                        shiny::p("No case matches. Try a shorter word (skew, density, modal class, among those, variance) or another category.")))
    keep <- shiny::isolate(input$case)
    shiny::selectInput("case", sprintf("Cases (%d)%s", length(rows), if (nzchar(trimws(input$q %||% ""))) ", best match first" else ""),
                       choices = stats::setNames(tpl$id[rows], sprintf("%s  -  %s", tpl$title[rows], tpl$id[rows])),
                       selected = if (!is.null(keep) && keep %in% tpl$id[rows]) keep else tpl$id[rows][1],
                       selectize = FALSE, size = min(22, max(8, length(rows))), width = "100%")
  })
  current <- shiny::reactive({
    id <- input$case
    if (is.null(id) || !id %in% tpl$id[visible()]) return(NULL)
    match(id, tpl$id)
  })
  shiny::observeEvent(current(), shown(FALSE), ignoreNULL = FALSE)
  shiny::observeEvent(input$showans, shown(TRUE))

  parts <- shiny::reactive({
    i <- current(); if (is.null(i)) return(character(0))
    unique(trimws(unlist(strsplit(tpl$placeholders[[i]], ",", fixed = TRUE))))
  })
  values <- shiny::reactive({
    p <- parts()
    stats::setNames(lapply(seq_along(p), function(k) input[[paste0("ph_", k)]] %||% ""), p)
  })

  output$card <- shiny::renderUI({
    i <- current()
    if (is.null(i)) return(shiny::p("Choose a case."))
    t <- tpl[i, ]
    src <- t$source_refs[[1]]; fm <- t$formulas[[1]]
    hide <- isTRUE(input$recall) && !shown()
    shiny::tagList(
      shiny::h4(t$title, style = "margin-top: 0;"),
      shiny::div(class = "sc-meta", sprintf("%s  |  %s  |  %s", t$id, secs$title[match(t$category, secs$id)], t$scope)),
      shiny::div(class = "sc-lab", "Use when"), shiny::div(class = "sc-use", t$use_when),
      shiny::div(class = "sc-lab", "Do not use when / exam trap"), shiny::div(class = "sc-avoid", t$avoid_when),
      shiny::div(class = "sc-lab", "Answer to study"),
      if (hide) shiny::actionButton("showans", "Show answer", class = "btn-sm")
      else shiny::div(class = "sc-ans", t$answer_template),
      if (length(fm)) shiny::tagList(shiny::div(class = "sc-lab", "Formulas"), shiny::div(paste(fm, collapse = "   ;   "))),
      shiny::div(class = "sc-lab", "Source / status"),
      shiny::div(class = "sc-meta", paste0(.ab_status(t$provenance),
                                            if (length(src)) paste0(" Related exam questions: ", paste(src, collapse = "; "), "."))))
  })
  output$fillform <- shiny::renderUI({
    p <- parts()
    if (!length(p)) return(shiny::div(class = "sc-lab", "No blanks to fill in this template"))
    shiny::tagList(shiny::div(class = "sc-lab", "Fill the blanks"),
                   lapply(seq_along(p), function(k) shiny::textInput(paste0("ph_", k), sprintf("[%s]", p[k]), width = "100%")))
  })
  shiny::observe({
    i <- current()
    txt <- if (is.null(i) || (isTRUE(input$recall) && !shown())) "" else .ab_fill_text(tpl$answer_template[i], values())
    shiny::updateTextAreaInput(session, "filled", value = txt)
  })
  output$todo <- shiny::renderUI({
    i <- current(); if (is.null(i)) return(NULL)
    v <- values(); filled <- names(v)[nzchar(trimws(unlist(v)))]
    unf <- Filter(function(p) !all(trimws(strsplit(p, ",", fixed = TRUE)[[1]]) %in% filled), tpl$placeholders[[i]])
    if (length(unf)) shiny::div(class = "sc-todo", sprintf("Still to fill: %s", paste(sprintf("[%s]", unf), collapse = "  ")))
    else shiny::div(class = "sc-meta", "All blanks filled. Check that the case's conditions hold for your data.")
  })
  shiny::observeEvent(input$reset, for (k in seq_along(parts())) shiny::updateTextInput(session, paste0("ph_", k), value = ""))
  shiny::observeEvent(input$done, {
    i <- current()
    shiny::stopApp(if (is.null(i)) NULL else list(id = tpl$id[i], values = Filter(nzchar, values())))
  })
  shiny::observeEvent(input$close, shiny::stopApp(NULL))
}
