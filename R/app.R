# ------------------------------------------------------------
# sc_app(): a point-and-click panel for the first-partial procedures,
# in RStudio's Viewer pane (also in the Addins menu). It is a standard
# miniUI gadget: the forms are built from .app_specs (R/app-spec.R) and
# every result is the same printout the console shows.
# ------------------------------------------------------------

#' Point-and-click panel (RStudio Viewer pane)
#'
#' Opens a panel with the procedures of the first partial: describing data,
#' random variables and the CLT, estimation of one mean and one proportion.
#' Pick a procedure, fill in the form (data from the objects loaded now, or
#' any R expression) and read the result: the same printout as in the
#' console, with exam wording and the UBStats call to practise. The plot is
#' in its own tab. "Print to console" closes the panel and prints the result
#' and its call in the console. Needs the shiny and miniUI packages; the
#' menu [sc()] works without them.
#'
#' @return Invisibly, the result printed to the console (or `NULL`).
#' @examples
#' if (interactive()) sc_app()
#' @export
sc_app <- function() {
  for (p in c("shiny", "miniUI"))
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("sc_app() needs the %s package: install.packages(c(\"shiny\", \"miniUI\")). The menu sc() works without it.", p), call. = FALSE)
  viewer <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) shiny::paneViewer(minHeight = 600)
            else shiny::browserViewer()
  cl <- shiny::runGadget(.app_ui(), .app_server, viewer = viewer, stopOnCancel = FALSE)
  if (!is.call(cl)) return(invisible(NULL))
  cat("> ", paste(deparse(cl, width.cutoff = 500L), collapse = " "), "\n", sep = "")
  invisible(.run_call(cl))
}

.app_choices <- function() {
  topics <- unique(vapply(.app_specs, `[[`, "", "topic"))
  stats::setNames(lapply(topics, function(tp) {
    sp <- .app_specs[vapply(.app_specs, function(s) identical(s$topic, tp), logical(1))]
    stats::setNames(names(sp), vapply(sp, `[[`, "", "label"))
  }), topics)
}

.app_ui <- function() {
  miniUI::miniPage(
    miniUI::gadgetTitleBar("statcram",
                           left = miniUI::miniTitleBarButton("close", "Close"),   # not the Cancel button: Esc in a dropdown must not close the panel
                           right = miniUI::miniTitleBarButton("done", "Print to console", primary = TRUE)),
    miniUI::miniContentPanel(
      shiny::fillRow(flex = c(4, 7),
        shiny::div(style = "height: 100%; overflow-y: auto; padding-right: 12px;",
                   shiny::selectInput("proc", "Procedure", choices = .app_choices(), selected = "desc_summary", width = "100%", selectize = FALSE),
                   shiny::uiOutput("form")),
        shiny::div(style = "height: 100%; overflow-y: auto;",
                   shiny::tabsetPanel(id = "tab",
                     shiny::tabPanel("Result", shiny::verbatimTextOutput("result")),
                     shiny::tabPanel("Plot", shiny::plotOutput("plot", height = "460px")))))))
}

# Objects that fit a data field, grouped by data frame.
.app_data_choices <- function(kind) {
  lo <- tryCatch(.live_objects(), error = function(e) NULL)
  if (is.null(lo) || !nrow(lo)) return(list())
  ok <- switch(kind, numeric = lo$kind %in% c("numeric", "binary"),
               categorical = lo$kind %in% c("categorical", "event", "binary"),
               lo$kind != "other")
  lo <- lo[ok, , drop = FALSE]
  split(stats::setNames(lo$expr, sprintf("%s   (%s)", lo$expr, lo$tag)), lo$group)
}

.app_field_ui <- function(f, pid) {
  id <- paste(pid, f$id, sep = "__")
  ui <- switch(f$type,
    data = shiny::selectizeInput(id, f$label, choices = c(list(""), .app_data_choices(f$kind)), selected = "", width = "100%",
                                 options = list(create = TRUE, placeholder = "choose, or type an R expression")),
    dataframe = shiny::selectInput(id, f$label, width = "100%", selectize = FALSE,
                                   choices = Filter(function(nm) is.data.frame(get(nm, envir = .GlobalEnv)), ls(envir = .GlobalEnv))),
    num = , nums = , vals = shiny::textInput(id, f$label, value = f$default %||% "", placeholder = f$hint %||% "", width = "100%"),
    levels = shiny::uiOutput(paste0("lv_", id)),
    choice = shiny::radioButtons(id, f$label, choices = stats::setNames(names(f$choices), unname(f$choices)),
                                 selected = f$default, inline = TRUE),
    check = shiny::checkboxInput(id, f$label, value = isTRUE(f$default)),
    pick = shiny::tagList(
      shiny::radioButtons(id, f$label, choices = stats::setNames(names(f$choices), unname(f$choices)), selected = f$default, inline = TRUE),
      shiny::textInput(paste0(id, "_value"), NULL, placeholder = f$hint %||% "value", width = "100%")))
  if (is.null(f$when)) return(ui)
  shiny::conditionalPanel(sprintf("input['%s__%s'] == '%s'", pid, names(f$when)[1], f$when[[1]]), ui)
}

.app_form <- function(pid) {
  sp <- .app_specs[[pid]]
  shiny::tagList(lapply(sp$fields, .app_field_ui, pid = pid),
                 shiny::helpText(sprintf("Same as %s() in the console. Numbers: 10 25 30, or any R expression (sqrt(380/80), mean(x)).", sp$fn)))
}

# Evaluate a call built by the panel: the function from statcram, the data from the workspace.
.app_eval <- function(cl) {
  env <- new.env(parent = .GlobalEnv)
  fn <- as.character(cl[[1]])
  assign(fn, get(fn, envir = environment(.app_eval)), envir = env)
  eval(cl, env)
}

.app_print <- function(cl) {
  if (!is.call(cl)) return(cl)
  op <- options(statcram.plot = FALSE); on.exit(options(op))
  out <- tryCatch(utils::capture.output(print(.app_eval(cl))), error = function(e) paste("Problem:", conditionMessage(e)))
  while (length(out) && !nzchar(out[1])) out <- out[-1]
  paste(c(paste(">", paste(deparse(cl, width.cutoff = 500L), collapse = " ")), "", out), collapse = "\n")
}

.app_plot <- function(cl) {
  if (!is.call(cl)) return(invisible(NULL))
  op <- options(statcram.plot = TRUE); on.exit(options(op))
  utils::capture.output(tryCatch(.app_eval(cl), error = function(e) NULL))
  invisible(NULL)
}

.app_server <- function(input, output, session) {
  output$form <- shiny::renderUI(.app_form(input$proc))
  # level pickers follow the variable chosen in their source field
  for (pid in names(.app_specs)) for (f in .app_specs[[pid]]$fields) if (f$type == "levels") local({
    fid <- paste(pid, f$id, sep = "__"); src <- paste(pid, f$src, sep = "__"); ff <- f
    output[[paste0("lv_", fid)]] <- shiny::renderUI({
      ch <- .app_levels(input[[src]])
      shiny::selectizeInput(fid, ff$label, choices = if (ff$multiple) ch else c("", ch), multiple = ff$multiple, width = "100%",
                            selected = intersect(shiny::isolate(input[[fid]]), ch),
                            options = list(placeholder = if (length(ch)) "click the categories" else "choose the variable first"))
    })
  })
  values <- shiny::reactive({
    pid <- input$proc
    ids <- unlist(lapply(.app_specs[[pid]]$fields, function(f) if (f$type == "pick") c(f$id, paste0(f$id, "_value")) else f$id))
    stats::setNames(lapply(ids, function(id) input[[paste(pid, id, sep = "__")]]), ids)
  })
  call_r <- shiny::debounce(shiny::reactive(.app_call(.app_specs[[input$proc]], values())), 400)
  output$result <- shiny::renderText(.app_print(call_r()))
  output$plot <- shiny::renderPlot(.app_plot(call_r()))
  shiny::observeEvent(input$done, {
    cl <- call_r()
    shiny::stopApp(if (is.call(cl)) cl else NULL)
  })
  shiny::observeEvent(input$close, shiny::stopApp(NULL))
}
