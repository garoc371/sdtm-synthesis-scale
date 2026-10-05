# The study board: what the build is doing, every dataset as a tile, what needs a look,
# and the ledger of decisions the policy made. Nothing here has to be confirmed.

STAGES <- c(waiting = "Waiting", queued = "Queued", running = "Building", built = "Built", failed = "Check failed",
  error = "Error", skipped = "Not generated")

board_ui <- function() tagList(
  uiOutput("board_header"),
  conditionalPanel("output.has_study",
    bslib::layout_columns(col_widths = bslib::breakpoints(md = 12, xl = c(8, 4)),
      bslib::card(
        bslib::card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          "Datasets", div(class = "d-flex flex-wrap gap-2 small fw-normal",
            lapply(c("queued", "running", "built", "failed", "skipped"), function(s) span(class = "legend", `data-stage` = s, STAGES[[s]])))),
        uiOutput("tiles")),
      bslib::card(
        bslib::card_header(class = "d-flex justify-content-between align-items-center", "Needs a look",
          uiOutput("attention_count", inline = TRUE)),
        uiOutput("attention"))),
    bslib::card(full_screen = TRUE,
      bslib::card_header(uiOutput("decisions_title", inline = TRUE)),
      hint("What the policy and the starting graphs decided, by how many source records each changes. ",
        "A calculated variable follows its rule in every generated record; its source exceptions are counted here. ",
        "Nothing is waiting: open a dataset to change any of these."),
      DT::DTOutput("decisions", fill = FALSE))))

board_server <- function(input, output, session, b, tick, open_dataset) {
  status <- reactive({
    tick()
    build_status(b)
  })
  output$has_study <- reactive({
    tick()
    !is.null(b$info)
  })
  outputOptions(output, "has_study", suspendWhenHidden = FALSE)
  observeEvent(input$open_dataset, open_dataset(input$open_dataset))

  output$board_header <- renderUI({
    tick()
    if (!is.null(b$error)) return(div(class = "alert alert-danger", strong("The source could not be read. "), b$error))
    if (is.null(b$running$load) && is.null(b$study)) return(bslib::card(empty_state("Load a study",
      "Pick the CDISC pilot, uploaded files or a folder. Every dataset is then built, fitted and sampled without further input.",
      div(class = "row text-start g-3 mt-2 justify-content-center",
        lapply(list(c("1", "One policy", "Found rules are accepted when they reproduce enough checked records, in every dataset at once."),
          c("2", "Built in the background", "Datasets build in parallel workers and are cached by content, so a change rebuilds only what it affects."),
          c("3", "Exceptions, not approvals", "The board lists datasets whose checks fail and the decisions that change most records. Open one to change it.")),
          function(x) div(class = "col-md-4", div(class = "d-flex gap-2", span(class = "step-number text-primary border-primary", x[1]),
            div(strong(x[2]), div(class = "small", x[3])))))))))
    s <- status()
    data <- b$study$source$data
    done <- sum(s$stage %in% c("built", "failed", "error", "skipped"))
    built <- sum(s$stage == "built")
    look <- sum(s$stage %in% c("failed", "error") | s$coherence_failed > 0)
    generating <- !is.null(b$running$generate)
    g <- b$generated
    load_stage <- if (is.null(b$study)) list("active", "Reading the source…") else
      list("done", sprintf("%d datasets, %s subjects · %.0f s", length(data), format(nrow(data$DM), big.mark = ","), b$loaded))
    build_stage <- if (is.null(s)) list("waiting", "") else if (done < nrow(s)) list("active", sprintf("%d of %d · %d running", done,
      nrow(s), sum(s$stage == "running"))) else list("done", sprintf("%d built · %.0f s", built, max(s$finished, na.rm = TRUE)))
    generate_stage <- if (generating) list("active", if (is.null(g)) "Sampling…" else "Updating…") else
      if (!is.null(b$generation_error)) list("error", b$generation_error) else
      if (is.null(g)) list("waiting", "After the build") else
      list(if (build_busy(b)) "stale" else "done", sprintf("%s subjects, %s records · %.0f s", format(g$n, big.mark = ","),
        format(sum(vapply(g$data, nrow, 0L)), big.mark = ","), b$generation$finished))
    stage <- function(title, x, bar = NULL) div(class = "pipeline-stage", `data-state` = x[[1]],
      div(class = "fw-bold", title), div(class = "small text-body-secondary text-truncate", x[[2]]), bar)
    tagList(
      div(class = "d-flex flex-wrap align-items-baseline justify-content-between gap-2 mb-3",
        h2(class = "fw-bold m-0", "Study"),
        span(class = "small text-body-secondary", if (!is.null(b$study)) sprintf("%d supplemental folded · %d trial design copied · policy %d%%",
          length(study_supp(b$study)), length(study_design(b$study)), round(100 * b$share)))),
      div(class = "pipeline mb-3",
        stage("Load", load_stage),
        stage("Build", build_stage, if (!is.null(s)) div(class = "progress mt-2", style = "height: 5px",
          div(class = "progress-bar bg-success", style = sprintf("width: %.1f%%", 100 * built / nrow(s))),
          div(class = "progress-bar bg-danger", style = sprintf("width: %.1f%%", 100 * sum(s$stage %in% c("failed", "error")) / nrow(s))))),
        stage("Generate", generate_stage)),
      if (!is.null(s) && look) div(class = "small text-danger-emphasis mb-2", icon("triangle-exclamation"),
        sprintf(" %s left out of the generated data or failing a coherence check: see Needs a look.", plural(look, "dataset"))))
  })

  output$tiles <- renderUI({
    s <- status()
    req(s)
    supp <- study_supp(b$study)
    tile <- function(i) {
      r <- s[i, ]
      children <- names(supp)[vapply(supp, `[[`, "", "parent") == r$dataset]
      seconds <- sum(c(r$discover, r$fit), na.rm = TRUE)
      detail <- switch(r$stage, built = if (r$cached) "cached" else sprintf("%.0f s", seconds),
        failed = plural(r$spec_failed, "failed check"), STAGES[[r$stage]])
      tags$button(type = "button", class = "tile", `data-stage` = r$stage,
        title = paste(c(r$dataset, if (nzchar(r$message)) r$message), collapse = ": "),
        onclick = sprintf("Shiny.setInputValue('open_dataset', '%s', {priority: 'event'})", r$dataset),
        div(class = "d-flex justify-content-between align-items-baseline gap-1",
          span(class = "tile-name", r$dataset),
          span(if (r$manual) icon("hand", title = "Changed by hand"),
            if (r$coherence_failed > 0) icon("triangle-exclamation", class = "text-danger", title = "Coherence check failed"))),
        div(class = "tile-detail", format(r$records, big.mark = ","), " · ", detail),
        if (length(children)) div(class = "tile-detail", paste0("+", children, collapse = " ")))
    }
    classes <- s$class
    groups <- lapply(intersect(names(CLASSES), classes), function(class) {
      rows <- which(classes == class)
      div(class = "mb-3", div(class = "small fw-bold text-uppercase text-body-secondary mb-1",
        sprintf("%s · %d", CLASSES[[class]], length(rows))), div(class = "tile-grid", lapply(rows, tile)))
    })
    design <- names(study_design(b$study))
    tagList(groups, if (length(design)) hint(sprintf("%s · copied unchanged: %s", CLASSES[["design"]], paste(design, collapse = ", "))))
  })

  # Datasets left out of generation (their build failed) or whose generated data break a
  # coherence check, with the first problem.
  attention <- reactive({
    s <- status()
    req(s)
    problem <- function(checks, name) {
      x <- checks[checks$Dataset == name & checks$Status == "failed", ]
      if (nrow(x)) paste0(x$Check[1], ": ", x$Problems[1], if (nrow(x) > 1) sprintf(" (+%d more)", nrow(x) - 1)) else ""
    }
    rows <- lapply(seq_len(nrow(s)), function(i) {
      r <- s[i, ]
      what <- if (r$stage == "error") c("Build failed", r$message) else
        if (r$stage == "failed") c("Not generated: a check fails", problem(b$checks[[r$dataset]], r$dataset)) else
        if (r$coherence_failed > 0) c("Coherence check fails", problem(b$coherence, r$dataset))
      if (length(what)) data.frame(dataset = r$dataset, what = what[1], detail = what[2])
    })
    do.call(rbind, rows)
  })
  output$attention_count <- renderUI({
    n <- NROW(attention())
    if (n) badge("danger", n) else badge("success", 0)
  })
  output$attention <- renderUI({
    x <- attention()
    s <- status()
    if (is.null(x)) return(div(class = "text-body-secondary small", if (all(s$stage %in% c("built", "skipped")) && !is.null(b$coherence))
      div(class = "alert alert-success mb-0 d-flex gap-2", icon("circle-check", class = "mt-1"), span(class = "text-body",
        sprintf("Nothing. %d datasets built and generated; every check passes.", sum(s$stage == "built")))) else
      "Nothing so far. Datasets appear here when a check of their graph or of their generated data fails."))
    div(class = "list-group list-group-flush", lapply(seq_len(nrow(x)), function(i) tags$button(type = "button",
      class = "list-group-item list-group-item-action px-0",
      onclick = sprintf("Shiny.setInputValue('open_dataset', '%s', {priority: 'event'})", x$dataset[i]),
      div(class = "d-flex justify-content-between gap-2", strong(x$dataset[i]), span(class = "small text-danger-emphasis", x$what[i])),
      div(class = "small text-body-secondary text-break", x$detail[i]))))
  })

  # The ledger is recomputed only when a dataset's build changed.
  ledger <- local({
    last <- NULL
    reactive({
      tick()
      keys <- vapply(b$info %||% list(), `[[`, "", "key")
      if (!identical(keys, last$keys)) {
        x <- build_decisions(b)
        if (!is.null(x)) x <- x[x$dataset %in% names(keys)[nzchar(keys)], ]
        last <<- list(keys = keys, decisions = x)
      }
      last$decisions
    })
  })
  output$decisions_title <- renderUI({
    x <- ledger()
    tagList("Decisions", span(class = "text-body-secondary small fw-normal ms-2", if (NROW(x))
      sprintf("%d in %d datasets · %d set by hand", nrow(x), length(unique(x$dataset)), sum(x$by == "hand"))))
  })
  output$decisions <- DT::renderDT({
    x <- ledger()
    if (!NROW(x)) return(compact_table(data.frame(Message = "No decisions yet.")))
    table <- data.frame(Dataset = x$dataset, Variable = x$variable,
      Decision = vapply(x$decision, function(d) as.character(badge(switch(d, Calculated = "warning", `Rule rejected` = NA,
        `New identifier` = "info", "secondary"), d)), ""),
      Detail = sprintf('<span class="d-inline-block text-truncate" style="max-width: 360px">%s</span>', htmltools::htmlEscape(x$detail)),
      Reproduced = vapply(x$share, share_bar, ""), Records = x$records, By = x$by, stringsAsFactors = FALSE)
    DT::datatable(table, rownames = FALSE, selection = "single", escape = FALSE, class = "compact hover",
      options = list(dom = "ftip", pageLength = 12, scrollX = TRUE, order = list(list(5, "desc")),
        language = list(search = "", searchPlaceholder = "Filter")))
  })
  observeEvent(input$decisions_rows_selected, open_dataset(ledger()$dataset[input$decisions_rows_selected]))
}
