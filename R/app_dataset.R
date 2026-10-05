# One dataset, opened from the board: where its build stands and what fails, then its
# graph, rules, variables and a comparison of generated with source data.

dataset_ui <- function() tagList(
  uiOutput("dataset_header"),
  uiOutput("dataset_problems"),
  bslib::navset_underline(id = "dataset_tabs",
    bslib::nav_panel("Graph", div(class = "pt-3", graph_tab_ui())),
    bslib::nav_panel("Rules", div(class = "pt-3", rules_tab_ui())),
    bslib::nav_panel("Variables", div(class = "pt-3", variables_tab_ui())),
    bslib::nav_panel("Compare", div(class = "pt-3", uiOutput("compare")))))

dataset_server <- function(input, output, session, b, tick, current, change, step, notify, open_dataset) {
  info <- reactive({
    tick()
    b$info[[req(current())$name]]
  })
  observeEvent(input$back, bslib::nav_select("page", "board"))
  observeEvent(input$reset_dataset, {
    reset_dataset(b, current()$name)
    step()
    notify(paste(current()$name, "is built by the policy again."))
  })

  output$dataset_header <- renderUI({
    ds <- req(current())
    i <- req(info())
    manual <- ds$name %in% b$manual
    tagList(
      tags$button(type = "button", class = "btn btn-link px-0 mb-2", onclick = "Shiny.setInputValue('back', Date.now())",
        icon("arrow-left"), " Study"),
      div(class = "d-flex flex-wrap align-items-center gap-2 mb-1",
        h2(class = "fw-bold m-0 me-2", ds$name), span(class = "stage-badge", `data-stage` = i$stage, STAGES[[i$stage]]),
        if (manual) badge("info", "Changed by hand"),
        if (manual) actionButton("reset_dataset", "Back to the policy", class = "btn-sm btn-outline-secondary", icon = icon("rotate-left"))),
      div(class = "small text-body-secondary mb-3", paste(c(CLASSES[[ds$class]], sprintf("%s records", format(i$records, big.mark = ",")),
        record_structure_label(ds), if (is.finite(i$fit)) sprintf("built in %.1f s%s", i$discover + i$fit, if (i$cached) " (cached)" else "")),
        collapse = " · ")))
  })

  # Failed checks of the graph (the dataset is then left out of generation), a failed
  # build, and failed coherence checks of the generated data.
  output$dataset_problems <- renderUI({
    ds <- req(current())
    i <- req(info())
    failed <- function(x) if (!is.null(x)) x[x$Dataset == ds$name & x$Status == "failed", ]
    spec <- failed(b$checks[[ds$name]])
    coherence <- failed(b$coherence)
    rows <- c(if (i$stage == "error") list(c("Build failed", i$message)),
      lapply(seq_len(NROW(spec)), function(k) c(spec$Check[k], spec$Problems[k])),
      lapply(seq_len(NROW(coherence)), function(k) c(coherence$Check[k], coherence$Problems[k])))
    if (!length(rows)) return(NULL)
    div(class = "alert alert-danger d-flex gap-2 small", icon("triangle-exclamation", class = "mt-1"),
      div(if (NROW(spec) || i$stage == "error") div(class = "fw-bold mb-1", "Left out of the generated data until this is fixed."),
        lapply(rows, function(r) div(strong(r[1]), " ", span(class = "text-body", r[2])))))
  })

  rules_tab_server(input, output, session, b, current, change, notify)
  graph_tab_server(input, output, session, b, current, change, notify)

  # ---- Compare ------------------------------------------------------------------------

  generated <- reactive({
    tick()
    b$generated
  })
  review_name <- reactive({
    name <- req(current())$name
    req(generated(), name %in% names(generated()$data))
    name
  })
  output$compare <- renderUI({
    name <- req(current())$name
    if (is.null(generated()) || !name %in% names(generated()$data)) return(bslib::card(empty_state("Not generated yet",
      "The comparison appears once the study has been sampled with this dataset in it.")))
    bslib::navset_card_underline(full_screen = TRUE, id = "review_tabs",
      title = span("Source and synthetic", span(class = "text-body-secondary small fw-normal ms-2",
        sprintf("%s synthetic records", format(nrow(generated()$data[[name]]), big.mark = ",")))),
      bslib::nav_panel("Marginals",
        bslib::layout_columns(col_widths = bslib::breakpoints(md = 12, lg = c(7, 5)),
          DT::DTOutput("marginal_table", fill = FALSE),
          div(plotOutput("marginal_plot", height = "320px")))),
      bslib::nav_panel("Trajectories",
        uiOutput("trajectory_controls"),
        plotOutput("trajectory_plot", height = "380px")),
      bslib::nav_panel("Event rates",
        plotOutput("event_plot", height = "360px")),
      bslib::nav_panel("Records",
        DT::DTOutput("preview_table", fill = FALSE)))
  })
  output$preview_table <- DT::renderDT({
    x <- utils::head(generated()$data[[review_name()]], 500)
    for (v in names(x)) if (inherits(x[[v]], "Date")) x[[v]] <- format(x[[v]])
    DT::datatable(x, rownames = FALSE, selection = "none", class = "compact hover nowrap",
      options = list(dom = "tip", pageLength = 15, scrollX = TRUE))
  })
  marginals <- reactive({
    name <- review_name()
    x <- compare_marginals(b$study, b$study$source$data[[name]], b$generated$data[[name]], name)
    x[order(match(x$Type, c("sampled", "key", "rule"))), ]
  })
  output$marginal_table <- DT::renderDT({
    x <- marginals()
    x$Type <- vapply(x$Type, function(t) as.character(type_badge(t)), "")
    compact_table(x[c("Variable", "Type", "Source", "Synthetic")], selection = list(mode = "single", selected = 1),
      escape = FALSE, height = "320px")
  })
  output$marginal_plot <- renderPlot({
    name <- review_name()
    variable <- marginals()$Variable[req(input$marginal_table_rows_selected)]
    d <- rbind(data.frame(Data = "Source", value = b$study$source$data[[name]][[variable]]),
      data.frame(Data = "Synthetic", value = b$generated$data[[name]][[variable]]))
    observed <- d[!is.na(d$value), ]
    if ((is.numeric(d$value) || inherits(d$value, "Date")) && length(unique(observed$value)) > MAX_DISCRETE_VALUES)
      return(ggplot2::ggplot(observed, ggplot2::aes(value, fill = Data, colour = Data)) +
        ggplot2::geom_density(alpha = 0.25) + ggplot2::scale_fill_manual(values = PLOT_COLORS) +
        ggplot2::scale_colour_manual(values = PLOT_COLORS) + ggplot2::labs(x = variable, y = NULL, fill = NULL, colour = NULL) +
        plot_theme())
    counts <- as.data.frame(table(Data = d$Data, value = level_string(d$value)), stringsAsFactors = FALSE)
    counts$share <- counts$Freq / ave(counts$Freq, counts$Data, FUN = sum)
    top <- utils::head(names(sort(tapply(counts$share, counts$value, sum), decreasing = TRUE)), 12)
    counts <- counts[counts$value %in% top, ]
    counts$value <- factor(counts$value, rev(top))
    ggplot2::ggplot(counts, ggplot2::aes(share, value, fill = Data)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
      ggplot2::scale_fill_manual(values = PLOT_COLORS) + ggplot2::scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL, title = variable) + plot_theme()
  }, res = 96)

  output$trajectory_controls <- renderUI({
    name <- review_name()
    ds <- b$study$datasets[[name]]
    s <- ds$structure
    numeric <- ds$variables[ds$kinds[ds$variables] == "numeric" &
      vapply(ds$variables, function(v) identical(node_type(ds, v), "sampled"), TRUE)]
    if (is.null(s$visit) || !length(numeric)) return(hint("Trajectories need a dataset with visits and a numeric result."))
    tests <- if (!is.null(s$test)) sort(unique(format_value(b$generated$data[[name]][[s$test]])))
    bslib::layout_columns(col_widths = c(6, 6),
      selectInput("trajectory_variable", "Result", numeric, numeric[1]),
      if (length(tests)) selectInput("trajectory_test", "Test", tests, tests[1]))
  })
  output$trajectory_plot <- renderPlot({
    name <- review_name()
    variable <- req(input$trajectory_variable)
    req(variable %in% b$study$datasets[[name]]$variables)
    test <- input$trajectory_test %||% NA
    arm <- arm_variable(b$study$datasets$DM)
    source <- trajectory_data(b$study, b$study$source$data, name, variable, test, arm)
    synthetic <- trajectory_data(b$study, b$generated$data, name, variable, test, arm)
    req(source, synthetic)
    d <- rbind(cbind(source, Data = "Source"), cbind(synthetic, Data = "Synthetic"))
    d$visit <- factor(format(d$visit), levels = format(sort(unique(d$visit))))
    ggplot2::ggplot(d, ggplot2::aes(visit, mean, colour = arm, linetype = Data, group = interaction(arm, Data))) +
      ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 1.6) +
      ggplot2::scale_linetype_manual(values = c(Source = "dashed", Synthetic = "solid")) +
      ggplot2::labs(x = "Visit", y = paste("Mean", variable), colour = NULL, linetype = NULL,
        title = if (!is.na(test)) test) + plot_theme()
  }, res = 96)

  output$event_plot <- renderPlot({
    name <- review_name()
    req(name != "DM")
    arm <- arm_variable(b$study$datasets$DM)
    d <- rbind(cbind(event_rate_data(b$study$source$data, name, arm), Data = "Source"),
      cbind(event_rate_data(b$generated$data, name, arm), Data = "Synthetic"))
    ggplot2::ggplot(d, ggplot2::aes(arm, rate, fill = Data)) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
      ggplot2::scale_fill_manual(values = PLOT_COLORS) +
      ggplot2::labs(x = NULL, y = paste(name, "records per subject"), fill = NULL) + plot_theme()
  }, res = 96)
}

# ---- Plots ------------------------------------------------------------------------------

plot_theme <- function() ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "top", panel.grid.minor = ggplot2::element_blank(),
    text = ggplot2::element_text(colour = "#1d2327"))
