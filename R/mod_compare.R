# The open dataset's generated data against its source: marginals, trajectories over visits,
# event rates by arm and the first records. The plots are in mod_compare_plots.R.

compare_ui <- function(id) {
  uiOutput(NS(id, "compare"))
}

# `generated`: the generated data, or NULL before the study is sampled. The study is read
# without following it: what is compared changes with the generated data.
compare_server <- function(id, dataset, study, generated) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # The open dataset, once it is generated.
    name <- reactive({
      name <- req(dataset())$name
      req(name %in% names(generated()$data))
      name
    })
    source <- function() isolate(study())$source$data[[name()]]
    synthetic <- function() generated()$data[[name()]]

    output$compare <- renderUI({
      data <- generated()$data[[req(dataset())$name]]
      if (is.null(data)) {
        return(card(empty_state(
          "Not generated yet",
          "The comparison appears once the study has been sampled with this dataset in it."
        )))
      }
      compare_card(ns, nrow(data))
    })

    marginals <- reactive({
      x <- compare_marginals(isolate(study()), source(), synthetic(), name())
      x[order(match(x$Type, c("sampled", "key", "rule"))), ]
    })
    output$marginal_table <- renderDT({
      x <- marginals()
      x$Type <- vapply(x$Type, function(t) as.character(type_badge(t)), "")
      compact_table(
        x[c("Variable", "Type", "Source", "Synthetic")],
        selection = list(mode = "single", selected = 1),
        escape = FALSE,
        height = "320px"
      )
    })
    output$marginal_plot <- renderPlot(
      {
        variable <- marginals()$Variable[req(input$marginal_table_rows_selected)]
        marginal_plot(source()[[variable]], synthetic()[[variable]], variable)
      },
      res = 96
    )

    output$trajectory_inputs <- renderUI({
      trajectory_inputs(ns, isolate(study())$datasets[[name()]], synthetic())
    })
    output$trajectory_plot <- renderPlot(
      {
        variable <- req(input$trajectory_variable)
        s <- isolate(study())
        req(variable %in% s$datasets[[name()]]$variables)
        trajectory_plot(s, generated()$data, name(), variable, input$trajectory_test %||% NA)
      },
      res = 96
    )

    output$event_plot <- renderPlot(
      {
        req(name() != "DM")
        event_plot(isolate(study()), generated()$data, name())
      },
      res = 96
    )

    output$records <- renderDT({
      x <- utils::head(synthetic(), 500)
      x[] <- lapply(x, function(column) {
        if (inherits(column, "Date")) format(column) else column
      })
      datatable(
        x,
        rownames = FALSE,
        selection = "none",
        class = "compact hover nowrap",
        options = list(dom = "tip", pageLength = 15, scrollX = TRUE)
      )
    })
  })
}

compare_card <- function(ns, records) {
  navset_card_underline(
    full_screen = TRUE,
    title = span("Source and synthetic", muted(sprintf("%s synthetic records", big(records)))),
    nav_panel(
      "Marginals",
      layout_columns(
        col_widths = breakpoints(md = 12, lg = c(7, 5)),
        DTOutput(ns("marginal_table"), fill = FALSE),
        plotOutput(ns("marginal_plot"), height = "320px")
      )
    ),
    nav_panel(
      "Trajectories",
      uiOutput(ns("trajectory_inputs")),
      plotOutput(ns("trajectory_plot"), height = "380px")
    ),
    nav_panel("Event rates", plotOutput(ns("event_plot"), height = "360px")),
    nav_panel("Records", DTOutput(ns("records"), fill = FALSE))
  )
}

# A numeric sampled result to follow over visits and, in a findings dataset, its test.
trajectory_inputs <- function(ns, ds, data) {
  sampled <- vapply(ds$variables, function(v) identical(node_type(ds, v), "sampled"), TRUE)
  numeric <- ds$variables[ds$kinds[ds$variables] == "numeric" & sampled]
  if (is.null(ds$structure$visit) || !length(numeric)) {
    return(hint("Trajectories need a dataset with visits and a numeric result."))
  }
  test <- ds$structure$test
  tests <- if (!is.null(test)) sort(unique(format_value(data[[test]])))
  layout_columns(
    selectInput(ns("trajectory_variable"), "Result", numeric),
    if (length(tests)) selectInput(ns("trajectory_test"), "Test", tests)
  )
}
