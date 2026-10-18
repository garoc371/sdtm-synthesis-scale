# One dataset, opened from the overview: where its build stands and what fails, then its
# graph, rules, variables and a comparison of generated with source data.

dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("header")),
    uiOutput(ns("problems")),
    navset_underline(
      nav_panel("Graph", graph_ui(ns("graph"))),
      nav_panel("Rules", rules_ui(ns("rules"))),
      nav_panel("Variables", variables_ui(ns("variables"))),
      nav_panel("Compare", uiOutput(ns("compare")))
    )
  )
}

dataset_server <- function(id, b, tick, current, change, step) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # The dataset's build and its failed checks, redrawn only when they change.
    state <- changes(function() {
      tick()
      ds <- current()
      if (is.null(ds)) {
        return(NULL)
      }
      failed <- function(x) {
        if (!is.null(x)) x[x$Dataset == ds$name & x$Status == "failed", ]
      }
      list(
        info = b$info[[ds$name]],
        manual = ds$name %in% b$manual,
        spec = failed(b$checks[[ds$name]]),
        coherence = failed(b$coherence)
      )
    })
    observeEvent(input$reset, {
      reset_dataset(b, current()$name)
      step()
      notify(paste(current()$name, "is built by the policy again."))
    })

    output$header <- renderUI({
      ds <- req(current())
      x <- req(state())
      i <- x$info
      built <- if (is.finite(i$fit)) {
        sprintf(
          "built in %.1f s%s",
          i$discover + i$fit,
          if (i$cached) " (cached)" else ""
        )
      }
      about <- c(
        CLASSES[[ds$class]],
        sprintf("%s records", big(i$records)),
        record_structure_label(ds),
        built
      )
      div(
        class = "page-title",
        h2(ds$name),
        stage_badge(i$stage),
        if (x$manual) badge("info", "Changed by hand"),
        if (x$manual) {
          actionButton(
            ns("reset"),
            "Back to the policy",
            class = "btn-sm btn-outline-secondary",
            icon = icon("rotate-left")
          )
        },
        muted(paste(about, collapse = " · "))
      )
    })

    # Failed checks of the graph (the dataset is then left out of generation), a failed
    # build, and the SDTM rules the generated data break: those broken more often than in the
    # source first.
    output$problems <- renderUI({
      x <- req(state())
      problems <- rbind(
        if (x$info$stage == "error") {
          data.frame(Check = "Build failed", Problems = x$info$message)
        },
        x$spec[c("Check", "Problems")]
      )
      rules <- x$coherence
      if (!NROW(problems) && !NROW(rules)) {
        return(NULL)
      }
      tagList(
        if (NROW(problems)) {
          div(
            class = "alert alert-danger",
            strong("Left out of the generated data until this is fixed."),
            tags$ul(Map(
              function(check, text) tags$li(strong(check), text),
              problems$Check,
              problems$Problems
            ))
          )
        },
        if (NROW(rules)) {
          div(
            class = "alert alert-danger",
            strong(paste0(plural(nrow(rules), "SDTM rule"), " the source keeps, broken in the generated data")),
            lapply(seq_len(nrow(rules)), function(i) broken_rule(rules[i, ]))
          )
        }
      )
    })

    study <- reactive({
      tick()
      b$study
    })
    share <- reactive({
      tick()
      b$share
    })
    changed <- merge_events(
      rules_server("rules", current, study, share),
      graph_server("graph", current, study),
      variables_server("variables", current, study)
    )
    observeEvent(changed(), change(changed()))

    # ---- Compare --------------------------------------------------------------------------

    generated <- changes(function() {
      tick()
      b$generated
    })
    name <- reactive({
      name <- req(current())$name
      req(name %in% names(generated()$data))
      name
    })
    output$compare <- renderUI({
      ds <- req(current())
      if (!ds$name %in% names(generated()$data)) {
        return(card(empty_state(
          "Not generated yet",
          "The comparison appears once the study has been sampled with this dataset in it."
        )))
      }
      records <- nrow(generated()$data[[ds$name]])
      navset_card_underline(
        full_screen = TRUE,
        title = span(
          "Source and synthetic",
          muted(sprintf("%s synthetic records", big(records)))
        ),
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
          uiOutput(ns("trajectory_controls")),
          plotOutput(ns("trajectory_plot"), height = "380px")
        ),
        nav_panel(
          "Event rates",
          plotOutput(ns("event_plot"), height = "360px")
        ),
        nav_panel("Records", DTOutput(ns("records"), fill = FALSE))
      )
    })

    output$records <- renderDT({
      x <- utils::head(generated()$data[[name()]], 500)
      for (v in names(x)) {
        if (inherits(x[[v]], "Date")) x[[v]] <- format(x[[v]])
      }
      datatable(
        x,
        rownames = FALSE,
        selection = "none",
        class = "compact hover nowrap",
        options = list(dom = "tip", pageLength = 15, scrollX = TRUE)
      )
    })

    marginals <- reactive({
      x <- compare_marginals(
        b$study,
        b$study$source$data[[name()]],
        generated()$data[[name()]],
        name()
      )
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
        variable <- marginals()$Variable[req(
          input$marginal_table_rows_selected
        )]
        marginal_plot(
          b$study$source$data[[name()]][[variable]],
          generated()$data[[name()]][[variable]],
          variable
        )
      },
      res = 96
    )

    output$trajectory_controls <- renderUI({
      ds <- b$study$datasets[[name()]]
      sampled <- vapply(
        ds$variables,
        function(v) identical(node_type(ds, v), "sampled"),
        TRUE
      )
      numeric <- ds$variables[ds$kinds[ds$variables] == "numeric" & sampled]
      if (is.null(ds$structure$visit) || !length(numeric)) {
        return(hint(
          "Trajectories need a dataset with visits and a numeric result."
        ))
      }
      test <- ds$structure$test
      tests <- if (!is.null(test)) {
        sort(unique(format_value(generated()$data[[name()]][[test]])))
      }
      layout_columns(
        selectInput(ns("trajectory_variable"), "Result", numeric),
        if (length(tests)) selectInput(ns("trajectory_test"), "Test", tests)
      )
    })
    output$trajectory_plot <- renderPlot(
      {
        variable <- req(input$trajectory_variable)
        req(variable %in% b$study$datasets[[name()]]$variables)
        test <- input$trajectory_test %||% NA
        arm <- arm_variable(b$study$datasets$DM)
        trajectory <- function(data) {
          trajectory_data(b$study, data, name(), variable, test, arm)
        }
        source <- trajectory(b$study$source$data)
        synthetic <- trajectory(generated()$data)
        req(source, synthetic)
        d <- rbind(
          cbind(source, Data = "Source"),
          cbind(synthetic, Data = "Synthetic")
        )
        d$visit <- factor(
          format(d$visit),
          levels = format(sort(unique(d$visit)))
        )
        ggplot(
          d,
          aes(
            visit,
            mean,
            colour = arm,
            linetype = Data,
            group = interaction(arm, Data)
          )
        ) +
          geom_line(linewidth = 0.8) +
          geom_point(size = 1.6) +
          scale_linetype_manual(
            values = c(Source = "dashed", Synthetic = "solid")
          ) +
          labs(
            x = "Visit",
            y = paste("Mean", variable),
            colour = NULL,
            linetype = NULL,
            title = if (!is.na(test)) test
          ) +
          plot_theme()
      },
      res = 96
    )

    output$event_plot <- renderPlot(
      {
        req(name() != "DM")
        arm <- arm_variable(b$study$datasets$DM)
        d <- rbind(
          cbind(
            event_rate_data(b$study$source$data, name(), arm),
            Data = "Source"
          ),
          cbind(
            event_rate_data(generated()$data, name(), arm),
            Data = "Synthetic"
          )
        )
        ggplot(d, aes(arm, rate, fill = Data)) +
          geom_col(position = position_dodge(width = 0.75), width = 0.7) +
          scale_fill_manual(values = PLOT_COLORS) +
          labs(
            x = NULL,
            y = paste(name(), "records per subject"),
            fill = NULL
          ) +
          plot_theme()
      },
      res = 96
    )
  })
}

# ---- Plots --------------------------------------------------------------------------------

# One variable in source and synthetic data: densities for many numeric values, otherwise
# the shares of the most common values.
marginal_plot <- function(source, synthetic, variable) {
  d <- rbind(
    data.frame(Data = "Source", value = source),
    data.frame(Data = "Synthetic", value = synthetic)
  )
  observed <- d[!is.na(d$value), ]
  continuous <- is.numeric(d$value) || inherits(d$value, "Date")
  if (continuous && length(unique(observed$value)) > MAX_DISCRETE_VALUES) {
    return(
      ggplot(observed, aes(value, fill = Data, colour = Data)) +
        geom_density(alpha = 0.25) +
        scale_fill_manual(values = PLOT_COLORS) +
        scale_colour_manual(values = PLOT_COLORS) +
        labs(x = variable, y = NULL, fill = NULL, colour = NULL) +
        plot_theme()
    )
  }
  counts <- as.data.frame(
    table(Data = d$Data, value = level_string(d$value)),
    stringsAsFactors = FALSE
  )
  counts$share <- counts$Freq / ave(counts$Freq, counts$Data, FUN = sum)
  totals <- sort(tapply(counts$share, counts$value, sum), decreasing = TRUE)
  top <- utils::head(names(totals), 12)
  counts <- counts[counts$value %in% top, ]
  counts$value <- factor(counts$value, rev(top))
  ggplot(counts, aes(share, value, fill = Data)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    scale_fill_manual(values = PLOT_COLORS) +
    scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(x = NULL, y = NULL, fill = NULL, title = variable) +
    plot_theme()
}

plot_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      text = element_text(colour = "#1d2327")
    )
}

# One broken SDTM rule: how many generated records break it, what to do and the first records.
broken_rule <- function(r) {
  category <- CHECK_CATEGORIES[[r$Category]]
  div(
    class = "broken-rule",
    div(class = "broken-rule-head", strong(r$Check)),
    p(
      sprintf(
        "%s of %s generated records; the source keeps it in all %s of its records. %s rule: %s.",
        big(r$Failed),
        big(r$Checked),
        big(r$SourceChecked),
        category$label,
        category$about
      ),
      category$fix
    ),
    hint("Generated: ", r$Problems)
  )
}
