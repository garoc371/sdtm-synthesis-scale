# The generation settings (rule policy, subjects, seed), the visit schedule, and downloads
# of the synthetic data and its specification.

generation_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Generation"),
    sliderInput(
      ns("policy"),
      "Minimum match rate for a rule",
      min = 90,
      max = 100,
      value = 100 * POLICY_SHARE,
      step = 1,
      post = "%",
      ticks = FALSE
    ),
    hint(
      "A rule computes a column from other columns, such as a test code from its name. It is used",
      "when it gives the source value in at least this share of the records it can be checked on",
      "(the Reproduced column); otherwise the column is sampled. Rules under 90% are never found.",
      "Applies to every dataset; changes made by hand are kept."
    ),
    layout_columns(
      col_widths = c(6, 6),
      numericInput(ns("n_subjects"), "Subjects", 200, min = 1, step = 50),
      numericInput(ns("seed"), "Seed", 1, min = 1, step = 1)
    ),
    uiOutput(ns("schedule")),
    div(
      class = "button-row",
      downloadButton(
        ns("download_data"),
        "Synthetic data",
        class = "btn-sm btn-primary"
      ),
      downloadButton(
        ns("download_bundle"),
        "Specification",
        class = "btn-sm btn-outline-secondary"
      )
    ),
    hint(
      "The specification holds graphs, fitted parameters, the visit schedule and every source",
      "subject's shifted records of keys, visits and dates: ",
      code("generate_study(readRDS(file), n, seed)"),
      "."
    )
  )
}

# `schedule`: the study's visit schedule and design datasets, NULL before loading.
# `outputs`: the study, what was generated and its seed. `bundle`: a reactive
# `build_bundle()`. Returns the policy's share and the sample (subjects, seed), each
# settled for half a second.
generation_server <- function(id, schedule, outputs, bundle) {
  moduleServer(id, function(input, output, session) {
    output$schedule <- renderUI({
      x <- req(schedule())
      schedule_summary(x$schedule, x$design)
    })

    output$download_data <- downloadHandler(
      filename = function() sprintf("synthetic-sdtm-seed%s.zip", outputs()$seed),
      content = function(file) {
        x <- outputs()
        shiny::validate(need(x$generated, "Nothing generated yet."))
        directory <- tempfile("synthetic-")
        files <- write_outputs(x$study, x$generated, directory)
        zip::zip(file, basename(files), root = directory)
      }
    )
    output$download_bundle <- downloadHandler(
      filename = function() "synthetic-sdtm-specification.rds",
      # Dated when downloaded.
      content = function(file) saveRDS(modifyList(bundle(), list(created = Sys.time())), file)
    )

    policy <- debounce(reactive(input$policy), 500)
    sample <- debounce(reactive(c(input$n_subjects, input$seed)), 500)
    list(
      share = reactive(policy() / 100),
      sample = reactive({
        x <- sample()
        req(length(x) == 2, all(is.finite(x)))
        c(n = max(1, round(x[1])), seed = x[2])
      })
    )
  })
}

# How visit days are drawn: the window for day 1, and per scheduled visit its planned day and
# the middle 90% of the source's deviations from it.
schedule_summary <- function(schedule, design) {
  if (is.null(schedule)) {
    return(hint("No planned visit days: there is no SV with visit numbers and RFSTDTC. Visits fall as far apart as the source's did."))
  }
  p <- schedule$planned
  p <- p[as.character(p$visit) %in% names(schedule$deviation), ]
  range <- vapply(as.character(p$visit), function(v) {
    q <- schedule$deviation[[v]]
    sprintf("%+g to %+g", round(q[6]), round(q[96]))
  }, "")
  tags$details(
    class = "checked-rules",
    tags$summary(sprintf(
      "Visit days: %d scheduled visits, all visits between %s and %s",
      nrow(p),
      format(schedule$calendar[1]),
      format(schedule$calendar[2])
    )),
    hint(
      sprintf(
        "Each visit falls on its planned day (%s) plus a deviation drawn from the source's at that visit.",
        if ("TV" %in% design) "TV's VISITDY" else "the source's median day, as there is no TV"
      ),
      "Other dates follow the visits: one on a visit date stays on it, one between visits keeps its",
      "distance to the nearer one. Day 1 (RFSTDTC) falls on a date that keeps the subject's visits",
      "between the source's first and last SV date. Study days are then calculated from the dates."
    ),
    tags$table(
      class = "table table-sm",
      tags$thead(tags$tr(tags$th("Visit"), tags$th("Planned day"), tags$th("Middle 90% of deviations"))),
      tags$tbody(lapply(seq_len(nrow(p)), function(i) {
        tags$tr(
          tags$td(
            if (!is.na(p$name[i])) paste0(p$name[i], " (", format_value(p$visit[i]), ")") else format_value(p$visit[i]),
            if (!is.na(p$arm[i])) paste(",", p$arm[i])
          ),
          tags$td(p$planned[i]),
          tags$td(range[i], " days")
        )
      }))
    )
  )
}
