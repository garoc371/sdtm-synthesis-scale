# The ledger of decisions the policy made that change generated records. Clicking a row
# opens its dataset.

decisions_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "decisions",
    full_screen = TRUE,
    card_header(uiOutput(ns("title"), inline = TRUE)),
    hint(
      "Decisions that make generated records differ from what the source would give, by how many",
      "source records each changes: a rule that some source records break (generation follows the",
      "rule), a rejected rule, a column not generated. Rules that every source record keeps, such as",
      "a test name set by its code, are on each dataset's Rules tab. Open a dataset to change any of these."
    ),
    DTOutput(ns("table"), fill = FALSE)
  )
}

# `ledger`: a reactive `built_decisions()`. Returns the dataset of the row clicked, as an event.
decisions_server <- function(id, ledger) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderUI({
      x <- ledger()
      detail <- if (!is.null(x)) {
        sprintf(
          "%d change generated records, in %d datasets · %d set by hand · %d more give every source record its value",
          nrow(x),
          length(unique(x$dataset)),
          sum(x$by == "hand"),
          attr(x, "unchanged")
        )
      }
      tagList("Decisions", muted(detail))
    })
    # Drawn once; later builds replace its rows, so that it keeps its page and filter.
    output$table <- renderDT({
      datatable(
        decision_table(isolate(ledger())),
        rownames = FALSE,
        selection = "single",
        escape = FALSE,
        class = "compact hover",
        options = list(
          dom = "ftip",
          pageLength = 12,
          scrollX = TRUE,
          order = list(list(5, "desc")),
          language = list(
            search = "",
            searchPlaceholder = "Filter",
            emptyTable = "No decisions yet."
          )
        )
      )
    })
    table <- dataTableProxy("table")
    observeEvent(ledger(), ignoreNULL = FALSE, {
      replaceData(
        table,
        decision_table(ledger()),
        rownames = FALSE,
        resetPaging = FALSE
      )
    })
    eventReactive(input$table_rows_selected, {
      ledger()$dataset[input$table_rows_selected]
    })
  })
}

decision_table <- function(x) {
  if (!NROW(x)) {
    x <- data.frame(
      dataset = character(),
      variable = character(),
      decision = character(),
      detail = character(),
      share = numeric(),
      records = numeric(),
      by = character()
    )
  }
  decision_color <- function(d) {
    switch(
      d,
      Calculated = "warning",
      `Rule rejected` = NA,
      `New identifier` = "info",
      "secondary"
    )
  }
  html <- function(x, f) {
    vapply(x, function(v) as.character(f(v)), "", USE.NAMES = FALSE)
  }
  data.frame(
    Dataset = x$dataset,
    Variable = x$variable,
    Decision = html(x$decision, function(d) badge(decision_color(d), d)),
    Detail = html(x$detail, function(d) span(class = "truncate", d)),
    Reproduced = vapply(x$share, share_bar, "", USE.NAMES = FALSE),
    Records = x$records,
    By = x$by,
    stringsAsFactors = FALSE
  )
}
