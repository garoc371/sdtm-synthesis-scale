# The selected rule: its error, how much of each test it reproduces, and its lookup or
# conversion table, which can be edited.

rule_detail_ui <- function(id) {
  uiOutput(NS(id, "detail"))
}

# `rule`: the selected rule. Returns the dataset with that rule's table edited, as an event.
rule_detail_server <- function(id, rule, dataset, study) {
  moduleServer(id, function(input, output, session) {
    output$detail <- renderUI(rule_detail(rule(), session$ns))
    output$by_test <- renderDT(by_test_table(rule()))
    output$lookup <- renderDT(lookup_table(req(rule()$params$table)))

    edited <- new_event()
    observeEvent(
      input$lookup_cell_edit,
      guarded({
        rule <- edit_table(rule(), input$lookup_cell_edit)
        ds <- dataset()
        ds$rules[[rule$target]] <- evaluate_rule(rule, dataset_frame(study(), ds$name))
        edited$send(ds)
      })
    )
    edited$value
  })
}

rule_detail <- function(rule, ns) {
  by_test <- !is.null(rule$by_test) && nrow(rule$by_test) > 1
  table <- rule$op %in% c("lookup", "convert") && !is.null(rule$params$table)
  req(by_test || table || !is.null(rule$error))
  div(
    class = "rule-detail",
    if (!is.null(rule$error)) div(class = "alert alert-danger", rule$error),
    layout_columns(
      col_widths = if (by_test && table) c(5, 7) else 12,
      if (by_test) {
        div(h6("Reproduced per test"), DTOutput(ns("by_test"), fill = FALSE))
      },
      if (table) {
        div(
          h6(if (rule$op == "lookup") "Lookup table" else "Conversion per group"),
          hint("Double-click a cell to edit it. Edited tables are kept as entered."),
          DTOutput(ns("lookup"), fill = FALSE)
        )
      }
    )
  )
}

by_test_table <- function(rule) {
  x <- rule$by_test[order(rule$by_test$share), ]
  x$share <- share_text(x$share)
  names(x) <- c("Test", "Checked", "Reproduced")
  if (rule$op == "group") {
    x$Calculation <- vapply(rule$params$derivations, derivation_label, "")[x$Test]
  }
  compact_table(x, height = "240px")
}

lookup_table <- function(x) {
  x[] <- lapply(x, function(v) if (inherits(v, "Date")) format(v) else v)
  datatable(
    x,
    rownames = FALSE,
    selection = "none",
    class = "compact",
    editable = "cell",
    options = list(
      dom = if (nrow(x) > 12) "tp" else "t",
      pageLength = 12,
      ordering = FALSE
    )
  )
}

# The rule with one cell of its table changed by hand, which keeps the table as entered.
edit_table <- function(rule, edit) {
  column <- names(rule$params$table)[edit$col + 1]
  values <- rule$params$table[[column]]
  rule$params$table[[column]][edit$row] <- parse_like(edit$value, values)
  rule$params$edited <- TRUE
  rule$manual <- TRUE
  rule
}
