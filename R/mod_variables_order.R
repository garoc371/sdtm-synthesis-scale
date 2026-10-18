# The order in which the dataset's variables are generated, with their fitted formulas.

order_ui <- function(id) {
  card(
    full_screen = TRUE,
    card_header("Generation order", muted("top to bottom, with fitted formulas")),
    DTOutput(NS(id, "order"), fill = FALSE)
  )
}

order_server <- function(id, dataset, study) {
  moduleServer(id, function(input, output, session) {
    output$order <- renderDT({
      ds <- req(dataset())
      order_table(ds, study()$fits[[ds$name]])
    })
  })
}

# The records first, then each variable in order.
order_table <- function(ds, fits) {
  order <- tryCatch(generation_order(ds), error = function(e) character())
  records <- paste(c(record_structure_label(ds), fits$structure$description), collapse = " · ")
  table <- data.frame(
    Step = seq_along(c("records", order)) - 1L,
    Variable = c("records", order),
    Type = c(
      as.character(badge("secondary", "Structure")),
      vapply(order, function(v) as.character(type_badge(node_type(ds, v))), "")
    ),
    Formula = htmltools::htmlEscape(c(
      records,
      vapply(order, function(v) node_formula(ds, v, fits$nodes[[v]]), "")
    ))
  )
  compact_table(table, escape = FALSE, height = "420px")
}
