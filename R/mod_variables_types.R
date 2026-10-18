# The variables not calculated, with buttons that set the type of those selected.

types_ui <- function(id) {
  ns <- NS(id)
  button <- function(id, label) {
    actionButton(ns(id), label, class = "btn-sm btn-outline-secondary", disabled = TRUE)
  }
  card(
    card_header("Variables not calculated", uiOutput(ns("remaining"), inline = TRUE)),
    hint("Each starts with a suggested type. Select rows to change it."),
    div(
      class = "button-row",
      button("set_sampled", "Sampled"),
      button("set_identifier", "Identifier"),
      button("set_blank", "Not generated")
    ),
    DTOutput(ns("variables"), fill = FALSE)
  )
}

# Returns the dataset with the types set, as an event.
types_server <- function(id, dataset, study) {
  moduleServer(id, function(input, output, session) {
    output$remaining <- renderUI({
      badge("secondary", length(remaining_variables(req(dataset()))))
    })
    # Redrawn with the dataset, not with every build of another one, to keep the selection.
    output$variables <- renderDT({
      ds <- req(dataset())
      variable_table(ds, isolate(study())$source$data[[ds$name]])
    })

    selected <- reactive(remaining_variables(dataset())[input$variables_rows_selected])
    observe({
      for (id in c("set_sampled", "set_identifier", "set_blank")) {
        updateActionButton(session, id, disabled = !length(selected()))
      }
    })

    # A newly sampled variable joins the end of the ordering, with no predictors yet.
    typed <- new_event()
    set_type <- function(type) {
      guarded({
        ds <- dataset()
        v <- req(selected())
        ds$nodes[v] <- lapply(ds$nodes[v], modifyList, list(type = type, manual = TRUE))
        if (type == "sampled") {
          ds$order <- union(ds$order, v)
        }
        typed$send(ds)
      })
    }
    observeEvent(input$set_sampled, set_type("sampled"))
    observeEvent(input$set_identifier, set_type("identifier"))
    observeEvent(input$set_blank, set_type("blank"))
    typed$value
  })
}

# The variables not calculated: their type, who set it, their level and source values.
variable_table <- function(ds, x) {
  remaining <- remaining_variables(ds)
  labels <- unname(ds$labels[remaining] %||% rep("", length(remaining)))
  labels[is.na(labels)] <- ""
  html <- function(f) vapply(remaining, function(v) as.character(f(v)), "", USE.NAMES = FALSE)
  table <- data.frame(
    Variable = mapply(
      function(v, label) as.character(span(title = label, v)),
      remaining,
      labels,
      USE.NAMES = FALSE
    ),
    Type = html(function(v) type_badge(node_type(ds, v))),
    `Set by` = ifelse(vapply(unname(ds$nodes[remaining]), is_manual, TRUE), "hand", "starting type"),
    Level = unname(ds$levels[remaining]),
    Values = html(function(v) span(class = "truncate", summarise_values(x[[v]]))),
    check.names = FALSE
  )
  compact_table(
    table,
    selection = "multiple",
    escape = FALSE,
    height = "420px",
    empty = "Every variable is a key or calculated."
  )
}
