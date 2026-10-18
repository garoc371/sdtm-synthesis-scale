# The node clicked in the graph: its formula and, for a sampled, identifier or blank node,
# its type, predictors and model.

node_panel_ui <- function(id) {
  uiOutput(NS(id, "node"))
}

# `node`: the name of the node clicked, or NULL. Returns the dataset with the node changed, as
# an event.
node_panel_server <- function(id, node, dataset, study) {
  moduleServer(id, function(input, output, session) {
    # Redrawn with the dataset, not with every build of another one, to keep what is entered.
    output$node <- renderUI({
      ds <- req(dataset())
      v <- node()
      if (is.null(v) || !v %in% ds$variables) {
        return(empty_state(
          "No node selected",
          "Click a node in the graph, or find it by name, to see its formula."
        ))
      }
      node_form(session$ns, ds, v, isolate(study()))
    })

    applied <- new_event()
    observeEvent(
      input$apply,
      guarded({
        ds <- dataset()
        v <- req(node())
        n <- ds$nodes[[v]]
        if (identical(node_type(ds, v), "sampled")) {
          n$predictors <- as.character(input$predictors)
          n$model <- input$model %||% n$model
          n$prev <- isTRUE(input$prev)
          n$vary <- isTRUE(input$vary)
        }
        n$type <- input$type
        ds$nodes[[v]] <- n
        order <- tryCatch(generation_order(ds), error = function(e) e)
        if (inherits(order, "error")) {
          return(notify(paste("That would create a cycle:", conditionMessage(order)), "error"))
        }
        if (n$type == "sampled") {
          ds$order <- union(ds$order, v)
        }
        ds$nodes[[v]]$manual <- TRUE
        applied$send(ds)
      })
    )
    applied$value
  })
}

# The node's formula, and the inputs that change it where it can be changed here.
node_form <- function(ns, ds, v, study) {
  type <- node_type(ds, v)
  header <- tagList(
    div(class = "node-title", h5(v), type_badge(type)),
    muted(ds$labels[[v]], "·", ds$levels[[v]], "level"),
    div(class = "formula", node_formula(ds, v, study$fits[[ds$name]]$nodes[[v]]))
  )
  if (!type %in% c("sampled", "identifier", "blank")) {
    text <- if (type == "rule") {
      "Calculated variables are edited in the Rules tab."
    } else {
      "Keys and timing are sampled with the spine: the visits attended and their days, then each visit's records from a source subject of the same type, dated on those days."
    }
    return(tagList(header, hint(text)))
  }
  tagList(
    header,
    selectInput(
      ns("type"),
      "Type",
      c(Sampled = "sampled", Identifier = "identifier", `Not generated` = "blank"),
      type
    ),
    if (type == "sampled") sampled_inputs(ns, ds, v, study),
    actionButton(ns("apply"), "Apply", class = "btn-sm btn-primary")
  )
}

# A sampled node's predictors and model. Possible predictors: generated nodes and inputs that
# do not already depend on it.
sampled_inputs <- function(ns, ds, v, study) {
  n <- ds$nodes[[v]]
  choices <- setdiff(
    c(generated_variables(ds), input_choices(study, ds$name)),
    c(v, "USUBJID", descendants(ds, v))
  )
  tagList(
    selectizeInput(
      ns("predictors"),
      "Predictors",
      setNames(choices, display_name(choices)),
      n$predictors,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    selectInput(ns("model"), "Model", setNames(names(NODE_MODELS), NODE_MODELS), n$model),
    if (!is.null(ds$structure$visit) && ds$levels[[v]] == "record") {
      checkboxInput(ns("prev"), "Depends on the previous value", isTRUE(n$prev))
    },
    if (!is.na(arm_variable(study$datasets$DM))) {
      checkboxInput(ns("vary"), "Draw arm effects from their uncertainty", isTRUE(n$vary))
    }
  )
}
