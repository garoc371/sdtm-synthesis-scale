# A dataset's graph and variables: the node panel edits one sampled node; the variables
# table changes types; the settings change inputs and order. Any
# change makes the dataset kept as set by hand, rebuilt alone.

graph_tab_ui <- function() bslib::card(full_screen = TRUE, class = "graph-card",
  bslib::card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
    uiOutput("graph_title", inline = TRUE),
    actionButton("rebuild_graph", "Rebuild starting graph", class = "btn-sm btn-outline-secondary", icon = icon("rotate"))),
  bslib::layout_sidebar(border = FALSE, fillable = TRUE,
    sidebar = bslib::sidebar(id = "node_sidebar", position = "right", width = 340, open = "closed", class = "border-start", bg = "#fbfbfa",
      uiOutput("node_panel")),
    uiOutput("graph", fill = TRUE)))

variables_tab_ui <- function() tagList(
  bslib::layout_columns(col_widths = bslib::breakpoints(md = 12, xl = c(7, 5)),
    bslib::card(
      bslib::card_header(class = "d-flex justify-content-between align-items-center",
        "Variables not calculated", uiOutput("remaining_count", inline = TRUE)),
      hint("Each starts with a suggested type. Select rows to change it."),
      div(class = "btn-toolbar gap-2 mb-2",
        actionButton("set_sampled", "Sampled", class = "btn-sm btn-outline-secondary", disabled = TRUE),
        actionButton("set_identifier", "Identifier", class = "btn-sm btn-outline-secondary", disabled = TRUE),
        actionButton("set_blank", "Not generated", class = "btn-sm btn-outline-secondary", disabled = TRUE)),
      DT::DTOutput("variable_table", fill = FALSE)),
    bslib::card(bslib::card_header("Dataset settings"),
      uiOutput("dataset_settings"))),
  bslib::card(full_screen = TRUE,
    bslib::card_header("Generation order", span(class = "text-body-secondary small fw-normal ms-2", "top to bottom, with fitted formulas")),
    DT::DTOutput("formula_table", fill = FALSE)))

graph_tab_server <- function(input, output, session, b, current, change, notify) {
  node <- reactiveVal(NULL)
  observeEvent(current()$name, node(NULL), ignoreInit = TRUE)

  output$remaining_count <- renderUI(badge("secondary", length(remaining_variables(req(current())))))

  output$variable_table <- DT::renderDT({
    ds <- req(current())
    remaining <- remaining_variables(ds)
    if (!length(remaining)) return(compact_table(data.frame(Message = "Every variable is a key or calculated.")))
    x <- b$study$source$data[[ds$name]]
    types <- vapply(remaining, node_type, "", ds = ds)
    labels <- unname(ds$labels[remaining] %||% rep("", length(remaining)))
    labels[is.na(labels)] <- ""
    table <- data.frame(
      Variable = sprintf('<span title="%s">%s</span>', htmltools::htmlEscape(labels, TRUE), remaining),
      Type = vapply(types, function(type) as.character(type_badge(type)), ""),
      `Set by` = ifelse(vapply(remaining, function(v) isTRUE(ds$nodes[[v]]$manual), TRUE), "hand", "starting type"),
      Level = unname(ds$levels[remaining]),
      Values = vapply(remaining, function(v) sprintf('<span class="d-inline-block text-truncate text-body-secondary" style="max-width: 280px">%s</span>',
        htmltools::htmlEscape(summarise_values(x[[v]]))), ""),
      stringsAsFactors = FALSE, check.names = FALSE)
    compact_table(table, selection = "multiple", escape = FALSE, height = "420px")
  })
  selected_variables <- reactive(remaining_variables(current())[input$variable_table_rows_selected])
  observe(for (id in c("set_sampled", "set_identifier", "set_blank"))
    updateActionButton(session, id, disabled = !length(selected_variables())))

  # A newly sampled variable joins the end of the ordering, with no predictors yet.
  set_type <- function(type) guarded(notify, {
    ds <- current()
    variables <- req(selected_variables())
    for (v in variables) {
      ds$nodes[[v]]$type <- type
      ds$nodes[[v]]$manual <- TRUE
    }
    if (type == "sampled") ds$order <- union(ds$order, variables)
    change(ds)
  })
  observeEvent(input$set_sampled, set_type("sampled"))
  observeEvent(input$set_identifier, set_type("identifier"))
  observeEvent(input$set_blank, set_type("blank"))
  observeEvent(input$rebuild_graph, guarded(notify, {
    ds <- current()
    change(starting_graph(b$study, ds))
    notify(paste("Rebuilt the starting graph for", ds$name))
  }))

  output$formula_table <- DT::renderDT({
    ds <- req(current())
    fits <- b$study$fits[[ds$name]]
    order <- tryCatch(generation_order(ds), error = function(e) character())
    structure <- data.frame(Step = 0L, Variable = "records",
      Type = as.character(badge("secondary", "Structure")),
      Formula = htmltools::htmlEscape(paste0(record_structure_label(ds),
        if (!is.null(fits$structure$description)) paste0(" · ", fits$structure$description) else "")),
      stringsAsFactors = FALSE)
    if (!length(order)) return(compact_table(structure, escape = FALSE))
    nodes <- data.frame(Step = seq_along(order), Variable = order,
      Type = vapply(order, function(v) as.character(type_badge(node_type(ds, v))), ""),
      Formula = vapply(order, function(v) htmltools::htmlEscape(node_formula(ds, v, fits$nodes[[v]])), ""),
      stringsAsFactors = FALSE)
    compact_table(rbind(structure, nodes), escape = FALSE, height = "420px")
  })

  # ---- Dataset settings -------------------------------------------------------------

  output$dataset_settings <- renderUI({
    ds <- req(current())
    study <- b$study
    s <- ds$structure
    choices <- input_choices(study, ds$name)
    tagList(
      if (ds$name != "DM") checkboxInput("ds_include", "Generate this dataset", ds$include),
      div(class = "d-flex gap-2 small", strong("Records"), span(record_structure_label(ds))),
      if (ds$name != "DM") selectizeInput("ds_inputs", "Inputs from DM, earlier datasets and the spine", setNames(choices, display_name(choices)),
        ds$inputs, multiple = TRUE, options = list(plugins = list("remove_button"))),
      selectizeInput("ds_order", "Order of sampled variables", ds$order, ds$order, multiple = TRUE,
        options = list(plugins = list("drag_drop", "remove_button"))),
      hint("Drag to reorder and save: each sampled variable is then predicted by the informative ones before it."),
      actionButton("save_dataset", "Save settings", class = "btn-sm btn-primary"))
  })
  observeEvent(input$save_dataset, guarded(notify, {
    ds <- current()
    if (ds$name != "DM") {
      ds$include <- isTRUE(input$ds_include)
      ds$inputs <- as.character(input$ds_inputs)
    }
    order <- as.character(input$ds_order)
    if (!identical(order, ds$order) && length(order)) ds <- starting_graph(b$study, ds, order)
    change(ds)
    notify("Dataset settings saved.")
  }))

  # ---- Graph ------------------------------------------------------------------------

  output$graph_title <- renderUI({
    ds <- req(current())
    nodes <- generated_variables(ds)
    span("Graph",
      span(class = "text-body-secondary small fw-normal ms-2", sprintf("%d generated, %d sampled", length(nodes),
        sum(vapply(nodes, function(v) identical(node_type(ds, v), "sampled"), TRUE)))))
  })
  output$graph <- renderUI({
    ds <- req(current())
    graph_view("graph_node", ds, isolate(node()))
  })
  observeEvent(input$graph_node, node(input$graph_node))
  # The node panel is open while a node is selected.
  observe(bslib::toggle_sidebar("node_sidebar", open = isTRUE(node() %in% current()$variables)))

  output$node_panel <- renderUI({
    ds <- req(current())
    v <- node()
    if (is.null(v) || !v %in% ds$variables) return(empty_state("No node selected",
      "Click a node in the graph, or find it by name, to see its formula and change its settings."))
    type <- node_type(ds, v)
    fit <- b$study$fits[[ds$name]]$nodes[[v]]
    header <- div(class = "d-flex flex-wrap align-items-center gap-2 mb-2", h5(class = "m-0 fw-bold", v), type_badge(type),
      span(class = "text-body-secondary", ds$labels[[v]] %||% ""),
      span(class = "text-body-secondary small", paste(ds$levels[[v]], "level")))
    formula <- div(class = "font-monospace small text-break bg-body-tertiary border rounded px-2 py-2 mb-3", node_formula(ds, v, fit))
    if (!identical(type, "sampled") && !identical(type, "identifier") && !identical(type, "blank"))
      return(div(class = "node-panel", header, formula,
        hint(if (identical(type, "rule")) "Calculated variables are edited in the Rules tab." else "Keys and timing are copied from the donor's spine.")))
    node <- ds$nodes[[v]]
    # Possible parents: generated nodes and inputs that do not already depend on this node.
    choices <- setdiff(c(generated_variables(ds), input_choices(b$study, ds$name)), c(v, "USUBJID"))
    choices <- setdiff(choices, descendants(ds, v))
    arm <- arm_variable(b$study$datasets$DM)
    editable <- identical(type, "sampled")
    div(class = "node-panel", header, formula,
      bslib::layout_columns(col_widths = 12,
        selectInput("node_type", "Type", c(Sampled = "sampled", Identifier = "identifier", `Not generated` = "blank"), type),
        if (editable) selectizeInput("node_predictors", "Predictors", setNames(choices, display_name(choices)),
          node$predictors, multiple = TRUE, options = list(plugins = list("remove_button")))),
      if (editable) bslib::layout_columns(col_widths = 12,
        selectInput("node_model", "Model", setNames(names(NODE_MODELS), NODE_MODELS), node$model),
        if (!is.null(ds$structure$visit) && ds$levels[[v]] == "record")
          checkboxInput("node_prev", "Depends on the previous value", isTRUE(node$prev)),
        if (!is.na(arm)) checkboxInput("node_vary", "Draw arm effects from their uncertainty", isTRUE(node$vary))),
      actionButton("apply_node", "Apply", class = "btn-sm btn-primary"))
  })
  observeEvent(input$apply_node, guarded(notify, {
    ds <- current()
    v <- req(node())
    n <- ds$nodes[[v]]
    editable <- identical(node_type(ds, v), "sampled")
    n$type <- input$node_type
    if (editable) {
      n$predictors <- as.character(input$node_predictors)
      if (!is.null(input$node_model)) n$model <- input$node_model
      n$prev <- isTRUE(input$node_prev)
      n$vary <- isTRUE(input$node_vary)
    }
    ds$nodes[[v]] <- n
    order <- tryCatch(generation_order(ds), error = function(e) e)
    if (inherits(order, "error")) return(notify(paste("That would create a cycle:", conditionMessage(order)), "error"))
    if (n$type == "sampled" && !v %in% ds$order) ds$order <- c(ds$order, v)
    n$manual <- TRUE
    ds$nodes[[v]] <- n
    change(ds)
  }))
}
