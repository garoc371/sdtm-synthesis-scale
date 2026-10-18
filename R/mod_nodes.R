# A dataset's graph and variables: the node panel edits one sampled node; the variables
# table changes types; the settings change inputs and order. Any change makes the dataset
# kept as set by hand, rebuilt alone.

graph_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    class = "graph-card",
    card_header(
      uiOutput(ns("title"), inline = TRUE),
      actionButton(
        ns("rebuild"),
        "Rebuild starting graph",
        class = "btn-sm btn-outline-secondary",
        icon = icon("rotate")
      )
    ),
    layout_sidebar(
      border = FALSE,
      sidebar = sidebar(
        id = ns("panel"),
        position = "right",
        width = 340,
        open = "closed",
        uiOutput(ns("node"))
      ),
      uiOutput(ns("graph"), fill = TRUE)
    )
  )
}

variables_ui <- function(id) {
  ns <- NS(id)
  button <- function(id, label) {
    actionButton(
      ns(id),
      label,
      class = "btn-sm btn-outline-secondary",
      disabled = TRUE
    )
  }
  tagList(
    layout_columns(
      col_widths = breakpoints(md = 12, xl = c(7, 5)),
      card(
        card_header(
          "Variables not calculated",
          uiOutput(ns("remaining"), inline = TRUE)
        ),
        hint("Each starts with a suggested type. Select rows to change it."),
        div(
          class = "button-row",
          button("set_sampled", "Sampled"),
          button("set_identifier", "Identifier"),
          button("set_blank", "Not generated")
        ),
        DTOutput(ns("variables"), fill = FALSE)
      ),
      card(card_header("Dataset settings"), uiOutput(ns("settings")))
    ),
    card(
      full_screen = TRUE,
      card_header(
        "Generation order",
        muted("top to bottom, with fitted formulas")
      ),
      DTOutput(ns("order"), fill = FALSE)
    )
  )
}

nodes_server <- function(id, b, current, change, notify) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    node <- reactiveVal(NULL)
    observeEvent(current()$name, node(NULL), ignoreInit = TRUE)

    # ---- Variables -----------------------------------------------------------------------

    output$remaining <- renderUI(badge(
      "secondary",
      length(remaining_variables(req(current())))
    ))
    output$variables <- renderDT({
      ds <- req(current())
      remaining <- remaining_variables(ds)
      x <- b$study$source$data[[ds$name]]
      labels <- unname(ds$labels[remaining] %||% rep("", length(remaining)))
      labels[is.na(labels)] <- ""
      table <- data.frame(
        Variable = vapply(
          seq_along(remaining),
          function(i) as.character(span(title = labels[i], remaining[i])),
          ""
        ),
        Type = vapply(
          remaining,
          function(v) as.character(type_badge(node_type(ds, v))),
          ""
        ),
        `Set by` = ifelse(
          vapply(remaining, function(v) isTRUE(ds$nodes[[v]]$manual), TRUE),
          "hand",
          "starting type"
        ),
        Level = unname(ds$levels[remaining]),
        Values = vapply(
          remaining,
          function(v) {
            as.character(span(class = "truncate", summarise_values(x[[v]])))
          },
          ""
        ),
        check.names = FALSE
      )
      compact_table(
        table,
        selection = "multiple",
        escape = FALSE,
        height = "420px",
        empty = "Every variable is a key or calculated."
      )
    })
    selected <- reactive(remaining_variables(current())[
      input$variables_rows_selected
    ])
    observe({
      for (id in c("set_sampled", "set_identifier", "set_blank")) {
        updateActionButton(session, id, disabled = !length(selected()))
      }
    })

    # A newly sampled variable joins the end of the ordering, with no predictors yet.
    set_type <- function(type) {
      guarded({
        ds <- current()
        for (v in req(selected())) {
          ds$nodes[[v]][c("type", "manual")] <- list(type, TRUE)
        }
        if (type == "sampled") {
          ds$order <- union(ds$order, selected())
        }
        change(ds)
      })
    }
    observeEvent(input$set_sampled, set_type("sampled"))
    observeEvent(input$set_identifier, set_type("identifier"))
    observeEvent(input$set_blank, set_type("blank"))

    output$order <- renderDT({
      ds <- req(current())
      fits <- b$study$fits[[ds$name]]
      order <- tryCatch(generation_order(ds), error = function(e) character())
      records <- paste(
        c(record_structure_label(ds), fits$structure$description),
        collapse = " · "
      )
      table <- data.frame(
        Step = seq_along(c("records", order)) - 1L,
        Variable = c("records", order),
        Type = c(
          as.character(badge("secondary", "Structure")),
          vapply(
            order,
            function(v) as.character(type_badge(node_type(ds, v))),
            ""
          )
        ),
        Formula = htmltools::htmlEscape(c(
          records,
          vapply(order, function(v) node_formula(ds, v, fits$nodes[[v]]), "")
        ))
      )
      compact_table(table, escape = FALSE, height = "420px")
    })

    # ---- Dataset settings ----------------------------------------------------------------

    output$settings <- renderUI({
      ds <- req(current())
      choices <- input_choices(b$study, ds$name)
      dm <- ds$name == "DM"
      tagList(
        if (!dm) {
          checkboxInput(ns("include"), "Generate this dataset", ds$include)
        },
        p(strong("Records "), record_structure_label(ds)),
        if (!dm) {
          selectizeInput(
            ns("inputs"),
            "Inputs from DM, earlier datasets and the spine",
            setNames(choices, display_name(choices)),
            ds$inputs,
            multiple = TRUE,
            options = list(plugins = list("remove_button"))
          )
        },
        selectizeInput(
          ns("order_input"),
          "Order of sampled variables",
          ds$order,
          ds$order,
          multiple = TRUE,
          options = list(plugins = list("drag_drop", "remove_button"))
        ),
        hint(
          "Drag to reorder and save: each sampled variable is then predicted by the informative ones before it."
        ),
        actionButton(
          ns("save_settings"),
          "Save settings",
          class = "btn-sm btn-primary"
        )
      )
    })
    observeEvent(
      input$save_settings,
      guarded({
        ds <- current()
        if (ds$name != "DM") {
          ds$include <- isTRUE(input$include)
          ds$inputs <- as.character(input$inputs)
        }
        order <- as.character(input$order_input)
        if (length(order) && !identical(order, ds$order)) {
          ds <- starting_graph(b$study, ds, order)
        }
        change(ds)
        notify("Dataset settings saved.")
      })
    )

    # ---- Graph ---------------------------------------------------------------------------

    observeEvent(
      input$rebuild,
      guarded({
        ds <- current()
        change(starting_graph(b$study, ds))
        notify(paste("Rebuilt the starting graph for", ds$name))
      })
    )
    output$title <- renderUI({
      ds <- req(current())
      nodes <- generated_variables(ds)
      sampled <- sum(vapply(
        nodes,
        function(v) identical(node_type(ds, v), "sampled"),
        TRUE
      ))
      tagList(
        "Graph",
        muted(sprintf("%d generated, %d sampled", length(nodes), sampled))
      )
    })
    output$graph <- renderUI(graph_view(
      ns("node_click"),
      req(current()),
      isolate(node())
    ))
    observeEvent(input$node_click, node(input$node_click))
    # The node panel is open while a node is selected.
    observe(toggle_sidebar(
      "panel",
      open = isTRUE(node() %in% current()$variables)
    ))

    output$node <- renderUI({
      ds <- req(current())
      v <- node()
      if (is.null(v) || !v %in% ds$variables) {
        return(empty_state(
          "No node selected",
          "Click a node in the graph, or find it by name, to see its formula."
        ))
      }
      type <- node_type(ds, v)
      header <- tagList(
        div(class = "node-title", h5(v), type_badge(type)),
        muted(ds$labels[[v]], "·", ds$levels[[v]], "level"),
        div(
          class = "formula",
          node_formula(ds, v, b$study$fits[[ds$name]]$nodes[[v]])
        )
      )
      if (!type %in% c("sampled", "identifier", "blank")) {
        text <- if (type == "rule") {
          "Calculated variables are edited in the Rules tab."
        } else {
          "Keys and timing are sampled with the spine: the visits attended and their days, then each visit's records from a source subject of the same type, dated on those days."
        }
        return(tagList(header, hint(text)))
      }
      n <- ds$nodes[[v]]
      # Possible parents: generated nodes and inputs that do not already depend on this node.
      choices <- setdiff(
        c(generated_variables(ds), input_choices(b$study, ds$name)),
        c(v, "USUBJID", descendants(ds, v))
      )
      sampled <- type == "sampled"
      tagList(
        header,
        selectInput(
          ns("type"),
          "Type",
          c(
            Sampled = "sampled",
            Identifier = "identifier",
            `Not generated` = "blank"
          ),
          type
        ),
        if (sampled) {
          tagList(
            selectizeInput(
              ns("predictors"),
              "Predictors",
              setNames(choices, display_name(choices)),
              n$predictors,
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            ),
            selectInput(
              ns("model"),
              "Model",
              setNames(names(NODE_MODELS), NODE_MODELS),
              n$model
            ),
            if (!is.null(ds$structure$visit) && ds$levels[[v]] == "record") {
              checkboxInput(
                ns("prev"),
                "Depends on the previous value",
                isTRUE(n$prev)
              )
            },
            if (!is.na(arm_variable(b$study$datasets$DM))) {
              checkboxInput(
                ns("vary"),
                "Draw arm effects from their uncertainty",
                isTRUE(n$vary)
              )
            }
          )
        },
        actionButton(ns("apply"), "Apply", class = "btn-sm btn-primary")
      )
    })
    observeEvent(
      input$apply,
      guarded({
        ds <- current()
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
          return(notify(
            paste("That would create a cycle:", conditionMessage(order)),
            "error"
          ))
        }
        if (n$type == "sampled") {
          ds$order <- union(ds$order, v)
        }
        ds$nodes[[v]]$manual <- TRUE
        change(ds)
      })
    )
  })
}

# ---- Graph drawing ------------------------------------------------------------------------

dot_id <- function(x) paste0('"', gsub('"', "", x), '"')

# One dataset's graph as Graphviz DOT: its generated nodes plus the inputs from DM and
# earlier datasets that it declares or uses (in rules and predictors), drawn by
# www/app.js. The selected node is outlined and its edges stay highlighted. Clicking a
# node sets the Shiny input `input`.
graph_view <- function(input, ds, selected = NULL) {
  # Node fill and outline: the badge colours, with inputs in white.
  graph_colors <- c(
    lapply(TYPE_COLORS, function(name) tint(COLORS[[name]], c(0.8, 0.35))),
    list(input = c("#ffffff", tint(COLORS[["secondary"]], 0.45)))
  )
  nodes <- generated_variables(ds)
  parents <- lapply(nodes, function(v) node_parents(ds, v))
  inputs <- unique(c(
    ds$inputs,
    grep("^(DM|N|SERIES|T)\\.", unlist(parents), value = TRUE)
  ))
  node_line <- function(v, type, label) {
    colors <- graph_colors[[type]]
    outline <- if (identical(v, selected)) {
      '#1d2327", penwidth="2.4'
    } else {
      colors[2]
    }
    sprintf(
      '%s [label=%s, fillcolor="%s", color="%s"];',
      dot_id(v),
      dot_id(label),
      colors[1],
      outline
    )
  }
  parents <- lapply(parents, intersect, c(nodes, inputs))
  to <- rep(nodes, lengths(parents))
  rule <- vapply(to, node_type, "", ds = ds) == "rule"
  dot <- c(
    "digraph G {",
    'graph [rankdir=TB, nodesep=0.4, ranksep=0.9, bgcolor="transparent", pad=0.2];',
    'node [shape=box, style="rounded,filled", fontname="Manrope", fontsize=13, height=0.36, margin="0.14,0.06", penwidth=1.2];',
    "edge [arrowsize=0.6, penwidth=1];",
    if (length(inputs)) {
      c(
        "subgraph cluster_inputs {",
        sprintf(
          'label="Inputs"; fontname="Manrope"; fontsize=10; fontcolor="%s"; style="rounded,dashed"; color="%s";',
          COLORS[["secondary"]],
          graph_colors$input[2]
        ),
        vapply(inputs, function(i) node_line(i, "input", display_name(i)), ""),
        "}"
      )
    },
    vapply(nodes, function(v) node_line(v, node_type(ds, v), v), ""),
    sprintf(
      '%s -> %s [color="%s", style=%s];',
      dot_id(unlist(parents)),
      dot_id(to),
      ifelse(
        rule,
        tint(COLORS[["warning"]], 0.4),
        tint(COLORS[["primary"]], 0.45)
      ),
      ifelse(rule, "dashed", "solid")
    ),
    "}"
  )
  tool <- function(action, title, ...) {
    tags$button(
      type = "button",
      class = "btn btn-light border",
      `data-action` = action,
      title = title,
      ...
    )
  }
  div(
    class = "dag",
    `data-dot` = paste(dot, collapse = "\n"),
    `data-input` = input,
    `data-key` = ds$name,
    `data-selected` = selected %||% "",
    div(
      class = "dag-tools",
      div(
        class = "dag-find input-group input-group-sm",
        span(class = "input-group-text", icon("magnifying-glass")),
        tags$input(
          type = "text",
          class = "form-control",
          list = paste0(input, "-variables"),
          placeholder = "Find a variable",
          `aria-label` = "Find a variable"
        ),
        tags$datalist(id = paste0(input, "-variables"))
      ),
      div(
        class = "btn-group btn-group-sm",
        tool("out", "Zoom out (−)", icon("minus")),
        tool("actual", "Actual size", class = "dag-scale", "100%"),
        tool("in", "Zoom in (+)", icon("plus")),
        tool("fit", "Fit the whole graph (0)", icon("expand")),
        tool("reset", "Put dragged nodes back", icon("rotate-left"))
      )
    ),
    div(
      class = "dag-canvas",
      tabindex = "0",
      div(class = "dag-loading", "Drawing graph…")
    ),
    div(
      class = "dag-legend",
      lapply(names(TYPE_COLORS), type_badge),
      badge(NA, "Input"),
      span(
        class = "dag-help",
        "Click, then scroll to zoom · drag the background to pan, a node to move it ·",
        "click a node to trace it, again or on the background to clear"
      )
    )
  )
}
