# A dataset's graph, with a panel that edits the node clicked (mod_graph_node.R). Any change
# makes the dataset kept as set by hand, rebuilt alone.

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
        node_panel_ui(ns("node"))
      ),
      uiOutput(ns("graph"), fill = TRUE)
    )
  )
}

# `dataset`: the open dataset; `study`: the study. Returns the dataset with its graph
# changed, as an event.
graph_server <- function(id, dataset, study) {
  moduleServer(id, function(input, output, session) {
    # The node clicked, cleared when another dataset opens.
    node <- reactiveVal(NULL)
    opened <- changes(function() dataset()$name)
    observeEvent(opened(), node(NULL), ignoreInit = TRUE)
    observeEvent(input$node_click, node(input$node_click))

    output$title <- renderUI({
      ds <- req(dataset())
      nodes <- generated_variables(ds)
      sampled <- sum(vapply(nodes, function(v) identical(node_type(ds, v), "sampled"), TRUE))
      tagList("Graph", muted(sprintf("%d generated, %d sampled", length(nodes), sampled)))
    })
    output$graph <- renderUI({
      graph_view(session$ns("node_click"), req(dataset()), isolate(node()))
    })
    # The node panel is open while a node is selected.
    observe(toggle_sidebar("panel", open = isTRUE(node() %in% dataset()$variables)))

    rebuilt <- new_event()
    observeEvent(
      input$rebuild,
      guarded({
        ds <- dataset()
        rebuilt$send(starting_graph(study(), ds))
        notify(paste("Rebuilt the starting graph for", ds$name))
      })
    )
    applied <- node_panel_server("node", node, dataset, study)
    merge_events(rebuilt$value, applied)
  })
}

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
