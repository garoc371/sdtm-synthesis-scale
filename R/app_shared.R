# Shared Shiny pieces: small display helpers, study state updates and the graph.

# Theme colours (Bootstrap names). Node types and statuses use their subtle variants.
COLORS <- c(primary = "#14706b", secondary = "#6c7276", success = "#2f7d4f", info = "#3a6ab0", warning = "#b7791f",
  danger = "#b3443a")
TYPE_COLORS <- c(key = "secondary", rule = "warning", identifier = "info", sampled = "primary")
STATUS_COLORS <- c(ok = "success", accepted = "success", proposed = "warning", error = "danger", failed = "danger")

TYPE_LABELS <- c(key = "Key", rule = "Calculated", identifier = "Identifier", sampled = "Sampled", blank = "Not generated")

# A pill in a theme colour, or outlined without one.
badge <- function(color, text) span(class = if (is.na(color)) "badge rounded-pill border text-body-secondary" else
  sprintf("badge rounded-pill bg-%s-subtle text-%s-emphasis", color, color), text)

type_badge <- function(type) badge(TYPE_COLORS[type], TYPE_LABELS[[type]])

status_badge <- function(status) badge(STATUS_COLORS[status], switch(status,
  ok = "OK", error = "Error", failed = "Failed", proposed = "Proposed", accepted = "Accepted", rejected = "Rejected", status))

share_text <- function(share) ifelse(is.na(share), "–", sprintf("%.1f%%", 100 * share))

share_bar <- function(share) {
  if (is.na(share)) return("–")
  as.character(div(class = "d-flex align-items-center gap-2",
    div(class = "progress flex-grow-1", style = "height: 6px; min-width: 50px; max-width: 90px",
      div(class = "progress-bar", style = sprintf("width: %.1f%%", 100 * share))),
    span(class = "small", share_text(share))))
}

# Large numbers with a label under each, named by the labels.
numbers <- function(...) {
  values <- list(...)
  div(class = "d-flex flex-wrap gap-4", Map(function(label, value)
    div(div(class = "fs-3 fw-bold lh-sm", value), div(class = "small text-body-secondary", label)), names(values), values))
}

empty_state <- function(title, text, ...) div(class = "text-center text-body-secondary py-5 px-3",
  h5(class = "text-body", title), p(text), ...)

hint <- function(...) div(class = "small text-body-secondary", ...)

# Run an action and report errors as a notification instead of ending the session.
guarded <- function(notify, expr) tryCatch(expr, error = function(e) {
  if (inherits(e, "shiny.silent.error")) return(invisible())
  notify(conditionMessage(e), "error")
})

compact_table <- function(data, selection = "none", escape = TRUE, height = NULL, ...) {
  DT::datatable(data, rownames = FALSE, selection = selection, escape = escape, class = "compact hover",
    options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE, scrollY = height, scrollCollapse = TRUE,
      autoWidth = FALSE, language = list(emptyTable = "Nothing to show")), ...)
}

plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")

# ---- Graph ----------------------------------------------------------------------------

# A colour mixed with white, as Bootstrap's tint-color().
tint <- function(color, white) vapply(white, function(w)
  grDevices::rgb(t(grDevices::col2rgb(color) * (1 - w) + 255 * w), maxColorValue = 255), "")

PLOT_COLORS <- c(Source = tint(COLORS[["secondary"]], 0.4), Synthetic = COLORS[["primary"]])

# Node fill and outline: the badge colours, with inputs from other datasets in white.
graph_colors <- c(lapply(TYPE_COLORS, function(name) tint(COLORS[[name]], c(0.8, 0.35))),
  list(input = c("#ffffff", tint(COLORS[["secondary"]], 0.45))))

dot_id <- function(x) paste0('"', gsub('"', "", x), '"')

# One dataset's graph: its generated nodes plus the inputs from DM and earlier datasets
# that it declares or uses (in rules and predictors), drawn
# by www/graph.js. The selected node is outlined and its edges stay highlighted.
graph_view <- function(id, ds, selected = NULL) {
  nodes <- generated_variables(ds)
  parents <- lapply(nodes, function(v) node_parents(ds, v))
  inputs <- unique(c(ds$inputs, grep("^(DM|N|SERIES|T)\\.", unlist(parents), value = TRUE)))
  node_line <- function(v, type, label) {
    colors <- graph_colors[[type]]
    sprintf('%s [label=%s, fillcolor="%s", color="%s"%s];', dot_id(v), dot_id(label), colors[1],
      if (identical(v, selected)) "#1d2327" else colors[2], if (identical(v, selected)) ", penwidth=2.4" else "")
  }
  parents <- lapply(parents, intersect, c(nodes, inputs))
  to <- rep(nodes, lengths(parents))
  rule <- vapply(to, node_type, "", ds = ds) == "rule"
  dot <- c(
    "digraph G {",
    'graph [rankdir=TB, nodesep=0.1, ranksep=0.5, bgcolor="transparent", pad=0.2];',
    'node [shape=box, style="rounded,filled", fontname="Manrope", fontsize=13, height=0.36, margin="0.14,0.06", penwidth=1.2];',
    'edge [arrowsize=0.6, penwidth=1];',
    if (length(inputs)) c("subgraph cluster_inputs {", sprintf('label="Inputs"; fontname="Manrope"; fontsize=10; fontcolor="%s"; style="rounded,dashed"; color="%s";',
      COLORS[["secondary"]], graph_colors$input[2]), vapply(inputs, function(i) node_line(i, "input", display_name(i)), ""), "}"),
    vapply(nodes, function(v) node_line(v, node_type(ds, v), v), ""),
    sprintf('%s -> %s [color="%s", style=%s];', dot_id(unlist(parents)), dot_id(to),
      ifelse(rule, tint(COLORS[["warning"]], 0.4), tint(COLORS[["primary"]], 0.45)), ifelse(rule, "dashed", "solid")),
    "}")
  zoom <- function(action, title, ...) tags$button(type = "button", class = "btn btn-light border", `data-action` = action,
    title = title, ...)
  div(class = "dag position-relative d-flex flex-column border rounded overflow-hidden", `data-dot` = paste(dot, collapse = "\n"),
    `data-input` = id, `data-key` = ds$name, `data-selected` = selected %||% "",
    div(class = "dag-tools position-absolute top-0 start-0 end-0 p-2 d-flex justify-content-between gap-2",
      div(class = "dag-find input-group input-group-sm shadow-sm",
        span(class = "input-group-text bg-body text-body-secondary", icon("magnifying-glass")),
        tags$input(type = "text", class = "form-control", list = paste0(id, "-variables"),
          placeholder = "Find a variable", `aria-label` = "Find a variable"), tags$datalist(id = paste0(id, "-variables"))),
      div(class = "btn-group btn-group-sm shadow-sm",
        zoom("out", "Zoom out (−)", icon("minus")),
        zoom("actual", "Actual size", class = "dag-scale fw-semibold", "100%"),
        zoom("in", "Zoom in (+)", icon("plus")),
        zoom("fit", "Fit the whole graph (0)", icon("expand")))),
    div(class = "dag-canvas", tabindex = "0",
      div(class = "h-100 d-flex align-items-center justify-content-center small text-body-secondary", "Drawing graph…")),
    div(class = "d-flex flex-wrap align-items-center gap-2 px-3 py-2 border-top bg-body small text-body-secondary",
      lapply(names(TYPE_COLORS), type_badge), badge(NA, "Input"),
      span(class = "ms-auto d-none d-md-inline", "Click, then scroll to zoom · drag to pan · click a node to trace it, again or on the background to clear")))
}
