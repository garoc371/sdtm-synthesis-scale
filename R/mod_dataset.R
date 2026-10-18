# One dataset, opened from the overview: its header and problems (mod_dataset_header.R), then
# its graph, rules, variables and a comparison of generated with source data.

dataset_ui <- function(id) {
  ns <- NS(id)
  tagList(
    dataset_header_ui(ns("header")),
    navset_underline(
      nav_panel("Graph", graph_ui(ns("graph"))),
      nav_panel("Rules", rules_ui(ns("rules"))),
      nav_panel("Variables", variables_ui(ns("variables"))),
      nav_panel("Compare", compare_ui(ns("compare")))
    )
  )
}

# `dataset`: the open dataset; `build`: the build as reactives (dataset_reactives()). Returns,
# as events, the dataset changed by hand (`changed`) and the name of one to build by the
# policy again (`reset`).
dataset_server <- function(id, dataset, build) {
  moduleServer(id, function(input, output, session) {
    reset <- dataset_header_server("header", dataset, build$state)
    compare_server("compare", dataset, build$study, build$generated)
    changed <- merge_events(
      rules_server("rules", dataset, build$study, build$share),
      graph_server("graph", dataset, build$study),
      variables_server("variables", dataset, build$study)
    )
    list(changed = changed, reset = reset)
  })
}
