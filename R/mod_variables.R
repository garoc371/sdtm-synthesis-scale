# A dataset's Variables tab: the types of the variables not calculated
# (mod_variables_types.R), the dataset's settings (mod_variables_settings.R) and the
# generation order (mod_variables_order.R). Any change makes the dataset kept as set by
# hand, rebuilt alone.

variables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = breakpoints(md = 12, xl = c(7, 5)),
      types_ui(ns("types")),
      settings_ui(ns("settings"))
    ),
    order_ui(ns("order"))
  )
}

# `dataset`: the open dataset; `study`: the study. Returns the dataset changed, as an event.
variables_server <- function(id, dataset, study) {
  moduleServer(id, function(input, output, session) {
    typed <- types_server("types", dataset, study)
    saved <- settings_server("settings", dataset, study)
    order_server("order", dataset, study)
    merge_events(typed, saved)
  })
}
