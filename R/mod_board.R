# The study overview: what the build is doing, every dataset as a tile, what needs a look,
# the generation settings and downloads, and the ledger of decisions the policy made.
# Nothing here has to be confirmed. Each card is a module (mod_board_*.R).

board_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "board",
    pipeline_ui(ns("pipeline")),
    layout_columns(
      col_widths = breakpoints(md = 12, xl = c(8, 4)),
      tiles_ui(ns("tiles")),
      div(attention_ui(ns("attention")), generation_ui(ns("generation")))
    ),
    decisions_ui(ns("decisions"))
  )
}

# `view`: the reactives of `board_reactives()`. Returns the dataset to open (an event), and
# the policy's share and the sample asked for.
board_server <- function(id, view) {
  moduleServer(id, function(input, output, session) {
    pipeline_server("pipeline", view$stages)
    tile <- tiles_server("tiles", view$status, view$parents, view$design)
    flagged <- attention_server("attention", view$attention)
    settings <- generation_server("generation", view$schedule, view$outputs, view$bundle)
    decision <- decisions_server("decisions", view$ledger)
    c(list(open = merge_events(tile, flagged, decision)), settings)
  })
}
