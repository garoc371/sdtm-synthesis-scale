# The start page: an example study, uploaded files or a folder on the server.

start_ui <- function(id) {
  ns <- NS(id)
  example <- function(key) {
    x <- EXAMPLES[[key]]
    files <- list.files(x$folder, "\\.(xpt|sas7bdat|csv)$", ignore.case = TRUE)
    input_button(
      ns("example"),
      key,
      class = "source-card",
      div(
        class = "source-title",
        x$title,
        muted(plural(length(files), "dataset"))
      ),
      div(class = "source-about", x$about)
    )
  }
  div(
    class = "start",
    h2("Load a study"),
    p(
      class = "lead-text",
      "Every dataset is then built, fitted and sampled without further input. ",
      "The overview lists what needs a look."
    ),
    h6(class = "section-label", "Examples"),
    div(class = "source-grid", lapply(names(EXAMPLES), example)),
    h6(class = "section-label", "Your data"),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(
        card_header("Upload files"),
        fileInput(
          ns("upload"),
          NULL,
          multiple = TRUE,
          accept = c(".xpt", ".sas7bdat", ".csv", ".xml"),
          placeholder = "xpt, sas7bdat, csv, define.xml"
        ),
        hint("DM is required. A define.xml adds variable labels.")
      ),
      card(
        card_header("Folder on this computer"),
        div(
          class = "input-group",
          tags$input(
            id = ns("folder"),
            type = "text",
            class = "form-control",
            placeholder = "/path/to/sdtm",
            spellcheck = "false",
            `aria-label` = "Path to SDTM files"
          ),
          actionButton(ns("load_folder"), "Load", class = "btn-primary")
        ),
        hint("Reads every xpt, sas7bdat, csv and define.xml file in it.")
      )
    )
  )
}

start_server <- function(id, load) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$example, {
      x <- EXAMPLES[[input$example]]
      load(normalizePath(x$folder), x$title)
    })
    observeEvent(input$upload, {
      load(input$upload$datapath, "Uploaded files", input$upload$name)
    })
    observeEvent(input$load_folder, {
      req(nzchar(input$folder))
      folder <- path.expand(input$folder)
      load(folder, basename(folder))
    })
  })
}
