# Synthetic SDTM. Loading a study is the only required step: every dataset is then built
# by one study-level policy, fitted and sampled in background workers, and the board shows
# progress and the few things that need a look. Run with shiny::runApp().
library(shiny)
options(shiny.maxRequestSize = 500 * 1024^2)
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)

WORKERS <- getOption("synth.cores", max(2L, min(8L, parallel::detectCores() - 1L)))
CACHE <- getOption("synth.cache", file.path(getwd(), "cache"))
start_workers(WORKERS)
onStop(function() mirai::daemons(0))

# Reload local assets after they change.
asset <- function(path) paste0(path, "?v=", as.integer(file.info(file.path("www", path))$mtime))

# A small graph: two nodes feeding a third.
logo <- HTML('<svg width="28" height="28" viewBox="0 0 28 28" aria-hidden="true"><rect width="28" height="28" rx="7" fill="#14706b"/>
  <path d="M9 9 L14 19 M19 9 L14 19" stroke="#bfe3df" stroke-width="1.6"/><circle cx="9" cy="9" r="3" fill="#fff"/>
  <circle cx="19" cy="9" r="3" fill="#fff"/><circle cx="14" cy="19" r="3.4" fill="#f2c46d"/></svg>')

section <- function(title, ...) div(div(class = "small fw-bold text-uppercase text-body-secondary mb-2", title), ...)

ui <- bslib::page_sidebar(
  window_title = "Synthetic SDTM",
  theme = do.call(bslib::bs_theme, c(as.list(COLORS), version = 5, bg = "#ffffff", fg = "#1d2327",
    base_font = "Manrope, 'Avenir Next', system-ui, sans-serif", code_font = "'SFMono-Regular', Consolas, monospace",
    font_scale = 0.92, `border-radius` = "0.5rem", `border-color` = "#e4e2dc",
    `headings-font-weight` = 700, `btn-font-weight` = 600, `form-label-font-weight` = 600,
    `badge-font-weight` = 600, `code-color` = "#0e524e", `card-cap-bg` = "transparent", `card-box-shadow` = "none")),
  fillable = FALSE,
  sidebar = bslib::sidebar(width = 280, bg = "#13312f", open = list(desktop = "open", mobile = "closed"), gap = "1.5rem",
    `data-bs-theme` = "dark",
    div(class = "d-flex align-items-center gap-2 fs-5 fw-bold text-white", logo, "Synthetic SDTM"),
    section("Source",
      div(class = "d-grid gap-2",
        actionButton("load_pilot", "CDISC pilot (10 datasets)", class = "btn-sm btn-outline-light", icon = icon("database"))),
      fileInput("upload", NULL, multiple = TRUE, accept = c(".xpt", ".sas7bdat", ".csv", ".xml"),
        buttonLabel = "Upload", placeholder = "xpt, sas7bdat, csv, define.xml"),
      div(class = "input-group input-group-sm",
        tags$input(id = "folder", type = "text", class = "form-control", placeholder = "/path/to/sdtm",
          `aria-label` = "Path to SDTM files", spellcheck = "false"),
        actionButton("load_folder", "Load", class = "btn-outline-light"))),
    section("Policy",
      sliderInput("policy", "Accept found rules reproducing at least", 90, 100, 100 * POLICY_SHARE, step = 1, post = "%",
        ticks = FALSE),
      hint("Applies to every dataset. Rules below it are rejected and their targets sampled. Standard SDTM rules and changes made by hand are kept.")),
    section("Sample",
      bslib::layout_columns(col_widths = c(6, 6),
        numericInput("n_subjects", "Subjects", 200, min = 1, step = 50),
        numericInput("seed", "Seed", 1, min = 1, step = 1))),
    section("Save",
      div(class = "d-grid gap-2",
        downloadButton("download_data", "Synthetic data (.zip)", class = "btn-sm btn-light"),
        downloadButton("download_bundle", "Specification (.rds)", class = "btn-sm btn-outline-light")),
      hint(class = "mt-2", "The specification holds graphs and fitted parameters, no source records: ",
        code("generate_study(readRDS(file), n, seed)"), "."))),
  tags$head(
    tags$link(rel = "stylesheet", href = asset("style.css")),
    tags$script(src = asset("vendor/viz-3.31.0.js")),
    tags$script(src = asset("graph.js"))),
  bslib::navset_hidden(id = "page",
    bslib::nav_panel_hidden("board", board_ui()),
    bslib::nav_panel_hidden("dataset", dataset_ui())))

server <- function(input, output, session) {
  b <- new_build(WORKERS, CACHE)
  tick <- reactiveVal(0)
  selected <- reactiveVal(NULL)
  current <- reactiveVal(NULL)
  notify <- function(text, type = "message") showNotification(text, type = type, duration = if (type == "error") 10 else 5)

  # One poll of the build. The selected dataset's graph is replaced only when it changed,
  # so that its page does not redraw while other datasets build.
  step <- function() {
    changed <- guarded(notify, build_step(b))
    if (isTRUE(changed)) tick(tick() + 1)
    name <- selected()
    ds <- if (!is.null(name)) b$study$datasets[[name]]
    if (!identical(ds, current())) current(ds)
  }
  observe({
    invalidateLater(if (build_busy(b)) 250 else 1000)
    isolate(step())
  })
  # A change made by hand goes to the build at once.
  change <- function(ds) {
    set_dataset(b, ds)
    step()
  }
  open_dataset <- function(name) {
    selected(name)
    current(b$study$datasets[[name]])
    bslib::nav_select("page", "dataset")
  }

  load <- function(files, names = basename(files)) {
    start_load(b, files, names)
    selected(NULL)
    current(NULL)
    bslib::nav_select("page", "board")
    step()
  }
  observeEvent(input$load_pilot, load(file.path(getwd(), "data", "pilot")))
  observeEvent(input$upload, load(input$upload$datapath, input$upload$name))
  observeEvent(input$load_folder, {
    req(nzchar(input$folder))
    load(path.expand(input$folder))
  })

  policy <- debounce(reactive(input$policy), 500)
  observeEvent(policy(), {
    set_policy(b, policy() / 100)
    step()
  }, ignoreInit = TRUE)
  sample <- debounce(reactive(c(input$n_subjects, input$seed)), 500)
  observeEvent(sample(), {
    req(length(sample()) == 2, all(is.finite(sample())))
    set_sample(b, max(1, round(sample()[1])), sample()[2])
    step()
  }, ignoreInit = TRUE)

  output$download_data <- downloadHandler(
    filename = function() sprintf("synthetic-sdtm-seed%s.zip", b$seed),
    content = function(file) {
      validate(need(b$generated, "Nothing generated yet."))
      directory <- tempfile("synthetic-")
      files <- write_outputs(b$study, b$generated, directory)
      zip::zip(file, basename(files), root = directory)
    })
  output$download_bundle <- downloadHandler(
    filename = function() "synthetic-sdtm-specification.rds",
    content = function(file) saveRDS(build_bundle(b), file))

  board_server(input, output, session, b, tick, open_dataset)
  dataset_server(input, output, session, b, tick, current, change, step, notify, open_dataset)
}

shinyApp(ui, server)
