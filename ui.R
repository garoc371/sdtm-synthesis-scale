# Three pages: the start page picks a study, the overview follows its build, and a
# dataset page shows one dataset. The sidebar navigates between them.

# Reload local assets after they change.
asset <- function(path) {
  paste0(path, "?v=", as.integer(file.info(file.path("www", path))$mtime))
}

sidebar_nav <- sidebar(
  width = 240,
  bg = "#13312f",
  class = "app-nav",
  `data-bs-theme` = "dark",
  open = list(desktop = "open", mobile = "closed"),
  div(class = "brand", tags$img(src = "logo.svg", alt = ""), "Synthetic SDTM"),
  uiOutput("nav"),
  selectizeInput(
    "dataset",
    NULL,
    choices = NULL,
    options = list(placeholder = "Go to a dataset")
  )
)

page_sidebar(
  window_title = "Synthetic SDTM",
  theme = app_theme(),
  fillable = FALSE,
  sidebar = sidebar_nav,
  tags$head(
    tags$link(rel = "stylesheet", href = asset("style.css")),
    tags$script(src = asset("vendor/viz-3.31.0.js")),
    tags$script(src = asset("app.js"))
  ),
  navset_hidden(
    id = "page",
    nav_panel_hidden("start", start_ui("start")),
    nav_panel_hidden("study", board_ui("board")),
    nav_panel_hidden("dataset", dataset_ui("dataset"))
  )
)
