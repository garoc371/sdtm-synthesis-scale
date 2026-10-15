# Loading a study is the only required step: every dataset is then built by one
# study-level policy, fitted and sampled in background workers (R/pipeline.R). The server
# polls the build and hands its state to the modules.

start_workers(WORKERS)
onStop(function() mirai::daemons(0))

function(input, output, session) {
  b <- new_build(WORKERS, CACHE)
  # Counts the polls that changed the build; outputs read the build after it.
  tick <- reactiveVal(0)
  page <- reactiveVal("start")
  selected <- reactiveVal(NULL)
  current <- reactiveVal(NULL)

  notify <- function(text, type = "message") {
    showNotification(
      text,
      type = type,
      duration = if (type == "error") 10 else 5
    )
  }

  # One poll of the build. The open dataset is replaced only when it changed, so that its
  # page does not redraw while other datasets build.
  step <- function() {
    changed <- guarded(notify, build_step(b))
    if (isTRUE(changed)) {
      tick(tick() + 1)
    }
    name <- selected()
    ds <- if (!is.null(name)) b$study$datasets[[name]]
    if (!identical(ds, current())) current(ds)
  }
  observe({
    invalidateLater(if (build_busy(b)) 250 else 1000)
    isolate(step())
  })

  go <- function(to) {
    page(to)
    nav_select("page", to, session = session)
  }
  open_dataset <- function(name) {
    selected(name)
    current(b$study$datasets[[name]])
    go("dataset")
  }
  open_overview <- function() {
    selected(NULL)
    go("study")
  }

  # A change made by hand goes to the build at once.
  change <- function(ds) {
    set_dataset(b, ds)
    step()
  }

  load <- function(files, title, names = basename(files)) {
    start_load(b, files, names)
    b$title <- title
    selected(NULL)
    current(NULL)
    go("study")
    step()
  }

  start_server("start", load)
  board_server("board", b, tick, step, open_dataset)
  dataset_server("dataset", b, tick, current, change, step, notify)

  observeEvent(input$open_dataset, open_dataset(input$open_dataset))

  # The sidebar: the start page, the study overview and a search for one dataset.
  observeEvent(
    input$nav,
    if (input$nav == "start") go("start") else open_overview()
  )
  output$nav <- renderUI({
    link <- function(label, to) {
      input_button(
        "nav",
        to,
        class = "nav-link",
        `aria-current` = if (page() == to) "page",
        label
      )
    }
    tagList(
      link(tagList(icon("plus"), "Load a study"), "start"),
      if (!is.null(b$title)) {
        link(tagList(icon("table-cells"), b$title), "study")
      }
    )
  })
  datasets <- changes(function() {
    tick()
    names(b$info)
  })
  observe({
    updateSelectizeInput(
      session,
      "dataset",
      choices = datasets(),
      selected = isolate(selected()) %||% ""
    )
  })
  observe(updateSelectizeInput(
    session,
    "dataset",
    selected = selected() %||% ""
  ))
  observeEvent(input$dataset, {
    if (nzchar(input$dataset) && !identical(input$dataset, selected())) {
      open_dataset(input$dataset)
    }
  })
}
