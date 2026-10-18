# Every dataset as a tile, by class. Clicking a tile opens the dataset.

tiles_ui <- function(id) {
  legend <- div(
    class = "legend",
    lapply(
      c("queued", "running", "built", "failed", "skipped"),
      function(stage) {
        span(`data-stage` = stage, STAGES[[stage]])
      }
    )
  )
  card(card_header("Datasets", legend), uiOutput(NS(id, "tiles")))
}

# `status`: a reactive `build_status()`. `parents`: the parent dataset of each supplemental
# dataset. `design`: the trial design datasets. Returns the dataset clicked, as an event.
tiles_server <- function(id, status, parents, design) {
  moduleServer(id, function(input, output, session) {
    output$tiles <- renderUI({
      s <- req(status())
      groups <- lapply(intersect(names(CLASSES), s$class), function(class) {
        rows <- which(s$class == class)
        div(
          class = "tile-group",
          div(
            class = "section-label",
            sprintf("%s · %d", CLASSES[[class]], length(rows))
          ),
          div(
            class = "tile-grid",
            lapply(rows, function(i) {
              children <- names(parents())[parents() == s$dataset[i]]
              dataset_tile(s[i, ], children, session$ns("open"))
            })
          )
        )
      })
      tagList(
        groups,
        if (length(design())) {
          hint(sprintf(
            "%s · copied unchanged: %s",
            CLASSES[["design"]],
            paste(design(), collapse = ", ")
          ))
        }
      )
    })
    reactive(input$open)
  })
}

dataset_tile <- function(r, children, input) {
  seconds <- sum(c(r$discover, r$fit), na.rm = TRUE)
  detail <- switch(
    r$stage,
    built = if (r$cached) "cached" else sprintf("%.0f s", seconds),
    failed = plural(r$spec_failed, "failed check"),
    STAGES[[r$stage]]
  )
  input_button(
    input,
    r$dataset,
    class = "tile",
    `data-stage` = r$stage,
    title = paste(
      c(r$dataset, if (nzchar(r$message)) r$message),
      collapse = ": "
    ),
    div(
      class = "tile-head",
      span(class = "tile-name", r$dataset),
      span(
        if (r$manual) icon("hand", title = "Changed by hand"),
        if (r$coherence_failed > 0) {
          icon(
            "triangle-exclamation",
            class = "text-danger",
            title = "Coherence check failed"
          )
        }
      )
    ),
    div(class = "tile-detail", big(r$records), " · ", detail),
    if (length(children)) {
      div(class = "tile-detail", paste0("+", children, collapse = " "))
    }
  )
}
