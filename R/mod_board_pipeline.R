# The overview's header: the study's title and where loading, building and generating stand.

pipeline_ui <- function(id) {
  uiOutput(NS(id, "header"))
}

# `stages`: a reactive `pipeline_stages()`.
pipeline_server <- function(id, stages) {
  moduleServer(id, function(input, output, session) {
    output$header <- renderUI({
      x <- stages()
      if (!is.null(x$error)) {
        return(div(
          class = "alert alert-danger",
          strong("The source could not be read. "),
          x$error
        ))
      }
      tagList(
        div(class = "page-title", h2(x$title), muted(x$summary)),
        div(
          class = "pipeline",
          pipeline_stage("Load", x$load),
          pipeline_stage("Build", x$build, progress_bar(x$built, x$broken)),
          pipeline_stage("Generate", x$generate)
        )
      )
    })
  })
}

# Each stage's state and text, and the shares of datasets built and broken.
pipeline_stages <- function(b, s) {
  data <- b$study$source$data
  g <- b$generated
  done <- sum(s$stage %in% c("built", "failed", "error", "skipped"))
  built <- sum(s$stage == "built")
  load <- if (is.null(b$study)) {
    c("active", "Reading the source…")
  } else {
    c(
      "done",
      sprintf(
        "%d datasets, %s subjects · %.0f s",
        length(data),
        big(nrow(data$DM)),
        b$loaded
      )
    )
  }
  build <- if (is.null(s)) {
    c("waiting", "")
  } else if (done < nrow(s)) {
    c(
      "active",
      sprintf("%d of %d · %d running", done, nrow(s), sum(s$stage == "running"))
    )
  } else {
    c(
      "done",
      sprintf("%d built · %.0f s", built, max(s$finished, na.rm = TRUE))
    )
  }
  generate <- if (!is.null(b$running$generate)) {
    c("active", if (is.null(g)) "Sampling…" else "Updating…")
  } else if (!is.null(b$generation_error)) {
    c("error", b$generation_error)
  } else if (is.null(g)) {
    c("waiting", "After the build")
  } else {
    records <- sum(vapply(g$data, nrow, 0L))
    text <- sprintf(
      "%s subjects, %s records · %.0f s",
      big(g$n),
      big(records),
      b$generation$finished
    )
    c(if (build_busy(b)) "stale" else "done", text)
  }
  summary <- if (!is.null(b$study)) {
    sprintf(
      "%d supplemental folded · %d trial design copied · policy %d%%",
      length(study_supp(b$study)),
      length(study_design(b$study)),
      round(100 * b$share)
    )
  }
  list(
    title = b$title,
    error = b$error,
    load = load,
    build = build,
    generate = generate,
    built = if (!is.null(s)) built / nrow(s),
    broken = if (!is.null(s)) sum(s$stage %in% c("failed", "error")) / nrow(s),
    summary = summary
  )
}

pipeline_stage <- function(title, state, bar = NULL) {
  div(
    class = "pipeline-stage",
    `data-state` = state[1],
    div(class = "pipeline-title", title),
    div(class = "pipeline-text", state[2]),
    bar
  )
}

progress_bar <- function(built, broken) {
  if (is.null(built)) {
    return(NULL)
  }
  div(
    class = "progress",
    div(
      class = "progress-bar bg-success",
      style = sprintf("width: %.1f%%", 100 * built)
    ),
    div(
      class = "progress-bar bg-danger",
      style = sprintf("width: %.1f%%", 100 * broken)
    )
  )
}
