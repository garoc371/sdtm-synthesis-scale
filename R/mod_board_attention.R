# Datasets that need a look, and the SDTM rules checked on the generated data. Clicking a
# dataset opens it.

attention_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Needs a look", uiOutput(ns("count"), inline = TRUE)),
    uiOutput(ns("list"))
  )
}

# `attention`: a reactive `attention_state()`. Returns the dataset clicked, as an event.
attention_server <- function(id, attention) {
  moduleServer(id, function(input, output, session) {
    output$count <- renderUI({
      n <- NROW(attention()$rows)
      badge(if (n) "danger" else "success", n)
    })
    output$list <- renderUI({
      x <- attention()
      tagList(
        attention_list(x, session$ns("open")),
        checked_rules(x$checks)
      )
    })
    reactive(input$open)
  })
}

# What needs a look, the coherence checks, and whether the build is done.
attention_state <- function(b) {
  s <- build_status(b)
  list(
    rows = attention_rows(b, s),
    checks = b$coherence,
    built = sum(s$stage == "built"),
    done = !is.null(s) &&
      all(s$stage %in% c("built", "skipped")) &&
      !is.null(b$coherence)
  )
}

# What needs a look, as a list of datasets to open.
attention_list <- function(x, input) {
  if (x$done && is.null(x$rows)) {
    return(div(
      class = "alert alert-success mb-0",
      icon("circle-check"),
      sprintf(
        " Nothing new. %d datasets built and generated; none breaks an SDTM rule more often than the source.",
        x$built
      )
    ))
  }
  if (is.null(x$rows)) {
    return(hint(
      "Nothing so far. Datasets appear here when a check of their graph fails, or their generated data break an SDTM rule more often than the source."
    ))
  }
  div(
    class = "list-group list-group-flush",
    lapply(seq_len(nrow(x$rows)), function(i) {
      r <- x$rows[i, ]
      input_button(
        input,
        r$dataset,
        class = "list-group-item list-group-item-action attention-item",
        div(
          class = "attention-head",
          strong(r$dataset),
          span(class = "text-danger-emphasis", r$what)
        ),
        hint(r$detail)
      )
    })
  )
}

# The SDTM rules checked on the generated data, by category, and how many the source breaks
# and so are not run.
checked_rules <- function(checks) {
  if (!NROW(checks)) {
    return(NULL)
  }
  categories <- intersect(names(CHECK_CATEGORIES), checks$Category)
  tags$details(
    class = "checked-rules",
    tags$summary(sprintf(
      "What is checked: %d SDTM rules the source keeps, in %d categories%s",
      nrow(checks),
      length(categories),
      if (NROW(attr(checks, "not_run"))) sprintf("; %d more are not run, as the source breaks them", nrow(attr(checks, "not_run"))) else ""
    )),
    tags$ul(lapply(categories, function(k) {
      x <- checks[checks$Category == k, ]
      broken <- x$Status == "failed"
      status <- if (!any(broken)) {
        sprintf("%s, all kept", plural(nrow(x), "rule"))
      } else {
        sprintf("%s, %d broken", plural(nrow(x), "rule"), sum(broken))
      }
      tags$li(
        strong(paste0(CHECK_CATEGORIES[[k]]$label, ":")),
        paste0(CHECK_CATEGORIES[[k]]$about, "."),
        muted(status)
      )
    }))
  )
}

# Datasets left out of generation (their build failed) or whose generated data break an
# SDTM rule the source keeps, with the first problem.
attention_rows <- function(b, s) {
  if (is.null(s)) {
    return(NULL)
  }
  problem <- function(checks, name) {
    x <- checks[checks$Dataset == name & checks$Status == "failed", ]
    if (!nrow(x)) {
      return("")
    }
    more <- if (nrow(x) > 1) sprintf(" (+%d more)", nrow(x) - 1)
    paste0(x$Check[1], ": ", x$Problems[1], more)
  }
  rows <- lapply(seq_len(nrow(s)), function(i) {
    r <- s[i, ]
    what <- if (r$stage == "error") {
      c("Build failed", r$message)
    } else if (r$stage == "failed") {
      c(
        "Not generated: a check fails",
        problem(b$checks[[r$dataset]], r$dataset)
      )
    } else if (r$coherence_failed > 0) {
      c(
        if (r$coherence_failed == 1) "Breaks an SDTM rule the source keeps" else
          sprintf("Breaks %d SDTM rules the source keeps", r$coherence_failed),
        problem(b$coherence, r$dataset)
      )
    }
    if (length(what)) {
      data.frame(dataset = r$dataset, what = what[1], detail = what[2])
    }
  })
  do.call(rbind, rows)
}
