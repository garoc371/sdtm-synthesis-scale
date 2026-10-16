# The study overview: what the build is doing, every dataset as a tile, what needs a look,
# the generation settings and downloads, and the ledger of decisions the policy made.
# Nothing here has to be confirmed.

board_ui <- function(id) {
  ns <- NS(id)
  legend <- div(
    class = "legend",
    lapply(
      c("queued", "running", "built", "failed", "skipped"),
      function(stage) {
        span(`data-stage` = stage, STAGES[[stage]])
      }
    )
  )
  div(
    class = "board",
    uiOutput(ns("header")),
    layout_columns(
      col_widths = breakpoints(md = 12, xl = c(8, 4)),
      card(card_header("Datasets", legend), uiOutput(ns("tiles"))),
      div(
        card(
          card_header(
            "Needs a look",
            uiOutput(ns("attention_count"), inline = TRUE)
          ),
          uiOutput(ns("attention"))
        ),
        generation_card(ns)
      )
    ),
    card(
      full_screen = TRUE,
      card_header(uiOutput(ns("decisions_title"), inline = TRUE)),
      hint(
        "What the policy and the starting graphs decided, by how many source records each",
        "changes. A calculated variable follows its rule in every generated record; its source",
        "exceptions are counted here. Open a dataset to change any of these."
      ),
      DTOutput(ns("decisions"), fill = FALSE)
    )
  )
}

generation_card <- function(ns) {
  card(
    card_header("Generation"),
    sliderInput(
      ns("policy"),
      "Minimum match rate for a rule",
      min = 90,
      max = 100,
      value = 100 * POLICY_SHARE,
      step = 1,
      post = "%",
      ticks = FALSE
    ),
    hint(
      "A rule computes a column from other columns, such as a test code from its name. It is used",
      "when it gives the source value in at least this share of the records it can be checked on",
      "(the Reproduced column); otherwise the column is sampled. Rules under 90% are never found.",
      "Applies to every dataset; changes made by hand are kept."
    ),
    layout_columns(
      col_widths = c(6, 6),
      numericInput(ns("n_subjects"), "Subjects", 200, min = 1, step = 50),
      numericInput(ns("seed"), "Seed", 1, min = 1, step = 1)
    ),
    div(
      class = "button-row",
      downloadButton(
        ns("download_data"),
        "Synthetic data",
        class = "btn-sm btn-primary"
      ),
      downloadButton(
        ns("download_bundle"),
        "Specification",
        class = "btn-sm btn-outline-secondary"
      )
    ),
    hint(
      "The specification holds graphs and fitted parameters, no source records: ",
      code("generate_study(readRDS(file), n, seed)"),
      "."
    )
  )
}

board_server <- function(id, b, tick, step, open_dataset) {
  moduleServer(id, function(input, output, session) {
    status <- changes(function() {
      tick()
      build_status(b)
    })

    # ---- Settings and downloads ---------------------------------------------------------

    policy <- debounce(reactive(input$policy), 500)
    observeEvent(
      policy(),
      {
        set_policy(b, policy() / 100)
        step()
      },
      ignoreInit = TRUE
    )
    sample <- debounce(reactive(c(input$n_subjects, input$seed)), 500)
    observeEvent(
      sample(),
      {
        req(length(sample()) == 2, all(is.finite(sample())))
        set_sample(b, max(1, round(sample()[1])), sample()[2])
        step()
      },
      ignoreInit = TRUE
    )
    output$download_data <- downloadHandler(
      filename = function() sprintf("synthetic-sdtm-seed%s.zip", b$seed),
      content = function(file) {
        validate(need(b$generated, "Nothing generated yet."))
        directory <- tempfile("synthetic-")
        files <- write_outputs(b$study, b$generated, directory)
        zip::zip(file, basename(files), root = directory)
      }
    )
    output$download_bundle <- downloadHandler(
      filename = function() "synthetic-sdtm-specification.rds",
      content = function(file) saveRDS(build_bundle(b), file)
    )

    # ---- Header: load, build, generate ---------------------------------------------------

    stages <- changes(function() {
      tick()
      pipeline_stages(b, build_status(b))
    })
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

    # ---- Tiles ---------------------------------------------------------------------------

    output$tiles <- renderUI({
      s <- req(status())
      supp <- study_supp(b$study)
      parents <- vapply(supp, `[[`, "", "parent")
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
              dataset_tile(s[i, ], names(supp)[parents == s$dataset[i]])
            })
          )
        )
      })
      design <- names(study_design(b$study))
      tagList(
        groups,
        if (length(design)) {
          hint(sprintf(
            "%s · copied unchanged: %s",
            CLASSES[["design"]],
            paste(design, collapse = ", ")
          ))
        }
      )
    })

    # ---- Needs a look --------------------------------------------------------------------

    attention <- changes(function() {
      tick()
      s <- build_status(b)
      list(
        rows = attention_rows(b, s),
        inherited = s$dataset[s$coherence_inherited > 0],
        checks = b$coherence,
        built = sum(s$stage == "built"),
        done = !is.null(s) &&
          all(s$stage %in% c("built", "skipped")) &&
          !is.null(b$coherence)
      )
    })
    output$attention_count <- renderUI({
      n <- NROW(attention()$rows)
      badge(if (n) "danger" else "success", n)
    })
    output$attention <- renderUI({
      x <- attention()
      tagList(
        attention_list(x),
        inherited_note(x$inherited),
        checked_rules(x$checks)
      )
    })

    # ---- Decisions -----------------------------------------------------------------------

    # Recomputed only when a dataset's build changed.
    keys <- changes(function() {
      tick()
      vapply(b$info %||% list(), `[[`, "", "key")
    })
    ledger <- reactive({
      built <- names(keys())[nzchar(keys())]
      x <- build_decisions(b)
      if (!is.null(x)) x[x$dataset %in% built, ]
    })
    output$decisions_title <- renderUI({
      x <- ledger()
      detail <- if (NROW(x)) {
        sprintf(
          "%d in %d datasets · %d set by hand",
          nrow(x),
          length(unique(x$dataset)),
          sum(x$by == "hand")
        )
      }
      tagList("Decisions", muted(detail))
    })
    # Drawn once; later builds replace its rows, so that it keeps its page and filter.
    output$decisions <- renderDT({
      datatable(
        decision_table(isolate(ledger())),
        rownames = FALSE,
        selection = "single",
        escape = FALSE,
        class = "compact hover",
        options = list(
          dom = "ftip",
          pageLength = 12,
          scrollX = TRUE,
          order = list(list(5, "desc")),
          language = list(
            search = "",
            searchPlaceholder = "Filter",
            emptyTable = "No decisions yet."
          )
        )
      )
    })
    decisions <- dataTableProxy("decisions")
    observeEvent(ledger(), ignoreNULL = FALSE, {
      replaceData(
        decisions,
        decision_table(ledger()),
        rownames = FALSE,
        resetPaging = FALSE
      )
    })
    observeEvent(input$decisions_rows_selected, {
      open_dataset(ledger()$dataset[input$decisions_rows_selected])
    })
  })
}

# ---- What the board shows ----------------------------------------------------------------

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

dataset_tile <- function(r, children) {
  seconds <- sum(c(r$discover, r$fit), na.rm = TRUE)
  detail <- switch(
    r$stage,
    built = if (r$cached) "cached" else sprintf("%.0f s", seconds),
    failed = plural(r$spec_failed, "failed check"),
    STAGES[[r$stage]]
  )
  input_button(
    "open_dataset",
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

# What needs a look, as a list of datasets to open.
attention_list <- function(x) {
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
        "open_dataset",
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

# Datasets that break rules about as often as their source.
inherited_note <- function(names) {
  if (!length(names)) {
    return(NULL)
  }
  div(
    class = "inherited-note hint",
    strong("As in the source: "),
    lapply(names, function(name) {
      input_button("open_dataset", name, class = "btn btn-link", name)
    }),
    " break SDTM rules about as often as the source does. Generated subjects copy the records of a source subject, so these come with them."
  )
}

# The SDTM rules checked on the generated data, by category.
checked_rules <- function(checks) {
  if (!NROW(checks)) {
    return(NULL)
  }
  categories <- intersect(names(CHECK_CATEGORIES), checks$Category)
  tags$details(
    class = "checked-rules",
    tags$summary(sprintf(
      "What is checked: %d SDTM rules in %d categories",
      nrow(checks),
      length(categories)
    )),
    tags$ul(lapply(categories, function(k) {
      x <- checks[checks$Category == k, ]
      broken <- x$Status == "failed"
      status <- if (!any(broken)) {
        sprintf("%s, all kept", plural(nrow(x), "rule"))
      } else {
        new <- sum(broken & !x$Inherited)
        paste0(
          sprintf("%s; %d broken as in the source", plural(nrow(x), "rule"), sum(x$Inherited)),
          if (new) sprintf(", %d more often than in the source", new)
        )
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
# SDTM rule more often than the source, with the first problem.
attention_rows <- function(b, s) {
  if (is.null(s)) {
    return(NULL)
  }
  problem <- function(checks, name) {
    x <- checks[checks$Dataset == name & checks$Status == "failed" & !(checks$Inherited %||% FALSE), ]
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
        if (r$coherence_failed == 1) "Breaks an SDTM rule more often than the source" else
          sprintf("Breaks %d SDTM rules more often than the source", r$coherence_failed),
        problem(b$coherence, r$dataset)
      )
    }
    if (length(what)) {
      data.frame(dataset = r$dataset, what = what[1], detail = what[2])
    }
  })
  do.call(rbind, rows)
}

decision_table <- function(x) {
  if (!NROW(x)) {
    x <- data.frame(
      dataset = character(),
      variable = character(),
      decision = character(),
      detail = character(),
      share = numeric(),
      records = numeric(),
      by = character()
    )
  }
  decision_color <- function(d) {
    switch(
      d,
      Calculated = "warning",
      `Rule rejected` = NA,
      `New identifier` = "info",
      "secondary"
    )
  }
  html <- function(x, f) {
    vapply(x, function(v) as.character(f(v)), "", USE.NAMES = FALSE)
  }
  data.frame(
    Dataset = x$dataset,
    Variable = x$variable,
    Decision = html(x$decision, function(d) badge(decision_color(d), d)),
    Detail = html(x$detail, function(d) span(class = "truncate", d)),
    Reproduced = vapply(x$share, share_bar, "", USE.NAMES = FALSE),
    Records = x$records,
    By = x$by,
    stringsAsFactors = FALSE
  )
}
