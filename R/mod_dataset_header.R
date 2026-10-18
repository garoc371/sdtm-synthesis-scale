# The open dataset's title, where its build stands, and what fails: failed checks of the
# graph (the dataset is then left out of generation), a failed build, and the SDTM rules the
# generated data break.

dataset_header_ui <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("title")), uiOutput(ns("problems")))
}

# `state`: the dataset's build and failed checks (dataset_state()). Returns the name of the
# dataset to build by the policy again, as an event.
dataset_header_server <- function(id, dataset, state) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderUI({
      dataset_title(session$ns, req(dataset()), req(state()))
    })
    output$problems <- renderUI(dataset_problems(req(state())))
    eventReactive(input$reset, dataset()$name)
  })
}

# The dataset's name, stage and size, with a way back to the policy once changed by hand.
dataset_title <- function(ns, ds, x) {
  i <- x$info
  built <- if (is.finite(i$fit)) {
    sprintf("built in %.1f s%s", i$discover + i$fit, if (i$cached) " (cached)" else "")
  }
  about <- c(
    CLASSES[[ds$class]],
    sprintf("%s records", big(i$records)),
    record_structure_label(ds),
    built
  )
  div(
    class = "page-title",
    h2(ds$name),
    stage_badge(i$stage),
    if (x$manual) badge("info", "Changed by hand"),
    if (x$manual) {
      actionButton(
        ns("reset"),
        "Back to the policy",
        class = "btn-sm btn-outline-secondary",
        icon = icon("rotate-left")
      )
    },
    muted(paste(about, collapse = " · "))
  )
}

# What leaves the dataset out of generation, then the SDTM rules it breaks.
dataset_problems <- function(x) {
  problems <- rbind(
    if (x$info$stage == "error") {
      data.frame(Check = "Build failed", Problems = x$info$message)
    },
    x$spec[c("Check", "Problems")]
  )
  rules <- x$coherence
  tagList(
    if (NROW(problems)) {
      div(
        class = "alert alert-danger",
        strong("Left out of the generated data until this is fixed."),
        tags$ul(Map(
          function(check, text) tags$li(strong(check), text),
          problems$Check,
          problems$Problems
        ))
      )
    },
    if (NROW(rules)) {
      div(
        class = "alert alert-danger",
        strong(paste0(plural(nrow(rules), "SDTM rule"), " the source keeps, broken in the generated data")),
        lapply(seq_len(nrow(rules)), function(i) broken_rule(rules[i, ]))
      )
    }
  )
}

# One broken SDTM rule: how many generated records break it, what to do and the first records.
broken_rule <- function(r) {
  category <- CHECK_CATEGORIES[[r$Category]]
  div(
    class = "broken-rule",
    div(class = "broken-rule-head", strong(r$Check)),
    p(
      sprintf(
        "%s of %s generated records; the source keeps it in all %s of its records. %s rule: %s.",
        big(r$Failed),
        big(r$Checked),
        big(r$SourceChecked),
        category$label,
        category$about
      ),
      category$fix
    ),
    hint("Generated: ", r$Problems)
  )
}
