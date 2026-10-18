# The editor of one rule, in a modal: its target, operation, inputs and parameters, with what
# the rule reproduces as it is changed. A saved rule is accepted and set by hand.

# `open`: the rule to edit, as an event. Returns the dataset with the saved rule, as an event.
rule_editor_server <- function(id, open, dataset, study) {
  moduleServer(id, function(input, output, session) {
    observeEvent(open(), showModal(rule_modal(session$ns, open(), dataset(), study())))
    output$op_help <- renderUI(hint(RULE_OPS[[req(input$op)]]$help))

    # Constants and date offsets keep the entered value; other parameters are learned.
    edited <- reactive({
      req(input$target, input$op)
      op <- input$op
      frame <- dataset_frame(study(), dataset()$name)
      params <- switch(
        op,
        constant = list(value = parse_like(input$value, frame[[input$target]]), edited = TRUE),
        date_offset = list(days = as.integer(input$days %||% 0), edited = TRUE),
        expression = list(text = input$text),
        list()
      )
      inputs <- if (op == "expression") {
        intersect(all.vars(parse(text = input$text)), names(frame))
      } else {
        input$inputs
      }
      rule <- new_rule(input$target, op, as.character(inputs), params, origin = "user")
      evaluate_rule(rule, frame)
    })
    attempt <- function() {
      tryCatch(edited(), error = function(e) list(error = conditionMessage(e)))
    }
    output$preview <- renderUI(rule_preview(attempt()))

    saved <- new_event()
    observeEvent(input$save, {
      rule <- attempt()
      if (!is.null(rule$error)) {
        return(notify(rule$error, "error"))
      }
      ds <- dataset()
      ds$rules[[rule$target]] <- modifyList(rule, list(status = "accepted", manual = TRUE))
      ds <- with_rules(study(), ds)
      if (!is.null(ds)) {
        saved$send(ds)
        removeModal()
      }
    })
    saved$value
  })
}

# A constant rule for the first column that is not a key, to start a new rule from.
blank_rule <- function(ds) {
  rule <- new_rule(setdiff(ds$variables, c("USUBJID", ds$structure$keys))[1], "constant")
  rule$origin <- NULL
  rule
}

rule_modal <- function(ns, rule, ds, study) {
  columns <- setdiff(names(dataset_frame(study, ds$name)), "USUBJID")
  targets <- setdiff(ds$variables, c("USUBJID", ds$structure$keys))
  ops <- setNames(names(RULE_OPS), vapply(RULE_OPS, `[[`, "", "name"))
  p <- rule$params
  only <- function(op, input) {
    conditionalPanel(sprintf("input.op == '%s'", op), ns = ns, input)
  }
  modalDialog(
    title = if (is.null(rule$origin)) "Add rule" else paste("Edit rule ·", rule$target),
    size = "l",
    easyClose = TRUE,
    layout_columns(
      selectInput(ns("target"), "Target", targets, rule$target),
      selectInput(ns("op"), "Operation", ops[ops != "group"], rule$op)
    ),
    selectizeInput(
      ns("inputs"),
      "Inputs",
      columns,
      rule$inputs,
      multiple = TRUE,
      options = list(plugins = list("drag_drop", "remove_button"))
    ),
    uiOutput(ns("op_help")),
    only("constant", textInput(ns("value"), "Value", format_value(p$value %||% ""))),
    only("date_offset", numericInput(ns("days"), "Days after the input date", p$days %||% 0)),
    only(
      "expression",
      textAreaInput(
        ns("text"),
        "R expression",
        p$text %||% "",
        rows = 3,
        placeholder = "e.g. round(AGE / 10) * 10"
      )
    ),
    uiOutput(ns("preview")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("save"), "Save and accept", class = "btn-primary")
    )
  )
}

rule_preview <- function(rule) {
  if (!is.null(rule$error)) {
    return(div(class = "alert alert-warning", rule$error))
  }
  div(
    class = "rule-preview",
    strong(rule_label(rule)),
    HTML(share_bar(rule$share)),
    muted(sprintf("%s checked records", big(rule$checked)))
  )
}
