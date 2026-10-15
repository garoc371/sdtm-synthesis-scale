# A dataset's rules: what the policy decided, with the evidence, and changes by hand. A
# rule changed here is kept as set (the policy no longer decides it) and the dataset is
# rebuilt alone.

rules_ui <- function(id) {
  ns <- NS(id)
  button <- function(id, label, icon = NULL, disabled = TRUE) {
    actionButton(
      ns(id),
      label,
      icon = icon,
      class = "btn-sm btn-outline-secondary",
      disabled = disabled
    )
  }
  help <- popover(
    tags$button(
      type = "button",
      class = "btn btn-link btn-sm p-0",
      `aria-label` = "How rules are found",
      icon("circle-info")
    ),
    title = "How rules are found",
    p(
      "Every column is searched for a rule that computes it from other columns of the record, its subject (DM.*)",
      "or its record counts (N.*)."
    ),
    p(
      "A rule's reproduced share counts only checked records: records that could contradict it. For a lookup,",
      "those whose key value occurs at least twice; a key seen once fits any table."
    ),
    p(
      sprintf(
        "Found rules need %g%% reproduced, including %g%% of the records the most common value gets wrong.",
        100 * SHARE_TO_PROPOSE,
        100 * EXPLAINED_TO_PROPOSE
      ),
      "The policy then accepts those at or above its share."
    )
  )
  card(
    full_screen = TRUE,
    card_header(
      span(uiOutput(ns("title"), inline = TRUE), help),
      div(
        class = "button-row",
        button("accept", "Accept", icon("check")),
        button("reject", "Reject", icon("xmark")),
        button("policy", "Back to the policy"),
        button("edit", "Edit", icon("pen")),
        button("add", "Add rule", icon("plus"), disabled = FALSE)
      )
    ),
    DTOutput(ns("table"), fill = FALSE),
    uiOutput(ns("detail"))
  )
}

rules_server <- function(id, b, current, change, notify) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Rule changes rebuild the starting graph here, so that a cycle is refused at once.
    save_rules <- function(ds) {
      ds <- refresh_dataset(b$study, ds)
      if (inherits(ds, "error")) {
        return(notify(
          paste("That would create a cycle:", conditionMessage(ds)),
          "error"
        ))
      }
      change(ds)
      TRUE
    }

    output$title <- renderUI({
      ds <- req(current())
      statuses <- rule_statuses(ds)
      manual <- sum(vapply(ds$rules, function(r) isTRUE(r$manual), TRUE))
      tagList(
        "Rules",
        muted(sprintf(
          "%d accepted, %d rejected, %d set by hand",
          sum(statuses == "accepted"),
          sum(statuses == "rejected"),
          manual
        ))
      )
    })

    # Rules set by hand first, then found ones.
    shown <- reactive({
      rules <- req(current())$rules
      origin <- vapply(
        rules,
        function(r) if (isTRUE(r$manual)) "hand" else r$origin %||% "hand",
        ""
      )
      rules[order(
        match(origin, c("hand", "found")),
        -vapply(rules, function(r) r$share %||% 1, 0)
      )]
    })
    output$table <- renderDT({
      rules <- unname(shown())
      escaped <- function(f) {
        vapply(rules, function(r) htmltools::htmlEscape(f(r)), "")
      }
      table <- data.frame(
        Status = vapply(
          rules,
          function(r) as.character(status_badge(r$status)),
          ""
        ),
        Target = escaped(function(r) r$target),
        Rule = escaped(rule_label),
        Inputs = escaped(function(r) {
          paste(display_name(rule_columns(r)), collapse = ", ")
        }),
        Reproduced = vapply(rules, function(r) share_bar(r$share), ""),
        Checked = vapply(rules, function(r) big(r$checked), ""),
        Source = vapply(
          rules,
          function(r) {
            if (identical(r$origin %||% "user", "user")) "added" else "found"
          },
          ""
        ),
        `Set by` = vapply(
          rules,
          function(r) {
            if (isTRUE(r$manual) || identical(r$origin, "user")) {
              "hand"
            } else {
              "policy"
            }
          },
          ""
        ),
        check.names = FALSE
      )
      compact_table(
        table,
        selection = "multiple",
        escape = FALSE,
        empty = "No rules found in this dataset."
      )
    })

    selected <- reactive(names(shown())[input$table_rows_selected])
    # Buttons that act on the selection are enabled while it fits them.
    observe({
      rules <- current()$rules[selected()]
      updateActionButton(session, "accept", disabled = !length(rules))
      updateActionButton(session, "reject", disabled = !length(rules))
      updateActionButton(
        session,
        "edit",
        disabled = length(rules) != 1 || identical(rules[[1]]$op, "group")
      )
      updateActionButton(
        session,
        "policy",
        disabled = !any(vapply(rules, function(r) isTRUE(r$manual), TRUE))
      )
    })

    set_status <- function(status) {
      ds <- current()
      for (t in selected()) {
        ds$rules[[t]]$status <- status
        ds$rules[[t]]$manual <- TRUE
      }
      save_rules(ds)
    }
    observeEvent(input$accept, set_status("accepted"))
    observeEvent(input$reject, set_status("rejected"))
    observeEvent(input$policy, {
      ds <- current()
      for (t in selected()) {
        ds$rules[[t]]$manual <- NULL
      }
      save_rules(resolve_rules(ds, b$share))
    })

    # ---- The selected rule: per test, and its table --------------------------------------

    rule <- reactive({
      req(length(selected()) == 1)
      current()$rules[[selected()]]
    })
    output$detail <- renderUI({
      rule <- rule()
      by_test <- !is.null(rule$by_test) && nrow(rule$by_test) > 1
      table <- rule$op %in%
        c("lookup", "convert") &&
        !is.null(rule$params$table)
      req(by_test || table || !is.null(rule$error))
      div(
        class = "rule-detail",
        if (!is.null(rule$error)) div(class = "alert alert-danger", rule$error),
        layout_columns(
          col_widths = if (by_test && table) c(5, 7) else 12,
          if (by_test) {
            div(
              h6("Reproduced per test"),
              DTOutput(ns("by_test"), fill = FALSE)
            )
          },
          if (table) {
            div(
              h6(
                if (rule$op == "lookup") {
                  "Lookup table"
                } else {
                  "Conversion per group"
                }
              ),
              hint(
                "Double-click a cell to edit it. Edited tables are kept as entered."
              ),
              DTOutput(ns("lookup"), fill = FALSE)
            )
          }
        )
      )
    })
    output$by_test <- renderDT({
      rule <- rule()
      x <- rule$by_test[order(rule$by_test$share), ]
      x$share <- share_text(x$share)
      names(x) <- c("Test", "Checked", "Reproduced")
      if (rule$op == "group") {
        x$Calculation <- vapply(rule$params$derivations, derivation_label, "")[
          x$Test
        ]
      }
      compact_table(x, height = "240px")
    })
    output$lookup <- renderDT({
      x <- req(rule()$params$table)
      for (v in names(x)) {
        if (inherits(x[[v]], "Date")) x[[v]] <- format(x[[v]])
      }
      datatable(
        x,
        rownames = FALSE,
        selection = "none",
        class = "compact",
        editable = "cell",
        options = list(
          dom = if (nrow(x) > 12) "tp" else "t",
          pageLength = 12,
          ordering = FALSE
        )
      )
    })
    observeEvent(
      input$lookup_cell_edit,
      guarded(notify, {
        ds <- current()
        rule <- rule()
        edit <- input$lookup_cell_edit
        column <- names(rule$params$table)[edit$col + 1]
        rule$params$table[[column]][edit$row] <- parse_like(
          edit$value,
          rule$params$table[[column]]
        )
        rule$params$edited <- TRUE
        rule$manual <- TRUE
        ds$rules[[rule$target]] <- evaluate_rule(
          rule,
          dataset_frame(b$study, ds$name)
        )
        change(ds)
      })
    )

    # ---- Rule editor ---------------------------------------------------------------------

    observeEvent(
      input$edit,
      showModal(rule_modal(ns, rule(), current(), b$study))
    )
    observeEvent(input$add, {
      ds <- current()
      rule <- new_rule(
        setdiff(ds$variables, c("USUBJID", ds$structure$keys))[1],
        "constant"
      )
      rule$origin <- NULL
      showModal(rule_modal(ns, rule, ds, b$study))
    })
    output$op_help <- renderUI(hint(RULE_OPS[[req(input$op)]]$help))

    # Constants and date offsets keep the entered value; other parameters are learned.
    edited <- reactive({
      req(input$target, input$op)
      op <- input$op
      frame <- dataset_frame(b$study, current()$name)
      params <- switch(
        op,
        constant = list(
          value = parse_like(input$value, frame[[input$target]]),
          edited = TRUE
        ),
        date_offset = list(days = as.integer(input$days %||% 0), edited = TRUE),
        expression = list(text = input$text),
        list()
      )
      inputs <- if (op == "expression") {
        intersect(all.vars(parse(text = input$text)), names(frame))
      } else {
        input$inputs
      }
      evaluate_rule(
        new_rule(
          input$target,
          op,
          as.character(inputs),
          params,
          origin = "user"
        ),
        frame
      )
    })
    attempt <- function() {
      tryCatch(edited(), error = function(e) list(error = conditionMessage(e)))
    }
    output$preview <- renderUI({
      rule <- attempt()
      if (!is.null(rule$error)) {
        return(div(class = "alert alert-warning", rule$error))
      }
      div(
        class = "rule-preview",
        strong(rule_label(rule)),
        HTML(share_bar(rule$share)),
        muted(sprintf("%s checked records", big(rule$checked)))
      )
    })
    observeEvent(input$save, {
      rule <- attempt()
      if (!is.null(rule$error)) {
        return(notify(rule$error, "error"))
      }
      ds <- current()
      rule$status <- "accepted"
      rule$manual <- TRUE
      ds$rules[[rule$target]] <- rule
      if (isTRUE(save_rules(ds))) removeModal()
    })
  })
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
    title = if (is.null(rule$origin)) {
      "Add rule"
    } else {
      paste("Edit rule ·", rule$target)
    },
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
    only(
      "constant",
      textInput(ns("value"), "Value", format_value(p$value %||% ""))
    ),
    only(
      "date_offset",
      numericInput(ns("days"), "Days after the input date", p$days %||% 0)
    ),
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

# Text entered for a value, read as the type of `like`.
parse_like <- function(text, like) {
  if (is.numeric(like)) {
    parse_number(text)
  } else if (inherits(like, "Date")) {
    as_date(text)
  } else {
    text
  }
}
