# A dataset's rules: what the policy decided, with the evidence, and changes by hand. A
# rule changed here is kept as set (the policy no longer decides it) and the dataset is
# rebuilt alone.

rules_tab_ui <- function() bslib::card(full_screen = TRUE,
  bslib::card_header(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
    div(class = "d-flex align-items-center gap-2", uiOutput("rules_title", inline = TRUE),
      bslib::popover(tags$button(type = "button", class = "btn btn-link btn-sm p-0 text-body-secondary", `aria-label` = "How rules are found",
        icon("circle-info")), title = "How rules are found",
        HTML(paste0("Every column is searched for a rule that computes it from other columns of the record, its subject (DM.*) or its record counts (N.*).<br><br>",
          "A rule's <b>reproduced</b> share counts only <b>checked</b> records: records that could contradict it. ",
          "For a lookup, that means records whose key value occurs at least twice. A key seen once fits any table, so it is not evidence. ",
          "Found rules need ", 100 * SHARE_TO_PROPOSE, "% reproduced, including ", 100 * EXPLAINED_TO_PROPOSE,
          "% of the records the most common value gets wrong. The policy then accepts those at or above its share.")))),
    div(class = "btn-toolbar gap-2",
      actionButton("accept_rules", "Accept", class = "btn-sm btn-outline-primary", icon = icon("check"), disabled = TRUE),
      actionButton("reject_rules", "Reject", class = "btn-sm btn-outline-secondary", icon = icon("xmark"), disabled = TRUE),
      actionButton("policy_rules", "Back to the policy", class = "btn-sm btn-outline-secondary", disabled = TRUE),
      actionButton("edit_rule", "Edit", class = "btn-sm btn-outline-secondary", icon = icon("pen"), disabled = TRUE),
      actionButton("add_rule", "Add rule", class = "btn-sm btn-outline-secondary", icon = icon("plus")))),
  DT::DTOutput("rules_table", fill = FALSE),
  uiOutput("rule_detail"))

rules_tab_server <- function(input, output, session, b, current, change, notify) {
  # Rule changes rebuild the starting graph here, so that a cycle is refused at once.
  save_rules <- function(ds) {
    ds <- refresh_dataset(b$study, ds)
    if (inherits(ds, "error")) return(notify(paste("That would create a cycle:", conditionMessage(ds)), "error"))
    change(ds)
    TRUE
  }

  output$rules_title <- renderUI({
    ds <- req(current())
    statuses <- rule_statuses(ds)
    tagList("Rules", span(class = "text-body-secondary small fw-normal ms-2", sprintf("%d accepted, %d rejected, %d set by hand",
      sum(statuses == "accepted"), sum(statuses == "rejected"), sum(vapply(ds$rules, function(r) isTRUE(r$manual), TRUE)))))
  })

  # Rules set by hand first, then found ones.
  shown_rules <- reactive({
    rules <- req(current())$rules
    origin <- vapply(rules, function(r) if (isTRUE(r$manual)) "hand" else r$origin %||% "hand", "")
    rules[order(match(origin, c("hand", "found")), -vapply(rules, function(r) r$share %||% 1, 0))]
  })
  output$rules_table <- DT::renderDT({
    if (!length(shown_rules())) return(compact_table(data.frame(Message = "No rules found in this dataset.")))
    rules <- unname(shown_rules())
    table <- data.frame(
      Status = vapply(rules, function(r) as.character(status_badge(r$status)), ""),
      Target = vapply(rules, `[[`, "", "target"),
      Rule = vapply(rules, function(r) htmltools::htmlEscape(rule_label(r)), ""),
      Inputs = vapply(rules, function(r) htmltools::htmlEscape(paste(display_name(rule_columns(r)), collapse = ", ")), ""),
      Reproduced = vapply(rules, function(r) share_bar(r$share), ""),
      Checked = vapply(rules, function(r) format(r$checked, big.mark = ","), ""),
      Source = vapply(rules, function(r) switch(r$origin %||% "user", user = "added", "found"), ""),
      `Set by` = vapply(rules, function(r) if (isTRUE(r$manual) || identical(r$origin, "user")) "hand" else "policy", ""),
      stringsAsFactors = FALSE)
    compact_table(table, selection = "multiple", escape = FALSE)
  })

  selected_targets <- reactive(names(shown_rules())[input$rules_table_rows_selected])
  # Buttons that act on the selection are enabled while it fits them.
  observe({
    n <- length(selected_targets())
    for (id in c("accept_rules", "reject_rules")) updateActionButton(session, id, disabled = !n)
    updateActionButton(session, "edit_rule", disabled = n != 1 || identical(current()$rules[[selected_targets()[1]]]$op, "group"))
    updateActionButton(session, "policy_rules", disabled = !any(vapply(current()$rules[selected_targets()],
      function(r) isTRUE(r$manual), TRUE)))
  })

  set_status <- function(targets, status) {
    ds <- current()
    for (t in targets) {
      ds$rules[[t]]$status <- status
      ds$rules[[t]]$manual <- TRUE
    }
    save_rules(ds)
  }
  observeEvent(input$accept_rules, set_status(selected_targets(), "accepted"))
  observeEvent(input$reject_rules, set_status(selected_targets(), "rejected"))
  observeEvent(input$policy_rules, {
    ds <- current()
    for (t in selected_targets()) ds$rules[[t]]$manual <- NULL
    save_rules(resolve_rules(ds, b$share))
  })

  output$rule_detail <- renderUI({
    targets <- selected_targets()
    if (length(targets) != 1) return(NULL)
    rule <- current()$rules[[targets]]
    by_test <- !is.null(rule$by_test) && nrow(rule$by_test) > 1
    table <- rule$op %in% c("lookup", "convert") && !is.null(rule$params$table)
    if (!by_test && !table && is.null(rule$error)) return(NULL)
    div(class = "border-top pt-3",
      if (!is.null(rule$error)) div(class = "alert alert-danger py-2", rule$error),
      bslib::layout_columns(col_widths = if (by_test && table) c(5, 7) else 12,
        if (by_test) div(h6("Reproduced per test"), DT::DTOutput("rule_by_test", fill = FALSE)),
        if (table) div(h6(if (rule$op == "lookup") "Lookup table" else "Conversion per group"),
          hint(class = "mb-2", "Double-click a cell to edit it. Edited tables are kept as entered."),
          DT::DTOutput("rule_table", fill = FALSE))))
  })
  output$rule_by_test <- DT::renderDT({
    rule <- current()$rules[[req(selected_targets())[1]]]
    req(rule$by_test)
    x <- rule$by_test[order(rule$by_test$share), ]
    x$share <- share_text(x$share)
    names(x) <- c("Test", "Checked", "Reproduced")
    if (rule$op == "group") x$Calculation <- vapply(rule$params$derivations, derivation_label, "")[x$Test]
    compact_table(x, height = "240px")
  })
  output$rule_table <- DT::renderDT({
    rule <- current()$rules[[req(selected_targets())[1]]]
    x <- req(rule$params$table)
    for (v in names(x)) if (inherits(x[[v]], "Date")) x[[v]] <- format(x[[v]])
    DT::datatable(x, rownames = FALSE, selection = "none", class = "compact", editable = "cell",
      options = list(dom = if (nrow(x) > 12) "tp" else "t", pageLength = 12, ordering = FALSE))
  })
  observeEvent(input$rule_table_cell_edit, guarded(notify, {
    target <- req(selected_targets())[1]
    ds <- current()
    rule <- ds$rules[[target]]
    edit <- input$rule_table_cell_edit
    column <- names(rule$params$table)[edit$col + 1]
    old <- rule$params$table[[column]]
    rule$params$table[[column]][edit$row] <- if (is.numeric(old)) parse_number(edit$value) else
      if (inherits(old, "Date")) as_date(edit$value) else edit$value
    rule$params$edited <- TRUE
    rule$manual <- TRUE
    ds$rules[[target]] <- evaluate_rule(rule, dataset_frame(b$study, ds$name))
    change(ds)
  }))

  # ---- Rule editor --------------------------------------------------------------------

  rule_modal <- function(rule, ds) {
    columns <- setdiff(names(dataset_frame(b$study, ds$name)), "USUBJID")
    targets <- setdiff(ds$variables, c("USUBJID", ds$structure$keys))
    p <- rule$params
    modalDialog(title = if (is.null(rule$origin)) "Add rule" else paste("Edit rule ·", rule$target), size = "l", easyClose = TRUE,
      bslib::layout_columns(col_widths = c(6, 6),
        selectInput("rule_target", "Target", targets, rule$target),
        selectInput("rule_op", "Operation", setNames(names(RULE_OPS), vapply(RULE_OPS, `[[`, "", "name"))[names(RULE_OPS) != "group"], rule$op)),
      selectizeInput("rule_inputs", "Inputs", columns, rule$inputs, multiple = TRUE,
        options = list(plugins = list("drag_drop", "remove_button"))),
      uiOutput("rule_input_help"),
      conditionalPanel("input.rule_op == 'constant'", textInput("rule_value", "Value", format_value(p$value %||% ""))),
      conditionalPanel("input.rule_op == 'date_offset'", numericInput("rule_days", "Days after the input date", p$days %||% 0)),
      conditionalPanel("input.rule_op == 'expression'", textAreaInput("rule_text", "R expression", p$text %||% "", rows = 3,
        placeholder = "e.g. round(AGE / 10) * 10")),
      uiOutput("rule_preview"),
      footer = tagList(modalButton("Cancel"), actionButton("save_rule", "Save and accept", class = "btn-primary")))
  }
  observeEvent(input$edit_rule, {
    ds <- current()
    showModal(rule_modal(ds$rules[[req(selected_targets())]], ds))
  })
  observeEvent(input$add_rule, {
    ds <- current()
    rule <- new_rule(setdiff(ds$variables, c("USUBJID", ds$structure$keys))[1], "constant")
    rule$origin <- NULL
    showModal(rule_modal(rule, ds))
  })
  output$rule_input_help <- renderUI(hint(RULE_OPS[[req(input$rule_op)]]$help))
  # Constants and date offsets keep the entered value; other parameters are learned.
  edited_rule <- reactive({
    req(input$rule_target, input$rule_op)
    op <- input$rule_op
    frame <- dataset_frame(b$study, current()$name)
    y <- frame[[input$rule_target]]
    value <- if (is.numeric(y)) parse_number(input$rule_value) else if (inherits(y, "Date")) as_date(input$rule_value) else input$rule_value
    params <- switch(op,
      constant = list(value = value, edited = TRUE),
      date_offset = list(days = as.integer(input$rule_days %||% 0), edited = TRUE),
      expression = list(text = input$rule_text),
      list())
    inputs <- if (op == "expression") intersect(all.vars(parse(text = input$rule_text)), names(frame)) else
      as.character(input$rule_inputs)
    evaluate_rule(new_rule(input$rule_target, op, inputs, params, origin = "user"), frame)
  })
  output$rule_preview <- renderUI({
    rule <- tryCatch(edited_rule(), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(rule$error)) return(div(class = "alert alert-warning py-2", rule$error))
    div(class = "d-flex flex-wrap align-items-center gap-3 border rounded bg-body-tertiary px-3 py-2 small",
      strong(rule_label(rule)), HTML(share_bar(rule$share)), span(class = "text-body-secondary", sprintf("%s checked records", format(rule$checked, big.mark = ","))))
  })
  observeEvent(input$save_rule, {
    rule <- tryCatch(edited_rule(), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(rule$error)) return(notify(rule$error, "error"))
    ds <- current()
    rule$status <- "accepted"
    rule$manual <- TRUE
    ds$rules[[rule$target]] <- rule
    if (isTRUE(save_rules(ds))) removeModal()
  })
}
