# The dataset's rules as a table, and buttons that accept, reject or give back to the policy
# the rules selected, edit one or add one. Its UI is in two parts: the card header and the
# table.

rule_list_header <- function(id) {
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
  tagList(
    span(uiOutput(ns("title"), inline = TRUE), rules_help()),
    div(
      class = "button-row",
      button("accept", "Accept", icon("check")),
      button("reject", "Reject", icon("xmark")),
      button("policy", "Back to the policy"),
      button("edit", "Edit", icon("pen")),
      button("add", "Add rule", icon("plus"), disabled = FALSE)
    )
  )
}

rule_list_ui <- function(id) {
  DTOutput(NS(id, "table"), fill = FALSE)
}

# Returns the selected rule while one is (`rule`); as events, the rule to edit, a new rule to
# add and the dataset with the rules these buttons set (`changed`).
rule_list_server <- function(id, dataset, study, share) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderUI({
      ds <- req(dataset())
      statuses <- rule_statuses(ds)
      tagList(
        "Rules",
        muted(sprintf(
          "%d accepted, %d rejected, %d set by hand",
          sum(statuses == "accepted"),
          sum(statuses == "rejected"),
          sum(vapply(ds$rules, is_manual, TRUE))
        ))
      )
    })
    shown <- reactive(shown_rules(req(dataset())$rules))
    output$table <- renderDT(rule_table(unname(shown())))

    selected <- reactive(names(shown())[input$table_rows_selected])
    # Buttons that act on the selection are enabled while it fits them.
    observe({
      rules <- dataset()$rules[selected()]
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
        disabled = !any(vapply(rules, is_manual, TRUE))
      )
    })

    changed <- new_event()
    # The selected rules with `values` set (NULL drops one), saved if they make no cycle.
    set_selected <- function(values, resolve = identity) {
      ds <- dataset()
      ds$rules[selected()] <- lapply(ds$rules[selected()], modifyList, values)
      ds <- with_rules(study(), resolve(ds))
      if (!is.null(ds)) changed$send(ds)
    }
    observeEvent(input$accept, set_selected(list(status = "accepted", manual = TRUE)))
    observeEvent(input$reject, set_selected(list(status = "rejected", manual = TRUE)))
    observeEvent(input$policy, {
      set_selected(list(manual = NULL), function(ds) resolve_rules(ds, share()))
    })

    rule <- reactive({
      req(length(selected()) == 1)
      dataset()$rules[[selected()]]
    })
    list(
      rule = rule,
      edit = eventReactive(input$edit, rule()),
      add = eventReactive(input$add, blank_rule(dataset())),
      changed = changed$value
    )
  })
}

is_manual <- function(rule) isTRUE(rule$manual)

# Rules set by hand first, then found ones by their reproduced share.
shown_rules <- function(rules) {
  origin <- vapply(
    rules,
    function(r) if (is_manual(r)) "hand" else r$origin %||% "hand",
    ""
  )
  rules[order(
    match(origin, c("hand", "found")),
    -vapply(rules, function(r) r$share %||% 1, 0)
  )]
}

rule_table <- function(rules) {
  escaped <- function(f) {
    vapply(rules, function(r) htmltools::htmlEscape(f(r)), "")
  }
  added <- function(r) identical(r$origin %||% "user", "user")
  table <- data.frame(
    Status = vapply(rules, function(r) as.character(status_badge(r$status)), ""),
    Target = escaped(function(r) r$target),
    Rule = escaped(rule_label),
    Inputs = escaped(function(r) paste(display_name(rule_columns(r)), collapse = ", ")),
    Reproduced = vapply(rules, function(r) share_bar(r$share), ""),
    Checked = vapply(rules, function(r) big(r$checked), ""),
    Source = vapply(rules, function(r) if (added(r)) "added" else "found", ""),
    `Set by` = vapply(
      rules,
      function(r) if (is_manual(r) || identical(r$origin, "user")) "hand" else "policy",
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
}

rules_help <- function() {
  popover(
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
}
