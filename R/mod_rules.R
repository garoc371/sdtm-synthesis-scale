# A dataset's rules: what the policy decided, with the evidence, and changes by hand. A
# rule changed here is kept as set (the policy no longer decides it) and the dataset is
# rebuilt alone. The card's parts are modules: the list of rules and its buttons
# (mod_rules_list.R), the selected rule (mod_rules_detail.R) and the editor
# (mod_rules_editor.R).

rules_ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header(rule_list_header(ns("list"))),
    rule_list_ui(ns("list")),
    rule_detail_ui(ns("detail"))
  )
}

# `dataset`: the open dataset; `study` and `share`: the study and the policy's share. Returns
# the dataset with its rules changed, as an event.
rules_server <- function(id, dataset, study, share) {
  moduleServer(id, function(input, output, session) {
    rules <- rule_list_server("list", dataset, study, share)
    edited <- rule_detail_server("detail", rules$rule, dataset, study)
    saved <- rule_editor_server("editor", merge_events(rules$edit, rules$add), dataset, study)
    merge_events(rules$changed, edited, saved)
  })
}

# A dataset with its rules changed and its starting graph rebuilt, so that a cycle is refused
# at once: NULL, after a notification, when the rules would create one.
with_rules <- function(study, ds) {
  ds <- refresh_dataset(study, ds)
  if (inherits(ds, "error")) {
    notify(paste("That would create a cycle:", conditionMessage(ds)), "error")
    return(NULL)
  }
  ds
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
