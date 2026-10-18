# The dataset's settings: whether it is generated, its inputs from DM, earlier datasets and
# the spine, and the order of its sampled variables.

settings_ui <- function(id) {
  card(card_header("Dataset settings"), uiOutput(NS(id, "settings")))
}

# Returns the dataset with the settings saved, as an event.
settings_server <- function(id, dataset, study) {
  moduleServer(id, function(input, output, session) {
    # Redrawn with the dataset, not with every build of another one, to keep what is entered.
    output$settings <- renderUI({
      ds <- req(dataset())
      settings_form(session$ns, ds, input_choices(isolate(study()), ds$name))
    })

    saved <- new_event()
    observeEvent(
      input$save,
      guarded({
        ds <- dataset()
        if (ds$name != "DM") {
          ds$include <- isTRUE(input$include)
          ds$inputs <- as.character(input$inputs)
        }
        order <- as.character(input$order)
        if (length(order) && !identical(order, ds$order)) {
          ds <- starting_graph(study(), ds, order)
        }
        saved$send(ds)
        notify("Dataset settings saved.")
      })
    )
    saved$value
  })
}

# DM is always generated and has no inputs.
settings_form <- function(ns, ds, choices) {
  dm <- ds$name == "DM"
  tagList(
    if (!dm) checkboxInput(ns("include"), "Generate this dataset", ds$include),
    p(strong("Records "), record_structure_label(ds)),
    if (!dm) {
      selectizeInput(
        ns("inputs"),
        "Inputs from DM, earlier datasets and the spine",
        setNames(choices, display_name(choices)),
        ds$inputs,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    },
    selectizeInput(
      ns("order"),
      "Order of sampled variables",
      ds$order,
      ds$order,
      multiple = TRUE,
      options = list(plugins = list("drag_drop", "remove_button"))
    ),
    hint("Drag to reorder and save: each sampled variable is then predicted by the informative ones before it."),
    actionButton(ns("save"), "Save settings", class = "btn-sm btn-primary")
  )
}
