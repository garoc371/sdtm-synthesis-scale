built <- list(stage = "built", fit = 1, discover = 0.5, cached = FALSE, records = 306)

test_that("the state keeps the open dataset's failed checks", {
  checks <- data.frame(Dataset = "DM", Status = c("failed", "passed"), Check = c("a", "b"))
  b <- list(info = list(DM = built), manual = "DM", checks = list(DM = checks))
  x <- dataset_state(b, dm)
  expect_true(x$manual)
  expect_equal(x$spec$Check, "a")
  expect_null(x$coherence)
  expect_null(dataset_state(b, NULL))
})

test_that("a failed build is a problem", {
  x <- list(info = list(stage = "error", message = "no rows"))
  expect_match(as.character(dataset_problems(x)), "Build failed")
  expect_equal(as.character(dataset_problems(list(info = built))), "")
})

test_that("the header sends a dataset changed by hand back to the policy", {
  state <- reactive(list(info = built, manual = TRUE))
  testServer(dataset_header_server, args = list(dataset = reactive(dm), state = state), {
    expect_match(output$title$html, "Back to the policy")
    session$setInputs(reset = 1)
    expect_equal(session$returned(), "DM")
  })
})

test_that("the comparison waits for the generated data", {
  generated <- reactiveVal(NULL)
  s <- study
  s$datasets$DM <- dm
  args <- list(dataset = reactive(dm), study = reactive(s), generated = generated)
  testServer(compare_server, args = args, {
    expect_match(output$compare$html, "Not generated yet")
    generated(list(data = list(DM = s$source$data$DM)))
    session$flushReact()
    expect_match(output$compare$html, "306 synthetic records")
    expect_true("AGE" %in% marginals()$Variable)
    expect_match(output$trajectory_inputs$html, "need a dataset with visits")
  })
})

test_that("the dataset page returns the changes of its tabs", {
  build <- list(
    study = reactive(study),
    share = reactive(1),
    state = reactive(NULL),
    generated = reactive(NULL)
  )
  testServer(dataset_server, args = list(dataset = reactive(dm), build = build), {
    session$setInputs(`variables-types-variables_rows_selected` = 3)
    session$setInputs(`variables-types-set_identifier` = 1)
    expect_equal(session$returned$changed()$nodes$SITEID$type, "identifier")
  })
})
