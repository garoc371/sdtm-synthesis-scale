# A dataset's Graph and Variables tabs: what each module returns, on the pilot study's DM.

dataset_study <- reactives[c("dataset", "study")]

test_that("a node applied in the panel is kept as set", {
  testServer(node_panel_server, args = c(list(node = reactive("AGE")), dataset_study), {
    expect_match(output$node$html, "Predictors", fixed = TRUE)
    session$setInputs(type = "blank", apply = 1)
    node <- session$returned()$nodes$AGE
    expect_equal(node$type, "blank")
    expect_true(node$manual)
  })
})

test_that("the graph is drawn and rebuilt", {
  testServer(graph_server, args = dataset_study, {
    expect_match(output$graph$html, "digraph G", fixed = TRUE)
    session$setInputs(rebuild = 1)
    expect_equal(session$returned()$name, "DM")
  })
})

test_that("the node clicked stays selected while its dataset changes", {
  dataset <- reactiveVal(dm)
  testServer(graph_server, args = list(dataset = dataset, study = reactive(study)), {
    session$setInputs(node_click = "AGE")
    dataset(modifyList(dm, list(order = rev(dm$order))))
    session$flushReact()
    expect_equal(node(), "AGE")
    dataset(modifyList(dm, list(name = "XX")))
    session$flushReact()
    expect_null(node())
  })
})

test_that("types are set on the variables selected", {
  testServer(types_server, args = dataset_study, {
    expect_match(output$remaining$html, ">7<", fixed = TRUE)
    session$setInputs(variables_rows_selected = c(4, 5), set_blank = 1)
    nodes <- session$returned()$nodes[c("AGE", "SEX")]
    expect_equal(unname(vapply(nodes, `[[`, "", "type")), c("blank", "blank"))
  })
})

test_that("settings save a new order of the sampled variables", {
  testServer(settings_server, args = dataset_study, {
    expect_match(output$settings$html, "Order of sampled variables", fixed = TRUE)
    session$setInputs(order = rev(dm$order), save = 1)
    expect_equal(session$returned()$order[1], "ETHNIC")
  })
})

test_that("the generation order starts with the records", {
  x <- order_table(dm, NULL)$x$data
  expect_equal(x$Variable[1:2], c("records", dm$order[1]))
})

test_that("the Variables tab returns the dataset any card changes", {
  testServer(variables_server, args = dataset_study, {
    session$setInputs(`types-variables_rows_selected` = 3, `types-set_identifier` = 1)
    expect_equal(session$returned()$nodes$SITEID$type, "identifier")
  })
})
