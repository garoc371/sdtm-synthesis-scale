# A dataset's Rules tab: what each module returns, on the pilot study's DM. Its rules are
# STUDYID, DOMAIN, DTHFL (a lookup), AGEU and COUNTRY, all accepted.

study <- load_study(test_path("../../data/pilot/dm.xpt"))
dm <- resolve_rules(discover_dataset(study, "DM")$datasets$DM, 1)
reactives <- list(dataset = reactive(dm), study = reactive(study), share = reactive(1))

test_that("a rule rejected and given back to the policy", {
  testServer(rule_list_server, args = reactives, {
    session$setInputs(table_rows_selected = 3)
    expect_equal(session$returned$rule()$target, "DTHFL")
    session$setInputs(reject = 1)
    rule <- session$returned$changed()$rules$DTHFL
    expect_equal(rule$status, "rejected")
    expect_true(rule$manual)
  })
  dm$rules$DTHFL <- modifyList(dm$rules$DTHFL, list(status = "rejected", manual = TRUE))
  testServer(rule_list_server, args = modifyList(reactives, list(dataset = reactive(dm))), {
    expect_match(output$title$html, "4 accepted, 1 rejected, 1 set by hand", fixed = TRUE)
    session$setInputs(table_rows_selected = 1, policy = 1)
    rule <- session$returned$changed()$rules$DTHFL
    expect_equal(rule$status, "accepted")
    expect_null(rule$manual)
  })
})

test_that("an edited lookup cell is kept as entered", {
  args <- c(list(rule = reactive(dm$rules$DTHFL)), reactives[c("dataset", "study")])
  testServer(rule_detail_server, args = args, {
    expect_match(output$detail$html, "Lookup table", fixed = TRUE)
    session$setInputs(lookup_cell_edit = list(row = 1, col = 1, value = "N"))
    rule <- session$returned()$rules$DTHFL
    expect_equal(rule$params$table[[2]][1], "N")
    expect_true(rule$params$edited)
    expect_true(rule$manual)
  })
})

test_that("a rule added in the editor is saved accepted", {
  args <- c(list(open = reactive(blank_rule(dm))), reactives[c("dataset", "study")])
  testServer(rule_editor_server, args = args, {
    session$setInputs(target = "AGEU", op = "constant", value = "YEARS", inputs = NULL)
    expect_match(output$preview$html, 'Always "YEARS"', fixed = TRUE)
    session$setInputs(save = 1)
    rule <- session$returned()$rules$AGEU
    expect_equal(rule$params$value, "YEARS")
    expect_equal(rule$status, "accepted")
    expect_true(rule$manual)
  })
})

test_that("the Rules tab returns the dataset any part changes", {
  testServer(rules_server, args = reactives, {
    session$setInputs(`list-table_rows_selected` = 2, `list-reject` = 1)
    expect_equal(session$returned()$rules$DOMAIN$status, "rejected")
  })
})
