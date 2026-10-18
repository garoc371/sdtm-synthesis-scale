# The study overview's modules: what each shows, and what it returns.

status_rows <- function() {
  data.frame(
    dataset = c("AE", "LB"), class = c("events", "findings"), records = c(10, 20),
    stage = c("built", "failed"), cached = c(TRUE, FALSE), discover = 1, fit = 1,
    message = "", manual = FALSE, spec_failed = c(0, 2), coherence_failed = 0,
    stringsAsFactors = FALSE
  )
}

test_that("a click on a tile is returned", {
  testServer(
    tiles_server,
    args = list(
      status = reactive(status_rows()),
      parents = reactive(c(SUPPAE = "AE")),
      design = reactive("TV")
    ),
    {
      html <- output$tiles$html
      expect_match(html, "+SUPPAE", fixed = TRUE)
      expect_match(html, "2 failed checks", fixed = TRUE)
      expect_match(html, "copied unchanged: TV", fixed = TRUE)
      session$setInputs(open = "LB")
      expect_equal(session$returned(), "LB")
    }
  )
})

test_that("the attention count follows its rows", {
  rows <- data.frame(dataset = "LB", what = "Build failed", detail = "")
  testServer(
    attention_server,
    args = list(attention = reactive(list(rows = rows, checks = NULL, built = 1, done = FALSE))),
    {
      expect_match(output$count$html, ">1<", fixed = TRUE)
      expect_match(output$list$html, "Build failed", fixed = TRUE)
      session$setInputs(open = "LB")
      expect_equal(session$returned(), "LB")
    }
  )
})

test_that("generation settings are returned settled and cleaned", {
  testServer(
    generation_server,
    args = list(schedule = reactive(NULL), outputs = reactive(NULL), bundle = reactive(NULL)),
    {
      session$setInputs(policy = 95, n_subjects = 120.4, seed = 3)
      session$elapse(600)
      expect_equal(session$returned$share(), 0.95)
      expect_equal(session$returned$sample(), c(n = 120, seed = 3))
      session$setInputs(seed = NA)
      session$elapse(600)
      expect_error(session$returned$sample(), class = "shiny.silent.error")
    }
  )
})

test_that("a decision row opens its dataset", {
  ledger <- data.frame(
    dataset = c("AE", "LB"), variable = "X", decision = "Calculated", detail = "",
    share = 0.95, records = 3, by = "policy"
  )
  attr(ledger, "unchanged") <- 0
  testServer(decisions_server, args = list(ledger = reactive(ledger)), {
    expect_match(output$title$html, "2 change generated records, in 2 datasets", fixed = TRUE)
    session$setInputs(table_rows_selected = 2)
    expect_equal(session$returned(), "LB")
  })
})

test_that("the board returns the dataset any card asks to open", {
  view <- list(
    stages = reactive(NULL), status = reactive(status_rows()), parents = reactive(character()),
    design = reactive(character()), attention = reactive(NULL), schedule = reactive(NULL),
    outputs = reactive(NULL), bundle = reactive(NULL), ledger = reactive(NULL)
  )
  testServer(board_server, args = list(view = view), {
    opened <- reactiveVal(character())
    observeEvent(session$returned$open(), opened(c(opened(), session$returned$open())))
    session$setInputs(`tiles-open` = "AE")
    session$setInputs(`attention-open` = "AE")
    session$setInputs(`tiles-open` = "LB")
    expect_equal(opened(), c("AE", "AE", "LB"))
  })
})
