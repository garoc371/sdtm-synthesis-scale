# Folding SUPP-- datasets into their parents and unfolding them again.
# Run from the app root: Rscript tests/test_supp.R

file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
if (length(file)) setwd(dirname(dirname(normalizePath(file))))
library(shiny)
library(testthat)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# Rows as text, NA as "", sorted, for comparing as a set.
as_text <- function(x, variables = names(x)) {
  x <- as.data.frame(lapply(x[variables], function(v) ifelse(is.na(v), "", as.character(v))), stringsAsFactors = FALSE)
  x <- x[do.call(order, c(unname(as.list(x)), list(method = "radix"))), , drop = FALSE]
  rownames(x) <- NULL
  x
}

# pharmaversesdtm datasets written as .xpt and read back, so conventions are the app's.
pharma_source <- function() {
  sets <- list(DM = pharmaversesdtm::dm, SUPPDM = pharmaversesdtm::suppdm, AE = pharmaversesdtm::ae,
    SUPPAE = pharmaversesdtm::suppae, TR_ONCO = pharmaversesdtm::tr_onco, SUPPTR_ONCO = pharmaversesdtm::supptr_onco)
  directory <- tempfile("supp")
  dir.create(directory)
  files <- vapply(names(sets), function(name) {
    path <- file.path(directory, paste0(name, ".xpt"))
    haven::write_xpt(sets[[name]], path, version = 8, name = name)
    path
  }, "")
  read_source(files)
}

src <- pharma_source()
original <- src$data
timing <- system.time(folded <- fold_supp(src))[["elapsed"]]
back <- unfold_supp(folded$data, folded$supp)
supp_names <- c("SUPPAE", "SUPPDM", "SUPPTR_ONCO")
cat(sprintf("fold_supp on all three pairs: %.2f s (SUPPTR_ONCO has %d rows)\n", timing, nrow(original$SUPPTR_ONCO)))

test_that("fold removes the SUPP datasets and keeps parent rows", {
  expect_false(any(grepl("^SUPP", names(folded$data))))
  expect_false(any(grepl("^SUPP", names(folded$labels))))
  expect_setequal(names(folded$supp), supp_names)
  for (parent in c("DM", "AE", "TR_ONCO")) expect_equal(nrow(folded$data[[parent]]), nrow(original[[parent]]))
  for (name in supp_names) {
    expect_equal(folded$supp[[name]]$orphans, 0L)
    expect_equal(folded$supp[[name]]$duplicates, 0L)
    expect_equal(folded$supp[[name]]$variables, names(original[[name]]))
    expect_equal(folded$supp[[name]]$empty, sum(is.na(original[[name]]$QVAL)))
  }
  # SUPPTR_ONCO has records without a QVAL: they carry no value and are dropped, not orphaned.
  expect_gt(folded$supp$SUPPTR_ONCO$empty, 0)
})

test_that("folded columns have the right type and label", {
  ae <- folded$data$AE
  expect_true("AETRTEM" %in% names(ae))
  expect_type(ae$AETRTEM, "character")
  expect_equal(folded$labels$AE[["AETRTEM"]], original$SUPPAE$QLABEL[1])
  expect_equal(folded$supp$SUPPAE$idvar[["AETRTEM"]], "AESEQ")
  expect_equal(folded$supp$SUPPAE$kind[["AETRTEM"]], "character")
  expect_equal(sum(!is.na(ae$AETRTEM)), sum(!is.na(original$SUPPAE$QVAL)))
  dm <- folded$data$DM
  flags <- unique(original$SUPPDM$QNAM)
  expect_length(flags, 6)
  expect_true(all(flags %in% names(dm)))
  expect_equal(folded$supp$SUPPDM$columns, setNames(flags, flags))
  expect_true(all(is.na(folded$supp$SUPPDM$idvar)))
  for (flag in flags) {
    expect_equal(sum(!is.na(dm[[flag]])), sum(original$SUPPDM$QNAM == flag))
    expect_equal(folded$labels$DM[[flag]], unique(original$SUPPDM$QLABEL[original$SUPPDM$QNAM == flag]))
  }
  expect_equal(folded$supp$SUPPDM$qeval[[flags[1]]], "CLINICAL STUDY SPONSOR")
  expect_true(is.na(folded$supp$SUPPTR_ONCO$qeval[["TRLOC"]]))
  expect_type(folded$data$TR_ONCO$TRLOC, "character")
  expect_equal(sum(!is.na(folded$data$TR_ONCO$TRLOC)), sum(!is.na(original$SUPPTR_ONCO$QVAL)))
  # Labels stay aligned with the columns.
  for (parent in c("DM", "AE", "TR_ONCO")) expect_equal(names(folded$labels[[parent]]), names(folded$data[[parent]]))
})

test_that("unfolding reproduces each SUPP dataset as a set of rows, less records without a QVAL", {
  expect_equal(names(back)[match(c("AE", "DM", "TR_ONCO"), names(back)) + 1], supp_names[c(1, 2, 3)])
  for (name in supp_names) {
    expect_equal(names(back[[name]]), names(original[[name]]))
    expect_equal(as_text(back[[name]]), as_text(original[[name]][!is.na(original[[name]]$QVAL), ]))
  }
  for (parent in c("DM", "AE", "TR_ONCO")) expect_identical(lapply(back[[parent]], identity), lapply(original[[parent]], identity))
})

test_that("unfolded records are sorted by subject, sequence number and QNAM", {
  x <- back$SUPPAE
  seq <- as.numeric(x$IDVARVAL)
  expect_false(is.unsorted(x$USUBJID, strictly = FALSE))
  within <- x$USUBJID[-1] == x$USUBJID[-nrow(x)]
  expect_true(all(diff(seq)[within] >= 0))
})

test_that("supp_checks passes on the round trip and fails on corrupt records", {
  checks <- supp_checks(back, folded$supp)
  expect_equal(nrow(checks), 6)
  expect_true(all(checks$Status == "ok"))
  expect_true(any(grepl("Every SUPPAE record points to a AE record", checks$Check)))
  bad <- back
  bad$SUPPAE$IDVARVAL[3] <- "99999"
  checks <- supp_checks(bad, folded$supp)
  failed <- checks[checks$Status == "failed", ]
  expect_equal(failed$Dataset, "SUPPAE")
  expect_match(failed$Check, "points to")
  expect_equal(failed$Problems, "3")
  bad <- back
  bad$SUPPDM <- rbind(bad$SUPPDM, bad$SUPPDM[5, ])
  failed <- supp_checks(bad, folded$supp)
  failed <- failed[failed$Status == "failed", ]
  expect_equal(failed$Check, "QNAM is unique per parent record")
  expect_equal(failed$Problems, as.character(nrow(bad$SUPPDM)))
})

test_that("fold_supp on the TR/SUPPTR pair is vectorised", {
  two <- list(data = src$data[c("DM", "TR_ONCO", "SUPPTR_ONCO")], labels = src$labels[c("DM", "TR_ONCO", "SUPPTR_ONCO")])
  elapsed <- system.time(fold_supp(two))[["elapsed"]]
  cat(sprintf("fold_supp on TR_ONCO/SUPPTR_ONCO: %.2f s\n", elapsed))
  expect_lt(elapsed, 3)
})

# ---- A synthetic source -----------------------------------------------------------------

synthetic <- function() {
  text <- function(...) data.frame(..., stringsAsFactors = FALSE)
  supp <- function(rdomain, usubjid, idvar, idvarval, qnam, qlabel, qval) text(STUDYID = "S", RDOMAIN = rdomain,
    USUBJID = usubjid, IDVAR = idvar, IDVARVAL = idvarval, QNAM = qnam, QLABEL = qlabel, QVAL = qval, QORIG = "CRF",
    QEVAL = NA_character_)
  xx <- text(STUDYID = "S", USUBJID = c("S1", "S1", "S1", "S2", "S2", "S3"), XXSEQ = c(1, 2, 3, 1, 2, 1),
    XXGRPID = c("G1", "G1", "G2", "G1", NA, NA), XXTERM = c("a", "b", "c", "d", "e", "f"))
  suppxx <- rbind(
    supp("XX", "S1", "XXGRPID", "G1", "GRPNOTE", "Group note", "shared"),
    supp("XX", "S2", "XXGRPID", "G1", "GRPNOTE", "Group note", "other"),
    supp("XX", "S1", "XXSEQ", "3", "SCORE", "Score", "12.5"),
    supp("XX", "S2", "XXSEQ", "1", "SCORE", "Score", "7"),
    supp("XX", "S1", "XXSEQ", "1", "XXRDTC", "Review date", "2020-03-05"),
    supp("XX", "S1", NA, NA, "SUBJFL", "Subject flag", "Y"),
    supp("XX", "S3", "XXSEQ", "1", "XXTERM", "Extra term", "extra"),
    supp("XX", "S9", "XXSEQ", "1", "SCORE", "Score", "3"))
  list(data = list(DM = text(STUDYID = "S", USUBJID = c("S1", "S2", "S3")), XX = xx, SUPPXX = suppxx,
    SUPPFOO = supp("XX", "S3", "XXSEQ", "1", "ZED", "Zed", "z"),
    LBCH = text(STUDYID = "S", USUBJID = "S1", LBSEQ = 1:2), SUPPLBCH = supp("LB", "S1", "LBSEQ", "2", "LBNOTE", "Note", "n"),
    SUPPNONE = supp("NONE", "S1", NA, NA, "KEEP", "Keep", "k")),
    labels = list(DM = c(STUDYID = "Study", USUBJID = "Subject"),
      XX = c(STUDYID = "Study", USUBJID = "Subject", XXSEQ = "Sequence", XXGRPID = "Group", XXTERM = "Term"),
      LBCH = c(STUDYID = "Study", USUBJID = "Subject", LBSEQ = "Sequence")))
}

test_that("a shared --GRPID, subject-level values, collisions, orphans and fallbacks", {
  folded <- fold_supp(synthetic())
  xx <- folded$data$XX
  entry <- folded$supp$SUPPXX
  expect_equal(entry$orphans, 1L)
  expect_equal(entry$duplicates, 0L)
  expect_equal(entry$parent, "XX")
  expect_equal(nrow(xx), 6)
  expect_equal(xx$GRPNOTE, c("shared", "shared", NA, "other", NA, NA))
  expect_equal(entry$idvar[["GRPNOTE"]], "XXGRPID")
  expect_equal(xx$SCORE, c(NA, NA, 12.5, 7, NA, NA))
  expect_equal(entry$kind[["SCORE"]], "numeric")
  expect_equal(xx$XXRDTC, as.Date(c("2020-03-05", NA, NA, NA, NA, NA)))
  expect_equal(entry$kind[["XXRDTC"]], "date")
  expect_equal(xx$SUBJFL, c("Y", "Y", "Y", NA, NA, NA))
  expect_true(is.na(entry$idvar[["SUBJFL"]]))
  expect_equal(xx$Q.XXTERM, c(rep(NA, 5), "extra"))
  expect_equal(entry$columns[["XXTERM"]], "Q.XXTERM")
  expect_equal(folded$labels$XX[["Q.XXTERM"]], "Extra term")
  expect_equal(folded$labels$XX[["SCORE"]], "Score")
  # SUPPFOO has no FOO parent and falls back to its RDOMAIN, SUPPLBCH to LBCH, SUPPNONE stays.
  expect_equal(folded$supp$SUPPFOO$parent, "XX")
  expect_equal(xx$ZED, c(rep(NA, 5), "z"))
  expect_equal(folded$supp$SUPPLBCH$parent, "LBCH")
  expect_equal(folded$supp$SUPPLBCH$rdomain, "LB")
  expect_equal(folded$data$LBCH$LBNOTE, c(NA, "n"))
  expect_equal(names(folded$data), c("DM", "XX", "LBCH", "SUPPNONE"))
  expect_false("SUPPNONE" %in% names(folded$supp))

  back <- unfold_supp(folded$data, folded$supp)
  expect_equal(names(back), c("DM", "XX", "SUPPXX", "SUPPFOO", "LBCH", "SUPPLBCH", "SUPPNONE"))
  expect_identical(lapply(back$XX, identity), lapply(synthetic()$data$XX, identity))
  expect_equal(nrow(back$SUPPXX), 7)
  original <- synthetic()$data$SUPPXX
  expect_equal(as_text(back$SUPPXX), as_text(original[original$USUBJID != "S9", ]))
  expect_equal(as_text(back$SUPPLBCH), as_text(synthetic()$data$SUPPLBCH))
  # Sequence numbers first (numerically), then other IDVAR values, then the subject level.
  expect_equal(back$SUPPXX$QNAM[back$SUPPXX$USUBJID == "S1"], c("XXRDTC", "SCORE", "GRPNOTE", "SUBJFL"))
  checks <- supp_checks(back, folded$supp)
  expect_true(all(checks$Status == "ok"))
})

test_that("repeated QNAM records on a parent record keep the first and are counted", {
  source <- synthetic()
  source$data$SUPPXX <- rbind(source$data$SUPPXX, within(source$data$SUPPXX[4, ], QVAL <- "8"))
  folded <- fold_supp(source)
  expect_equal(folded$supp$SUPPXX$duplicates, 1L)
  expect_equal(folded$supp$SUPPXX$orphans, 1L)
  expect_equal(folded$data$XX$SCORE[4], 7)
})

test_that("IDVARVAL written differently from the parent number still matches", {
  source <- synthetic()
  source$data$SUPPXX$IDVARVAL[source$data$SUPPXX$QNAM == "SCORE"] <- c("3.0", "1", "1")
  expect_equal(fold_supp(source)$data$XX$SCORE, c(NA, NA, 12.5, 7, NA, NA))
})
