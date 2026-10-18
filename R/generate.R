# Generation: subjects, then records, then values in graph order.
#
# `generate_study()` needs only the specification, fits and spine (see `study_bundle()`),
# not the source data, so a saved bundle generates new data without Shiny.

# Each synthetic subject's spine is generated first (`compose_spine()` in spine.R): their
# records in every dataset, with all dates, visits and record keys. Datasets are then filled
# in the specification's order, the order their inputs assume.
# R's default generator is set with the seed: background workers default to another one,
# and the same seed must give the same data in the app, a worker or a script.
generate_study <- function(study, n, seed = 1) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  ids <- sprintf("SYN-%0*d", max(4, nchar(n)), seq_len(n))
  spine <- compose_spine(study, n, ids)
  built <- list()
  for (name in intersect(names(study$datasets), names(study$fits))) built[[name]] <- generate_dataset(study, name, built, spine)
  list(data = built, n = n, seed = seed)
}

# A dataset's records are its spine; its other values are drawn in graph order. Datasets not
# generated yet are read from the spine (DM's dates and SV's visits give the spine features).
# Records repeating the define.xml's keys are dropped where no source record repeats them.
# Records are sorted by subject, record keys and the dataset's main date (--STDTC or
# --DTC). Rules that depend on record order (sequence numbers, and record selections when
# dates tie), and the rules calculated from them, are redone after sorting.
generate_dataset <- function(study, name, built, spine) {
  ds <- study$datasets[[name]]
  fits <- study$fits[[name]]
  s <- ds$structure
  data <- spine
  data[names(built)] <- built
  if (!is.null(built$DM)) data$DM <- cbind(built$DM, spine$DM[match(built$DM$USUBJID, spine$DM$USUBJID), startsWith(names(spine$DM), "T."), drop = FALSE])
  data[[name]] <- spine[[name]]
  frame <- dataset_frame(study, name, data)
  width <- max(4, nchar(nrow(spine$DM)))
  nodes <- generation_order(ds)
  for (v in nodes) frame[[v]] <- switch(node_type(ds, v),
    key = frame[[v]],
    rule = RULE_OPS[[ds$rules[[v]]$op]]$apply(ds$rules[[v]], frame),
    identifier = if (v == "USUBJID") frame$USUBJID else if (name == "DM")
      sprintf("%0*d", width, seq_len(nrow(frame))) else as.character(stats::ave(seq_len(nrow(frame)), frame$USUBJID, FUN = seq_along)),
    sampled = draw_node(ds, v, fits$nodes[[v]], frame))
  if (length(ds$unique_keys) && all(ds$unique_keys %in% names(frame))) frame <- frame[!duplicated(frame[ds$unique_keys]), , drop = FALSE]
  columns <- intersect(c("USUBJID", s$keys, main_date(ds$prefix, names(frame))), names(frame))
  frame <- frame[do.call(order, unname(lapply(frame[columns], function(v) if (is.character(v)) v else as.numeric(v)))), , drop = FALSE]
  ordered <- Filter(function(r) r$op %in% c("sequence", "record_flag"), accepted_rules(ds))
  frame[names(Filter(function(r) r$op == "sequence", ordered))] <- NULL
  # Tests calculated from other records of their group replace what the variable's rule or
  # model gave; rules reading a renumbered or recalculated variable are applied again.
  calculated <- vapply(Filter(function(r) r$op == "group", accepted_rules(ds)), `[[`, "", "target")
  for (v in intersect(nodes, descendants(ds, c(names(ordered), calculated)))) {
    if (node_type(ds, v) == "rule") frame[[v]] <- rule_values(ds, ds$rules[[v]], frame) else
      if (v %in% calculated) frame[[v]] <- RULE_OPS$group$apply(group_rule(ds, v), frame)
  }
  frame <- sdtm_results(frame, ds$prefix, fits$nodes[[paste0(ds$prefix, "STRESN")]]$missing_text, s$test)
  for (v in setdiff(ds$variables, names(frame))) frame[[v]] <- switch(ds$kinds[[v]],
    date = as.Date(rep(NA, nrow(frame))), numeric = rep(NA_real_, nrow(frame)), rep(NA_character_, nrow(frame)))
  rownames(frame) <- NULL
  frame[ds$variables]
}

# SDTMIG result conventions: a test not done (--STAT) has no result and only it has a
# reason (--REASND); --STRESN is --STRESC as a number, or --STRESC is --STRESN as text where
# --STRESC is not generated (in full, and a missing number as the source's text for it in
# its test: `missing_text`, from the --STRESN fit).
sdtm_results <- function(frame, prefix, missing_text = NULL, test = NULL) {
  p <- function(v) paste0(prefix, v)
  not_done <- rep(FALSE, nrow(frame))
  if (p("STAT") %in% names(frame)) {
    not_done <- frame[[p("STAT")]] %in% "NOT DONE"
    for (v in intersect(p(c("ORRES", "ORRESU", "STRESC", "STRESN", "STRESU")), names(frame))) frame[[v]][not_done] <- NA
    if (p("REASND") %in% names(frame)) frame[[p("REASND")]][!not_done] <- NA
  }
  if (all(p(c("STRESC", "STRESN")) %in% names(frame)))
    frame[[p("STRESN")]] <- suppressWarnings(as.numeric(as.character(frame[[p("STRESC")]]))) else if (p("STRESN") %in% names(frame)) {
    x <- frame[[p("STRESN")]]
    by <- if (identical(names(missing_text), "")) rep("", nrow(frame)) else if (!is.null(test) && test %in% names(frame)) format_value(frame[[test]])
    text <- if (length(missing_text) && !is.null(by)) unname(missing_text[by]) else rep(NA_character_, nrow(frame))
    frame[[p("STRESC")]] <- ifelse(!is.na(x), as.character(x), ifelse(not_done, NA, text))
  }
  frame
}

# ---- Saving -------------------------------------------------------------------------

# Everything generation needs: the graphs and fits, the spine (the source's timing and
# record keys, shifted, with subjects numbered, which new spines are composed from), how SUPP-- variables unfold, and the trial
# design datasets, which hold no subject data.
study_bundle <- function(study, seed = 1) {
  list(datasets = study$datasets, fits = study$fits, spine = study$spine, schedule = study$schedule, supp = study_supp(study),
    design = study_design(study), define = study$source$define, seed = seed, created = Sys.time())
}

study_supp <- function(study) study$supp %||% study$source$supp

study_design <- function(study) study$design %||% study$source$design

# The files a submission holds: generated datasets with their SUPP-- datasets unfolded
# after each parent, then the trial design datasets as they are in the source.
output_datasets <- function(study, generated) c(unfold_supp(generated$data, study_supp(study)), study_design(study))

# SDTM files hold ISO 8601 dates and empty text for missing values.
write_outputs <- function(study, generated, directory, formats = c("xpt", "csv"))
  write_datasets(output_datasets(study, generated), directory, formats)

write_datasets <- function(data, directory, formats = c("xpt", "csv")) {
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  files <- character()
  for (name in names(data)) {
    x <- data[[name]]
    x[] <- lapply(x, function(v) if (is.numeric(v)) v else ifelse(is.na(v), "", format_value(v)))
    for (format in formats) {
      path <- file.path(directory, paste0(tolower(name), ".", format))
      if (format == "xpt") haven::write_xpt(x, path, version = 5, name = name) else utils::write.csv(x, path, row.names = FALSE)
      files <- c(files, path)
    }
  }
  files
}
