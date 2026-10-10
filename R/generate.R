# Generation: subjects, then records, then values in graph order.
#
# `generate_study()` needs only the specification, fits and spine (see `study_bundle()`),
# not the source data, so a saved bundle generates new data without Shiny.

# Each synthetic subject copies the spine of one source subject, its donor (see spine.R):
# their records in every dataset, with all dates, visits and record keys. Datasets are then
# filled in the specification's order, the order their inputs assume.
# R's default generator is set with the seed: background workers default to another one,
# and the same seed must give the same data in the app, a worker or a script.
generate_study <- function(study, n, seed = 1) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  ids <- sprintf("SYN-%0*d", max(4, nchar(n)), seq_len(n))
  spine <- draw_spine(study$spine, spine_donors(study$spine, n), ids)
  built <- list()
  for (name in intersect(names(study$datasets), names(study$fits))) built[[name]] <- generate_dataset(study, name, built, spine)
  list(data = built, n = n, seed = seed)
}

# A dataset's records are its spine; its other values are drawn in graph order. Datasets not
# generated yet are read from the spine (DM's dates and SV's visits give the spine features).
# Records are sorted by subject, record keys and the dataset's main date (--STDTC or
# --DTC). Rules that depend on record order (sequence numbers, and record selections when
# dates tie), and the rules calculated from them, are redone after sorting.
generate_dataset <- function(study, name, built, spine) {
  ds <- study$datasets[[name]]
  fits <- study$fits[[name]]
  s <- ds$structure
  data <- spine
  data[names(built)] <- built
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
  columns <- intersect(c("USUBJID", s$keys, main_date(ds$prefix, names(frame))), names(frame))
  frame <- frame[do.call(order, unname(lapply(frame[columns], function(v) if (is.character(v)) v else as.numeric(v)))), , drop = FALSE]
  ordered <- Filter(function(r) r$op %in% c("sequence", "record_flag"), accepted_rules(ds))
  frame[names(Filter(function(r) r$op == "sequence", ordered))] <- NULL
  for (v in intersect(nodes, descendants(ds, names(ordered)))) if (node_type(ds, v) == "rule")
    frame[[v]] <- RULE_OPS[[ds$rules[[v]]$op]]$apply(ds$rules[[v]], frame)
  for (v in setdiff(ds$variables, names(frame))) frame[[v]] <- switch(ds$kinds[[v]],
    date = as.Date(rep(NA, nrow(frame))), numeric = rep(NA_real_, nrow(frame)), rep(NA_character_, nrow(frame)))
  rownames(frame) <- NULL
  frame[ds$variables]
}

# ---- Saving -------------------------------------------------------------------------

# Everything generation needs: the graphs and fits, the spine (the source's timing and
# record keys, shifted, with subjects numbered), how SUPP-- variables unfold, and the trial
# design datasets, which hold no subject data.
study_bundle <- function(study, seed = 1) {
  list(datasets = study$datasets, fits = study$fits, spine = study$spine, supp = study_supp(study), design = study_design(study),
    seed = seed, created = Sys.time())
}

study_supp <- function(study) study$supp %||% study$source$supp

study_design <- function(study) study$design %||% study$source$design

# The files a submission holds: generated datasets with their SUPP-- datasets unfolded
# after each parent, then the trial design datasets as they are in the source.
output_datasets <- function(study, generated) c(unfold_supp(generated$data, study_supp(study)), study_design(study))

# SDTM files hold ISO 8601 dates and empty text for missing values.
write_outputs <- function(study, generated, directory, formats = c("xpt", "csv")) {
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  files <- character()
  data <- output_datasets(study, generated)
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
