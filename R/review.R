# Review of generated data: coherence checks that must hold exactly, and comparisons with
# the source that show how closely the models reproduce it.

# Each check lists the failing record numbers of its dataset.
coherence_checks <- function(study, generated) {
  data <- generated$data
  rows <- list(check_row("DM", "One record per subject", which(duplicated(data$DM$USUBJID))))
  for (name in names(data)) {
    ds <- study$datasets[[name]]
    x <- data[[name]]
    frame <- dataset_frame(study, name, data)
    sequence <- names(Filter(function(r) r$op == "sequence", accepted_rules(ds)))
    keys <- intersect(c("USUBJID", if (isTRUE(ds$structure$unique)) ds$structure$keys else sequence[1]), names(x))
    starts <- grep("STDTC$", names(x), value = TRUE)
    ends <- sub("STDTC$", "ENDTC", starts)
    subjects <- unique(x$USUBJID)
    fixed <- ds$fixed
    rows <- c(rows,
      if (name != "DM") list(check_row(name, "Every subject is in DM", which(!x$USUBJID %in% data$DM$USUBJID))),
      if (name != "DM" && length(keys) > 1) list(check_row(name, paste("Unique", paste(keys, collapse = " + ")), which(duplicated(x[keys])))),
      # Problems here name subjects, not records.
      if (length(fixed)) list(check_row(name, paste("Every subject has", paste(fixed$records, fixed$variable, fixed$value, collapse = "; ")),
        unlist(lapply(seq_len(nrow(fixed)), function(i) {
          n <- tabulate(match(x$USUBJID[format_value(x[[fixed$variable[i]]]) %in% fixed$value[i]], subjects), length(subjects))
          sprintf("%s has %d %s", subjects, n, fixed$value[i])[n != fixed$records[i]]
        })))),
      lapply(accepted_rules(ds), function(rule) check_row(name, paste0(rule$target, " = ", rule_label(rule)),
        which(!same_value(tryCatch(RULE_OPS[[rule$op]]$apply(rule, frame), error = function(e) rep(NA, nrow(frame))), x[[rule$target]])))),
      Map(function(start, end) check_row(name, sprintf("%s on or before %s", start, end), which(x[[start]] > x[[end]])),
        starts[ends %in% names(x)], ends[ends %in% names(x)]),
      implication_checks(study, name, x, frame))
  }
  # Supplemental qualifiers are generated as columns of their parent: once unfolded they
  # must point to parent records. Trial design datasets are copied unchanged.
  output <- output_datasets(study, generated)
  design <- study_design(study)
  rows <- c(rows, list(supp_checks(output, study_supp(study))),
    lapply(names(design), function(n) check_row(n, "Copied unchanged", if (!identical(output[[n]], design[[n]])) 1L)))
  do.call(rbind, rows)
}

# Implications of sampled categories (see spine.R), at the node's level rows.
implication_checks <- function(study, name, x, frame) {
  ds <- study$datasets[[name]]
  fits <- study$fits[[name]]$nodes
  lapply(names(Filter(function(f) !is.null(f$implications), fits)), function(v) {
    level <- if (name == "DM") "record" else ds$levels[[v]]
    rows <- level_rows(frame, level, ds$structure)
    check_row(name, sprintf("%s keeps its %d implications", v, nrow(fits[[v]]$implications)),
      rows[implication_violations(fits[[v]]$implications, x[[v]][rows], frame[rows, , drop = FALSE])])
  })
}

# ---- Comparisons ----------------------------------------------------------------------

summarise_values <- function(v) {
  missing <- mean(is.na(v))
  observed <- v[!is.na(v)]
  if (!length(observed)) return("all missing")
  text <- if (inherits(v, "Date")) sprintf("median %s (%s to %s)", format(stats::median(observed)),
    format(min(observed)), format(max(observed)))
  else if (is.numeric(v)) sprintf("mean %s, SD %s", signif(mean(observed), 3), signif(stats::sd(observed), 3))
  else {
    counts <- sort(table(observed), decreasing = TRUE)
    top <- utils::head(counts, 3)
    paste0(paste(sprintf("%s %.0f%%", names(top), 100 * top / length(observed)), collapse = ", "),
      if (length(counts) > 3) sprintf(" (+%d more)", length(counts) - 3) else "")
  }
  if (missing > 0) paste0(text, sprintf("; %.0f%% missing", 100 * missing)) else text
}

compare_marginals <- function(study, source, generated, name) {
  ds <- study$datasets[[name]]
  variables <- ds$variables[vapply(ds$variables, function(v) node_type(ds, v) %in% c("sampled", "rule", "key"), TRUE)]
  data.frame(Variable = variables, Type = vapply(variables, function(v) node_type(ds, v), ""),
    Source = vapply(variables, function(v) summarise_values(source[[v]]), ""),
    Synthetic = vapply(variables, function(v) summarise_values(generated[[v]]), ""),
    Records = sprintf("%d / %d", nrow(source), nrow(generated)), stringsAsFactors = FALSE, row.names = NULL)
}

# Mean value per visit and arm for one test.
trajectory_data <- function(study, data, name, variable, test, arm) {
  ds <- study$datasets[[name]]
  s <- ds$structure
  x <- data[[name]]
  if (!is.null(s$test)) x <- x[format_value(x[[s$test]]) == test, , drop = FALSE]
  value <- parse_number(x[[variable]])
  group <- if (!is.na(arm)) level_string(data$DM[[arm]][match(x$USUBJID, data$DM$USUBJID)]) else rep("all", nrow(x))
  keep <- !is.na(value) & !is.na(x[[s$visit]])
  if (!any(keep)) return(NULL)
  out <- stats::aggregate(list(mean = value[keep]), list(visit = as.numeric(x[[s$visit]][keep]), arm = group[keep]), mean)
  out$n <- stats::aggregate(value[keep], list(as.numeric(x[[s$visit]][keep]), group[keep]), length)$x
  out
}

# Records per subject by arm, for event and history datasets.
event_rate_data <- function(data, name, arm) {
  dm <- data$DM
  counts <- tabulate(match(data[[name]]$USUBJID, dm$USUBJID), nrow(dm))
  group <- if (!is.na(arm)) level_string(dm[[arm]]) else rep("all", nrow(dm))
  stats::aggregate(list(rate = counts), list(arm = group), mean)
}
