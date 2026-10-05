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
    pinned <- pinned_records(ds, x, data$DM)
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
      lapply(accepted_rules(ds), function(rule) check_row(name, paste0(rule$target, " = ", rule_label(rule),
        if (!is.null(pinned[[rule$target]])) ", except where pinned"),
        which(!same_value(tryCatch(RULE_OPS[[rule$op]]$apply(rule, frame), error = function(e) rep(NA, nrow(frame))), x[[rule$target]]) &
          !(pinned[[rule$target]] %||% FALSE)))),
      Map(function(start, end) check_row(name, sprintf("%s on or before %s", start, end), which(x[[start]] > x[[end]])),
        starts[ends %in% names(x)], ends[ends %in% names(x)]),
      timeline_checks(study, data, name, frame, pinned))
  }
  # Supplemental qualifiers are generated as columns of their parent: once unfolded they
  # must point to parent records. Trial design datasets are copied unchanged.
  output <- output_datasets(study, generated)
  design <- study_design(study)
  rows <- c(rows, list(supp_checks(output, study_supp(study))),
    lapply(names(design), function(n) check_row(n, "Copied unchanged", if (!identical(output[[n]], design[[n]])) 1L)))
  do.call(rbind, rows)
}

# Records whose date a pin set: they keep the DM date where a rule or their visit gives another.
pinned_records <- function(ds, x, dm) {
  out <- list()
  for (p in Filter(function(p) p$column %in% names(x) && p$target %in% names(dm), ds$pins)) {
    m <- dm[[p$target]][match(x$USUBJID, dm$USUBJID)]
    rows <- if (p$where == "-") TRUE else eval(parse(text = p$where), x) %in% TRUE
    out[[p$column]] <- (out[[p$column]] %||% FALSE) | (rows & !is.na(m) & same_value(x[[p$column]], m))
  }
  out
}

# One check per timeline edge that ends in this dataset (see `timeline_edges()`), and fatal
# outcomes only in subjects with a death date. Pin and milestone checks list subjects, the
# others records.
timeline_checks <- function(study, data, name, frame, pinned = list()) {
  ds <- study$datasets[[name]]
  x <- data[[name]]
  dm <- data$DM
  edges <- timeline_edges(study)
  edges <- edges[edge_dataset(edges$to) == name, , drop = FALSE]
  column <- function(node) sub("^[^.]+\\.", "", node)
  label <- function(node) if (edge_dataset(node) == name) column(node) else node
  value <- function(node) {
    v <- if (edge_dataset(node) == name) x[[column(node)]] %||% frame[[column(node)]] else frame[[node]]
    if (is.null(v)) stop(node, " is not generated")
    as.numeric(v)
  }
  outcome <- x[[paste0(ds$prefix, "OUT")]]
  dead <- !is.na(dm$DTHDTC[match(x$USUBJID, dm$USUBJID)] %||% NA)
  c(lapply(seq_len(nrow(edges)), function(i) {
    e <- edges[i, ]
    switch(e$kind,
      order = , bound = check_row(name, sprintf("%s on or %s %s", label(e$to), if (e$relation == ">=") "after" else "before", label(e$from)),
        tryCatch(which(if (e$relation == ">=") value(e$to) < value(e$from) else value(e$to) > value(e$from)), error = conditionMessage)),
      pin = {
        p <- list(dataset = name, column = column(e$to), pick = sub("= ", "", e$relation), where = e$where, target = column(e$from))
        v <- pin_value(p, data, dm$USUBJID)
        checked <- if (e$all) rep(TRUE, nrow(dm)) else !is.na(dm[[p$target]]) & !is.na(v)
        check_row(name, pin_label(p), dm$USUBJID[checked & !same_value(v, dm[[p$target]])])
      },
      milestone = {
        m <- as.numeric(dm[[column(e$from)]])[match(x$USUBJID, dm$USUBJID)]
        to <- value(e$to)
        hit <- tapply(!is.na(m) & !is.na(to) & to == m, x$USUBJID, any)
        has <- tapply(!is.na(m), x$USUBJID, any)
        check_row(name, sprintf("%s is the date of one of the subject's %s visits", e$from, VISIT_HUB), names(hit)[has & !hit])
      },
      visit = check_row(name, sprintf("Every subject visit is in %s", VISIT_HUB), which(!visit_keys(x) %in% visit_keys(data[[VISIT_HUB]]))),
      `on visit` = {
        start <- frame[[e$from]]
        end <- frame[[paste0(VISIT_HUB, ".", sub("STDTC$", "ENDTC", column(e$from)))]] %||% start
        to <- x[[column(e$to)]]
        check_row(name, sprintf("%s within the %s visit's dates", label(e$to), VISIT_HUB),
          which((to < start | to > end) & !(pinned[[column(e$to)]] %||% FALSE)))
      },
      collected = {
        sv <- data[[VISIT_HUB]]
        to <- x[[column(e$to)]]
        check_row(name, sprintf("%s is the date of one of the subject's %s visits", label(e$to), VISIT_HUB),
          which(!is.na(to) & !paste(x$USUBJID, as.numeric(to)) %in% paste(sv$USUBJID, as.numeric(sv[[column(e$from)]]))))
      },
      span = {
        first <- startsWith(e$relation, "first")
        at <- as.numeric(subject_date(data, e$from, x$USUBJID))
        gap <- (if (first) 1 else -1) * (as.numeric(x[[column(e$to)]]) - at)
        late <- tapply(gap, x$USUBJID, function(z) !all(is.na(z)) && min(z, na.rm = TRUE) > 0)
        check_row(name, sprintf("Subject's %s %s %s", if (first) "first" else "last", label(e$to),
          if (first) paste("on or before", e$from) else paste("on or after", e$from)), names(late)[late])
      })
  }),
  if (isTRUE(ds$links$fatal >= SHARE_TO_PROPOSE) && !is.null(outcome)) list(check_row(name, "Fatal outcomes only in subjects with a death date",
    which(outcome %in% "FATAL" & !dead))))
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
