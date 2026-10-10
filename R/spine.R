# The time spine: when each subject's records happen, taken from the source and never modelled.
#
# On loading, every date of a subject is shifted by one number of days so that their reference
# start date (DM.RFSTDTC) falls on SPINE_ANCHOR (the PHUSE de-identification standard's date
# offset). Gaps between a subject's dates, study days and visit numbers are unchanged, so day 1
# of every subject is the same calendar date and the screening visits stay before it.
#
# The spine of a dataset is its timing: every date, the SDTM timing variables (VISITNUM,
# VISIT, EPOCH, study days, time points, …) and the record keys (the planned tests, the
# disposition category). DM's spine adds the arm, which sets the path through the trial
# design that the timing follows (a titration visit). Each synthetic subject copies the whole
# spine of one source subject, its donor: its arm, how many records it has in each dataset, at
# which visits and on which days.
# Everything else (the subject's characteristics and the content of every record) is generated
# given the spine, through the spine features T.*:
#   T.TREATED    Y when the subject has first and last exposure (or reference) dates, STARTED
#                when only the first, N when neither
#   T.DIED       Y when the subject has a death date
#   T.EXPOSED    days from first to last exposure, both included
#   T.LASTVISIT  the subject's last visit number in SV, among the visits that mark progress
#                (progress_visits: not an AE follow-up after an early stop)
#   T.<DATASET>  Y when the subject has records in that dataset (T.AE)
#   T.PHASE      the record's main date before, during or after the reference period
#   T.<D>=<M>    Y when the record's date D is the subject's DM date M (a DS date on DTHDTC)
#   T.<D>.KNOWN  Y when the record has its date D, for dates some records lack (an adverse
#                event without an end date is ongoing, so it has not resolved)

SPINE_ANCHOR <- "2020-01-01"

TIMING_VARIABLES <- c("VISITNUM", "VISIT", "VISITDY", "TAETORD", "EPOCH")
TIMING_SUFFIXES <- c("DY", "STDY", "ENDY", "DUR", "TPT", "TPTNUM", "ELTM", "TPTREF", "RFTDTC", "STRF", "ENRF",
  "EVLINT", "STRTPT", "STTPT", "ENRTPT", "ENTPT")
SPINE_SUBJECT_FEATURES <- c("T.TREATED", "T.DIED", "T.EXPOSED", "T.LASTVISIT")
SPINE_ARM <- c("ARMCD", "ARM", "ACTARMCD", "ACTARM", "ARMNRS", "ACTARMUD")

# Each subject's reference date: RFSTDTC, or else (screen failures) their first SV date, their
# informed consent date or their earliest date, placed where that date usually falls relative
# to RFSTDTC (the median gap among subjects who have both). Subjects without any date keep NA.
shift_dates <- function(data, anchor = as.Date(getOption("synth.anchor", SPINE_ANCHOR))) {
  ids <- data$DM$USUBJID
  first_date <- function(x, columns) {
    if (is.null(x) || !length(columns)) return(rep(NA_real_, length(ids)))
    d <- do.call(pmin, c(lapply(x[columns], as.numeric), na.rm = TRUE))
    keep <- !is.na(d)
    as.numeric(tapply(d[keep], x$USUBJID[keep], min)[ids])
  }
  dates <- function(x) names(Filter(function(v) inherits(v, "Date"), x))
  sv <- data[[VISIT_HUB]]
  fallbacks <- list(first_date(sv, intersect(main_date(VISIT_HUB, names(sv)), dates(sv))),
    first_date(data$DM, intersect("RFICDTC", dates(data$DM))),
    do.call(pmin, c(lapply(Filter(function(x) "USUBJID" %in% names(x), data), function(x) first_date(x, dates(x))), na.rm = TRUE)))
  reference <- first_date(data$DM, intersect("RFSTDTC", dates(data$DM)))
  for (fallback in fallbacks) {
    gap <- stats::median(reference - fallback, na.rm = TRUE)
    reference <- ifelse(is.na(reference), fallback + if (is.na(gap)) 0 else gap, reference)
  }
  delta <- reference - as.numeric(anchor)
  lapply(data, function(x) {
    if (!"USUBJID" %in% names(x)) return(x)
    shift <- delta[match(x$USUBJID, ids)]
    for (v in dates(x)) x[[v]] <- x[[v]] - shift
    x
  })
}

spine_columns <- function(name, prefix, variables, kinds, keys) {
  variables[kinds[variables] == "date" | variables %in% c(TIMING_VARIABLES, paste0(prefix, TIMING_SUFFIXES), keys,
    if (name == "DM") SPINE_ARM)]
}

# The source spine, kept in the specification for generation: records point to their subject
# by number (.donor), never by identifier. A series (one event recorded at several visits)
# keeps which records belong together (.series).
spine_records <- function(study) {
  ids <- study$source$data$DM$USUBJID
  lapply(study$datasets, function(ds) {
    x <- study$source$data[[ds$name]]
    out <- data.frame(.donor = match(x$USUBJID, ids), x[ds$structure$spine], check.names = FALSE, stringsAsFactors = FALSE)
    if (identical(ds$structure$type, "series")) out$.series <- group_index(x, c("USUBJID", ds$structure$series))
    out[!is.na(out$.donor), , drop = FALSE]
  })
}

# Donors are drawn without replacement while there are enough source subjects.
spine_donors <- function(spine, n) {
  available <- nrow(spine$DM)
  sample.int(available, n, replace = n > available)
}

# Each synthetic subject's records in every dataset: their donor's spine.
draw_spine <- function(spine, donors, ids) {
  lapply(spine, function(x) {
    rows <- split(seq_len(nrow(x)), x$.donor)[as.character(donors)]
    subject <- rep(seq_along(donors), lengths(rows))
    out <- x[unlist(rows), , drop = FALSE]
    if (!is.null(out$.series)) out$.series <- group_index(data.frame(subject, out$.series), 1:2)
    out <- data.frame(USUBJID = ids[subject], out[setdiff(names(out), ".donor")], check.names = FALSE, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    out
  })
}

# The visits that mark how far a subject went, in number order: a visit counts when most of
# its subjects attended the latest counted visit with at least as many subjects. Visits after
# an early stop (AE FOLLOW-UP 101, RETRIEVAL 201) mostly lack the last scheduled visit, so a
# subject who stopped at week 8 does not look like one who went past week 26.
progress_visits <- function(sv) {
  number <- as.numeric(sv$VISITNUM)
  subjects <- lapply(split(sv$USUBJID, number), unique)
  visits <- as.numeric(names(subjects))
  size <- lengths(subjects)
  kept <- 1L
  for (i in seq_along(visits)[-1]) {
    ref <- kept[size[kept] >= size[i]]
    ref <- if (length(ref)) ref[length(ref)] else kept[1]
    if (mean(subjects[[i]] %in% subjects[[ref]]) >= 0.5) kept <- c(kept, i)
  }
  visits[kept]
}

# Subject-level spine features for `ids`, from DM's dates, SV's visits and which datasets
# have their records.
spine_subjects <- function(data, ids) {
  dm <- data$DM[match(ids, data$DM$USUBJID), , drop = FALSE]
  date <- function(columns) {
    v <- intersect(columns, names(Filter(function(v) inherits(v, "Date"), dm)))[1]
    if (is.na(v)) rep(NA_real_, length(ids)) else as.numeric(dm[[v]])
  }
  start <- date(c("RFXSTDTC", "RFSTDTC"))
  end <- date(c("RFXENDTC", "RFENDTC"))
  out <- data.frame(T.TREATED = ifelse(is.na(start), "N", ifelse(is.na(end), "STARTED", "Y")), T.DIED = ifelse(is.na(date("DTHDTC")), "N", "Y"),
    T.EXPOSED = end - start + 1, stringsAsFactors = FALSE)
  sv <- data[[VISIT_HUB]]
  last <- if (!is.null(sv$VISITNUM)) {
    sv <- sv[as.numeric(sv$VISITNUM) %in% progress_visits(sv), , drop = FALSE]
    tapply(as.numeric(sv$VISITNUM), sv$USUBJID, max, na.rm = TRUE)
  }
  out$T.LASTVISIT <- if (is.null(last)) NA_character_ else format_value(unname(last[ids]))
  for (d in setdiff(names(data), "DM")) out[[paste0("T.", d)]] <- ifelse(ids %in% data[[d]]$USUBJID, "Y", "N")
  out
}

spine_subject_names <- function(study) c(SPINE_SUBJECT_FEATURES, paste0("T.", setdiff(names(study$source$data), "DM")))

# Record-level spine features. `x` holds the records with DM's variables joined as DM.*.
spine_record_features <- function(x, ds) {
  dates <- ds$variables[ds$kinds[ds$variables] == "date"]
  main <- main_date(ds$prefix, dates)
  d <- if (is.na(main)) rep(NA_real_, nrow(x)) else as.numeric(x[[main]])
  start <- as.numeric(x[["DM.RFSTDTC"]] %||% rep(NA, nrow(x)))
  end <- as.numeric(x[["DM.RFENDTC"]] %||% rep(NA, nrow(x)))
  x$T.PHASE <- ifelse(is.na(d) | is.na(start), "UNKNOWN", ifelse(d < start, "BEFORE", ifelse(is.na(end) | d <= end, "DURING", "AFTER")))
  for (v in ds$known_dates %||% character()) x[[paste0("T.", v, ".KNOWN")]] <- ifelse(is.na(x[[v]]), "N", "Y")
  for (m in spine_dm_dates(x)) for (v in dates) {
    same <- as.numeric(x[[v]]) == as.numeric(x[[paste0("DM.", m)]])
    x[[paste0("T.", v, "=", m)]] <- ifelse(!is.na(same) & same, "Y", "N")
  }
  x
}

spine_dm_dates <- function(x) sub("^DM\\.", "", grep("^DM\\.", names(Filter(function(v) inherits(v, "Date"), x)), value = TRUE))

# The names of the spine features a dataset's frame has (see `dataset_frame()`).
spine_feature_names <- function(study, name) {
  if (name == "DM") return(spine_subject_names(study))
  ds <- study$datasets[[name]]
  dm <- study$datasets$DM
  dates <- ds$variables[ds$kinds[ds$variables] == "date"]
  dm_dates <- dm$variables[dm$kinds[dm$variables] == "date"]
  c(spine_subject_names(study), "T.PHASE", known_date_names(ds), if (length(dates) && length(dm_dates))
    as.vector(outer(dates, dm_dates, function(v, m) paste0("T.", v, "=", m))))
}

# Dates that some records of the dataset have and others lack, set when the dataset is read.
known_dates <- function(x, dates) dates[vapply(x[dates], function(v) anyNA(v) && !all(is.na(v)), TRUE)]
known_date_names <- function(ds) if (length(ds$known_dates)) paste0("T.", ds$known_dates, ".KNOWN")

# ---- Implications ----------------------------------------------------------------------

# A sampled category may be tied to a spine feature, a DM variable or a spine column of its own
# record in a way no model with few records keeps: a value of the key that always comes with
# one value of the node (a DS date on the death date: DEATH), or a value of the node that only
# comes with some values of the key (DEATH only on the death date; a related adverse event only
# during or after treatment). Each is kept when it holds on every source record, in at least two
# subjects, and would hold by chance with probability below IMPLICATION_P if the two were
# independent. Subjects count, not records (records of a subject are alike): a share is
# the mean over subjects of the share of their records. Generation redraws the second and
# then sets the first.
IMPLICATION_P <- 1e-4
IMPLICATION_KEY_VALUES <- 50

implication_keys <- function(ds, frame) {
  candidates <- c(grep("^T\\.", names(frame), value = TRUE), if (ds$name != "DM") grep("^DM\\.", names(frame), value = TRUE),
    intersect(ds$structure$spine, names(frame)))
  Filter(function(k) (is.character(frame[[k]]) || k %in% ds$structure$spine && is.numeric(frame[[k]])) &&
    length(unique(frame[[k]])) %in% 2:IMPLICATION_KEY_VALUES, candidates)
}

fit_implications <- function(y, frame, keys) {
  values <- level_string(y)
  subject <- factor(frame$USUBJID, unique(frame$USUBJID))
  # Per level: the subjects that have it and the mean of their record shares.
  subjects <- function(groups) colSums(table(subject, groups) > 0)
  share <- function(groups) {
    t <- table(subject, groups)
    colMeans(t / rowSums(t))
  }
  n_value <- subjects(values)
  p_value <- share(values)
  rows <- list()
  for (k in keys) {
    key <- level_string(frame[[k]])
    present <- table(key, values) > 0
    n_key <- subjects(key)[rownames(present)]
    p_key <- share(key)[rownames(present)]
    forced <- which(n_key >= 2 & rowSums(present) == 1)
    with <- apply(present[forced, , drop = FALSE], 1, which)
    forced <- forced[p_value[colnames(present)][with]^n_key[forced] < IMPLICATION_P]
    # Key levels a value is never at are ruled out when, by chance, some of its subjects would
    # have been there: a level alone, or all of them together (no single level of treatment
    # status or last visit is enough). A set of levels is picked after the fact, so its chance
    # is multiplied by the number of sets of as many levels: many sparse levels (a study day)
    # always leave some empty.
    n <- n_value[colnames(present)]
    absent <- !present
    together <- (1 - colSums(absent * p_key))^n * choose(nrow(present), colSums(absent)) < IMPLICATION_P
    never <- absent & (outer(1 - p_key, n, `^`) < IMPLICATION_P | rep(together, each = nrow(absent)))
    only <- which(n >= 2 & colSums(never) > 0)
    if (length(forced)) rows[[length(rows) + 1]] <- data.frame(kind = "forced", key = k, level = I(as.list(rownames(present)[forced])),
      value = I(as.list(colnames(present)[with[names(forced)]])), stringsAsFactors = FALSE)
    if (length(only)) rows[[length(rows) + 1]] <- data.frame(kind = "only", key = k,
      level = I(lapply(only, function(j) rownames(present)[!never[, j]])), value = I(as.list(colnames(present)[only])), stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  if (is.null(out)) return(NULL)
  rownames(out) <- NULL
  out$typed <- I(lapply(out$value, function(v) if (v == MISSING) NA else y[match(v, values)]))
  out$via <- NA_character_
  out
}

# A category that lookups derive from the node alone (CMROUTE from CMTRT) keeps its "only"
# implications through the node: the node values that give it (every topical medication)
# only where the key allows it. One drug is too rare to show the pattern; its route is not.
derived_implications <- function(ds, variable, y, frame, keys) {
  chain <- list()
  known <- variable
  repeat {
    new <- Filter(function(r) r$op == "lookup" && !r$target %in% known && all(r$inputs %in% known), accepted_rules(ds))
    if (!length(new)) break
    chain <- c(chain, new)
    known <- c(known, vapply(new, `[[`, "", "target"))
  }
  values <- level_string(y)
  first <- frame[!duplicated(values), , drop = FALSE]
  for (r in chain) first[[r$target]] <- RULE_OPS$lookup$apply(r, first)
  do.call(rbind, lapply(chain, function(r) {
    derived <- level_string(first[[r$target]])[match(values, level_string(first[[variable]]))]
    out <- fit_implications(derived, frame, keys)
    out <- out[out$kind == "only", , drop = FALSE]
    if (!NROW(out)) return(NULL)
    out$via <- paste(r$target, "=", unlist(out$value))
    out$value <- I(lapply(unlist(out$value), function(v) unique(values[derived == v])))
    out$typed <- I(rep(list(NULL), nrow(out)))
    out
  }))
}

# The rows an implication covers: those of its test, or all.
implication_rows <- function(implications, i, frame) {
  within <- implications$within[i]
  if (is.null(within) || is.na(within)) rep(TRUE, nrow(frame)) else format_value(frame[[within]]) == implications$part[i]
}

# Rows breaking an implication: a forced key value with another value, or an "only" value with another key value.
implication_violations <- function(implications, values, frame) {
  bad <- rep(FALSE, length(values))
  s <- level_string(values)
  for (i in seq_len(NROW(implications))) {
    key <- level_string(frame[[implications$key[i]]])
    bad <- bad | implication_rows(implications, i, frame) & if (implications$kind[i] == "forced")
      key %in% implications$level[[i]] & s != implications$value[[i]] else s %in% implications$value[[i]] & !key %in% implications$level[[i]]
  }
  bad
}

# "Only" values out of place are redrawn from the model a few times, then replaced by a
# value drawn for another record of the same test that is allowed there; forced values are
# then set.
apply_implications <- function(implications, values, frame, redraw) {
  if (is.null(implications)) return(values)
  only <- implications[implications$kind == "only", , drop = FALSE]
  misplaced <- function(v, rows) implication_violations(only, v, frame[rows, , drop = FALSE])
  bad <- which(misplaced(values, seq_along(values)))
  for (attempt in 1:20) {
    if (!length(bad)) break
    values[bad] <- redraw(bad)
    bad <- bad[misplaced(values[bad], bad)]
  }
  within <- stats::na.omit(implications$within)[1]
  test <- if (length(within) && !is.na(within)) format_value(frame[[within]]) else rep("", length(values))
  for (i in bad) {
    same <- setdiff(which(test == test[i]), bad)
    pool <- level_string(values[same])
    options <- values[same][!duplicated(pool)]
    weight <- tabulate(match(pool, unique(pool)))
    ok <- which(!misplaced(options, rep(i, length(options))))
    if (length(ok)) values[i] <- options[ok[sample.int(length(ok), 1, prob = weight[ok])]]
  }
  forced <- implications[implications$kind == "forced", , drop = FALSE]
  for (i in seq_len(nrow(forced))) {
    rows <- level_string(frame[[forced$key[i]]]) %in% forced$level[[i]] & implication_rows(forced, i, frame)
    values[rows] <- forced$typed[[i]]
  }
  values
}

implication_label <- function(r) {
  level <- r$level[[1]]
  test <- if (!is.null(r$within) && !is.na(r$within)) sprintf(" (%s %s)", r$within, r$part) else ""
  if (r$kind == "forced") return(sprintf("%s = %s gives %s%s", display_name(r$key), level, r$value[[1]], test))
  sprintf("%s only where %s %s%s", if (is.na(r$via)) r$value[[1]] else r$via, display_name(r$key),
    if (length(level) == 1) paste("=", level) else sprintf("in {%s}", paste(level, collapse = ", ")), test)
}
