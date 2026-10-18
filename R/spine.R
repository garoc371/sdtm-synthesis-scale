# The time spine: when each subject's records happen, generated from the trial design and the source.
#
# On loading, every date of a subject is shifted by one number of days so that their reference
# start date (DM.RFSTDTC) falls on SPINE_ANCHOR (the PHUSE de-identification standard's date
# offset). Gaps between a subject's dates, study days and visit numbers are unchanged, so day 1
# of every subject is the same calendar date and the screening visits stay before it.
#
# The spine of a dataset is its timing: every date, the SDTM timing variables (VISITNUM,
# VISIT, EPOCH, study days, time points, …) and the record keys (the planned tests, the
# disposition category). DM's spine adds the arm, which sets the path through the trial
# design that the timing follows (a titration visit). A synthetic subject's spine is generated
# from the trial design and from distributions pooled over source subjects, never taken from
# one source subject (see `compose_spine()`): a profile (arm, last visit, death, treatment),
# the visits attended and their days (TV's planned day plus a deviation), the records at each
# visit, and every date as a landmark (a visit, a DM date) plus a gap; then day 1 on a random
# calendar date within the source's visits.
# Everything else (the subject's characteristics and the content of every record) is generated
# given the spine, through the spine features T.*:
#   T.TREATED    Y when the subject has first and last exposure (or reference) dates, STARTED
#                when only the first, N when neither
#   T.DIED       Y when the subject has a death date
#   T.EXPOSED    days from first to last exposure, both included
#   T.LASTVISIT  the subject's last visit number in SV, among the visits that mark progress
#                (progress_visits: not an AE follow-up after an early stop)
#   T.<DATASET>  Y when the subject has records in that dataset (T.AE)
#   T.<DATASET>.<CATEGORY>  Y when the subject has records of that --CAT, for categories some
#                of the dataset's subjects lack (a questionnaire not given after an early stop)
#   T.PHASE      the record's main date before, during or after the reference period
#   T.<D>=<M>    Y when the record's date D is the subject's DM date M (a DS date on DTHDTC)
#   T.<D>.KNOWN  Y when the record has its date D, for dates some records lack (an adverse
#                event without an end date is ongoing, so it has not resolved)
#
# The steps of `compose_spine()` live in their own files:
#   spine_profiles.R  each subject's profile and flags (arm, last visit, which datasets)
#   spine_visits.R    the visits attended and their days
#   spine_dm.R        DM's own columns and dates, and DM dates taken from other datasets
#   spine_records.R   each dataset's records: panels, sets, series, then their visits and dates
#   spine_dates.R     a date as a landmark plus a gap
#   spine_elements.R  SE's elements and each record's EPOCH
#   pooling.R         draws pooled over source subjects
#   spine_features.R  the spine features T.* that models are fitted on

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
  sv <- data[[VISIT_HUB]]
  fallbacks <- list(first_date(sv, intersect(main_date(VISIT_HUB, names(sv)), date_columns(sv))),
    first_date(data$DM, intersect("RFICDTC", date_columns(data$DM))),
    do.call(pmin, c(lapply(Filter(function(x) "USUBJID" %in% names(x), data), function(x) first_date(x, date_columns(x))), na.rm = TRUE)))
  reference <- first_date(data$DM, intersect("RFSTDTC", date_columns(data$DM)))
  for (fallback in fallbacks) {
    gap <- stats::median(reference - fallback, na.rm = TRUE)
    reference <- ifelse(is.na(reference), fallback + if (is.na(gap)) 0 else gap, reference)
  }
  delta <- reference - as.numeric(anchor)
  lapply(data, function(x) {
    if (!"USUBJID" %in% names(x)) return(x)
    dates <- date_columns(x)
    x[dates] <- lapply(x[dates], `-`, delta[match(x$USUBJID, ids)])
    x
  })
}

spine_columns <- function(name, prefix, variables, kinds, keys) {
  variables[kinds[variables] == "date" | variables %in% c(TIMING_VARIABLES, paste0(prefix, TIMING_SUFFIXES), keys,
    if (name == "DM") SPINE_ARM)]
}

# The source spine, kept in the specification for generation: records point to their subject
# by number (.subject), never by identifier. A series (one event recorded at several visits)
# keeps which records belong together (.series), and a dataset with a category (QSCAT) keeps
# it (.category) for the T.<DATASET>.<CATEGORY> features.
spine_records <- function(study) {
  ids <- study$source$data$DM$USUBJID
  lapply(study$datasets, function(ds) {
    x <- study$source$data[[ds$name]]
    out <- data.frame(.subject = match(x$USUBJID, ids), x[ds$structure$spine], check.names = FALSE, stringsAsFactors = FALSE)
    if (ds$name == "DM") {
      categories <- spine_categories(study$source$data, x$USUBJID)
      if (length(categories)) out <- cbind(out, categories)
    }
    if (identical(ds$structure$type, "series")) out$.series <- group_index(x, c("USUBJID", ds$structure$series))
    category <- x[[paste0(ds$name, "CAT")]]
    if (ds$name != "DM" && !is.null(category)) out$.category <- as.character(category)
    out[!is.na(out$.subject), , drop = FALSE]
  })
}

# ---- Composing the spine ---------------------------------------------------------------

# The spine of n synthetic subjects, generated subject by subject from the trial design and
# from distributions pooled over source subjects. No visit, record or date comes from one
# source subject:
#   1. Profile (`draw_profiles()`): arm, last visit that marks progress (T.LASTVISIT), and
#      death and treatment status, each drawn given those before; then flags for which
#      datasets, categories and values the subject has, each yes as often as among source
#      subjects of the same profile and earlier flags, and fixed where these fix it.
#   2. Visits (`draw_visits()`): a visit that marks progress is attended at the subject's
#      last, missed after it and attended before it as often as by source subjects who went
#      past it; any other visit (unscheduled, AE follow-up) as often as by source subjects
#      on the same side of their last visit. Days are the planned day (TV) plus a deviation
#      (`visit_days()`).
#   3. DM's dates, then each dataset in turn (`dataset_order()`): SV first, then the
#      datasets DM dates are taken from (`derived_dates()`: RFXSTDTC from EX, RFENDTC from
#      DS), then the rest. Records (`compose_records()`) come as a panel at each visit
#      attended (a visit's tests), as a set per subject (dispositions, SE's elements, medical
#      history) or as series (a medication recorded at several visits), each drawn whole.
#   4. Dates (`draw_dates()`): each is a landmark (the record's visit, the next visit, a DM
#      date, the record's main date, ...) plus a gap drawn from the source's.
#   5. Day 1 on a calendar date drawn so that the subject's visits lie between the source's
#      first and last visit; study days follow.
# Every draw pools source subjects of the same profile, coarsened step by step
# (`profile_levels()`), and draws only values (a visit attended, a panel, a number of series)
# held by at least min_subjects() of them (`pooled_draw()`), yes or no as often as among
# them (`pooled_share()`), and numbers within the ends of all source values (`pooled_number()`).
compose_spine <- function(study, n, ids) {
  dm <- study$spine$DM
  ctx <- spine_context(study, n)
  dates <- date_columns(dm)
  derived <- derived_dates(study, dates)
  ctx$derived <- derived$from
  composed <- list(DM = compose_dm(dm, dates, ctx))
  ctx$dm_src <- date_matrix(dm, dates)
  ctx$dm_syn <- date_matrix(composed$DM, dates)
  for (name in dataset_order(study, ctx)) {
    composed[[name]] <- compose_records(study, name, composed, ctx)
    if (name == VISIT_HUB) ctx <- visits_as_composed(ctx, composed[[name]])
    settled <- settle_dm_dates(composed, ctx, derived, name, study$datasets[[name]]$prefix)
    composed <- settled$composed
    ctx$dm_syn <- settled$dm_syn
    if (name != VISIT_HUB) ctx$visit_dates[[name]] <- visit_dates(study, name, composed[[name]], ctx)
  }
  composed$DM[dates] <- lapply(dates, function(m) ctx$dm_syn[, m])
  composed <- element_epochs(study, composed[names(study$spine)])
  composed$DM <- composed_categories(composed$DM, composed)
  shift <- calendar_shift(ctx$syn_days, as.numeric(study$schedule$calendar), ctx$a)
  composed <- Map(function(x, source) on_calendar(x, date_columns(source), shift, ids), composed, study$spine[names(composed)])
  study_days(composed)
}

# What every draw of the spine works from: the source's visits and profiles, the synthetic
# subjects' profiles, the visits they attend and their days there.
spine_context <- function(study, n) {
  spine <- study$spine
  dm <- spine$DM
  N <- nrow(dm)
  a <- as.numeric(as.Date(SPINE_ANCHOR))
  visits <- spine_visits(spine, N)
  src <- spine_profiles(spine, visits)
  values <- value_flags(study, N)
  src$flags <- cbind(src$flags, values$flags)
  syn <- draw_profiles(src, n)
  ctx <- list(N = N, n = n, a = a, visits = visits, src = src, syn = syn, have = profile_levels(src), want = profile_levels(syn),
    values = values$spec)
  ctx$att <- draw_visits(ctx)
  arm <- if (is.null(dm$ARMCD)) rep(NA_character_, n) else dm$ARMCD[match(syn$arm, src$arm)]
  days <- round(visit_days(study$schedule, visits, ctx$att, arm, a))
  ctx$src_days <- visits$day
  ctx$syn_days <- ifelse(ctx$att, days, NA)
  ctx
}

# A subject-by-date matrix of DM's dates, as numbers.
date_matrix <- function(dm, dates) matrix(vapply(dates, function(v) as.numeric(dm[[v]]), numeric(nrow(dm))), nrow(dm),
  dimnames = list(NULL, dates))

# Every later record at a visit is dated from SV's visits as composed.
visits_as_composed <- function(ctx, sv) {
  seen <- visit_matrix(sv, sv$.j, ctx$visits$number, ctx$n)
  ctx$att <- seen$attended
  ctx$syn_days <- ifelse(seen$attended, ifelse(is.na(seen$day), ctx$syn_days, seen$day), NA)
  ctx$syn_ends <- seen$end
  ctx
}

# The T.<DATASET>.<CATEGORY> flags as composed.
composed_categories <- function(dm, composed) {
  for (f in grep("^T\\.", names(dm), value = TRUE)) {
    parts <- regmatches(f, regexec("^T\\.([^.]+)\\.(.+)$", f))[[1]]
    x <- if (length(parts) == 3) composed[[parts[2]]]
    if (!is.null(x$.category)) dm[[f]] <- ifelse(seq_len(nrow(dm)) %in% x$.j[x$.category %in% parts[3]], "Y", "N")
  }
  dm
}

# Days added to each subject's dates to put day 1 on the calendar: drawn so that the
# subject's visits lie between the source's first and last (`window`).
calendar_shift <- function(days, window, a) {
  if (length(window) != 2) return(rep(0, nrow(days)))
  seen <- cbind(days, a)
  first <- window[1] + a - apply(seen, 1, min, na.rm = TRUE)
  last <- pmax(first, window[2] - (apply(seen, 1, max, na.rm = TRUE) - a))
  first - a + vapply(last - first + 1, sample.int, 1L, size = 1) - 1
}

# A composed dataset with its dates on the calendar and its subjects by identifier.
on_calendar <- function(x, dates, shift, ids) {
  x[dates] <- lapply(x[dates], function(d) as.Date(as.numeric(d) + shift[x$.j], origin = "1970-01-01"))
  x <- data.frame(USUBJID = ids[x$.j], x[setdiff(names(x), c("USUBJID", ".j", ".category"))], check.names = FALSE, stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

# SV first, then the datasets DM dates are taken from, those at visits first and the latest
# date first (DS's disposition before EX), then the rest; a linked dataset after the one it
# links to (TR after TU).
dataset_order <- function(study, ctx) {
  spine <- study$spine
  rest <- setdiff(names(spine), c("DM", VISIT_HUB))
  derived <- ctx$derived
  late <- vapply(names(derived), function(m) mean(as.numeric(spine$DM[[m]]) - ctx$a, na.rm = TRUE), 0)
  at_visit <- vapply(derived, function(r) !is.null(spine[[r$dataset]]$VISITNUM), TRUE)
  first <- intersect(unique(vapply(derived[order(!at_visit, -late)], `[[`, "", "dataset")), rest)
  out <- c(intersect(VISIT_HUB, names(spine)), first, setdiff(rest, first))
  for (name in out) {
    to <- study$datasets[[name]]$link$dataset
    if (!is.null(to) && to %in% out && match(to, out) > match(name, out)) {
      out <- setdiff(out, name)
      out <- append(out, name, after = match(to, out))
    }
  }
  out
}

# --DY, --STDY and --ENDY from their date and the subject's RFSTDTC, with no day 0.
study_days <- function(data) {
  lapply(data, function(x) {
    if (!"USUBJID" %in% names(x) || is.null(data$DM$RFSTDTC)) return(x)
    start <- as.numeric(data$DM$RFSTDTC[match(x$USUBJID, data$DM$USUBJID)])
    for (v in date_columns(x)) {
      day <- sub("DTC$", "DY", v)
      gap <- as.numeric(x[[v]]) - start
      if (day != v && is.numeric(x[[day]])) x[[day]] <- gap + (gap >= 0)
    }
    x
  })
}
