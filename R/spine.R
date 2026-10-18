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


# The visit schedule, learnt on loading. Each scheduled visit has a planned study day: TV's
# VISITDY (per arm when TV gives one), or without TV the median day the source's subjects
# attended it, for the visits that mark progress and that at least SCHEDULED_SHARE of the
# subjects attended (in the pilot, WEEK 26 has 36% of them, an unscheduled visit at most
# 17%). How far the source's subjects were from it (their study day minus the planned day)
# is kept as percentiles whose ends leave out the k subjects furthest either way (see
# `number_margin()`). Visits without a planned day
# (unscheduled, AE follow-up) are not scheduled: they fall some days after the previous
# visit (see `visit_days()`).
# `calendar` is the first and last SV date of the source before the shift (`visit_window()`),
# the study's first and last visit, which a protocol report gives anyway.
SCHEDULED_SHARE <- 0.25

visit_schedule <- function(data, design, calendar) {
  sv <- data[[VISIT_HUB]]
  date <- if (!is.null(sv)) main_date(VISIT_HUB, names(sv)) else NA
  if (is.na(date) || is.null(sv$VISITNUM) || is.null(data$DM$RFSTDTC)) return(NULL)
  start <- as.numeric(data$DM$RFSTDTC[match(sv$USUBJID, data$DM$USUBJID)])
  gap <- as.numeric(sv[[date]]) - start
  x <- data.frame(USUBJID = sv$USUBJID, visit = as.numeric(sv$VISITNUM), day = gap + (gap >= 0),
    arm = data$DM$ARMCD[match(sv$USUBJID, data$DM$USUBJID)] %||% NA_character_, stringsAsFactors = FALSE)
  x <- x[!is.na(x$day) & !is.na(x$visit), , drop = FALSE]
  tv <- design$TV
  planned <- if (!is.null(tv$VISITDY) && any(!is.na(tv$VISITDY))) {
    tv <- data.frame(visit = as.numeric(tv$VISITNUM), name = as.character(tv$VISIT %||% NA), arm = tv$ARMCD %||% NA_character_,
      planned = as.numeric(tv$VISITDY), stringsAsFactors = FALSE)
    tv[!is.na(tv$planned), , drop = FALSE]
  } else {
    attended <- tapply(sv$USUBJID, as.numeric(sv$VISITNUM), function(id) length(unique(id)))
    common <- as.numeric(names(attended))[attended >= SCHEDULED_SHARE * length(unique(sv$USUBJID))]
    x <- x[x$visit %in% intersect(progress_visits(sv), common), , drop = FALSE]
    names <- if (!is.null(sv$VISIT)) tapply(as.character(sv$VISIT), as.numeric(sv$VISITNUM), most_common)
    visits <- sort(unique(x$visit))
    data.frame(visit = visits, name = if (is.null(names)) NA_character_ else unname(names[as.character(visits)]), arm = NA_character_,
      planned = as.numeric(tapply(x$day, x$visit, stats::median)), stringsAsFactors = FALSE)
  }
  x$planned <- schedule_day(planned, x$visit, x$arm)
  x <- x[!is.na(x$planned), , drop = FALSE]
  if (!nrow(x)) return(NULL)
  deviation <- lapply(split(x, x$visit), function(v) number_margin(v$day - v$planned, v$USUBJID))
  list(planned = planned, deviation = deviation, calendar = calendar)
}

visit_window <- function(data) {
  sv <- data[[VISIT_HUB]]
  date <- if (!is.null(sv)) main_date(VISIT_HUB, names(sv)) else NA
  if (!is.na(date) && any(!is.na(sv[[date]]))) range(sv[[date]], na.rm = TRUE)
}

# The planned day of each visit, for the subject's arm where TV has one.
schedule_day <- function(planned, visit, arm) {
  by_arm <- planned$planned[match(paste(visit, arm), paste(planned$visit, planned$arm))]
  any_arm <- planned$planned[is.na(planned$arm)][match(visit, planned$visit[is.na(planned$arm)])]
  ifelse(is.na(by_arm), any_arm, by_arm)
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
  dates <- names(Filter(function(v) inherits(v, "Date"), dm))
  derived <- derived_dates(study, dates)
  ctx$derived <- derived$from
  composed <- list(DM = compose_dm(dm, dates, ctx))
  ctx$dm_src <- matrix(vapply(dates, function(v) as.numeric(dm[[v]]), numeric(N)), N, dimnames = list(NULL, dates))
  ctx$dm_syn <- matrix(vapply(dates, function(v) as.numeric(composed$DM[[v]]), numeric(n)), n, dimnames = list(NULL, dates))
  for (name in dataset_order(study, ctx)) {
    composed[[name]] <- compose_records(study, name, composed, ctx)
    # Every later record at a visit is dated from SV's visits as composed.
    if (name == VISIT_HUB) {
      seen <- visit_matrix(composed[[name]], composed[[name]]$.j, visits$number, n)
      ctx$att <- seen$attended
      ctx$syn_days <- ifelse(seen$attended, ifelse(is.na(seen$day), ctx$syn_days, seen$day), NA)
      ctx$syn_ends <- seen$end
    }
    for (m in names(Filter(function(r) r$dataset == name, ctx$derived))) {
      value <- derived_value(composed, ctx$derived[[m]], n)
      keep <- !is.na(value) & !is.na(composed$DM[[m]])
      composed$DM[[m]][keep] <- ctx$dm_syn[keep, m] <- value[keep]
    }
    # A DM date bounding a dataset's first or last date moves to it where it is passed, once
    # both that dataset and the one the date is taken from are composed; the latter's date
    # moves with it.
    for (r in derived$bounds) {
      from <- ctx$derived[[r$date]]
      if (!name %in% c(r$dataset, from$dataset) || !all(c(r$dataset, from$dataset) %in% names(composed))) next
      value <- derived_value(composed, r, n)
      over <- which(!is.na(value) & !is.na(composed$DM[[r$date]]) & (if (r$end == "max") value > ctx$dm_syn[, r$date] else value < ctx$dm_syn[, r$date]))
      if (!length(over)) next
      composed$DM[[r$date]][over] <- ctx$dm_syn[over, r$date] <- value[over]
      if (!is.null(from)) composed[[from$dataset]] <- tie_date(composed[[from$dataset]], from, ctx$dm_syn[, r$date])
    }
    se <- composed[[name]]
    if (name == "SE") o <- order(se$.j, as.numeric(se$SESTDTC %||% 0), as.numeric(se$SEENDTC %||% 0))
    for (r in Filter(function(r) r$dataset == name, derived$tied))
      if (is.null(ctx$derived[[r$date]]) || ctx$derived[[r$date]]$dataset %in% names(composed))
        composed[[name]] <- tie_date(composed[[name]], r, ctx$dm_syn[, r$date], if (name != VISIT_HUB) visit_span(composed[[name]], ctx))
    # A tie moves an element's start (the follow-up's to RFENDTC); the elements around it follow.
    if (name == "SE") composed[[name]] <- chain_elements(composed[[name]], study$datasets[[name]]$prefix, o)
    if (name != VISIT_HUB) ctx$visit_dates[[name]] <- visit_dates(study, name, composed[[name]], ctx)
  }
  composed <- element_epochs(study, composed[names(spine)])
  # T.<DATASET>.<CATEGORY> as composed.
  for (f in grep("^T\\.", names(dm), value = TRUE)) {
    parts <- regmatches(f, regexec("^T\\.([^.]+)\\.(.+)$", f))[[1]]
    x <- if (length(parts) == 3) composed[[parts[2]]]
    if (!is.null(x$.category)) composed$DM[[f]] <- ifelse(seq_len(n) %in% x$.j[x$.category %in% parts[3]], "Y", "N")
  }
  # Day 1 on the calendar, so that the subject's visits lie between the source's first and last.
  window <- as.numeric(study$schedule$calendar)
  shift <- rep(0, n)
  if (length(window) == 2) for (j in seq_len(n)) {
    seen <- c(ctx$syn_days[j, ], a)
    first <- window[1] + a - min(seen, na.rm = TRUE)
    last <- max(first, window[2] - (max(seen, na.rm = TRUE) - a))
    shift[j] <- first - a + sample.int(last - first + 1, 1) - 1
  }
  for (name in names(composed)) {
    x <- composed[[name]]
    for (v in names(Filter(function(v) inherits(v, "Date"), spine[[name]])))
      x[[v]] <- as.Date(as.numeric(x[[v]]) + shift[x$.j], origin = "1970-01-01")
    x <- data.frame(USUBJID = ids[x$.j], x[setdiff(names(x), c("USUBJID", ".j", ".category"))], check.names = FALSE, stringsAsFactors = FALSE)
    rownames(x) <- NULL
    composed[[name]] <- x
  }
  study_days(composed)
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

# The DM dates that are a dataset's first or last date in at least DERIVED_SHARE of the source
# subjects that have them (RFXENDTC the last EXENDTC, RFENDTC the DS date of the DISPOSITION
# EVENT), as the dataset, column, end ("min" or "max") and, for a dataset planned by category
# or test, the key and its value. RFSTDTC stays day 1.
DERIVED_SHARE <- 0.9

derived_value <- function(composed, r, n) {
  x <- composed[[r$dataset]]
  rows <- if (is.null(r$key)) TRUE else level_string(x[[r$key]]) == r$level
  subject_extreme(as.numeric(x[[r$variable]])[rows], x$.j[rows], n, r$end)
}

# Each subject's main date at each visit (the first, NA where none) in dataset `name`, in the
# source and as composed; NULL for a dataset without visits or dates, or whose records are not
# drawn at visits (a series: a medication's start is no visit date).
visit_dates <- function(study, name, composed, ctx) {
  x <- study$spine[[name]]
  s <- study$datasets[[name]]$structure
  main <- main_date(study$datasets[[name]]$prefix, names(Filter(function(v) inherits(v, "Date"), x)))
  V <- ctx$visits$number
  if (is.null(x$VISITNUM) || is.na(main) || !length(V)) return(NULL)
  if (identical(s$type, "series") || is.null(s$visit) && !identical(s$type, "count")) return(NULL)
  at <- function(owner, number, d, size) {
    k <- match(as.numeric(number), V)
    ok <- !is.na(k) & !is.na(d)
    out <- tapply(d[ok], list(factor(owner[ok], seq_len(size)), factor(k[ok], seq_along(V))), min)
    matrix(as.numeric(out), size, length(V))
  }
  list(src = at(x$.subject, x$VISITNUM, as.numeric(x[[main]]), ctx$N), syn = at(composed$.j, composed$VISITNUM, as.numeric(composed[[main]]), ctx$n))
}

# A dataset's dates set so that the subject's first or last (`r$end`) is the DM date `target`
# where it has both, and no other comes before or after it; but no record at a visit is
# moved out of its SV visit (`span`), which the source never does.
tie_date <- function(x, r, target, span = NULL) {
  v <- old <- as.numeric(x[[r$variable]])
  t <- target[x$.j]
  rows <- if (is.null(r$key)) seq_along(v) else which(level_string(x[[r$key]]) == r$level)
  rows <- rows[!is.na(v[rows]) & !is.na(t[rows])]
  if (!length(rows)) return(x)
  v[rows] <- if (r$end == "max") pmin(v[rows], t[rows]) else pmax(v[rows], t[rows])
  ext <- subject_extreme(v[rows], x$.j[rows], length(target), r$end)
  at <- rows[v[rows] == ext[x$.j[rows]]]
  v[at] <- t[at]
  if (!is.null(span)) {
    off <- !is.na(span$start) & !is.na(v) & (v < span$start | v > span$end)
    v[off] <- old[off]
  }
  x[[r$variable]] <- v
  x
}

# The SV visit of each of records `x` (from SVSTDTC to SVENDTC), NA for records off visits;
# NULL for a dataset without visits.
visit_span <- function(x, ctx) {
  if (is.null(x$VISITNUM) || is.null(ctx$syn_ends)) return(NULL)
  at <- cbind(x$.j, match(as.numeric(x$VISITNUM), ctx$visits$number))
  start <- ctx$syn_days[at]
  list(start = start, end = ifelse(is.na(ctx$syn_ends[at]), start, pmax(ctx$syn_ends[at], start)))
}

# The DM dates taken from a dataset (`from`: SV where it gives one, as DMDTC its first visit),
# and those other datasets' first or last dates
# equal in at least DERIVED_SHARE of the source subjects that have both, at least
# min_subjects() of them (`tied`: XD's last XDENDTC, a copy of EX's, at RFXENDTC), and the
# other datasets' first or last dates that are never after (before) a DM date in every source
# subject that has both, at least min_subjects() of them (`bounds`: no visit after RFPENDTC).
derived_dates <- function(study, dates) {
  spine <- study$spine
  N <- nrow(spine$DM)
  candidates <- list()
  for (d in setdiff(names(spine), "DM")) {
    x <- spine[[d]]
    st <- study$datasets[[d]]$structure
    key <- if (identical(st$type, "schedule") && is.null(st$visit)) st$keys[1]
    levels <- c(NA, if (!is.null(key)) unique(level_string(x[[key]])))
    for (v in names(Filter(function(v) inherits(v, "Date"), x))) for (u in levels) for (end in c("min", "max")) {
      rows <- if (is.na(u)) TRUE else level_string(x[[key]]) == u
      candidates[[length(candidates) + 1]] <- list(dataset = d, variable = v, end = end, key = if (!is.na(u)) key, level = u,
        value = subject_extreme(x[[v]][rows], x$.subject[rows], N, end))
    }
  }
  out <- list()
  for (m in setdiff(dates, "RFSTDTC")) {
    target <- as.numeric(spine$DM[[m]])
    has <- which(!is.na(target))
    if (length(has) < min_subjects() || !length(candidates)) next
    share <- vapply(candidates, function(c) mean((c$value[has] == target[has]) %in% TRUE), 0)
    hub <- vapply(candidates, function(c) c$dataset == VISIT_HUB, TRUE) & share >= DERIVED_SHARE
    if (any(hub)) share[!hub] <- 0
    if (max(share) >= DERIVED_SHARE) out[[m]] <- candidates[[which.max(share)]][c("dataset", "variable", "end", "key", "level")]
  }
  tied <- list()
  for (m in dates) {
    target <- as.numeric(spine$DM[[m]])
    for (c in candidates) {
      if (identical(out[[m]]$dataset, c$dataset)) next
      both <- which(!is.na(target) & !is.na(c$value))
      if (length(both) >= min_subjects() && mean(c$value[both] == target[both]) >= DERIVED_SHARE)
        tied[[length(tied) + 1]] <- c(list(date = m), c[c("dataset", "variable", "end", "key", "level")])
    }
  }
  bounds <- list()
  for (m in setdiff(dates, "RFSTDTC")) {
    target <- as.numeric(spine$DM[[m]])
    for (c in candidates) {
      if (identical(out[[m]]$dataset, c$dataset)) next
      both <- which(!is.na(target) & !is.na(c$value))
      kept <- if (c$end == "max") c$value[both] <= target[both] else c$value[both] >= target[both]
      if (length(both) >= min_subjects() && all(kept)) bounds[[length(bounds) + 1]] <- c(list(date = m), c[c("dataset", "variable", "end", "key", "level")])
    }
  }
  list(from = out, tied = tied, bounds = bounds)
}

# Each subject's first ("min") or last ("max") date of `v`, for subjects 1 to `size`.
subject_extreme <- function(v, owner, size, end) {
  v <- as.numeric(v)
  ok <- !is.na(v) & !is.na(owner)
  as.numeric(tapply(v[ok], factor(owner[ok], seq_len(size)), end))
}

# The source's visits: the visit numbers of SV in order, which visits each subject attended
# and on which day (their first date there), and the visits that mark progress.
spine_visits <- function(spine, N) {
  sv <- spine[[VISIT_HUB]]
  if (is.null(sv$VISITNUM) || !nrow(sv))
    return(list(number = numeric(), day = matrix(NA_real_, N, 0), attended = matrix(FALSE, N, 0), progress = numeric()))
  number <- as.numeric(sv$VISITNUM)
  visits <- sort(unique(stats::na.omit(number)))
  c(list(number = visits), visit_matrix(sv, sv$.subject, visits, N),
    list(progress = progress_visits(data.frame(USUBJID = sv$.subject, VISITNUM = number))))
}

# Which of visits `V` each of subjects 1 to `size` has in SV records `sv` (subjects `owner`),
# on which day (their first SV date there) and until which (their last SVENDTC there).
visit_matrix <- function(sv, owner, V, size) {
  k <- match(as.numeric(sv$VISITNUM), V)
  attended <- matrix(FALSE, size, length(V))
  attended[cbind(owner, k)[!is.na(k), , drop = FALSE]] <- TRUE
  day <- matrix(NA_real_, size, length(V))
  main <- main_date(VISIT_HUB, names(sv))
  if (!is.na(main)) {
    d <- as.numeric(sv[[main]])
    o <- which(!is.na(d) & !is.na(k))
    o <- o[order(d[o], decreasing = TRUE)]
    day[cbind(owner[o], k[o])] <- d[o]
  }
  end <- matrix(NA_real_, size, length(V))
  if (!is.null(sv$SVENDTC)) {
    e <- as.numeric(sv$SVENDTC)
    o <- which(!is.na(e) & !is.na(k))
    o <- o[order(e[o])]
    end[cbind(owner[o], k[o])] <- e[o]
  }
  list(day = day, end = end, attended = attended)
}

# Each source subject's profile: arm (DM's arm columns), last visit that marks progress, and
# death and treatment status; and the study's last visit that marks progress.
spine_profiles <- function(spine, visits) {
  dm <- spine$DM
  N <- nrow(dm)
  date <- function(columns) {
    v <- intersect(columns, names(Filter(function(v) inherits(v, "Date"), dm)))[1]
    if (is.na(v)) rep(NA_real_, N) else as.numeric(dm[[v]])
  }
  start <- date(c("RFXSTDTC", "RFSTDTC"))
  end <- date(c("RFXENDTC", "RFENDTC"))
  last <- rep(NA_real_, N)
  for (k in which(visits$number %in% visits$progress)) last[visits$attended[, k]] <- visits$number[k]
  arm <- intersect(SPINE_ARM, names(dm))
  has <- vapply(setdiff(names(spine), "DM"), function(d) ifelse(tabulate(spine[[d]]$.subject, N) > 0, "Y", "N"), character(N))
  list(arm = if (length(arm)) key_strings(dm[arm]) else rep("", N), last = last,
    status = paste(ifelse(is.na(date("DTHDTC")), "N", "Y"), ifelse(is.na(start), "N", ifelse(is.na(end), "STARTED", "Y"))),
    end = max(c(visits$progress, NA), na.rm = TRUE), flags = cbind(matrix(has, N, dimnames = list(NULL, colnames(has))),
      as.matrix(dm[grep("^T\\.", names(dm))])))
}

# Keys that pool source subjects, finest first: arm, last visit and status; arm, whether the
# subject reached the study's last visit that marks progress, and status; that and status;
# status; none.
profile_levels <- function(p) {
  last <- level_string(p$last)
  done <- as.character(!is.na(p$last) & p$last %in% p$end)
  list(paste(p$arm, last, p$status), paste(p$arm, done, p$status), paste(done, p$status), p$status, rep("", length(p$status)))
}

# Synthetic profiles: the arm, then the last visit given the arm, then the status given both;
# then, dataset by dataset (and category by category, T.<DATASET>.<CATEGORY>), whether the
# subject has records, given the profile and the one or two flags drawn before that tell most
# about it in the source (`flag_parents()`: RS's subjects all have the CIBIC+ questionnaire),
# else those flags alone, else the profile alone; as often as among those source subjects
# (`pooled_share()`), or fixed where the profile or an earlier flag fixes it (`flag_forced()`).
draw_profiles <- function(src, n) {
  N <- length(src$arm)
  none <- rep("", n)
  all <- rep("", N)
  arm <- pooled_draw(src$arm, seq_len(N), list(none), list(all))
  last_src <- level_string(src$last)
  last <- pooled_draw(last_src, seq_len(N), list(arm, none), list(src$arm, all))
  status <- pooled_draw(src$status, seq_len(N), list(paste(arm, last), arm, none), list(paste(src$arm, last_src), src$arm, all))
  syn <- list(arm = arm, last = src$last[match(last, last_src)], status = status, end = src$end)
  want <- profile_levels(syn)
  have <- profile_levels(src)
  F <- src$flags
  profile_src <- cbind(src$arm, last_src, src$status)
  profile_syn <- cbind(arm, last, status)
  syn$flags <- matrix("N", n, ncol(F), dimnames = list(NULL, colnames(F)))
  for (i in seq_len(ncol(F))) {
    before <- seq_len(i - 1)
    parents <- flag_parents(F[, i], F[, before, drop = FALSE])
    forced <- flag_forced(F[, i], cbind(profile_src, F[, before, drop = FALSE]))
    given <- function(flags, levels, profile) {
      keys <- lapply(rev(seq_along(parents)), function(p) do.call(paste0, c(list(""), unname(as.data.frame(flags[, parents[seq_len(p)], drop = FALSE])))))
      lapply(c(unlist(lapply(keys, function(k) c(lapply(levels, paste, k), list(k))), recursive = FALSE), levels), paste, forced(cbind(profile, flags[, before, drop = FALSE])))
    }
    yes <- pooled_share(F[, i] == "Y", seq_len(N), given(syn$flags, want, profile_syn), given(F, have, profile_src))
    syn$flags[, i] <- ifelse(yes, "Y", "N")
  }
  syn
}

# Where the profile or an earlier flag (`before`, columns) fixes `flag` in the source (every
# subject with RS has the CIBIC+ questionnaire, only placebo subjects the placebo epoch; at
# least min_subjects() of them), a function giving each subject the value their columns fix,
# or "" where none does.
flag_forced <- function(flag, before) {
  rules <- list()
  for (h in seq_len(ncol(before))) for (u in unique(before[, h])) {
    v <- unique(flag[before[, h] == u])
    if (length(v) == 1 && sum(before[, h] == u) >= min_subjects()) rules[[length(rules) + 1]] <- list(h = h, u = u, v = v)
  }
  function(flags) {
    out <- rep("", nrow(flags))
    for (r in rev(rules)) out[flags[, r$h] == r$u] <- r$v
    out
  }
}

# The flags `before` (columns) that tell most about `flag` in the source, at most two, each
# chosen to add the most mutual information given those chosen.
flag_parents <- function(flag, before) {
  info <- function(key) {
    p <- table(flag, key) / length(flag)
    e <- outer(rowSums(p), colSums(p))
    sum(ifelse(p > 0, p * log(p / e), 0))
  }
  chosen <- integer()
  key <- rep("", length(flag))
  for (r in 1:2) {
    rest <- setdiff(seq_len(ncol(before)), chosen)
    if (!length(rest)) break
    gain <- vapply(rest, function(h) info(paste0(key, before[, h])), 0)
    if (max(gain) <= info(key) + 1e-9) break
    chosen <- c(chosen, rest[which.max(gain)])
    key <- paste0(key, before[, rest[which.max(gain)]])
  }
  chosen
}

# Whether each source subject has a record with each value of a dataset's content columns of
# at most VALUE_FLAG_LEVELS values (TUORRES NEW, TULOC BONE), for values at least
# min_subjects() subjects have: flags V.<DATASET>.<COLUMN>=<VALUE> (`spec` says which).
# Series are left out.
VALUE_FLAG_LEVELS <- 10

value_flags <- function(study, N) {
  flags <- list()
  spec <- list()
  for (d in setdiff(names(study$spine), "DM")) {
    x <- study$spine[[d]]
    ds <- study$datasets[[d]]
    if (identical(ds$structure$type, "series")) next
    dates <- names(Filter(function(v) inherits(v, "Date"), x))
    for (v in setdiff(content_columns(x, ds, dates), ".category")) {
      s <- ifelse(is.na(x[[v]]), NA, level_string(x[[v]]))
      u <- unique(s[!is.na(s)])
      if (length(u) < 2 || length(u) > VALUE_FLAG_LEVELS) next
      for (w in u) {
        holders <- unique(x$.subject[s %in% w])
        if (length(holders) < min_subjects()) next
        f <- paste0("V.", d, ".", v, "=", w)
        flags[[f]] <- ifelse(seq_len(N) %in% holders, "Y", "N")
        spec[[f]] <- list(column = v, value = w)
      }
    }
  }
  list(flags = matrix(unlist(flags) %||% character(), N, length(flags), dimnames = list(NULL, names(flags))), spec = spec)
}

# Each subject's flags for dataset `d`: whether they have records, in which categories and,
# unless `values` is FALSE, with which values.
presence <- function(flags, d, values = TRUE) {
  f <- colnames(flags)
  cols <- f[f == d | startsWith(f, paste0("T.", d, ".")) | values & startsWith(f, paste0("V.", d, "."))]
  if (!length(cols)) return(rep("", nrow(flags)))
  do.call(paste0, unname(as.data.frame(flags[, cols, drop = FALSE])))
}

# Visits attended (see `compose_spine()`).
draw_visits <- function(ctx) {
  V <- ctx$visits$number
  last <- ctx$syn$last
  att <- matrix(FALSE, ctx$n, length(V))
  for (k in seq_along(V)) {
    if (V[k] %in% ctx$visits$progress) {
      att[, k] <- !is.na(last) & last == V[k]
      free <- which(!is.na(last) & last > V[k])
      pool <- which(!is.na(ctx$src$last) & ctx$src$last > V[k])
    } else {
      free <- which(!is.na(last))
      pool <- which(!is.na(ctx$src$last))
    }
    if (!length(free) || !length(pool)) next
    side <- function(l) as.character(l > V[k])
    att[free, k] <- pooled_draw(as.character(ctx$visits$attended[pool, k]), pool, pool_keys(ctx$want, free, side(last[free]), FALSE),
      pool_keys(ctx$have, pool, side(ctx$src$last[pool]), FALSE)) %in% "TRUE"
  }
  att
}

# Each synthetic subject's day at every visit, as a date with day 1 on `a`: drawn for the
# visits they attend (see `compose_spine()`); for the others, the planned day or the
# previous visit's.
visit_days <- function(schedule, visits, att, arm, a) {
  V <- visits$number
  n <- nrow(att)
  N <- nrow(visits$day)
  draw <- function(q) round(quantile_draw(matrix(q, n, length(q), byrow = TRUE)))
  drawn <- matrix(NA_real_, n, length(V))
  if (!is.null(schedule)) for (k in seq_along(V)) {
    q <- schedule$deviation[[as.character(V[k])]]
    s <- schedule_day(schedule$planned, rep(V[k], n), arm) + if (is.null(q)) 0 else draw(q)
    drawn[, k] <- a + s - (s > 0)
  }
  # Days since the previous visit attended (since day 1 before the first), in the source.
  gaps <- matrix(NA_real_, N, length(V))
  before <- rep(a, N)
  for (k in seq_along(V)) {
    here <- !is.na(visits$day[, k])
    gaps[here, k] <- visits$day[here, k] - before[here]
    before[here] <- visits$day[here, k]
  }
  following <- matrix(NA_real_, n, length(V))
  next_day <- rep(NA_real_, n)
  for (k in rev(seq_along(V))) {
    following[, k] <- next_day
    planned <- att[, k] & !is.na(drawn[, k])
    next_day[planned] <- drawn[planned, k]
  }
  syn <- matrix(NA_real_, n, length(V))
  previous <- rep(NA_real_, n)
  for (k in seq_along(V)) {
    gap <- if (any(!is.na(gaps[, k]))) draw(number_margin(gaps[, k], seq_len(N))) else rep(0, n)
    from <- ifelse(is.na(previous), a, previous)
    d <- ifelse(is.na(drawn[, k]), pmin(from + gap, following[, k], na.rm = TRUE), drawn[, k])
    d <- pmax(d, previous, na.rm = TRUE)
    syn[, k] <- ifelse(att[, k], d, ifelse(is.na(drawn[, k]), from, drawn[, k]))
    previous[att[, k]] <- d[att[, k]]
  }
  syn
}

# DM's spine: the drawn arm's columns, and the dates (`draw_dates()`) in order of their median
# day, each placed from the visits, day 1 and the dates before it. A date taken from a dataset
# (`derived_dates()`) is replaced once that dataset is composed, so no other date is placed from it.
compose_dm <- function(dm, dates, ctx) {
  N <- ctx$N
  n <- ctx$n
  out <- data.frame(.j = seq_len(n))
  arm <- intersect(SPINE_ARM, names(dm))
  for (v in arm) out[[v]] <- dm[[v]][match(ctx$syn$arm, ctx$src$arm)]
  flags <- grep("^T\\.", names(dm), value = TRUE)
  for (v in flags) out[[v]] <- rep("N", n)
  for (v in setdiff(names(dm), c(".subject", arm, flags, dates))) {
    s <- level_string(dm[[v]])
    out[[v]] <- dm[[v]][match(pooled_draw(s, seq_len(N), ctx$want, ctx$have), s)]
  }
  typical <- vapply(dates, function(v) stats::median(as.numeric(dm[[v]]) - ctx$a, na.rm = TRUE), 0)
  placed_src <- placed_syn <- NULL
  for (v in dates[order(is.na(typical), typical)]) {
    d <- as.numeric(dm[[v]])
    out[[v]] <- draw_dates(d, landmarks(seq_len(N), NA, ctx$src_days, placed_src, ctx$a), seq_len(N), rep("", N),
      landmarks(seq_len(n), NA, ctx$syn_days, placed_syn, ctx$a), seq_len(n), rep("", n), ctx)
    if (is.null(ctx$derived[[v]])) {
      placed_src <- cbind(placed_src, d)
      placed_syn <- cbind(placed_syn, out[[v]])
      colnames(placed_src)[ncol(placed_src)] <- colnames(placed_syn)[ncol(placed_syn)] <- v
    }
  }
  out[c(".j", setdiff(names(dm), ".subject"))]
}

# One dataset's records (see `compose_spine()`), dates as numbers, with the synthetic
# subject's number (.j). A record's content (`content_columns()`: the test, the category, the
# time point) comes whole with its panel, set or series; at a visit, the visit's name and
# planned day are those most of the dataset's subjects have there, else SV's.
compose_records <- function(study, name, composed, ctx) {
  ds <- study$datasets[[name]]
  s <- ds$structure
  x <- study$spine[[name]]
  columns <- setdiff(names(x), ".subject")
  V <- ctx$visits$number
  dates <- names(Filter(function(v) inherits(v, "Date"), x))
  main <- main_date(ds$prefix, dates)
  number <- if (!is.null(x$VISITNUM)) as.numeric(x$VISITNUM)
  series <- identical(s$type, "series")
  by_visit <- !series && !is.null(number) && (!is.null(s$visit) || identical(s$type, "count"))
  at_visits <- series && identical(s$keys[1], "VISITNUM")
  o <- do.call(order, c(list(x$.subject), if (series) list(x$.series), if (!is.null(number)) list(number),
    if (!is.na(main)) list(as.numeric(x[[main]]))))
  x <- x[o, , drop = FALSE]
  number <- number[o]
  # Drawn only among source subjects with the same flags for the dataset (see `draw_profiles()`),
  # with the same values too and the same datasets where enough have them (a TR link group of
  # subjects without RS from subjects without RS); a panel, where none of these has enough,
  # among those who have the dataset (`ctx$last`: a rare set of MH categories).
  within <- function(levels, flags) {
    all <- presence(flags, name)
    some <- presence(flags, name, values = FALSE)
    every <- do.call(paste0, c(list(all), unname(as.data.frame(flags[, !grepl(".", colnames(flags), fixed = TRUE), drop = FALSE]))))
    c(lapply(levels, paste, every), lapply(levels, paste, all), if (!identical(all, some)) lapply(levels, paste, some))
  }
  if (name %in% colnames(ctx$src$flags)) {
    ctx$last_have <- lapply(ctx$have, paste, unname(ctx$src$flags[, name]))
    ctx$last_want <- lapply(ctx$want, paste, unname(ctx$syn$flags[, name]))
  }
  ctx$have <- within(ctx$have, ctx$src$flags)
  ctx$want <- within(ctx$want, ctx$syn$flags)
  content <- content_columns(x, ds, dates)
  tuple <- if (length(content)) key_strings(x[content]) else rep("", nrow(x))
  code <- match(tuple, unique(tuple))
  row_of <- match(seq_along(unique(tuple)), code)
  # A linked record (TR to its TU lesion) is drawn among subjects with the same linked
  # records where enough have them, and dropped when the subject has none to link to.
  link <- ds$link
  target <- if (!is.null(link)) composed[[link$dataset]]
  extra_src <- extra_syn <- NULL
  if (!is.null(target)) {
    y <- study$spine[[link$dataset]]
    id_set <- function(id, owner, size) vapply(split(level_string(id), factor(owner, seq_len(size))),
      function(v) paste(sort(unique(v)), collapse = " "), "")
    extra_src <- id_set(y[[link$by]], y$.subject, ctx$N)
    extra_syn <- id_set(target[[link$by]], target$.j, ctx$n)
  }
  draw <- function(who, ctx) if (by_visit) draw_visit_records(x, code, number, ctx, extra_src, extra_syn, who) else
    draw_record_set(x, code, ctx, extra_src, extra_syn, who)
  ta <- study_design(study)$TA
  elements <- name == "SE" && !is.null(x$ETCD) && all(c("ARMCD", "ETCD", "TAETORD") %in% names(ta))
  rows <- if (elements) draw_elements(x, code, ta, subject_arm(study$spine$DM), subject_arm(composed$DM), ctx) else
    if (series) draw_series(x, code, number, at_visits, ctx) else draw(seq_len(ctx$n), ctx)
  # Redrawn, up to 20 times, for subjects whose records do not have the dataset, categories
  # and values their flags say (visits that all drew no CIBIC+ questionnaire): among source
  # subjects with the same flags of those missed so far, else the same of the one fewest
  # source subjects have (a NEW lesion, where few have the subject's TULOC flags too); a
  # subject keeps the earlier records where the new ones miss more.
  f <- colnames(ctx$syn$flags)
  flagged <- f[f == name | startsWith(f, paste0("T.", name, ".")) | startsWith(f, paste0("V.", name, "."))]
  if (!series && !elements && length(flagged)) {
    category <- if (!is.null(x$.category)) x$.category[row_of]
    wrong_flags <- function(rows) vapply(flagged, function(g) {
      v <- ctx$values[[g]]
      keep <- if (g == name) TRUE else if (!is.null(v)) level_string(x[[v$column]][row_of][rows$code]) %in% v$value else
        category[rows$code] %in% substring(g, nchar(name) + 4)
      ifelse(seq_len(ctx$n) %in% rows$.j[keep], "Y", "N")
    }, character(ctx$n)) != ctx$syn$flags[, flagged, drop = FALSE]
    holders <- vapply(flagged, function(g) table(factor(ctx$src$flags[, g], c("N", "Y"))), integer(2))
    flag_key <- function(flags, g) do.call(paste0, unname(as.data.frame(flags[, g, drop = FALSE])))
    missed <- matrix(FALSE, ctx$n, length(flagged))
    wrong <- wrong_flags(rows)
    for (try in 1:20) {
      bad <- which(rowSums(wrong) > 0)
      if (!length(bad)) break
      missed <- missed | wrong
      rarest <- vapply(bad, function(i) {
        m <- which(missed[i, ])
        m[which.min(holders[cbind(match(ctx$syn$flags[i, flagged[m]], c("N", "Y")), m)])]
      }, 0L)
      group <- paste(apply(missed[bad, , drop = FALSE], 1, function(m) paste(which(m), collapse = " ")), rarest)
      redrawn <- do.call(rbind, lapply(unique(group), function(g) {
        all <- flagged[which(missed[bad[group == g][1], ])]
        one <- flagged[rarest[group == g][1]]
        same <- ctx
        same$have <- c(lapply(ctx$have, paste, flag_key(ctx$src$flags, all)), lapply(ctx$have, paste, flag_key(ctx$src$flags, one)), ctx$have)
        same$want <- c(lapply(ctx$want, paste, flag_key(ctx$syn$flags, all)), lapply(ctx$want, paste, flag_key(ctx$syn$flags, one)), ctx$want)
        draw(bad[group == g], same)
      }))
      tried <- rbind(rows[!rows$.j %in% bad, , drop = FALSE], redrawn)
      now <- wrong_flags(tried)
      worse <- bad[rowSums(now[bad, , drop = FALSE]) > rowSums(wrong[bad, , drop = FALSE])]
      rows <- rbind(tried[!tried$.j %in% worse, , drop = FALSE], rows[rows$.j %in% worse, , drop = FALSE])
      wrong <- now
      wrong[worse, ] <- wrong_flags(rows)[worse, , drop = FALSE]
    }
  }
  if (!nrow(rows)) return(data.frame(.j = integer(), x[0, columns, drop = FALSE], check.names = FALSE))
  out <- data.frame(.j = rows$.j, x[row_of[rows$code], content, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  if (series) out$.series <- rows$series
  if (!is.null(number)) {
    out$VISITNUM <- if (by_visit || at_visits) rows$visit else assign_visits(number, x$.subject, occurrence(x$.subject, code), out$.j,
      occurrence(rows$.j, rows$code), ctx)
    hub <- study$spine[[VISIT_HUB]]
    for (v in intersect(c("VISIT", "VISITDY"), columns)) {
      out[[v]] <- visit_values(x[[v]], number, x$.subject, out$VISITNUM)
      gap <- is.na(out[[v]])
      if (any(gap) && !is.null(hub[[v]])) out[[v]][gap] <- visit_values(hub[[v]], as.numeric(hub$VISITNUM), hub$.subject, out$VISITNUM[gap])
    }
  }
  # Dates, the main date (--STDTC, else --DTC) first, by visit (and time point) for records at
  # visits, by content (and occurrence) otherwise: once per visit and time point for records at
  # visits, once per series for the dates a series shares (`series_dates()`).
  lean <- Filter(function(m) is.null(ctx$derived[[m]]) || ctx$derived[[m]]$dataset != name && ctx$derived[[m]]$dataset %in% names(composed),
    colnames(ctx$dm_src))
  L_src <- landmarks(x$.subject, if (!is.null(number)) match(number, V) else NA, ctx$src_days, ctx$dm_src[, lean, drop = FALSE], ctx$a)
  L_syn <- landmarks(out$.j, if (!is.null(out$VISITNUM)) match(out$VISITNUM, V) else NA, ctx$syn_days, ctx$dm_syn[, lean, drop = FALSE], ctx$a)
  # And the end of the record's SV visit, once SV is composed.
  if (!is.null(number) && !is.null(ctx$syn_ends)) {
    L_src <- cbind(L_src, visit_end = ctx$visits$end[cbind(x$.subject, match(number, V))])
    L_syn <- cbind(L_syn, visit_end = ctx$syn_ends[cbind(out$.j, match(out$VISITNUM, V))])
  }
  # And the subject's date at the same visit in each dataset composed before (LBHE's at LBCH's).
  if (!is.null(number) && length(ctx$visit_dates)) {
    at_src <- cbind(x$.subject, match(number, V))
    at_syn <- cbind(out$.j, match(out$VISITNUM, V))
    L_src <- cbind(L_src, do.call(cbind, lapply(ctx$visit_dates, function(e) e$src[at_src])))
    L_syn <- cbind(L_syn, do.call(cbind, lapply(ctx$visit_dates, function(e) e$syn[at_syn])))
  }
  # A visit's time points (PCTPT) are dated apart.
  point <- intersect(paste0(ds$prefix, c("TPTREF", "TPTNUM", "TPT", "ELTM")), content)
  point_src <- if (length(point)) key_strings(x[point]) else ""
  point_syn <- if (length(point)) key_strings(out[point]) else ""
  stratum_src <- if (by_visit) paste(level_string(number), point_src) else if (series) as.character(code) else occurrence(x$.subject, code)
  stratum_syn <- if (by_visit) paste(level_string(out$VISITNUM), point_syn) else if (series) as.character(rows$code) else occurrence(rows$.j, rows$code)
  # A series' dates by its first visit too where enough subjects have it (a medication first
  # recorded at week 8 started near then).
  if (series && !is.null(number)) {
    start_src <- paste(stratum_src, stats::ave(number, x$.subject, x$.series, FUN = min))
    start_syn <- paste(stratum_syn, stats::ave(out$VISITNUM, out$.j, out$.series, FUN = min))
    big <- unique(start_src[subjects_holding(start_src, x$.subject) >= min_subjects()])
    stratum_src <- ifelse(start_src %in% big, start_src, stratum_src)
    stratum_syn <- ifelse(start_syn %in% big, start_syn, stratum_syn)
  }
  whole <- if (series) series_dates(x, dates) else character()
  source_dates <- lapply(x[dates], as.numeric)
  for (v in c(intersect(main, dates), setdiff(dates, main))) {
    is_main <- identical(v, main)
    unit_src <- if (v %in% whole) paste(x$.subject, x$.series) else if (by_visit && is_main) paste(x$.subject, number, point_src) else seq_len(nrow(x))
    unit_syn <- if (v %in% whole) paste(out$.j, out$.series) else if (by_visit && is_main) paste(out$.j, out$VISITNUM, point_syn) else seq_len(nrow(out))
    from_src <- L_src
    from_syn <- L_syn
    if (!is_main && !is.na(main)) {
      from_src <- cbind(main = source_dates[[main]], following = next_date(source_dates[[main]], x$.subject), from_src)
      from_syn <- cbind(main = out[[main]], following = next_date(out[[main]], out$.j), from_syn)
    }
    first <- !duplicated(unit_src)
    out[[v]] <- draw_dates(source_dates[[v]][first], from_src[first, , drop = FALSE], x$.subject[first], stratum_src[first],
      from_syn, out$.j, stratum_syn, ctx, unit_syn)
  }
  if (elements) out <- chain_elements(out, ds$prefix)
  for (v in setdiff(columns, names(out))) out[[v]] <- x[[v]][rep(NA_integer_, nrow(out))]
  if (!is.null(target)) out <- out[is.na(out[[link$key]]) | paste(out$.j, out[[link$key]]) %in% paste(target$.j, target[[link$by]]), , drop = FALSE]
  # Records the source never repeats within a subject are not repeated.
  keys <- intersect(s$keys, names(out))
  if (length(keys) && !anyDuplicated(x[c(".subject", keys)])) out <- out[!duplicated(out[c(".j", keys)]), , drop = FALSE]
  # Sorted as generation sorts them; then numbered.
  sort <- intersect(c(".j", s$keys, main), names(out))
  out <- out[do.call(order, unname(lapply(out[sort], function(v) if (is.character(v)) v else as.numeric(v)))), , drop = FALSE]
  for (v in setdiff(s$id, c(TIMING_VARIABLES, paste0(ds$prefix, TIMING_SUFFIXES))))
    out[[v]] <- if (v %in% content) free_ids(out[[v]], out$.j) else stats::ave(seq_len(nrow(out)), out$.j, FUN = seq_along)
  if (series) {
    first <- !duplicated(out$.series)
    rank <- stats::ave(seq_len(sum(first)), out$.j[first], FUN = seq_along)[match(out$.series, out$.series[first])]
    spid <- s$series
    if (spid %in% columns) out[[spid]] <- series_ids(x[[spid]], rank)
    out$.series <- group_index(out, c(".j", ".series"))
  }
  rownames(out) <- NULL
  out[c(".j", columns)]
}

# SE's elements for each synthetic subject, in order: the first k of the arm's path in TA, k
# drawn as among source subjects of the same profile, and the elements off the path (a
# follow-up, an unplanned element, a screen failure's screening), each after the same number
# of planned elements as in the source, drawn given k. An element is the record most of the
# arm's source subjects have for it.
draw_elements <- function(x, code, ta, arm_src, arm_syn, ctx) {
  ta <- ta[order(ta$TAETORD), , drop = FALSE]
  path <- split(level_string(ta$ETCD), level_string(ta$ARMCD))
  etcd <- level_string(x$ETCD)
  arm <- arm_src[x$.subject]
  planned <- vapply(seq_along(etcd), function(i) etcd[i] %in% unlist(path[arm[i]]), TRUE)
  after <- stats::ave(as.numeric(planned), x$.subject, FUN = cumsum)
  subjects <- factor(x$.subject, seq_len(ctx$N))
  k_src <- tabulate(x$.subject[planned], ctx$N)
  off_src <- vapply(split(paste(etcd, after, sep = "@")[!planned], subjects[!planned]), paste, "", collapse = " ")
  k <- as.integer(pooled_draw(as.character(k_src), seq_len(ctx$N), ctx$want, ctx$have))
  own <- lapply(arm_syn, function(a) unlist(path[a]) %||% character())
  k <- pmin(k, lengths(own))
  off <- pooled_draw(off_src, seq_len(ctx$N), c(lapply(ctx$want, paste, k), ctx$want), c(lapply(ctx$have, paste, k_src), ctx$have))
  top <- tapply(code, paste(arm, etcd), function(z) as.integer(names(which.max(table(z)))))
  any_arm <- tapply(code, etcd, function(z) as.integer(names(which.max(table(z)))))
  out <- lapply(seq_len(ctx$n), function(j) {
    e <- own[[j]][seq_len(k[j])]
    at <- seq_along(e)
    if (!is.na(off[j]) && nzchar(off[j])) {
      o <- strsplit(strsplit(off[j], " ", fixed = TRUE)[[1]], "@", fixed = TRUE)
      e <- c(e, vapply(o, `[`, "", 1))
      at <- c(at, pmin(as.numeric(vapply(o, `[`, "", 2)), k[j]) + seq_along(o) / (length(o) + 1))
    }
    e <- e[order(at)]
    cd <- top[paste(arm_syn[j], e)]
    cd[is.na(cd)] <- any_arm[e[is.na(cd)]]
    data.frame(.j = rep(j, length(e)), code = unname(cd))
  })
  do.call(rbind, out) %||% data.frame(.j = integer(), code = integer())
}

# SE's elements in order `o` (rows in their order by default), each starting no earlier than
# the one before and ending where the next starts.
chain_elements <- function(x, prefix, o = seq_len(nrow(x))) {
  start <- paste0(prefix, "STDTC")
  end <- paste0(prefix, "ENDTC")
  if (!nrow(x) || !start %in% names(x)) return(x)
  j <- x$.j[o]
  st <- stats::ave(ifelse(is.na(x[[start]][o]), -Inf, x[[start]][o]), j, FUN = cummax)
  x[[start]][o] <- ifelse(is.finite(st), st, NA)
  if (end %in% names(x)) {
    following <- c(x[[start]][o][-1], NA)
    following[c(j[-1] != j[-length(j)], TRUE)] <- NA
    x[[end]][o] <- ifelse(is.na(following), pmax(x[[end]][o], x[[start]][o]), following)
  }
  x
}

# Each subject's actual arm (ACTARMCD), else the planned one (ARMCD), as in TA.
subject_arm <- function(dm) {
  arm <- if (!is.null(dm$ACTARMCD)) level_string(dm$ACTARMCD) else rep(MISSING, nrow(dm))
  planned <- if (!is.null(dm$ARMCD)) level_string(dm$ARMCD) else rep(MISSING, nrow(dm))
  ifelse(arm == MISSING | !nzchar(arm), planned, arm)
}

# EPOCH of each dataset's records: TA's epoch of the subject's SE element holding the record's
# start (--STDTC, else --DTC), in the source dataset's spelling; the drawn one where no planned
# element holds it.
element_epochs <- function(study, composed) {
  ta <- study_design(study)$TA
  se <- composed$SE
  if (is.null(se) || !all(c("ARMCD", "ETCD", "EPOCH") %in% names(ta)) || !all(c("ETCD", "SESTDTC") %in% names(se))) return(composed)
  epoch <- stats::setNames(level_string(ta$EPOCH), paste(level_string(ta$ARMCD), level_string(ta$ETCD)))
  arm <- subject_arm(composed$DM)[match(se$.j, composed$DM$.j)]
  se <- data.frame(.j = se$.j, start = as.numeric(se$SESTDTC), end = as.numeric(se$SEENDTC %||% se$SESTDTC),
    epoch = unname(epoch[paste(arm, level_string(se$ETCD))]))
  se$end[is.na(se$end)] <- Inf
  se <- se[!is.na(se$epoch) & !is.na(se$start), , drop = FALSE]
  for (name in setdiff(names(composed), c("SE", "DM"))) {
    x <- composed[[name]]
    if (is.null(x$EPOCH) || !nrow(x)) next
    prefix <- study$datasets[[name]]$prefix
    v <- intersect(paste0(prefix, c("STDTC", "DTC")), names(x))[1]
    if (is.na(v)) next
    d <- as.numeric(x[[v]])
    m <- merge(data.frame(i = seq_len(nrow(x)), .j = x$.j, d = d), se)
    m <- m[!is.na(m$d) & m$d >= m$start & m$d <= m$end, , drop = FALSE]
    m <- m[!duplicated(m$i), , drop = FALSE]
    spelled <- level_string(study$spine[[name]]$EPOCH)
    spelling <- tapply(spelled, toupper(spelled), most_common)
    new <- spelling[toupper(m$epoch)]
    x$EPOCH[m$i] <- ifelse(is.na(new), m$epoch, new)
    composed[[name]] <- x
  }
  composed
}

# Contents numbered by occurrence within the subject (a second OTHER EVENT apart from the first).
occurrence <- function(owner, code) paste(code, stats::ave(seq_along(code), owner, code, FUN = seq_along))

# The spine columns that make up a record's content: all but the dates, the visit with its
# name and planned day, study days and numbering (--SEQ, a series' --SPID), unless the record
# keys give the number (QSSEQ in blocks per questionnaire).
content_columns <- function(x, ds, dates) {
  s <- ds$structure
  drop <- c(".subject", ".series", dates, sub("DTC$", "DY", dates), if (!is.null(x$VISITNUM)) c("VISITNUM", "VISIT", "VISITDY"),
    Filter(function(v) !keyed_id(x, s$keys, v), s$id), if (identical(s$type, "series")) s$series)
  setdiff(names(x), drop)
}

# Records at visits: at each visit, the panel of each of synthetic subjects `who` who attended it
# (`draw_panels()`) among the source subjects who attended it, on the same side of their last
# visit that marks progress (the questionnaire at an early stop); at a visit SV does not have
# (an extra lab visit), among all.
draw_visit_records <- function(x, code, number, ctx, extra_src, extra_syn, who = seq_len(ctx$n)) {
  V <- ctx$visits$number
  out <- list()
  side <- function(last, u) ifelse(is.na(last), "", sign(last - u))
  for (u in unique(number)) {
    at <- which(number %in% u)
    k <- match(u, V)
    src <- if (is.na(k)) seq_len(ctx$N) else sort(union(which(ctx$visits$attended[, k]), x$.subject[at]))
    to <- if (is.na(k)) who else intersect(which(ctx$att[, k]), who)
    if (!length(to)) next
    panels <- draw_panels(code[at], x$.subject[at], src, to, ctx, paste(extra_src, side(ctx$src$last, u)), paste(extra_syn, side(ctx$syn$last, u)))
    out[[length(out) + 1]] <- data.frame(.j = rep(to, lengths(panels)), code = unlist(panels) %||% integer(), visit = rep(u, sum(lengths(panels))))
  }
  do.call(rbind, out) %||% data.frame(.j = integer(), code = integer(), visit = numeric())
}

# Records per subject (dispositions, SE's elements): the set of each of synthetic subjects
# `who`, among all source subjects.
draw_record_set <- function(x, code, ctx, extra_src, extra_syn, who = seq_len(ctx$n)) {
  panels <- draw_panels(code, x$.subject, seq_len(ctx$N), who, ctx, extra_src, extra_syn)
  data.frame(.j = rep(who, lengths(panels)), code = unlist(panels) %||% integer())
}

# For synthetic subjects `to`, a panel (the contents `code` of their records, none being a
# panel too) drawn among those of source subjects `src` (`owner` holds each record), within
# the subjects' linked records (`extra_src`, `extra_syn`) where enough have the same. In the
# finest cell of at least min_subjects() source subjects, the set of contents is drawn whole
# where enough of them share it, else whether the subject has each content, among contents
# enough subjects have. Then each content's number of records is drawn given the set
# (`pooled_number()`: three historical diagnoses).
draw_panels <- function(code, owner, src, to, ctx, extra_src = NULL, extra_syn = NULL) {
  by <- factor(owner, src)
  set <- unname(vapply(split(code, by), function(z) paste(sort(unique(z)), collapse = " "), ""))
  have <- c(pool_keys(ctx$have, src, extra_src[src]), lapply(ctx$last_have, `[`, src))
  want <- c(pool_keys(ctx$want, to, extra_syn[to]), lapply(ctx$last_want, `[`, to))
  held <- unique(code[subjects_holding(code, owner) >= min_subjects()])
  has <- as.vector(table(by, factor(code, held)) > 0)
  out <- rep(list(integer()), length(to))
  todo <- seq_along(to)
  for (l in seq_along(want)) {
    size <- table(have[[l]])
    cell <- todo[want[[l]][todo] %in% names(size)[size >= min_subjects()]]
    if (!length(cell)) next
    drawn <- pooled_draw(set, src, list(want[[l]][cell]), list(have[[l]]), fallback = FALSE)
    whole <- !is.na(drawn)
    out[cell[whole]] <- lapply(drawn[whole], function(p) if (p == "") integer() else as.integer(strsplit(p, " ", fixed = TRUE)[[1]]))
    rest <- cell[!whole]
    if (length(rest) && length(held)) {
      down <- l:length(want)
      m <- pooled_share(has, rep(src, length(held)), lapply(want[down], function(w) paste(rep(held, each = length(rest)), w[rest])),
        lapply(have[down], function(h) paste(rep(held, each = length(src)), h)))
      m <- matrix(m, length(rest))
      out[rest] <- lapply(seq_along(rest), function(i) held[m[i, ]])
    }
    todo <- setdiff(todo, cell)
    if (!length(todo)) break
  }
  # Records per content, among the source's subjects with that content (and the same set).
  pair <- !duplicated(paste(owner, code))
  size <- as.vector(table(paste(owner, code))[paste(owner, code)[pair]])
  if (all(size == 1)) return(out)
  i <- rep(seq_along(to), lengths(out))
  cc <- unlist(out) %||% integer()
  sets <- vapply(out, function(z) paste(sort(z), collapse = " "), "")
  levels_src <- lapply(pool_keys(ctx$have, owner[pair], extra_src[owner[pair]]), paste, code[pair])
  levels_syn <- lapply(pool_keys(ctx$want, to[i], extra_syn[to[i]]), paste, cc)
  count <- pmax(1, pooled_number(size, owner[pair], c(lapply(levels_syn, paste, sets[i]), levels_syn),
    c(lapply(levels_src, paste, set[match(owner[pair], src)]), levels_src)))
  count[is.na(count)] <- 1
  unname(split(rep(cc, count), factor(rep(i, count), seq_along(to))))
}

# Series (a medication recorded at several visits): each synthetic subject's number of
# series, then each series' content, and its number of records given the content; a series at
# visits, its first visit given the content, then given both whether it runs to the last visit
# of the dataset the subject attended, as in the source, or else its number of records, and it takes the
# dataset's visits attended from the first at or after that visit.
draw_series <- function(x, code, number, at_visits, ctx) {
  first <- !duplicated(x$.series)
  owner <- x$.subject[first]
  size <- tabulate(match(x$.series, x$.series[first]))
  count <- pooled_number(tabulate(owner, ctx$N), seq_len(ctx$N), ctx$want, ctx$have)
  count[is.na(count)] <- 0
  j <- rep(seq_len(ctx$n), count)
  if (!length(j)) return(data.frame(.j = integer(), code = integer(), visit = numeric(), series = integer()))
  content <- as.integer(pooled_draw(as.character(code[first]), owner, pool_keys(ctx$want, j), pool_keys(ctx$have, owner)))
  want <- function(syn) c(pool_keys(ctx$want, j, syn, alone = FALSE), pool_keys(ctx$want, j, content))
  have <- function(src) c(pool_keys(ctx$have, owner, src, alone = FALSE), pool_keys(ctx$have, owner, code[first]))
  given <- function(v, src = code[first], syn = content) pooled_draw(v, owner, want(syn), have(src))
  if (!at_visits) {
    records <- pmax(1, pooled_number(size, owner, want(content), have(code[first])), na.rm = TRUE)
    visit <- rep(NA_real_, sum(records))
  } else {
    V <- ctx$visits$number
    start_src <- number[first]
    start <- pooled_number(start_src, owner, want(content), have(code[first]))
    # A series that ends at the subject's last visit of the dataset attended in the source ends there.
    seen <- ctx$visits$attended[, V %in% number, drop = FALSE]
    last <- apply(seen, 1, function(a) max(V[V %in% number][a], -Inf))[owner]
    ends <- number[!duplicated(x$.series, fromLast = TRUE)] == last
    to_end <- pooled_share(ends, owner, want(paste(content, start)), have(paste(code[first], start_src)))
    other <- which(!ends)
    size_to <- if (length(other)) pooled_number(size[other], owner[other], want(paste(content, start)),
      lapply(have(paste(code[first], start_src)), `[`, other)) else rep(NA_real_, length(j))
    size_to <- pmax(1, round(size_to), na.rm = TRUE)
    visits <- lapply(seq_along(j), function(i) {
      k <- which(ctx$att[j[i], ] & V %in% number)
      if (!length(k)) return(numeric())
      p <- which(V[k] >= start[i])[1]
      if (is.na(p)) p <- if (is.na(start[i])) 1L else length(k)
      V[k[p:if (to_end[i]) length(k) else min(length(k), p + size_to[i] - 1)]]
    })
    records <- lengths(visits)
    visit <- unlist(visits) %||% numeric()
  }
  i <- rep(seq_along(j), records)
  data.frame(.j = j[i], code = content[i], visit = visit, series = i)
}

# The dates a series has the same on all its records (a medication's start), in at least 95%
# of the series with several records.
series_dates <- function(x, dates) {
  several <- x$.series %in% x$.series[duplicated(x$.series)]
  if (!any(several)) return(character())
  Filter(function(v) mean(tapply(level_string(x[[v]])[several], x$.series[several], function(z) length(unique(z)) == 1)) >= 0.95, dates)
}

# A series' identifiers (--SPID) numbered by `rank`, in the form most of the source's have
# (a prefix and as many digits).
series_ids <- function(v, rank) {
  if (!is.character(v)) return(rank)
  parts <- regmatches(v, regexec("^(\\D*)(\\d+)$", v))
  parts <- parts[lengths(parts) == 3]
  if (!length(parts)) return(as.character(rank))
  form <- most_common(vapply(parts, function(p) paste(p[2], nchar(p[3]), sep = "\r"), ""))
  form <- strsplit(form, "\r", fixed = TRUE)[[1]]
  sprintf("%s%0*d", if (length(form) == 2) form[1] else "", as.integer(form[length(form)]), rank)
}

# The visit of records drawn without one where the dataset has VISITNUM (a disposition): per
# content, a fixed visit (the one most subjects have), the subject's first visit, last visit
# that marks progress, last visit or none, whichever most source records have; a visit the
# subject does not have in SV gives the nearest one they have.
assign_visits <- function(number, owner, code, j, code_to, ctx) {
  V <- ctx$visits$number
  out <- rep(NA_real_, length(j))
  if (!length(V)) return(out)
  ends <- function(days) {
    seen <- !is.na(days)
    list(first = apply(seen, 1, function(a) V[which(a)[1]]), last = apply(seen, 1, function(a) V[rev(which(a))[1]]))
  }
  src <- ends(ctx$src_days)
  syn <- ends(ctx$syn_days)
  same <- function(a, b) !is.na(a) & !is.na(b) & a == b
  for (cd in unique(code_to)) {
    rows <- which(code_to == cd)
    at <- which(code == cd)
    if (!length(at)) next
    v <- number[at]
    ok <- !is.na(v) & subjects_holding(level_string(v), owner[at]) >= min_subjects()
    fixed <- if (any(ok)) {
      holders <- tapply(owner[at][ok], v[ok], function(o) length(unique(o)))
      as.numeric(names(which.max(holders)))
    }
    share <- c(fixed = if (length(fixed)) mean(same(v, fixed)) else -1, first = mean(same(v, src$first[owner[at]])),
      progress = mean(same(v, ctx$src$last[owner[at]])), last = mean(same(v, src$last[owner[at]])), none = mean(is.na(v)))
    out[rows] <- switch(names(which.max(share)), fixed = fixed,
      first = syn$first[j[rows]], progress = ctx$syn$last[j[rows]], last = syn$last[j[rows]], none = NA_real_)
  }
  k <- match(out, V)
  on <- which(!is.na(k))
  out[on] <- V[nearest_visit(ctx$att, j[on], k[on], V)]
  out
}

# A visit's name or planned day (VISIT, VISITDY) at each of `visit`: the value most of the
# dataset's subjects have at that visit number, among those at least min_subjects() have.
visit_values <- function(v, number, owner, visit) {
  s <- level_string(v)
  key <- paste(number, s, sep = "\r")
  held <- subjects_holding(key, owner) >= min_subjects()
  if (!any(held)) return(v[rep(NA_integer_, length(visit))])
  holders <- tapply(owner[held], key[held], function(o) length(unique(o)))
  at <- sub("\r.*", "", names(holders))
  best <- vapply(split(seq_along(holders), at), function(i) names(holders)[i][which.max(holders[i])], "")
  v[match(best[as.character(visit)], key)]
}

# Each record's next main date: the earliest of the subject's main dates after its own.
next_date <- function(d, owner) {
  out <- rep(NA_real_, length(d))
  ok <- which(!is.na(d))
  if (!length(ok)) return(out)
  key <- paste(owner[ok], d[ok])
  u <- ok[!duplicated(key)]
  u <- u[order(owner[u], d[u])]
  following <- c(d[u][-1], NA)
  following[c(owner[u][-1] != owner[u][-length(u)], TRUE)] <- NA
  out[ok] <- following[match(key, paste(owner[u], d[u]))]
  out
}

# The landmarks a date is placed from (see `draw_dates()`), for records of subjects `j` at
# visit columns `k` (NA without one), from the subjects' visit dates (`days`, NA where missed)
# and DM dates (`dates`): the record's visit, the next visit attended after it, the DM dates,
# the first and the last visit attended, and day 1.
landmarks <- function(j, k, days, dates, a) {
  k <- rep_len(k, length(j))
  first <- last <- rep(NA_real_, nrow(days))
  following <- days
  if (ncol(days)) {
    first <- suppressWarnings(apply(days, 1, min, na.rm = TRUE))
    last <- suppressWarnings(apply(days, 1, max, na.rm = TRUE))
    first[!is.finite(first)] <- last[!is.finite(last)] <- NA
    upcoming <- rep(NA_real_, nrow(days))
    for (c in rev(seq_len(ncol(days)))) {
      following[, c] <- upcoming
      upcoming <- ifelse(is.na(days[, c]), upcoming, days[, c])
    }
  }
  at <- cbind(j, k)
  out <- cbind(visit = if (ncol(days)) days[at] else rep(NA_real_, length(j)), next_visit = if (ncol(days)) following[at] else rep(NA_real_, length(j)))
  if (length(dates)) out <- cbind(out, dates[j, , drop = FALSE])
  cbind(out, first = first[j], last = last[j], day1 = rep(a, length(j)))
}

# Dates of synthetic records (see `compose_spine()`): from the source dates `d` with their
# records' landmarks `L`, subjects `owner` and strata `stratum` (a disposition's dates apart
# from a milestone's; strata of fewer than min_subjects() subjects pooled), for synthetic
# records with landmarks `to`, subjects `j` and strata `stratum_to`. Within a stratum, whether
# the date is known is drawn as often as in the source. A known date is the landmark whose gap
# to the date varies least in the source (10th to 90th percentile), among those at least
# min_subjects() subjects have, or the next where a record lacks it, plus a gap drawn from the
# source's (`pooled_number()`); and it is kept on or after the landmarks the source's dates
# are always on or after, and on or before those they are always on or before (an adverse
# event's end after its start, a disposition by the last visit), and above all within the
# record's SV visit. Records of one `unit` (a visit's tests) share their date.
draw_dates <- function(d, L, owner, stratum, to, j, stratum_to, ctx, unit = seq_along(j)) {
  k <- min_subjects()
  out <- rep(NA_real_, length(j))
  if (!length(j) || !length(d)) return(out)
  big <- unique(stratum[subjects_holding(stratum, owner) >= k])
  stratum[!stratum %in% big] <- RARE
  stratum_to[!stratum_to %in% big] <- RARE
  first <- which(!duplicated(unit))
  for (s in unique(stratum_to[first])) {
    src <- which(stratum == s)
    if (!length(src)) src <- seq_along(d)
    rows <- first[stratum_to[first] == s]
    have <- lapply(ctx$have, `[`, owner[src])
    rows <- rows[pooled_draw(as.character(!is.na(d[src])), owner[src], lapply(ctx$want, `[`, j[rows]), have) %in% "TRUE"]
    if (!length(rows)) next
    want <- lapply(ctx$want, `[`, j[rows])
    gap <- d[src] - L[src, , drop = FALSE]
    fit <- lapply(seq_len(ncol(L)), function(c) {
      ok <- which(!is.na(gap[, c]))
      if (length(unique(owner[src][ok])) < k) return(NULL)
      g <- gap[ok, c]
      list(ok = ok, spread = diff(stats::quantile(g, c(0.1, 0.9), names = FALSE)), after = all(g >= 0), before = all(g <= 0))
    })
    useful <- which(!vapply(fit, is.null, TRUE))
    value <- rep(NA_real_, length(rows))
    for (c in useful[order(vapply(fit[useful], `[[`, 0, "spread"))]) {
      need <- which(is.na(value) & !is.na(to[rows, c]))
      if (!length(need)) next
      ok <- fit[[c]]$ok
      value[need] <- to[rows[need], c] + pooled_number(gap[ok, c], owner[src][ok], lapply(want, `[`, need), lapply(have, `[`, ok))
    }
    bound <- function(side, f) {
      on <- useful[vapply(fit[useful], `[[`, TRUE, side)]
      b <- if (length(on)) suppressWarnings(apply(to[rows, on, drop = FALSE], 1, f, na.rm = TRUE)) else rep(NA_real_, length(rows))
      ifelse(is.finite(b), b, NA)
    }
    value <- pmin(pmax(value, bound("after", max), na.rm = TRUE), bound("before", min), na.rm = TRUE)
    # Within the record's SV visit above all, where the source's always are.
    visit <- function(side, column) {
      on <- intersect(useful[vapply(fit[useful], `[[`, TRUE, side)], which(colnames(to) == column))
      if (length(on)) to[rows, on] else rep(NA_real_, length(rows))
    }
    known <- !is.na(value)
    value[known] <- pmin(pmax(value, visit("after", "visit"), na.rm = TRUE), visit("before", "visit_end"), na.rm = TRUE)[known]
    out[rows] <- ifelse(is.na(value), NA, value)
  }
  out[first[match(unit, unit[first])]]
}

# Pooling keys for units of subjects `i`: each level of `levels` within `extra` (a record's
# content, a subject's linked records) where given, then, unless `alone` is FALSE, without it.
pool_keys <- function(levels, i, extra = NULL, alone = TRUE) {
  out <- lapply(levels, `[`, i)
  if (is.null(extra)) return(out)
  within <- lapply(out, function(l) paste(extra, l, sep = "\r"))
  if (alone) c(within, out) else within
}

# A value for each synthetic unit (keys `want`, one vector per level, finest first), drawn
# among the values of source units (`value`, of subjects `owner`, keys `have`) in proportion
# to the units holding them, at the finest level where the unit's cell has a value held by at
# least min_subjects() subjects; values held by fewer are never drawn. Where no level has one,
# the value most subjects hold, or NA without `fallback`.
pooled_draw <- function(value, owner, want, have, fallback = TRUE) {
  out <- rep(NA_character_, length(want[[1]]))
  if (!length(value)) return(out)
  todo <- seq_along(out)
  for (l in seq_along(want)) {
    if (!length(todo)) break
    held <- subjects_holding(paste(have[[l]], value, sep = "\r"), owner) >= min_subjects()
    if (!any(held)) next
    pools <- split(value[held], have[[l]][held])
    hit <- todo[want[[l]][todo] %in% names(pools)]
    for (g in split(hit, want[[l]][hit])) {
      p <- pools[[match(want[[l]][g[1]], names(pools))]]
      out[g] <- p[sample.int(length(p), length(g), replace = TRUE)]
    }
    todo <- setdiff(todo, hit)
  }
  if (length(todo) && fallback) out[todo] <- value[which.max(subjects_holding(value, owner))]
  out
}

# Yes or no for each synthetic unit (keys `want`), yes as often as among the source units
# (`yes`, of subjects `owner`, keys `have`) in the finest cell that at least min_subjects()
# subjects are in; no where there is none.
pooled_share <- function(yes, owner, want, have) {
  p <- rep(NA_real_, length(want[[1]]))
  for (l in seq_along(want)) {
    todo <- which(is.na(p))
    if (!length(todo)) break
    size <- tapply(owner, have[[l]], function(o) length(unique(o)))
    share <- tapply(yes, have[[l]], mean)
    ok <- todo[want[[l]][todo] %in% names(size)[size >= min_subjects()]]
    p[ok] <- share[want[[l]][ok]]
  }
  stats::runif(length(p)) < ifelse(is.na(p), 0, p)
}

# A number for each synthetic unit (keys `want`), drawn from the percentiles of the source
# values `v` (of subjects `owner`, keys `have`) in the finest cell that at least
# min_subjects() subjects are in, kept within the ends of all the values (`number_ends()`:
# trimming each cell instead would leave a small cell its median), as whole numbers.
pooled_number <- function(v, owner, want, have) {
  out <- rep(NA_real_, length(want[[1]]))
  ends <- number_ends(v[!is.na(v)], owner[!is.na(v)])
  todo <- seq_along(out)
  for (l in seq_along(want)) {
    if (!length(todo)) break
    size <- tapply(owner, have[[l]], function(o) length(unique(o)))
    hit <- todo[want[[l]][todo] %in% names(size)[size >= min_subjects()]]
    for (g in split(hit, want[[l]][hit])) {
      cell <- have[[l]] == want[[l]][g[1]]
      q <- stats::quantile(v[cell], NUMBER_QUANTILES, names = FALSE, na.rm = TRUE)
      q <- pmin(pmax(q, ends[1]), ends[2])
      out[g] <- quantile_draw(matrix(q, length(g), length(q), byrow = TRUE))
    }
    todo <- setdiff(todo, hit)
  }
  round(out)
}

# Identifiers `v` that repeat within a subject `j` take the subject's next free numbers.
free_ids <- function(v, j) {
  again <- which(!is.na(v) & duplicated(paste(j, v)))
  if (!length(again) || !is.numeric(v)) return(v)
  top <- tapply(v, j, max, na.rm = TRUE)
  v[again] <- top[as.character(j[again])] + stats::ave(again, j[again], FUN = seq_along)
  v
}

# Whether the record keys give the identifier `v` (QSSEQ in blocks per questionnaire) in at
# least 95% of the source records.
keyed_id <- function(x, keys, v) {
  if (!length(keys)) return(FALSE)
  pair <- group_index(x, c(keys, v))
  first <- !duplicated(pair)
  sum(tapply(tabulate(pair)[pair[first]], group_index(x, keys)[first], max)) >= 0.95 * nrow(x)
}

# For records at visit k of synthetic subjects j, the nearest visit (by number) they attended.
nearest_visit <- function(att, j, k, V) {
  if (!ncol(att)) return(k)
  bad <- which(!is.na(k))
  bad <- bad[!att[cbind(j[bad], k[bad])]]
  if (length(bad)) {
    distance <- abs(outer(V[k[bad]], V, `-`))
    distance[!att[j[bad], , drop = FALSE]] <- Inf
    near <- max.col(-distance, ties.method = "first")
    near[!is.finite(distance[cbind(seq_along(bad), near)])] <- NA
    k[bad] <- near
  }
  k
}

# --DY, --STDY and --ENDY from their date and the subject's RFSTDTC, with no day 0.
study_days <- function(data) {
  lapply(data, function(x) {
    if (!"USUBJID" %in% names(x) || is.null(data$DM$RFSTDTC)) return(x)
    start <- as.numeric(data$DM$RFSTDTC[match(x$USUBJID, data$DM$USUBJID)])
    for (v in names(Filter(function(v) inherits(v, "Date"), x))) {
      day <- sub("DTC$", "DY", v)
      if (day == v || !is.numeric(x[[day]])) next
      gap <- as.numeric(x[[v]]) - start
      x[[day]] <- gap + (gap >= 0)
    }
    x
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
  stored <- startsWith(names(data$DM), "T.")
  categories <- if (any(stored)) data$DM[match(ids, data$DM$USUBJID), stored, drop = FALSE] else spine_categories(data, ids)
  if (length(categories)) out <- cbind(out, categories)
  out
}

# The T.<DATASET>.<CATEGORY> features. A category is not always a spine column (QSCAT follows
# from the test), so the spine keeps them with DM's records (see `spine_records()`).
spine_categories <- function(data, ids) {
  out <- list()
  for (d in setdiff(names(data), "DM")) {
    x <- data[[d]]
    category <- x[[paste0(d, "CAT")]]
    if (is.null(category)) next
    has <- ids %in% x$USUBJID
    for (v in sort(unique(category[!is.na(category)]))) {
      y <- ids %in% x$USUBJID[category %in% v]
      if (any(y != has)) out[[paste0("T.", d, ".", v)]] <- ifelse(y, "Y", "N")
    }
  }
  data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

spine_subject_names <- function(study) c(SPINE_SUBJECT_FEATURES, paste0("T.", setdiff(names(study$source$data), "DM")),
  names(spine_categories(study$source$data, study$source$data$DM$USUBJID)))

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
# record (not its sequence number, which only counts the subject's records) in a way no model
# with few records keeps: a value of the key that always comes with one value of the node (a
# DS date on the death date: DEATH), or a value of the node that only comes with some values
# of the key (DEATH only on the death date; a related adverse event only during or after
# treatment). Each is kept when it holds on every source record, in at least two
# subjects, and would hold by chance with probability below IMPLICATION_P if the two were
# independent. Subjects count, not records (records of a subject are alike): a share is
# the mean over subjects of the share of their records. Generation redraws the second and
# then sets the first.
IMPLICATION_P <- 1e-4
IMPLICATION_KEY_VALUES <- 50

implication_keys <- function(ds, frame) {
  candidates <- c(grep("^T\\.", names(frame), value = TRUE), if (ds$name != "DM") grep("^DM\\.", names(frame), value = TRUE),
    names(frame)[subject_result_column(names(frame))], grep("^LINK\\.", names(frame), value = TRUE),
    intersect(setdiff(ds$structure$spine, ds$structure$id), names(frame)))
  Filter(function(k) (is.character(frame[[k]]) || k %in% ds$structure$spine && is.numeric(frame[[k]])) &&
    length(unique(frame[[k]])) %in% 2:IMPLICATION_KEY_VALUES, candidates)
}

# Values of fewer than min_subjects() subjects (rare) count as one pooled value: they still
# stand against a forced value, but no implication of their own is kept. Key levels of fewer
# than min_subjects() subjects are pooled the same way (a value never at any rare visit is
# not drawn there); `known` keeps the key levels that are not pooled. A death feature
# (DEATH_KEY) forces its value as the source has it (`source`), however rare.
fit_implications <- function(y, frame, keys, rare = rep(FALSE, length(y)), source = y) {
  values <- level_string(y)
  values[rare] <- RARE
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
    common <- subjects_holding(key, frame$USUBJID) >= min_subjects()
    known <- unique(key[common])
    key[!common] <- RARE
    present <- table(key, values) > 0
    n_key <- subjects(key)[rownames(present)]
    p_key <- share(key)[rownames(present)]
    forced <- which(n_key >= min_subjects() & rowSums(present) == 1)
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
    only <- which(n >= min_subjects() & colSums(never) > 0)
    if (length(forced)) rows[[length(rows) + 1]] <- data.frame(kind = "forced", key = k, level = I(as.list(rownames(present)[forced])),
      value = I(as.list(colnames(present)[with[names(forced)]])), known = I(rep(list(known), length(forced))), stringsAsFactors = FALSE)
    if (length(only)) rows[[length(rows) + 1]] <- data.frame(kind = "only", key = k,
      level = I(lapply(only, function(j) rownames(present)[!never[, j]])), value = I(as.list(colnames(present)[only])),
      known = I(rep(list(known), length(only))), stringsAsFactors = FALSE)
    if (grepl(DEATH_KEY, k) && !"Y" %in% rownames(present)[forced]) {
      died <- level_string(frame[[k]]) == "Y"
      v <- unique(level_string(source)[died])
      if (length(v) == 1 && mean(level_string(source) == v)^length(unique(frame$USUBJID[died])) < IMPLICATION_P)
        rows[[length(rows) + 1]] <- data.frame(kind = "forced", key = k, level = I(list("Y")), value = I(list(v)),
          known = I(list(union(known, "Y"))), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  if (!is.null(out)) out <- out[!vapply(out$value, identical, TRUE, RARE), , drop = FALSE]
  if (!NROW(out)) return(NULL)
  rownames(out) <- NULL
  out$typed <- I(lapply(out$value, function(v) if (v == MISSING) NA else c(y[values == v], source[level_string(source) == v])[1]))
  out$via <- NA_character_
  out
}

# A category that lookups derive from the node alone (CMROUTE from CMTRT) keeps its "only"
# implications through the node: the node values that give it (every topical medication)
# only where the key allows it. One drug is too rare to show the pattern; its route is not.
# The route is the source's where it has the column and the table was not edited, as the
# table leaves out rare drugs (see `coarse_values()`); the implication lists the values as
# drawn (`drawn`), without the rare ones.
derived_implications <- function(ds, variable, y, frame, keys, drawn = y, rare = rep(FALSE, length(y))) {
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
    derived <- if (!isTRUE(r$params$edited) && r$target %in% names(frame)) level_string(frame[[r$target]]) else
      level_string(first[[r$target]])[match(values, level_string(first[[variable]]))]
    out <- fit_implications(derived, frame, keys)
    out <- out[out$kind == "only", , drop = FALSE]
    if (!NROW(out)) return(NULL)
    out$via <- paste(r$target, "=", unlist(out$value))
    out$value <- I(lapply(unlist(out$value), function(v) unique(level_string(drawn)[derived == v & !rare])))
    out <- out[lengths(out$value) > 0, , drop = FALSE]
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
  if (!NROW(implications)) return(bad)
  s <- level_string(values)
  # Each column's strings once, not once per implication.
  strings <- function(columns, f) lapply(stats::setNames(nm = unique(stats::na.omit(columns))), function(v) f(frame[[v]]))
  keys <- strings(implications$key, level_string)
  tests <- strings(implications$within, format_value)
  for (i in seq_len(NROW(implications))) {
    key <- implication_key(implications, i, keys[[implications$key[i]]])
    within <- implications$within[i]
    rows <- if (is.null(within) || is.na(within)) TRUE else tests[[within]] == implications$part[i]
    bad <- bad | rows & if (implications$kind[i] == "forced")
      key %in% implications$level[[i]] & (if (identical(implications$value[[i]], NONE)) !no_amount(values) else s != implications$value[[i]]) else s %in% implications$value[[i]] & !key %in% implications$level[[i]]
  }
  bad
}

# The key's strings, with the levels the implication pooled (see `fit_implications()`) as RARE.
implication_key <- function(implications, i, key) {
  known <- if (!is.null(implications$known)) implications$known[[i]]
  if (is.null(known)) key else ifelse(key %in% known, key, RARE)
}

# "Only" values out of place are redrawn from the model a few times, then replaced by a
# value drawn for another record of the same test that is allowed there, or left missing
# where none is (where the source has only values too rare to draw); forced values are
# then set.
apply_implications <- function(implications, values, frame, redraw) {
  if (is.null(implications)) return(values)
  only <- implications[implications$kind == "only", , drop = FALSE]
  read <- frame[unique(c(only$key, stats::na.omit(only$within)))]
  misplaced <- function(v, rows) implication_violations(only, v, read[rows, , drop = FALSE])
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
    else if (!misplaced(NA, i)) values[i] <- NA
  }
  forced <- implications[implications$kind == "forced", , drop = FALSE]
  for (i in seq_len(nrow(forced))) {
    rows <- implication_key(forced, i, level_string(frame[[forced$key[i]]])) %in% forced$level[[i]] & implication_rows(forced, i, frame)
    if (identical(forced$value[[i]], NONE)) rows <- rows & !no_amount(values)
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
