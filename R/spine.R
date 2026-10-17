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
# design that the timing follows (a titration visit). A synthetic subject's spine is sampled
# piece by piece from the source's (see `compose_spine()`): their type (arm, last visit,
# death, treatment, which datasets they have), the visits they attend and on which days, and
# at each visit the records of a source subject of the same type, placed on those days.
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
# by number (.donor), never by identifier. A series (one event recorded at several visits)
# keeps which records belong together (.series), and a dataset with a category (QSCAT) keeps
# it (.category) for the T.<DATASET>.<CATEGORY> features.
spine_records <- function(study) {
  ids <- study$source$data$DM$USUBJID
  lapply(study$datasets, function(ds) {
    x <- study$source$data[[ds$name]]
    out <- data.frame(.donor = match(x$USUBJID, ids), x[ds$structure$spine], check.names = FALSE, stringsAsFactors = FALSE)
    if (ds$name == "DM") {
      categories <- spine_categories(study$source$data, x$USUBJID)
      if (length(categories)) out <- cbind(out, categories)
    }
    if (identical(ds$structure$type, "series")) out$.series <- group_index(x, c("USUBJID", ds$structure$series))
    category <- x[[paste0(ds$name, "CAT")]]
    if (ds$name != "DM" && !is.null(category)) out$.category <- as.character(category)
    out[!is.na(out$.donor), , drop = FALSE]
  })
}

# The source subject whose type each synthetic subject has (see `compose_spine()`).
draw_types <- function(spine, n) {
  available <- nrow(spine$DM)
  sample.int(available, n, replace = n > available)
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

# The spine of n synthetic subjects, sampled piece by piece so that no subject copies the
# timing of one source subject:
#   1. Type: arm, last visit (T.LASTVISIT), death, treatment status and which datasets and
#      categories the subject has, as one source subject has them. Types are drawn without
#      replacement while there are enough source subjects.
#   2. Visits attended, in number order, each as a source subject of the same type who
#      attended (or missed) the previous visit as the synthetic subject did. The last visit
#      is attended; the later visits that mark progress are not.
#   3. Visit days (`visit_days()`): a planned visit on its planned day plus a deviation (see
#      `visit_schedule()`), any other visit (unscheduled, AE follow-up) some days after the
#      previous one, as the source's were at that visit, and not after the next planned one.
#   4. Records, unit by unit, each from a source subject of the same type that has the unit
#      (`compose_dataset()`). The units of a dataset planned by visit are its visits: the
#      records a source subject had at a visit the synthetic subject attends, from the
#      subject the visit came from in SV where they have the same arms, so that a visit's
#      records of all datasets come from one subject. Otherwise they are the values of the
#      planned test or category (a subject's DS milestones, SE's elements), each from a subject with the type's set of values who last attended the
#      same visit and whose last visit in the datasets composed already matches (EX before
#      SE, so the high-dose element comes with the high dose), coarsened as needed; a series
#      (a medication recorded at several visits) with the number of series from another
#      subject, or single records with the number of records from another subject. A record
#      linked to a record of another dataset (TR to its TU lesion) comes from a subject with
#      the same link identifiers where enough have them, and is dropped when the synthetic
#      subject has no record to link to. A subject whose type has the dataset but who drew
#      no records gets those of a source subject at their first attended visit that has any
#      (or of the most common category), so the type's datasets stay as drawn.
#   5. Dates (`place_dates()`): a source date keeps its place among its source subject's
#      visits and DM dates, on the synthetic subject's: on the record's visit, on a DM date (a
#      DS date on the death date), or some days from the nearest visit (an adverse event, the
#      day before the next dose). A record's other dates keep their order and equality to its
#      main date. DM's dates come from one more source subject of the same type, placed on the
#      visits (and day 1) the same way. A DM date that is a dataset's first or last date in
#      the source (RFXENDTC the last EXENDTC, RFENDTC the disposition event's DSSTDTC, see
#      `derived_dates()`) is then taken from the composed records, and the dates placed on it
#      move with it. A date equal to several DM dates goes on the one its source column
#      equals most often, and a date on or after the source subject's last visit stays on
#      or after the synthetic subject's.
#   6. A record moved to another visit (a medication recorded at a visit the synthetic
#      subject missed goes to the nearest one they attended) takes that visit's VISIT and
#      VISITDY, and is dropped if that makes it the same as another of the subject's
#      records where the source had none the same. Sequence numbers and series identifiers
#      (--SPID) are numbered again per subject, unless the record keys give them (QSSEQ in
#      blocks per questionnaire). Each
#      subject's day 1 falls on a calendar date drawn so that their visits lie between the
#      source's first and last visit, and study days follow.
# A unit's source subject is drawn among those of the same type that have the unit (who
# attended that visit); when fewer than min_subjects() do, the type is coarsened step by step
# (`type_levels()`), though records at visits keep the arms (planned and actual) wherever
# any subject has them, as their content follows the arm (EX's dose by visit and ARMCD). The
# types drawn are kept as the attribute "types", for privacy checks.
DM_DATE_ORDER <- c("DTHDTC", "RFXSTDTC", "RFXENDTC", "RFENDTC", "RFPENDTC", "RFICDTC", "DMDTC", "RFSTDTC")

compose_spine <- function(study, n, ids) {
  spine <- study$spine
  dm <- spine$DM
  N <- nrow(dm)
  a <- as.numeric(as.Date(SPINE_ANCHOR))
  visits <- spine_visits(spine, N)
  src <- spine_types(spine, visits)
  type <- draw_types(spine, n)
  att <- attend_visits(src, type, visits)
  syn <- visit_days(study$schedule, visits, att, if (is.null(dm$ARMCD)) rep(NA_character_, n) else dm$ARMCD[type], a)
  # DM: the type's arm and categories, and the dates of one more subject of the same type.
  levels <- type_levels(src)
  b <- pick_source(levels$want(type), levels$have, seq_len(N))
  dates <- names(Filter(function(v) inherits(v, "Date"), dm))
  dates <- c(intersect(DM_DATE_ORDER, dates), setdiff(dates, DM_DATE_ORDER))
  derived <- derived_dates(study, dates)
  out <- data.frame(.j = seq_len(n), dm[type, setdiff(names(dm), ".donor"), drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  own <- setdiff(names(out), c(".j", SPINE_ARM, grep("^T\\.", names(out), value = TRUE)))
  out[own] <- dm[b, own, drop = FALSE]
  dm_src <- matrix(vapply(dates, function(v) as.numeric(dm[[v]]), numeric(N)), N, dimnames = list(NULL, dates))
  dm_syn <- matrix(NA_real_, n, length(dates), dimnames = list(NULL, dates))
  for (v in dates)
    dm_syn[, v] <- out[[v]] <- as.numeric(place_dates(dm_src[b, v], b, seq_len(n), NULL, NULL, cbind(visits$day, a), cbind(syn, a), cbind(att, TRUE)))
  composed <- list(DM = out)
  ctx <- list(src = src, type = type, att = att, visits = visits, syn = syn, dm_src = dm_src, dm_syn = dm_syn, derived = derived)
  # The datasets DM dates are taken from come first, those at visits before the others (EX
  # before SE, whose elements follow its doses), the latest date first (DS's disposition
  # before EX), each DM date taken at once so that later records keep within it.
  rest <- setdiff(names(spine), c("DM", VISIT_HUB))
  late <- vapply(names(derived), function(m) mean(dm_src[, m] - a, na.rm = TRUE), 0)
  at_visit <- vapply(derived, function(r) !is.null(spine[[r$dataset]]$VISITNUM), TRUE)
  first <- intersect(unique(vapply(derived[order(!at_visit, -late)], `[[`, "", "dataset")), rest)
  for (name in c(intersect(VISIT_HUB, names(spine)), first, setdiff(rest, first))) {
    composed[[name]] <- compose_dataset(study, name, composed, ctx)
    if (name == VISIT_HUB) ctx$hub <- visit_donors(composed[[name]], visits$number, n)
    for (m in names(Filter(function(r) r$dataset == name, derived))) {
      value <- derived_value(composed, derived[[m]], n)
      keep <- !is.na(value) & !is.na(composed$DM[[m]])
      composed$DM[[m]][keep] <- ctx$dm_syn[keep, m] <- value[keep]
    }
  }
  composed <- composed[names(spine)]
  # DM dates taken from the records (see `derived_dates()`), the dates placed on them moved
  # with them, and a record's other dates kept in order to its main date (see
  # `compose_dataset()`); twice, since each step can move the other's dates.
  take_derived <- function(composed) {
    for (m in names(derived)) {
      value <- derived_value(composed, derived[[m]], n)
      keep <- !is.na(value) & !is.na(composed$DM[[m]])
      composed$DM[[m]][keep] <- value[keep]
    }
    composed
  }
  for (pass in 1:2) {
    composed <- take_derived(composed)
    for (name in setdiff(names(composed), "DM")) {
      x <- composed[[name]]
      for (marker in grep("^\\.dm\\.", names(x), value = TRUE)) {
        on <- which(x[[marker]] %in% names(derived))
        v <- sub("^\\.dm\\.", "", marker)
        moved <- composed$DM[cbind(x$.j[on], match(x[[marker]][on], names(composed$DM)))]
        x[[v]][on] <- pmax(moved, x[[paste0(".end.", v)]][on], na.rm = TRUE)
      }
      main <- main_date(study$datasets[[name]]$prefix, names(Filter(function(v) inherits(v, "Date"), study$spine[[name]])))
      for (g in grep("^\\.gap\\.", names(x), value = TRUE)) {
        v <- sub("^\\.gap\\.", "", g)
        x[[v]] <- keep_gap(as.numeric(x[[v]]), as.numeric(x[[main]]), x[[g]], x[[sub("gap", "cap", g, fixed = TRUE)]])
      }
      composed[[name]] <- x
    }
  }
  composed <- take_derived(composed)
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
    seen <- c(syn[j, att[j, ]], a)
    first <- window[1] + a - min(seen)
    last <- max(first, window[2] - (max(seen) - a))
    shift[j] <- first - a + sample.int(last - first + 1, 1) - 1
  }
  for (name in names(composed)) {
    x <- composed[[name]]
    for (v in names(Filter(function(v) inherits(v, "Date"), spine[[name]])))
      x[[v]] <- as.Date(as.numeric(x[[v]]) + shift[x$.j], origin = "1970-01-01")
    x <- data.frame(USUBJID = ids[x$.j], x[setdiff(names(x), c("USUBJID", ".j", ".category", grep("^\\.(b$|(dm|end|gap|cap)\\.)", names(x), value = TRUE)))], check.names = FALSE, stringsAsFactors = FALSE)
    rownames(x) <- NULL
    composed[[name]] <- x
  }
  composed <- study_days(composed)
  attr(composed, "types") <- type
  composed
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
        value = subject_extreme(x[[v]][rows], x$.donor[rows], N, end))
    }
  }
  out <- list()
  for (m in setdiff(dates, "RFSTDTC")) {
    target <- as.numeric(spine$DM[[m]])
    has <- which(!is.na(target))
    if (length(has) < min_subjects() || !length(candidates)) next
    share <- vapply(candidates, function(c) mean((c$value[has] == target[has]) %in% TRUE), 0)
    if (max(share) >= DERIVED_SHARE) out[[m]] <- candidates[[which.max(share)]][c("dataset", "variable", "end", "key", "level")]
  }
  out
}

# Each subject's first ("min") or last ("max") date of `v`, for subjects 1 to `size`.
subject_extreme <- function(v, owner, size, end) {
  v <- as.numeric(v)
  ok <- !is.na(v) & !is.na(owner)
  as.numeric(tapply(v[ok], factor(owner[ok], seq_len(size)), end))
}

# Each source subject's visits: the visit numbers of SV in order, which visits each subject
# attended and on which day (their first date there), and the visits that mark progress.
spine_visits <- function(spine, N) {
  sv <- spine[[VISIT_HUB]]
  if (is.null(sv$VISITNUM) || !nrow(sv))
    return(list(number = numeric(), day = matrix(NA_real_, N, 0), attended = matrix(FALSE, N, 0), progress = numeric()))
  number <- as.numeric(sv$VISITNUM)
  visits <- sort(unique(stats::na.omit(number)))
  k <- match(number, visits)
  attended <- matrix(FALSE, N, length(visits))
  attended[cbind(sv$.donor, k)[!is.na(k), , drop = FALSE]] <- TRUE
  day <- matrix(NA_real_, N, length(visits))
  main <- main_date(VISIT_HUB, names(sv))
  if (!is.na(main)) {
    d <- as.numeric(sv[[main]])
    o <- which(!is.na(d) & !is.na(k))
    o <- o[order(d[o], decreasing = TRUE)]
    day[cbind(sv$.donor[o], k[o])] <- d[o]
  }
  list(number = visits, day = day, attended = attended, progress = progress_visits(data.frame(USUBJID = sv$.donor, VISITNUM = number)))
}

# Each source subject's type: death, treatment status, arm and last visit that marks progress.
spine_types <- function(spine, visits) {
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
  out <- list(died = ifelse(is.na(date("DTHDTC")), "N", "Y"), treated = ifelse(is.na(start), "N", ifelse(is.na(end), "STARTED", "Y")),
    arm = do.call(paste, c(list(rep("", N)), unname(as.list(dm[intersect(SPINE_ARM, names(dm))])))), last = last)
  actual <- intersect(c("ACTARMCD", "ACTARM"), names(dm))
  out$actual <- if (length(actual)) do.call(paste, unname(as.list(dm[actual]))) else out$arm
  out$base <- paste(out$died, out$treated, out$arm, out$last, sep = "|")
  out
}

# The keys a source subject is drawn by, finest first: the type with the dataset's categories
# (`extra`); without the last visit (`by_visit`, records drawn per visit attended) or else
# without the arm, then without the last visit (records that end with the subject: a
# disposition, the last adverse event; the arm first when `by_arm`, SE's elements); with the
# actual arm alone; death and treatment status alone; and none. A linked dataset first asks
# for the same set of link identifiers.
type_levels <- function(src, extra = "", id_src = NULL, id_syn = NULL, by_visit = TRUE, by_arm = by_visit) {
  key <- function(...) paste(src$died, src$treated, ..., extra)
  arm <- key(src$arm)
  last <- key(src$last)
  status <- key()
  middle <- if (by_visit) list(arm) else if (by_arm) list(arm, last) else list(last, arm)
  base <- c(list(paste(src$base, extra)), middle, list(key(src$actual), status, paste(src$died, src$treated), rep("", length(src$base))))
  if (is.null(id_src)) return(list(have = base, want = function(type) lapply(base, `[`, type)))
  list(have = c(list(paste(id_src, base[[1]]), paste(id_src, status), id_src), base),
    want = function(type) c(list(paste(id_syn, base[[1]][type]), paste(id_syn, status[type]), id_syn), lapply(base, `[`, type)))
}

# Whether some value of the dataset's first key is held, by at least min_subjects() source
# subjects, in one arm only (SE's elements).
arm_bound <- function(x, key, arm) {
  if (is.null(key) || is.null(x[[key]]) || inherits(x[[key]], "Date") || length(unique(arm)) < 2) return(FALSE)
  value <- level_string(x[[key]])
  arms <- tapply(arm[x$.donor], value, function(a) length(unique(a)))
  holders <- tapply(x$.donor, value, function(z) length(unique(z)))
  any(arms == 1 & holders >= min_subjects())
}

# The source subject each synthetic subject's visit came from in the visit hub (SV), as a
# subject-by-visit matrix.
visit_donors <- function(hub, V, n) {
  out <- matrix(NA_integer_, n, length(V))
  k <- if (!is.null(hub$VISITNUM)) match(as.numeric(hub$VISITNUM), V) else integer()
  ok <- which(!is.na(k))
  out[cbind(hub$.j[ok], k[ok])] <- hub$.b[ok]
  out
}

# For each synthetic subject (keys `want`, one vector per level), a source subject of `pool`
# with the same keys at the finest level where at least min_subjects() of the pool have
# them, or else where any do. NA when the pool is empty.
pick_source <- function(want, have, pool) {
  out <- rep(NA_integer_, length(want[[1]]))
  todo <- seq_along(out)
  for (threshold in unique(c(min_subjects(), 1L))) for (l in seq_along(want)) {
    if (!length(todo) || !length(pool)) break
    groups <- split(pool, have[[l]][pool])
    size <- lengths(groups)[want[[l]][todo]]
    ok <- !is.na(size) & size >= threshold
    out[todo[ok]] <- vapply(groups[want[[l]][todo[ok]]], function(p) p[sample.int(length(p), 1)], 0L)
    todo <- todo[!ok]
  }
  out
}

# Visits attended by synthetic subjects of these types (see `compose_spine()`).
attend_visits <- function(src, type, visits) {
  V <- visits$number
  n <- length(type)
  att <- matrix(FALSE, n, length(V))
  last <- src$last[type]
  before_src <- rep("", length(src$base))
  before <- rep("", n)
  pool <- which(rowSums(visits$attended) > 0)
  for (k in seq_along(V)) {
    x <- rep(NA, n)
    x[which(is.na(last) | V[k] %in% visits$progress & V[k] > last)] <- FALSE
    x[which(V[k] == last)] <- TRUE
    free <- which(is.na(x))
    if (length(free)) {
      side_src <- format(src$last >= V[k])
      side <- format(last >= V[k])
      have <- list(paste(src$base, before_src), paste(src$last, before_src), paste(side_src, before_src), side_src)
      want <- list(paste(src$base[type], before), paste(last, before), paste(side, before), side)
      x[free] <- visits$attended[cbind(pick_source(lapply(want, `[`, free), have, pool), k)]
    }
    x[is.na(x)] <- FALSE
    att[, k] <- x
    before_src <- format(visits$attended[, k])
    before <- format(x)
  }
  att
}

# Each synthetic subject's day at every visit, as a date with day 1 on `a`: drawn for the
# visits they attend (see `compose_spine()`); for the others, which other dates may still
# be placed by, the planned day or the previous visit's.
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

# One dataset's composed records (see `compose_spine()`), dates as numbers, with the
# synthetic subject's number (.j).
compose_dataset <- function(study, name, composed, ctx) {
  x <- study$spine[[name]]
  ds <- study$datasets[[name]]
  s <- ds$structure
  N <- length(ctx$src$base)
  V <- ctx$visits$number
  type <- ctx$type
  columns <- setdiff(names(x), ".donor")
  holders <- tabulate(x$.donor, N) > 0
  js <- which(holders[type])
  flags <- grep(paste0("^T\\.", name, "\\."), names(study$spine$DM), value = TRUE)
  extra <- if (length(flags)) do.call(paste, unname(as.list(study$spine$DM[flags]))) else ""
  target <- if (!is.null(ds$link)) composed[[ds$link$dataset]]
  number <- if (!is.null(x$VISITNUM)) as.numeric(x$VISITNUM)
  by_visit <- !is.null(s$visit) || identical(s$type, "count") && !is.null(number)
  by_arm <- by_visit || arm_bound(x, s$keys[1], ctx$src$arm)
  levels <- if (is.null(target)) type_levels(ctx$src, extra, by_visit = by_visit, by_arm = by_arm) else {
    y <- study$spine[[ds$link$dataset]]
    id_set <- function(id, owner, size) vapply(split(id, factor(owner, seq_len(size))),
      function(z) paste(sort(unique(stats::na.omit(z))), collapse = " "), "")
    type_levels(ctx$src, extra, id_set(y[[ds$link$by]], y$.donor, N), id_set(target[[ds$link$by]], target$.j, length(type)), by_visit, by_arm)
  }
  want <- levels$want(type)
  pick <- function(js, pool) pick_source(lapply(want, `[`, js), levels$have, pool)
  one <- function(groups, b) vapply(groups[as.character(b)], function(p) p[sample.int(length(p), 1)], 0L)
  pieces <- list()
  add <- function(j, rows, series = NULL) pieces[[length(pieces) + 1]] <<- list(j = rep(j, lengths(rows)), rows = unlist(rows),
    series = if (!is.null(series)) rep(series, lengths(rows)))
  if (length(js) && by_visit) {
    index <- split(seq_len(nrow(x)), paste(x$.donor, number))
    k_record <- match(number, V)
    for (k in seq_along(V)) {
      here <- intersect(js, which(ctx$att[, k]))
      pool <- which(holders & (ctx$visits$attended[, k] | tabulate(x$.donor[k_record %in% k], N) > 0))
      if (!length(here)) next
      # From a subject of the same arms (planned and actual) where any has them, as the
      # records' content follows the arm (EX's dose by visit and ARMCD); else as the type gives.
      b <- if (is.null(target)) pick_source(lapply(want[1:2], `[`, here), levels$have[1:2], pool) else rep(NA_integer_, length(here))
      b[is.na(b)] <- pick(here[is.na(b)], pool)
      # The visit's source subject in SV, when they have these records and the same arm: a
      # visit's records of all datasets come from one subject, on the same days.
      h <- if (!is.null(ctx$hub) && is.null(target)) ctx$hub[here, k]
      same <- which(!is.na(h) & h %in% pool & levels$have[[2]][h] == want[[2]][here])
      b[same] <- h[same]
      add(here, index[paste(b, V[k])])
    }
    for (u in unique(number[is.na(k_record)])) add(js, index[paste(pick(js, which(holders)), u)])
    for (k in seq_along(V)) {
      here <- intersect(setdiff(js, unlist(lapply(pieces, `[[`, "j"))), which(ctx$att[, k]))
      pool <- which(tabulate(x$.donor[k_record %in% k], N) > 0)
      if (length(here) && length(pool)) add(here, index[paste(pick(here, pool), V[k])])
    }
  } else if (length(js) && identical(s$type, "schedule")) {
    value <- level_string(x[[s$keys[1]]])
    index <- split(seq_len(nrow(x)), paste(x$.donor, value, sep = "\r"))
    # Each value from a subject with the type's set of values (SE's elements), the same last
    # visit and latest visit attended (a retrieval visit, its DS record and follow-up element)
    # and the same last visit with records in each dataset a DM date is taken from, composed
    # already (EX at a visit of the high dose, its element), at any level of the type but none;
    # where none has them, with the same visits but another set, then without those datasets,
    # then the set and the last visit, then the set alone (the type itself has its set); else as
    # the type alone gives it.
    set_src <- vapply(split(value, factor(x$.donor, seq_len(N))), function(v) paste(sort(unique(v)), collapse = "\r"), "")
    fits <- list(list(src = set_src, syn = set_src[type]))
    if (length(V)) {
      latest <- function(att) apply(att, 1, function(a) max(c(0, which(a))))
      last_visit <- function(y, owner, size) {
        k <- match(as.numeric(y$VISITNUM), V)
        out <- vapply(split(k, factor(owner, seq_len(size))), function(z) max(c(0, z), na.rm = TRUE), 0)
        unname(out)
      }
      ends <- Filter(function(e) !is.null(composed[[e]]$VISITNUM), setdiff(unique(vapply(ctx$derived, `[[`, "", "dataset")), name))
      ends_src <- do.call(paste, c(list(""), lapply(ends, function(e) last_visit(study$spine[[e]], study$spine[[e]]$.donor, N))))
      ends_syn <- do.call(paste, c(list(""), lapply(ends, function(e) last_visit(composed[[e]], composed[[e]]$.j, length(type)))))
      last_src <- paste(set_src, ctx$src$last)
      latest_src <- paste(ctx$src$last, latest(ctx$visits$attended))
      latest_syn <- paste(ctx$src$last[type], latest(ctx$att))
      fit <- function(src, syn) list(src = src, syn = syn)
      fits <- c(list(fit(paste(set_src, latest_src, ends_src), paste(set_src[type], latest_syn, ends_syn)),
        fit(paste(latest_src, ends_src), paste(latest_syn, ends_syn)), fit(paste(set_src, latest_src), paste(set_src[type], latest_syn)),
        fit(latest_src, latest_syn), fit(last_src, last_src[type])), fits)
    }
    L <- seq_len(length(want) - 1)
    pick_fit <- function(js, pool) {
      b <- rep(NA_integer_, length(js))
      for (f in fits) {
        todo <- which(is.na(b))
        if (length(todo)) b[todo] <- pick_source(lapply(want[L], function(w) paste(w[js[todo]], f$syn[js[todo]])), lapply(levels$have[L], paste, f$src), pool)
      }
      miss <- which(is.na(b))
      if (length(miss)) b[miss] <- pick(js[miss], pool)
      b
    }
    for (u in unique(value)) add(js, index[paste(pick_fit(js, which(holders)), u, sep = "\r")])
    common <- names(which.max(tapply(x$.donor, value, function(z) length(unique(z)))))
    here <- setdiff(js, unlist(lapply(pieces, `[[`, "j")))
    if (length(here)) add(here, index[paste(pick(here, unique(x$.donor[value == common])), common, sep = "\r")])
  } else if (length(js)) {
    whole <- identical(s$type, "series")
    unit <- if (whole) x$.series else seq_len(nrow(x))
    first <- !duplicated(unit)
    owned <- split(unit[first], x$.donor[first])
    count <- lengths(owned)[as.character(pick(js, which(holders)))]
    each <- rep(js, ifelse(is.na(count), 0, count))
    chosen <- one(owned, pick(each, which(holders)))
    add(each, split(seq_len(nrow(x)), unit)[as.character(chosen)], if (whole) seq_along(each))
  }
  j <- unlist(lapply(pieces, `[[`, "j"))
  rows <- unlist(lapply(pieces, `[[`, "rows"))
  out <- data.frame(.j = j %||% integer(), x[rows %||% integer(), columns, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  if (!is.null(out$.series)) out$.series <- unlist(lapply(pieces, `[[`, "series")) %||% integer()
  b <- out$.b <- x$.donor[rows]
  if (!is.null(target)) out$.linked <- (paste(x$.donor, x[[ds$link$key]]) %in% paste(y$.donor, y[[ds$link$by]]))[rows %||% integer()]
  # Visits the subject missed go to the nearest one attended, with its VISIT and VISITDY.
  k_src <- k_syn <- NULL
  if (!is.null(out$VISITNUM)) {
    k_src <- match(as.numeric(out$VISITNUM), V)
    k_syn <- nearest_visit(ctx$att, out$.j, k_src, V)
    moved <- which(!is.na(k_src) & !is.na(k_syn) & k_syn != k_src)
    out$VISITNUM[moved] <- V[k_syn[moved]]
    hub <- composed[[VISIT_HUB]]
    i <- match(paste(out$.j[moved], V[k_syn[moved]]), paste(hub$.j, as.numeric(hub$VISITNUM)))
    for (v in intersect(c("VISIT", "VISITDY"), intersect(names(out), names(hub)))) out[[v]][moved] <- hub[[v]][i]
  }
  dates <- names(Filter(function(v) inherits(v, "Date"), x))
  main <- main_date(ds$prefix, dates)
  source_dates <- lapply(out[dates], as.numeric)
  # Not on a DM date taken from this dataset's records: that date follows them. A date equal
  # to several goes on the one the source's dates of its column equal most often (a follow-up
  # element's start on RFENDTC rather than on an equal RFXENDTC).
  own <- names(Filter(function(r) r$dataset == name, ctx$derived))
  keep <- setdiff(colnames(ctx$dm_src), own)
  for (v in dates) {
    equal <- vapply(keep, function(m) sum(as.numeric(x[[v]]) == ctx$dm_src[x$.donor, m], na.rm = TRUE), 0)
    keep_v <- keep[order(-equal)]
    placed <- place_dates(source_dates[[v]], b, out$.j, k_src, k_syn, ctx$visits$day, ctx$syn, ctx$att,
      ctx$dm_src[, keep_v, drop = FALSE], ctx$dm_syn[, keep_v, drop = FALSE])
    out[[v]] <- as.numeric(placed)
    out[[paste0(".dm.", v)]] <- attr(placed, "dm")
    out[[paste0(".end.", v)]] <- attr(placed, "end")
  }
  # A record's other dates keep their order and equality to its main date, at most the
  # synthetic subject's last visit when the source date was at most the source subject's.
  last <- function(m) suppressWarnings(apply(m, 1, max, na.rm = TRUE))
  end_src <- if (ncol(ctx$visits$day)) last(ctx$visits$day)[b] else rep(Inf, nrow(out))
  end_syn <- if (ncol(ctx$syn)) last(ctx$syn + ifelse(ctx$att, 0, NA))[out$.j] else rep(Inf, nrow(out))
  if (!is.na(main)) for (v in setdiff(dates, main)) {
    gap <- source_dates[[v]] - source_dates[[main]]
    cap <- ifelse(!is.na(source_dates[[v]]) & source_dates[[v]] <= end_src & is.finite(end_syn), end_syn, Inf)
    new <- out[[v]] - out[[main]]
    tie <- which(!is.na(gap) & !is.na(new) & gap == 0)
    out[[paste0(".dm.", v)]][tie] <- out[[paste0(".dm.", main)]][tie]
    out[[paste0(".dm.", v)]][which(!is.na(gap) & !is.na(new) & sign(new) == -sign(gap))] <- NA
    out[[v]] <- keep_gap(out[[v]], out[[main]], gap, cap)
    out[[paste0(".gap.", v)]] <- gap
    out[[paste0(".cap.", v)]] <- cap
  }
  if (!is.null(out$.series)) out <- out[!duplicated(paste(out$.j, out$.series, level_string(out[[s$keys[1]]]))), , drop = FALSE]
  if (!is.null(target)) {
    out <- out[!out$.linked | paste(out$.j, out[[ds$link$key]]) %in% paste(target$.j, target[[ds$link$by]]), , drop = FALSE]
    out$.linked <- NULL
  }
  # Records that a move made the same as another of the subject's, where the source had none
  # (study days are calculated again from the dates).
  same <- setdiff(columns, c(s$id, sub("DTC$", "DY", dates)))
  if (length(same) && !anyDuplicated(x[c(".donor", same)])) {
    key <- do.call(paste, c(list(out$.j), unname(lapply(out[same], function(v) if (is.character(v)) v else as.numeric(v)))))
    out <- out[!duplicated(key), , drop = FALSE]
  }
  # Sorted as generation sorts them; then numbered again.
  sort <- intersect(c(".j", s$keys, main), names(out))
  out <- out[do.call(order, unname(lapply(out[sort], function(v) if (is.character(v)) v else as.numeric(v)))), , drop = FALSE]
  for (v in setdiff(s$id, c(TIMING_VARIABLES, paste0(ds$prefix, TIMING_SUFFIXES))))
    out[[v]] <- if (keyed_id(x, s$keys, v)) free_ids(out[[v]], out$.j) else stats::ave(seq_len(nrow(out)), out$.j, FUN = seq_along)
  if (!is.null(out$.series)) {
    first <- !duplicated(out$.series)
    rank <- stats::ave(seq_len(sum(first)), out$.j[first], FUN = seq_along)[match(out$.series, out$.series[first])]
    spid <- s$series
    if (endsWith(spid, "SPID") && is.character(out[[spid]])) {
      parts <- regmatches(out[[spid]], regexec("^(\\D*)(\\d+)$", out[[spid]]))
      ok <- lengths(parts) == 3
      out[[spid]][ok] <- sprintf("%s%0*d", vapply(parts[ok], `[`, "", 2), nchar(vapply(parts[ok], `[`, "", 3)), rank[ok])
    }
    out$.series <- group_index(out, c(".j", ".series"))
  }
  out
}

# Date `v` of records whose main date is `main`, where the source had `v - main = gap`: equal
# to the main date where the source's were, else, where their order turned, `gap` days from it
# and at most `cap`.
keep_gap <- function(v, main, gap, cap) {
  new <- v - main
  tie <- which(!is.na(gap) & !is.na(new) & gap == 0)
  v[tie] <- main[tie]
  turn <- which(!is.na(gap) & !is.na(new) & sign(new) == -sign(gap))
  v[turn] <- pmax(pmin(main[turn] + gap[turn], cap[turn]), pmin(main[turn], cap[turn]))
  v
}

# Identifiers `v` that repeat within a subject `j` (records from two source subjects) take the
# subject's next free numbers.
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

# Source dates `d` of source subjects `b`, placed for synthetic subjects `j`: on the record's
# visit (k_src for the source, k_syn for the synthetic subject) when the date is that visit's,
# else on a DM date it equals (the columns of dm_src, in order), else as many days from the
# nearest of the source subject's visits (columns of `day`) that the synthetic subject attends
# (`att`) or DM dates as it was, and between the synthetic days of those either side. The
# source subject's last visit counts as the synthetic subject's last, so a date before it
# stays before and a date after it stays after. A date without any of these stays as it was.
# Attribute "dm" names the DM date each date was placed on, and "end" the synthetic last visit
# for dates on or after the source's.
place_dates <- function(d, b, j, k_src, k_syn, day, syn, att, dm_src = NULL, dm_syn = NULL) {
  out <- rep(NA_real_, length(d))
  open <- !is.na(d) & !is.na(b)
  hit <- function(x) open & !is.na(x) & x
  if (!is.null(k_src) && ncol(day)) {
    on <- hit(d == day[cbind(b, k_src)] & !is.na(k_syn))
    out[on] <- syn[cbind(j[on], k_syn[on])]
    open <- open & !on
  }
  # The source subject's last visit stands for the synthetic subject's last: a date before
  # it stays before, and a date on or after it is placed from it, on a DM date only if that
  # is on or after it too.
  last <- function(m) suppressWarnings(apply(m, 1, max, na.rm = TRUE))
  end_src <- floor <- rep(-Inf, length(d))
  if (ncol(day)) {
    end_src[open] <- last(day[b[open], , drop = FALSE])
    floor[open] <- last(syn[j[open], , drop = FALSE] + ifelse(att[j[open], , drop = FALSE], 0, NA))
  }
  late <- is.finite(end_src) & is.finite(floor) & d >= end_src
  dm <- rep(NA_character_, length(d))
  for (m in colnames(dm_src)) {
    on <- hit(d == dm_src[b, m] & !is.na(dm_syn[j, m]) & !(late & dm_syn[j, m] < floor))
    out[on] <- dm_syn[j[on], m]
    dm[on] <- m
    open <- open & !on
  }
  rows <- which(open)
  if (length(rows) && ncol(day)) {
    src <- day[b[rows], , drop = FALSE]
    to <- syn[j[rows], , drop = FALSE]
    to[!att[j[rows], , drop = FALSE]] <- NA
    src[is.na(to)] <- NA
    to[is.na(src)] <- NA
    end_src <- end_src[rows]
    end_syn <- floor[rows]
    extra <- is.finite(end_src) & is.finite(end_syn)
    src[which(extra & d[rows] >= end_src), ] <- NA
    src <- cbind(src, ifelse(extra, end_src, NA))
    to <- cbind(to, ifelse(extra, end_syn, NA))
    if (length(dm_src)) {
      src <- cbind(src, dm_src[b[rows], , drop = FALSE])
      to <- cbind(to, dm_syn[j[rows], , drop = FALSE])
      to[cbind(late[rows], FALSE)[, c(rep(2, ncol(to) - ncol(dm_src)), rep(1, ncol(dm_src)))] & to < end_syn] <- NA
      src[is.na(to)] <- NA
      to[is.na(src)] <- NA
    }
    distance <- abs(src - d[rows])
    distance[is.na(distance)] <- Inf
    near <- max.col(-distance, ties.method = "first")
    found <- which(is.finite(distance[cbind(seq_along(rows), near)]))
    i <- rows[found]
    placed <- to[cbind(found, near[found])] + d[i] - src[cbind(found, near[found])]
    # Kept between the synthetic days of the landmarks either side.
    below <- ifelse(src[found, , drop = FALSE] <= d[i], to[found, , drop = FALSE], NA)
    above <- ifelse(src[found, , drop = FALSE] >= d[i], to[found, , drop = FALSE], NA)
    low <- suppressWarnings(apply(below, 1, max, na.rm = TRUE))
    high <- suppressWarnings(apply(above, 1, min, na.rm = TRUE))
    out[i] <- pmin(pmax(placed, low), high)
    open[i] <- FALSE
  }
  out[open] <- d[open]
  structure(out, dm = dm, end = ifelse(late, floor, NA))
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
      key %in% implications$level[[i]] & s != implications$value[[i]] else s %in% implications$value[[i]] & !key %in% implications$level[[i]]
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
