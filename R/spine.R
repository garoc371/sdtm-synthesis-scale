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
# keeps which records belong together (.series).
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

# The visit schedule, learnt on loading. Each scheduled visit has a planned study day: TV's
# VISITDY (per arm when TV gives one), or without TV the median day the source's subjects
# attended it, for the visits that mark progress and that at least SCHEDULED_SHARE of the
# subjects attended (in the pilot, WEEK 26 has 36% of them, an unscheduled visit at most
# 17%). How far the source's subjects were from it (their study day minus the planned day)
# is kept as percentiles whose ends leave out the k subjects furthest either way (see
# `number_margin()`). Visits without a planned day
# (unscheduled, AE follow-up) are not scheduled: they move with the visits around them.
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

# After generation, each subject's visit days are drawn from the schedule: the planned day
# plus a deviation drawn from the source's at that visit. Their other dates follow, through
# one map for all datasets: a date on a visit (a DS date, an SE start, a findings record)
# stays on that visit, a date between two visits (an adverse event) keeps its distance to the
# nearer one, so it stays in its epoch (on a tie, the one nearer day 1; when the gap shrinks,
# only within a quarter of the new gap, the dates in the middle being squeezed), and a date
# before the first or after the last visit keeps its distance to it. A gap shrinks only as far
# as the subject's dates in it still fall on different days, so dates keep their order, dates
# on one day stay on one day and dates on different days stay apart. RFSTDTC stays day 1,
# which then falls on a calendar date drawn so that the subject's visits lie between the
# source's first and last visit. Every study day is then the SDTM one: the date minus
# RFSTDTC, plus one from day 1 on. synth.schedule = FALSE keeps the donors' days, with day 1
# on SPINE_ANCHOR.
schedule_dates <- function(data, schedule, anchor = as.Date(SPINE_ANCHOR)) {
  sv <- data[[VISIT_HUB]]
  date <- if (!is.null(sv)) main_date(VISIT_HUB, names(sv)) else NA
  if (!getOption("synth.schedule", TRUE) || is.null(schedule) || is.na(date) || is.null(data$DM)) return(data)
  arm <- data$DM$ARMCD[match(sv$USUBJID, data$DM$USUBJID)] %||% rep(NA_character_, nrow(sv))
  points <- data.frame(USUBJID = sv$USUBJID, visit = as.numeric(sv$VISITNUM), day = as.numeric(sv[[date]]), arm = arm,
    stringsAsFactors = FALSE)
  points$planned <- schedule_day(schedule$planned, points$visit, points$arm)
  points <- points[!is.na(points$planned) & !is.na(points$day), , drop = FALSE]
  points <- points[order(points$USUBJID, points$day), , drop = FALSE]
  points <- points[!duplicated(points[c("USUBJID", "visit")]), , drop = FALSE]
  q <- do.call(rbind, schedule$deviation[as.character(points$visit)])
  drawn <- points$planned + if (nrow(points)) round(quantile_draw(q)) else numeric()
  points$new <- as.numeric(anchor) + drawn - (drawn > 0)
  dated <- Filter(function(x) "USUBJID" %in% names(x), data)
  days <- do.call(rbind, lapply(dated, function(x) do.call(rbind, lapply(Filter(function(v) inherits(v, "Date"), x),
    function(v) data.frame(USUBJID = x$USUBJID, day = as.numeric(v))))))
  days <- split(days$day[!is.na(days$day)], days$USUBJID[!is.na(days$day)])
  visits <- split(points, factor(points$USUBJID, data$DM$USUBJID))
  attended <- split(as.numeric(sv[[date]]), factor(sv$USUBJID, data$DM$USUBJID))
  window <- as.numeric(schedule$calendar)
  a <- as.numeric(anchor)
  maps <- do.call(rbind, Map(function(s, p, attended) {
    d <- unique(c(days[[s]], a))
    m <- if (nrow(p)) day_map(d, p$day, c(0, diff(p$new)), a) else data.frame(old = d, new = d)
    m$new <- m$new + a - m$new[m$old == a]
    # Day 1 on the calendar, so that the subject's visits lie within the source's.
    seen <- c(m$new[m$old %in% attended], a)
    first <- window[1] + a - min(seen)
    last <- max(first, window[2] - (max(seen) - a))
    m$new <- m$new - a + first + sample.int(last - first + 1, 1) - 1
    data.frame(USUBJID = s, m)
  }, names(visits), visits, attended))
  mapped <- paste(maps$USUBJID, maps$old)
  data <- lapply(data, function(x) {
    if (!"USUBJID" %in% names(x)) return(x)
    for (v in names(Filter(function(v) inherits(v, "Date"), x))) {
      old <- as.numeric(x[[v]])
      new <- maps$new[match(paste(x$USUBJID, old), mapped)]
      new[is.na(new)] <- old[is.na(new)]
      x[[v]] <- as.Date(new, origin = "1970-01-01")
    }
    x
  })
  study_days(data)
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

# One subject's map: their days (all dates, as numbers), and their visit days (old, in order)
# with the drawn gaps that lead to them; each gap is widened to hold the days inside it.
day_map <- function(days, old, gap, anchor) {
  days <- sort(unique(c(days, old)))
  g <- diff(old)
  inner <- vapply(seq_along(g), function(j) sum(days > old[j] & days < old[j + 1]), 0)
  g2 <- ifelse(g == 0, 0, pmax(gap[-1], inner + 1))
  new <- old[1] + c(0, cumsum(g2))
  m <- length(old)
  i <- findInterval(days, old)
  out <- days + ifelse(i == 0, new[1] - old[1], new[m] - old[m])
  on <- days %in% old
  out[on] <- new[match(days[on], old)]
  for (j in which(inner > 0)) {
    k <- which(days > old[j] & days < old[j + 1])
    out[k] <- new[j] + inside_days(days[k] - old[j], g[j], g2[j], abs(old[j + 1] - anchor) < abs(old[j] - anchor))
  }
  data.frame(old = days, new = out)
}

# Where the days a (in order, strictly between two visits g days apart) go when the visits
# are g2 days apart; there are fewer than g2 of them. A day halfway keeps its distance to the
# later visit when `later`.
inside_days <- function(a, g, g2, later) {
  if (g2 >= g) return(ifelse(a < g - a | a == g - a & !later, a, g2 - (g - a)))
  l <- g2 / 4
  t <- round(ifelse(a <= l, a, ifelse(g - a <= l, g2 - (g - a), l + (a - l) * (g2 - 2 * l) / (g - 2 * l))))
  t <- pmin(pmax(t, 1), g2 - 1)
  n <- length(t)
  for (i in seq_len(n)[-1]) t[i] <- max(t[i], t[i - 1] + 1)
  t[n] <- min(t[n], g2 - 1)
  for (i in rev(seq_len(n))[-1]) t[i] <- min(t[i], t[i + 1] - 1)
  t
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
