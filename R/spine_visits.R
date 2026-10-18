
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

# The SV visit of each of records `x` (from SVSTDTC to SVENDTC), NA for records off visits;
# NULL for a dataset without visits.
visit_span <- function(x, ctx) {
  if (is.null(x$VISITNUM) || is.null(ctx$syn_ends)) return(NULL)
  at <- cbind(x$.j, match(as.numeric(x$VISITNUM), ctx$visits$number))
  start <- ctx$syn_days[at]
  list(start = start, end = ifelse(is.na(ctx$syn_ends[at]), start, pmax(ctx$syn_ends[at], start)))
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

# Visits attended (see `compose_spine()`).
draw_visits <- function(ctx) {
  V <- ctx$visits$number
  last <- ctx$syn$last
  last_src <- ctx$src$last
  matrix(vapply(seq_along(V), function(k) {
    progress <- V[k] %in% ctx$visits$progress
    att <- progress & !is.na(last) & last == V[k]
    free <- which(!is.na(last) & (!progress | last > V[k]))
    pool <- which(!is.na(last_src) & (!progress | last_src > V[k]))
    if (!length(free) || !length(pool)) return(att)
    side <- function(l) as.character(l > V[k])
    att[free] <- pooled_draw(as.character(ctx$visits$attended[pool, k]), pool, pool_keys(ctx$want, free, side(last[free]), FALSE),
      pool_keys(ctx$have, pool, side(last_src[pool]), FALSE)) %in% "TRUE"
    att
  }, logical(ctx$n)), ctx$n, length(V))
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
  if (!is.null(schedule)) drawn[] <- vapply(seq_along(V), function(k) {
    q <- schedule$deviation[[as.character(V[k])]]
    s <- schedule_day(schedule$planned, rep(V[k], n), arm) + if (is.null(q)) 0 else draw(q)
    a + s - (s > 0)
  }, numeric(n))
  # Days since the previous visit attended (since day 1 before the first), in the source.
  gaps <- visits$day - before_in_row(visits$day, a)
  following <- after_in_row(ifelse(att, drawn, NA))
  # Each visit after the one before, planned or a gap after it.
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

# The visit of records drawn without one where the dataset has VISITNUM (a disposition): per
# content, a fixed visit (the one most subjects have), the subject's first visit, last visit
# that marks progress, last visit or none, whichever most source records have; a visit the
# subject does not have in SV gives the nearest one they have.
assign_visits <- function(number, owner, code, j, code_to, ctx) {
  V <- ctx$visits$number
  out <- rep(NA_real_, length(j))
  if (!length(V)) return(out)
  src <- visit_ends(ctx$src_days, V)
  syn <- visit_ends(ctx$syn_days, V)
  same <- function(a, b) !is.na(a) & !is.na(b) & a == b
  for (cd in intersect(code_to[!is.na(code_to)], code)) {
    rows <- which(code_to == cd)
    at <- which(code == cd)
    v <- number[at]
    fixed <- common_visit(v, owner[at])
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

# Each subject's first and last visit attended (NA where none), from their days at visits `V`.
visit_ends <- function(days, V) {
  seen <- !is.na(days)
  list(first = apply(seen, 1, function(a) V[which(a)[1]]), last = apply(seen, 1, function(a) V[rev(which(a))[1]]))
}

# The visit most subjects (`owner`) have among visits `v` that at least min_subjects() have;
# NULL where none has.
common_visit <- function(v, owner) {
  ok <- !is.na(v) & subjects_holding(level_string(v), owner) >= min_subjects()
  if (any(ok)) as.numeric(names(which.max(tapply(owner[ok], v[ok], function(o) length(unique(o))))))
}

# For each cell of matrix `m`, the last value before it in its row that is not NA (`start`
# where none is); `after_in_row()`, the first after it.
before_in_row <- function(m, start = NA) {
  out <- matrix(NA_real_, nrow(m), ncol(m))
  last <- rep(start, nrow(m))
  for (k in seq_len(ncol(m))) {
    out[, k] <- last
    last <- ifelse(is.na(m[, k]), last, m[, k])
  }
  out
}

after_in_row <- function(m) {
  back <- rev(seq_len(ncol(m)))
  before_in_row(m[, back, drop = FALSE])[, back, drop = FALSE]
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

