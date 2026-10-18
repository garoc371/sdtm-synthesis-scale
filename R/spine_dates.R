# Each subject's main date at each visit (the first, NA where none) in dataset `name`, in the
# source and as composed; NULL for a dataset without visits or dates, or whose records are not
# drawn at visits (a series: a medication's start is no visit date).
visit_dates <- function(study, name, composed, ctx) {
  x <- study$spine[[name]]
  s <- study$datasets[[name]]$structure
  main <- main_date(study$datasets[[name]]$prefix, date_columns(x))
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
  if (!ncol(days)) days <- cbind(rep(NA_real_, nrow(days)))
  at <- cbind(j, rep_len(k, length(j)))
  first <- suppressWarnings(apply(days, 1, min, na.rm = TRUE))
  last <- suppressWarnings(apply(days, 1, max, na.rm = TRUE))
  first[!is.finite(first)] <- last[!is.finite(last)] <- NA
  out <- cbind(visit = days[at], next_visit = after_in_row(days)[at])
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
    out[rows] <- stratum_dates(d[src], L[src, , drop = FALSE], owner[src], to[rows, , drop = FALSE], j[rows], ctx)
  }
  out[first[match(unit, unit[first])]]
}

# One stratum's dates (see `draw_dates()`), NA where the date is drawn unknown.
stratum_dates <- function(d, L, owner, to, j, ctx) {
  k <- min_subjects()
  out <- rep(NA_real_, length(j))
  have <- lapply(ctx$have, `[`, owner)
  known <- which(pooled_draw(as.character(!is.na(d)), owner, lapply(ctx$want, `[`, j), have) %in% "TRUE")
  if (!length(known)) return(out)
  to <- to[known, , drop = FALSE]
  want <- lapply(ctx$want, `[`, j[known])
  gap <- d - L
  fit <- lapply(seq_len(ncol(L)), function(c) {
    ok <- which(!is.na(gap[, c]))
    if (length(unique(owner[ok])) < k) return(NULL)
    g <- gap[ok, c]
    list(ok = ok, spread = diff(stats::quantile(g, c(0.1, 0.9), names = FALSE)), after = all(g >= 0), before = all(g <= 0))
  })
  useful <- which(!vapply(fit, is.null, TRUE))
  value <- rep(NA_real_, length(known))
  for (c in useful[order(vapply(fit[useful], `[[`, 0, "spread"))]) {
    need <- which(is.na(value) & !is.na(to[, c]))
    if (!length(need)) next
    ok <- fit[[c]]$ok
    value[need] <- to[need, c] + pooled_number(gap[ok, c], owner[ok], lapply(want, `[`, need), lapply(have, `[`, ok))
  }
  bound <- function(side, f) {
    on <- useful[vapply(fit[useful], `[[`, TRUE, side)]
    b <- if (length(on)) suppressWarnings(apply(to[, on, drop = FALSE], 1, f, na.rm = TRUE)) else rep(NA_real_, length(known))
    ifelse(is.finite(b), b, NA)
  }
  value <- pmin(pmax(value, bound("after", max), na.rm = TRUE), bound("before", min), na.rm = TRUE)
  # Within the record's SV visit above all, where the source's always are.
  visit <- function(side, column) {
    on <- intersect(useful[vapply(fit[useful], `[[`, TRUE, side)], which(colnames(to) == column))
    if (length(on)) to[, on] else rep(NA_real_, length(known))
  }
  set <- !is.na(value)
  value[set] <- pmin(pmax(value, visit("after", "visit"), na.rm = TRUE), visit("before", "visit_end"), na.rm = TRUE)[set]
  out[known] <- ifelse(is.na(value), NA, value)
  out
}
