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

# The DM dates taken from a dataset (`from`: SV where it gives one, as DMDTC its first visit),
# and those other datasets' first or last dates
# equal in at least DERIVED_SHARE of the source subjects that have both, at least
# min_subjects() of them (`tied`: XD's last XDENDTC, a copy of EX's, at RFXENDTC), and the
# other datasets' first or last dates that are never after (before) a DM date in every source
# subject that has both, at least min_subjects() of them (`bounds`: no visit after RFPENDTC).
derived_dates <- function(study, dates) {
  dm <- study$spine$DM
  candidates <- date_candidates(study)
  later <- setdiff(dates, "RFSTDTC")
  from <- Filter(Negate(is.null), lapply(stats::setNames(nm = later), function(m) taken_from(candidates, as.numeric(dm[[m]]))))
  # The other datasets' dates that hold against DM date `m` in the source.
  related <- function(m, holds) {
    target <- as.numeric(dm[[m]])
    lapply(Filter(function(c) !identical(from[[m]]$dataset, c$dataset) && holds(c$value, target, c$end), candidates),
      function(c) c(list(date = m), c[DATE_FIELDS]))
  }
  enough <- function(both) length(both) >= min_subjects()
  equal <- function(value, target, end) {
    both <- which(!is.na(target) & !is.na(value))
    enough(both) && mean(value[both] == target[both]) >= DERIVED_SHARE
  }
  never_past <- function(value, target, end) {
    both <- which(!is.na(target) & !is.na(value))
    enough(both) && all(if (end == "max") value[both] <= target[both] else value[both] >= target[both])
  }
  list(from = from, tied = unlist(lapply(dates, related, equal), recursive = FALSE),
    bounds = unlist(lapply(later, related, never_past), recursive = FALSE))
}

DATE_FIELDS <- c("dataset", "variable", "end", "key", "level")

# Each dataset's first and last date of every subject, per date column: over all its records
# and, for a dataset planned by category or test, over each key value's.
date_candidates <- function(study) {
  spine <- study$spine
  N <- nrow(spine$DM)
  unlist(lapply(setdiff(names(spine), "DM"), function(d) {
    x <- spine[[d]]
    st <- study$datasets[[d]]$structure
    key <- if (identical(st$type, "schedule") && is.null(st$visit)) st$keys[1]
    grid <- expand.grid(end = c("min", "max"), level = c(NA, if (!is.null(key)) unique(level_string(x[[key]]))),
      variable = date_columns(x), stringsAsFactors = FALSE)
    mapply(function(v, u, end) {
      rows <- if (is.na(u)) TRUE else level_string(x[[key]]) == u
      list(dataset = d, variable = v, end = end, key = if (!is.na(u)) key, level = u,
        value = subject_extreme(x[[v]][rows], x$.subject[rows], N, end))
    }, grid$variable, grid$level, grid$end, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }), recursive = FALSE)
}

# The candidate a DM date is taken from: one equal to it in at least DERIVED_SHARE of the
# subjects that have it, SV's where one is, else the one most often equal; NULL where none is.
taken_from <- function(candidates, target) {
  has <- which(!is.na(target))
  if (length(has) < min_subjects() || !length(candidates)) return(NULL)
  share <- vapply(candidates, function(c) mean((c$value[has] == target[has]) %in% TRUE), 0)
  hub <- vapply(candidates, function(c) c$dataset == VISIT_HUB, TRUE) & share >= DERIVED_SHARE
  if (any(hub)) share[!hub] <- 0
  if (max(share) >= DERIVED_SHARE) candidates[[which.max(share)]][DATE_FIELDS]
}

# Once dataset `name` is composed: the DM dates taken from it, the DM dates that bound it
# (moved to its date where it passes them, the date they are taken from with them), and its
# dates tied to DM dates. Returns the composed datasets and DM's dates (`dm_syn`).
settle_dm_dates <- function(composed, ctx, derived, name, prefix) {
  dm <- ctx$dm_syn
  from <- derived$from
  for (m in names(Filter(function(r) r$dataset == name, from))) {
    value <- derived_value(composed, from[[m]], ctx$n)
    keep <- !is.na(value) & !is.na(dm[, m])
    dm[keep, m] <- value[keep]
  }
  for (r in derived$bounds) {
    source <- from[[r$date]]
    both <- c(r$dataset, source$dataset)
    if (!name %in% both || !all(both %in% names(composed))) next
    value <- derived_value(composed, r, ctx$n)
    over <- which(if (r$end == "max") value > dm[, r$date] else value < dm[, r$date])
    if (!length(over)) next
    dm[over, r$date] <- value[over]
    if (!is.null(source)) composed[[source$dataset]] <- tie_date(composed[[source$dataset]], source, dm[, r$date])
  }
  ties <- Filter(function(r) r$dataset == name && (is.null(from[[r$date]]) || from[[r$date]]$dataset %in% names(composed)), derived$tied)
  span <- if (name != VISIT_HUB) visit_span(composed[[name]], ctx)
  composed[[name]] <- tie_dates(composed[[name]], ties, dm, span, if (name == "SE") prefix)
  list(composed = composed, dm_syn = dm)
}

# Dates tied to DM dates (`tie_date()`). SE's elements (`prefix` given) keep their order: a tie
# moves an element's start (the follow-up's to RFENDTC), and the elements around it follow.
tie_dates <- function(x, ties, dm, span, prefix = NULL) {
  o <- if (!is.null(prefix)) order(x$.j, as.numeric(x$SESTDTC %||% 0), as.numeric(x$SEENDTC %||% 0))
  for (r in ties) x <- tie_date(x, r, dm[, r$date], span)
  if (!is.null(prefix)) x <- chain_elements(x, prefix, o)
  x
}

# Each subject's first ("min") or last ("max") date of `v`, for subjects 1 to `size`.
subject_extreme <- function(v, owner, size, end) {
  v <- as.numeric(v)
  ok <- !is.na(v) & !is.na(owner)
  as.numeric(tapply(v[ok], factor(owner[ok], seq_len(size)), end))
}

# DM's spine: the drawn arm's columns, and the dates (`draw_dates()`) in order of their median
# day, each placed from the visits, day 1 and the dates before it. A date taken from a dataset
# (`derived_dates()`) is replaced once that dataset is composed, so no other date is placed from it.
compose_dm <- function(dm, dates, ctx) {
  N <- ctx$N
  n <- ctx$n
  out <- data.frame(.j = seq_len(n))
  arm <- intersect(SPINE_ARM, names(dm))
  out[arm] <- lapply(dm[arm], `[`, match(ctx$syn$arm, ctx$src$arm))
  flags <- grep("^T\\.", names(dm), value = TRUE)
  out[flags] <- lapply(flags, function(v) rep("N", n))
  other <- setdiff(names(dm), c(".subject", arm, flags, dates))
  out[other] <- lapply(dm[other], function(v) {
    s <- level_string(v)
    v[match(pooled_draw(s, seq_len(N), ctx$want, ctx$have), s)]
  })
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

