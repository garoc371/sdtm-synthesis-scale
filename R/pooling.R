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

