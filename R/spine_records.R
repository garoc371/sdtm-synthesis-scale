# One dataset's records (see `compose_spine()`), dates as numbers, with the synthetic
# subject's number (.j). A record's content (`content_columns()`: the test, the category, the
# time point) comes whole with its panel, set or series; at a visit, the visit's name and
# planned day are those most of the dataset's subjects have there, else SV's.
compose_records <- function(study, name, composed, ctx) {
  r <- record_source(study, name)
  ctx <- record_pools(ctx, name)
  link <- link_sets(study, r$ds$link, composed, ctx)
  draw <- function(who, ctx) if (r$by_visit) draw_visit_records(r$x, r$code, r$number, ctx, link$src, link$syn, who) else
    draw_record_set(r$x, r$code, ctx, link$src, link$syn, who)
  ta <- study_design(study)$TA
  elements <- name == "SE" && !is.null(r$x$ETCD) && all(c("ARMCD", "ETCD", "TAETORD") %in% names(ta))
  rows <- if (elements) draw_elements(r$x, r$code, ta, subject_arm(study$spine$DM), subject_arm(composed$DM), ctx) else
    if (r$series) draw_series(r$x, r$code, r$number, r$at_visits, ctx) else draw(seq_len(ctx$n), ctx)
  if (!r$series && !elements) rows <- redraw_flags(rows, r, name, draw, ctx)
  if (!nrow(rows)) return(data.frame(.j = integer(), r$x[0, r$columns, drop = FALSE], check.names = FALSE))
  out <- data.frame(.j = rows$.j, r$x[r$row_of[rows$code], r$content, drop = FALSE], check.names = FALSE, stringsAsFactors = FALSE)
  if (r$series) out$.series <- rows$series
  if (!is.null(r$number)) out <- record_visits(out, r, rows, study, ctx)
  out <- record_dates(out, r, rows, name, composed, ctx)
  if (elements) out <- chain_elements(out, r$ds$prefix)
  finish_records(out, r, link$target)
}

# A dataset's source records for `compose_records()`, sorted by subject (then series, visit
# and main date), with their contents numbered (`code`; `row_of`, each content's first
# record) and how they are drawn: at visits (`by_visit`), as series, or series at visits.
record_source <- function(study, name) {
  ds <- study$datasets[[name]]
  s <- ds$structure
  x <- study$spine[[name]]
  dates <- date_columns(x)
  main <- main_date(ds$prefix, dates)
  number <- if (!is.null(x$VISITNUM)) as.numeric(x$VISITNUM)
  series <- identical(s$type, "series")
  o <- do.call(order, c(list(x$.subject), if (series) list(x$.series), if (!is.null(number)) list(number),
    if (!is.na(main)) list(as.numeric(x[[main]]))))
  x <- x[o, , drop = FALSE]
  content <- content_columns(x, ds, dates)
  tuple <- if (length(content)) key_strings(x[content]) else rep("", nrow(x))
  code <- match(tuple, unique(tuple))
  list(ds = ds, s = s, x = x, columns = setdiff(names(x), ".subject"), dates = dates, main = main, number = number[o],
    series = series, by_visit = !series && !is.null(number) && (!is.null(s$visit) || identical(s$type, "count")),
    at_visits = series && identical(s$keys[1], "VISITNUM"), content = content, code = code,
    row_of = match(seq_along(unique(tuple)), code))
}

# Keys that pool source subjects for a dataset's records: only those with the same flags for
# the dataset (see `draw_profiles()`), with the same values too and the same datasets where
# enough have them (a TR link group of subjects without RS from subjects without RS); a panel,
# where none of these has enough, among those who have the dataset (`last_have`: a rare set
# of MH categories).
record_pools <- function(ctx, name) {
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
  ctx
}

# A linked record (TR to its TU lesion) is drawn among subjects with the same linked records
# where enough have them, and dropped when the subject has none to link to: each subject's
# linked records in the source (`src`) and as composed (`syn`), and the composed dataset
# linked to (`target`); none without a link.
link_sets <- function(study, link, composed, ctx) {
  target <- if (!is.null(link)) composed[[link$dataset]]
  if (is.null(target)) return(list())
  y <- study$spine[[link$dataset]]
  id_set <- function(id, owner, size) vapply(split(level_string(id), factor(owner, seq_len(size))),
    function(v) paste(sort(unique(v)), collapse = " "), "")
  list(src = id_set(y[[link$by]], y$.subject, ctx$N), syn = id_set(target[[link$by]], target$.j, ctx$n), target = target)
}

# Records `rows` redrawn, up to 20 times, for subjects whose records do not have the dataset,
# categories and values their flags say (visits that all drew no CIBIC+ questionnaire): among
# source subjects with the same flags of those missed so far, else the same of the one fewest
# source subjects have (a NEW lesion, where few have the subject's TULOC flags too); a
# subject keeps the earlier records where the new ones miss more.
redraw_flags <- function(rows, r, name, draw, ctx) {
  x <- r$x
  row_of <- r$row_of
  f <- colnames(ctx$syn$flags)
  flagged <- f[f == name | startsWith(f, paste0("T.", name, ".")) | startsWith(f, paste0("V.", name, "."))]
  if (!length(flagged)) return(rows)
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
  rows
}

# Composed records' visits (`out`, drawn as `rows`): drawn with the records, else assigned
# (`assign_visits()`); and the visit's name and planned day.
record_visits <- function(out, r, rows, study, ctx) {
  x <- r$x
  out$VISITNUM <- if (r$by_visit || r$at_visits) rows$visit else assign_visits(r$number, x$.subject, occurrence(x$.subject, r$code), out$.j,
    occurrence(rows$.j, rows$code), ctx)
  hub <- study$spine[[VISIT_HUB]]
  for (v in intersect(c("VISIT", "VISITDY"), r$columns)) {
    out[[v]] <- visit_values(x[[v]], r$number, x$.subject, out$VISITNUM)
    gap <- is.na(out[[v]])
    if (any(gap) && !is.null(hub[[v]])) out[[v]][gap] <- visit_values(hub[[v]], as.numeric(hub$VISITNUM), hub$.subject, out$VISITNUM[gap])
  }
  out
}

# Composed records' dates, the main date (--STDTC, else --DTC) first, by visit (and time
# point) for records at visits, by content (and occurrence) otherwise: once per visit and time
# point for records at visits, once per series for the dates a series shares (`series_dates()`).
record_dates <- function(out, r, rows, name, composed, ctx) {
  x <- r$x
  main <- r$main
  L <- record_landmarks(out, r, name, composed, ctx)
  # A visit's time points (PCTPT) are dated apart.
  point <- intersect(paste0(r$ds$prefix, c("TPTREF", "TPTNUM", "TPT", "ELTM")), r$content)
  point_src <- if (length(point)) key_strings(x[point]) else ""
  point_syn <- if (length(point)) key_strings(out[point]) else ""
  stratum <- date_strata(out, r, rows, point_src, point_syn)
  whole <- if (r$series) series_dates(x, r$dates) else character()
  source_dates <- lapply(x[r$dates], as.numeric)
  for (v in c(intersect(main, r$dates), setdiff(r$dates, main))) {
    is_main <- identical(v, main)
    unit_src <- if (v %in% whole) paste(x$.subject, x$.series) else if (r$by_visit && is_main) paste(x$.subject, r$number, point_src) else seq_len(nrow(x))
    unit_syn <- if (v %in% whole) paste(out$.j, out$.series) else if (r$by_visit && is_main) paste(out$.j, out$VISITNUM, point_syn) else seq_len(nrow(out))
    from_src <- L$src
    from_syn <- L$syn
    if (!is_main && !is.na(main)) {
      from_src <- cbind(main = source_dates[[main]], following = next_date(source_dates[[main]], x$.subject), from_src)
      from_syn <- cbind(main = out[[main]], following = next_date(out[[main]], out$.j), from_syn)
    }
    first <- !duplicated(unit_src)
    out[[v]] <- draw_dates(source_dates[[v]][first], from_src[first, , drop = FALSE], x$.subject[first], stratum$src[first],
      from_syn, out$.j, stratum$syn, ctx, unit_syn)
  }
  out
}

# The landmarks records' dates are placed from (`landmarks()`), in the source (`src`) and as
# composed (`syn`): with the end of the record's SV visit once SV is composed, and the
# subject's date at the same visit in each dataset composed before (LBHE's at LBCH's).
record_landmarks <- function(out, r, name, composed, ctx) {
  x <- r$x
  number <- r$number
  V <- ctx$visits$number
  lean <- Filter(function(m) is.null(ctx$derived[[m]]) || ctx$derived[[m]]$dataset != name && ctx$derived[[m]]$dataset %in% names(composed),
    colnames(ctx$dm_src))
  src <- landmarks(x$.subject, if (!is.null(number)) match(number, V) else NA, ctx$src_days, ctx$dm_src[, lean, drop = FALSE], ctx$a)
  syn <- landmarks(out$.j, if (!is.null(out$VISITNUM)) match(out$VISITNUM, V) else NA, ctx$syn_days, ctx$dm_syn[, lean, drop = FALSE], ctx$a)
  if (!is.null(number) && !is.null(ctx$syn_ends)) {
    src <- cbind(src, visit_end = ctx$visits$end[cbind(x$.subject, match(number, V))])
    syn <- cbind(syn, visit_end = ctx$syn_ends[cbind(out$.j, match(out$VISITNUM, V))])
  }
  if (!is.null(number) && length(ctx$visit_dates)) {
    at_src <- cbind(x$.subject, match(number, V))
    at_syn <- cbind(out$.j, match(out$VISITNUM, V))
    src <- cbind(src, do.call(cbind, lapply(ctx$visit_dates, function(e) e$src[at_src])))
    syn <- cbind(syn, do.call(cbind, lapply(ctx$visit_dates, function(e) e$syn[at_syn])))
  }
  list(src = src, syn = syn)
}

# The strata records' dates are drawn in, in the source (`src`) and as composed (`syn`): the
# visit and time point for records at visits, the content for a series, else the content and
# its occurrence; a series' by its first visit too where enough subjects have it (a
# medication first recorded at week 8 started near then).
date_strata <- function(out, r, rows, point_src, point_syn) {
  x <- r$x
  number <- r$number
  src <- if (r$by_visit) paste(level_string(number), point_src) else if (r$series) as.character(r$code) else occurrence(x$.subject, r$code)
  syn <- if (r$by_visit) paste(level_string(out$VISITNUM), point_syn) else if (r$series) as.character(rows$code) else occurrence(rows$.j, rows$code)
  if (r$series && !is.null(number)) {
    start_src <- paste(src, stats::ave(number, x$.subject, x$.series, FUN = min))
    start_syn <- paste(syn, stats::ave(out$VISITNUM, out$.j, out$.series, FUN = min))
    big <- unique(start_src[subjects_holding(start_src, x$.subject) >= min_subjects()])
    src <- ifelse(start_src %in% big, start_src, src)
    syn <- ifelse(start_syn %in% big, start_syn, syn)
  }
  list(src = src, syn = syn)
}

# Composed records finished: dropped where the record they link to (`target`) is missing, not
# repeated where the source never repeats them within a subject, sorted as generation sorts
# them, then numbered.
finish_records <- function(out, r, target) {
  x <- r$x
  s <- r$s
  link <- r$ds$link
  for (v in setdiff(r$columns, names(out))) out[[v]] <- x[[v]][rep(NA_integer_, nrow(out))]
  if (!is.null(target)) out <- out[is.na(out[[link$key]]) | paste(out$.j, out[[link$key]]) %in% paste(target$.j, target[[link$by]]), , drop = FALSE]
  keys <- intersect(s$keys, names(out))
  if (length(keys) && !anyDuplicated(x[c(".subject", keys)])) out <- out[!duplicated(out[c(".j", keys)]), , drop = FALSE]
  sort <- intersect(c(".j", s$keys, r$main), names(out))
  out <- out[do.call(order, unname(lapply(out[sort], function(v) if (is.character(v)) v else as.numeric(v)))), , drop = FALSE]
  for (v in setdiff(s$id, c(TIMING_VARIABLES, paste0(r$ds$prefix, TIMING_SUFFIXES))))
    out[[v]] <- if (v %in% r$content) free_ids(out[[v]], out$.j) else stats::ave(seq_len(nrow(out)), out$.j, FUN = seq_along)
  if (r$series) {
    first <- !duplicated(out$.series)
    rank <- stats::ave(seq_len(sum(first)), out$.j[first], FUN = seq_along)[match(out$.series, out$.series[first])]
    spid <- s$series
    if (spid %in% r$columns) out[[spid]] <- series_ids(x[[spid]], rank)
    out$.series <- group_index(out, c(".j", ".series"))
  }
  rownames(out) <- NULL
  out[c(".j", r$columns)]
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
  side <- function(last, u) ifelse(is.na(last), "", sign(last - u))
  out <- lapply(unique(number), function(u) {
    at <- which(number %in% u)
    k <- match(u, V)
    src <- if (is.na(k)) seq_len(ctx$N) else sort(union(which(ctx$visits$attended[, k]), x$.subject[at]))
    to <- if (is.na(k)) who else intersect(which(ctx$att[, k]), who)
    if (!length(to)) return(NULL)
    panels <- draw_panels(code[at], x$.subject[at], src, to, ctx, paste(extra_src, side(ctx$src$last, u)), paste(extra_syn, side(ctx$syn$last, u)))
    data.frame(.j = rep(to, lengths(panels)), code = unlist(panels) %||% integer(), visit = rep(u, sum(lengths(panels))))
  })
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

