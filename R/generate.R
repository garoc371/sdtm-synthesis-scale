# Generation: subjects, then records, then values in graph order.
#
# `generate_study()` needs only the specification and fits (see `study_bundle()`), not the
# source data, so a saved bundle generates new data without Shiny.

# Datasets are drawn in the specification's order, the order their inputs assume.
# R's default generator is set with the seed: background workers default to another one,
# and the same seed must give the same data in the app, a worker or a script.
generate_study <- function(study, n, seed = 1) {
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  built <- list()
  for (name in intersect(names(study$datasets), names(study$fits))) built[[name]] <- generate_dataset(study, name, built, n)
  list(data = built, n = n, seed = seed)
}

# Records are sorted by subject, record keys and the dataset's main date (--STDTC or
# --DTC). Rules that depend on record order (sequence numbers, and record selections when
# dates tie), and the rules calculated from them, are redone after sorting.
generate_dataset <- function(study, name, built, n) {
  ds <- study$datasets[[name]]
  fits <- study$fits[[name]]
  s <- ds$structure
  built[[name]] <- if (name == "DM") data.frame(row.names = seq_len(n)) else RECORD_STRUCTURES[[s$type]]$draw(fits$structure,
    subject_inputs(study, built, name), paste0("DM.", treatment_variable(study$datasets$DM)), s, subject_visits(built, ds))
  frame <- dataset_frame(study, name, built)
  width <- max(4, nchar(n))
  nodes <- generation_order(ds)
  beyond <- rep(FALSE, nrow(frame))
  for (v in nodes) {
    value <- switch(node_type(ds, v),
      key = frame[[v]],
      rule = RULE_OPS[[ds$rules[[v]]$op]]$apply(ds$rules[[v]], frame),
      identifier = if (v == "USUBJID") sprintf("SYN-%0*d", width, seq_len(nrow(frame))) else if (name == "DM")
        sprintf("%0*d", width, seq_len(nrow(frame))) else as.character(stats::ave(seq_len(nrow(frame)), frame$USUBJID, FUN = seq_along)),
      sampled = draw_node(ds, v, fits$nodes[[v]], frame))
    beyond <- beyond | (attr(value, "beyond") %||% FALSE)
    attr(value, "beyond") <- NULL
    frame[[v]] <- value
    # A sampled visit is one the subject attended, at its drawn position among their visits,
    # and the record reads that visit's SV dates.
    if (v == "VISITNUM" && isTRUE(s$sv) && node_type(ds, v) == "sampled") {
      frame$VISITNUM <- attended_visit(frame, built, ds)
      frame$VISITNUM <- placed_visit(frame, built, ds, fits$position, subject_inputs(study, built, name))
      frame <- join_visits(frame, built)
    }
  }
  # Records planned at a visit after the subject's end of participation did not take place;
  # a subject with none left keeps its first, at the end of its window.
  main <- main_date(ds$prefix, names(frame))
  first <- order(frame$USUBJID, as.numeric(frame$VISITNUM %||% seq_len(nrow(frame))))
  first <- first[!duplicated(frame$USUBJID[first])]
  alone <- first[beyond[first] & !stats::ave(!beyond, frame$USUBJID, FUN = any)[first]]
  if (length(alone) && !is.null(frame[[ds$window$end %||% ""]])) {
    frame[[main]][alone] <- frame[[ds$window$end]][alone]
    frame <- redo_rules(ds, frame, main, alone)
    beyond[alone] <- FALSE
  }
  frame <- apply_pins(study, ds, frame[!beyond, , drop = FALSE], built)
  columns <- intersect(c("USUBJID", s$keys, main_date(ds$prefix, names(frame))), names(frame))
  frame <- frame[do.call(order, unname(lapply(frame[columns], function(v) if (is.character(v)) v else as.numeric(v)))), , drop = FALSE]
  ordered <- Filter(function(r) r$op %in% c("sequence", "record_flag"), accepted_rules(ds))
  frame[names(Filter(function(r) r$op == "sequence", ordered))] <- NULL
  for (v in intersect(nodes, descendants(ds, names(ordered)))) if (node_type(ds, v) == "rule")
    frame[[v]] <- RULE_OPS[[ds$rules[[v]]$op]]$apply(ds$rules[[v]], frame)
  for (v in setdiff(ds$variables, names(frame))) frame[[v]] <- switch(ds$kinds[[v]],
    date = as.Date(rep(NA, nrow(frame))), numeric = rep(NA_real_, nrow(frame)), rep(NA_character_, nrow(frame)))
  rownames(frame) <- NULL
  frame[ds$variables]
}

# Pins and milestones put DM's dates into the dataset (see `timeline_edges()`). The record
# nearest the DM date takes it. On the start side, the subject's earlier dates move up to it;
# on the end side, later records are dropped when planned by visit, and otherwise moved back
# to it. A date read from the SV visit moves its record to the subject's visit on that date,
# or else their last visit before it. A collected date takes the subject's SV date nearest
# its drawn value among those its bounds allow (the window, and dates of the record and of
# the subject it keeps an order with), or else their first SV date on or after the lower
# bound. A subject whose first (last) date in a column misses its span takes it on the record
# nearest the subject date: on the SV date nearest it within the record's bounds when the
# date is collected, on the SV visit nearest it when the date is read from the visit, or else
# on the subject date itself; the record's dates that keep an order with it move along, and
# a series recorded once is also recorded at a visit that meets a span its record cannot. Rules calculated from a changed date
# are redone for the changed records.
apply_pins <- function(study, ds, frame, built) {
  dm <- built$DM
  sv <- built[[VISIT_HUB]]
  fits <- study$fits[[ds$name]]$nodes
  sampled <- variables_of_type(ds, "sampled")
  days <- if (!is.null(sv)) split(as.numeric(sv[[main_date(VISIT_HUB, names(sv))]]), sv$USUBJID) else list()
  # A record's bounds on a date: the window, and its dates and the subject dates it keeps an order with.
  bound <- function(names, pick, none) do.call(pick, c(lapply(intersect(names, names(frame)), function(n) as.numeric(frame[[n]])), none, na.rm = TRUE))
  lower <- function(v) bound(c(ds$window$start, fits[[v]]$after, Filter(function(u) v %in% fits[[u]]$before, sampled)), pmax, -Inf)
  upper <- function(v) bound(c(ds$window$end, fits[[v]]$before, Filter(function(u) v %in% fits[[u]]$after, sampled)), pmin, Inf)
  # Record dates keep their order with the series' first and last dates (SERIES.FIRST, .LAST):
  # moving the series date of record r stays on or after the dates bounded by the series'
  # last date when r alone reaches them, and on or before those bounding its first date
  # likewise. Dates on the other side move along (see `carry()`).
  s <- ds$structure
  series_reach <- function(r, x) {
    moves <- !is.null(frame$.series) && !is.null(s$date) && (v == s$date || by_visit && startsWith(s$date, paste0(VISIT_HUB, ".")))
    if (!moves) return(c(-Inf, Inf))
    rows <- which(frame$.series == frame$.series[r])
    own <- if (v == s$date) x else as.numeric(frame[[s$date]])
    rest <- own[setdiff(rows, r)]
    need <- function(side, of, pick) {
      u <- Filter(function(u) side %in% fits[[u]][[of]], sampled)
      z <- unlist(lapply(intersect(u, names(frame)), function(n) as.numeric(frame[[n]])[rows]))
      if (any(!is.na(z))) pick(z, na.rm = TRUE) else NA
    }
    last <- need("SERIES.LAST", "before", max)
    first <- need("SERIES.FIRST", "after", min)
    c(max(c(if (!any(rest >= last, na.rm = TRUE)) last), -Inf, na.rm = TRUE),
      min(c(if (!any(rest <= first, na.rm = TRUE)) first), Inf, na.rm = TRUE))
  }
  # A date moved earlier (later) takes the record's dates that stay on or before (after) it
  # along, on the whole series for a series-level date.
  carry <- function(frame, v, r, first) {
    to <- as.numeric(frame[[v]])
    for (u in intersect(Filter(function(u) v %in% fits[[u]][[if (first) "before" else "after"]], sampled), names(frame))) {
      y <- as.numeric(frame[[u]])
      rr <- r
      if (identical(ds$levels[[u]], "series") && !is.null(frame$.series)) rr <- which(frame$.series %in% frame$.series[r])
      cap <- if (first) min(to[r], na.rm = TRUE) else max(to[r], na.rm = TRUE)
      out <- rr[!is.na(y[rr]) & (if (first) y[rr] > cap else y[rr] < cap)]
      if (!length(out)) next
      frame[[u]][out] <- as.Date(cap, origin = "1970-01-01")
      frame <- redo_rules(ds, frame, u, out)
    }
    frame
  }
  for (v in intersect(intersect(ds$links$collected, sampled), names(frame))) {
    lo <- lower(v)
    hi <- upper(v)
    x <- as.numeric(frame[[v]])
    rows <- which(!is.na(x) & frame$USUBJID %in% names(days))
    x[rows] <- vapply(rows, function(i) {
      d <- days[[frame$USUBJID[i]]]
      d <- d[!is.na(d)]
      ok <- d[d >= lo[i] & d <= hi[i]]
      if (length(ok)) ok[which.min(abs(ok - x[i]))] else if (any(d >= lo[i])) min(d[d >= lo[i]]) else if (length(d)) max(d) else x[i]
    }, 0)
    frame[[v]] <- as.Date(x, origin = "1970-01-01")
    frame <- redo_rules(ds, frame, v, rows)
  }
  spans <- ds$links$spans
  for (i in seq_len(NROW(spans))) {
    v <- spans$column[i]
    by_visit <- v %in% ds$links$on_visit && isTRUE(ds$structure$sv) && "VISITNUM" %in% names(frame)
    if (!v %in% names(frame) || !v %in% sampled && !by_visit) next
    first <- spans$side[i] == "first"
    sign <- if (first) 1 else -1
    at <- as.numeric(subject_date(built, spans$node[i], frame$USUBJID))
    if (!length(at)) next
    x <- as.numeric(frame[[v]])
    # The record's dates it keeps an order with move along (see `carry()`), so only the
    # window and the subject's dates bound it.
    lo <- rep_len(bound(c(ds$window$start, setdiff(fits[[v]]$after, sampled)), pmax, -Inf), nrow(frame))
    hi <- rep_len(bound(c(ds$window$end, setdiff(fits[[v]]$before, sampled)), pmin, Inf), nrow(frame))
    if (by_visit) visits <- allowed_visits(built, ds)
    late <- tapply(sign * (x - at), frame$USUBJID, function(z) all(is.na(z)) || min(z, na.rm = TRUE) > 0)
    for (u in names(late)[late]) {
      rows <- which(frame$USUBJID == u & !is.na(x) & !is.na(at) & (if (first) lo <= at else hi >= at))
      if (!length(rows)) next
      r <- rows[which.min(sign * x[rows])]
      reach <- series_reach(r, x)
      free <- days[[u]]
      free <- free[!is.na(free) & free >= lo[r] & free <= hi[r] & sign * (free - at[r]) <= 0]
      d <- free[free >= reach[1] & free <= reach[2]]
      # A subject's only date is both their first and last: it keeps the other side's span too.
      # In a series, a record that cannot move (it alone holds the series' first or last date,
      # or the subject's other span) stays, and the series is also collected at a visit that
      # meets this span.
      kept <- d
      only <- sum(frame$USUBJID == u & !is.na(x)) == 1
      if (only) for (j in setdiff(which(spans$column == v), i)) {
        keep <- sign * (kept - as.numeric(subject_date(built, spans$node[j], u))) >= 0
        kept <- kept[is.na(keep) | keep]
      }
      add <- identical(ds$structure$type, "series") && by_visit && !length(kept) && length(free)
      d <- if (add) free else kept
      if (by_visit) {
        own <- visits[visits$USUBJID == u & as.numeric(visits$date) %in% d, , drop = FALSE]
        if (!nrow(own)) next
        if (add) {
          frame <- rbind(frame, frame[r, , drop = FALSE])
          x <- c(x, x[r]); at <- c(at, at[r])
          lo <- c(lo, lo[r]); hi <- c(hi, hi[r])
          r <- nrow(frame)
        }
        frame$VISITNUM[r] <- as.numeric(own$visit[which.min(abs(as.numeric(own$date) - at[r]))])
        frame <- redo_rules(ds, join_visits(frame, built), c("VISITNUM", grep(paste0("^", VISIT_HUB, "\\."), names(frame), value = TRUE)), r)
        x <- as.numeric(frame[[v]])
        frame <- carry(frame, v, r, first)
        next
      }
      if (v %in% ds$links$collected) {
        if (!length(d)) next
        x[r] <- d[which.min(abs(d - at[r]))]
      } else if (at[r] >= reach[1] && at[r] <= reach[2]) x[r] <- at[r] else next
      # A date of the series is moved on all its records.
      if (identical(ds$levels[[v]], "series") && !is.null(frame$.series)) {
        x[frame$.series == frame$.series[r]] <- x[r]
        r <- which(frame$.series == frame$.series[r])
      }
      frame[[v]][r] <- as.Date(x[r], origin = "1970-01-01")
      frame <- redo_rules(ds, frame, v, r)
      frame <- carry(frame, v, r, first)
    }
  }
  at <- match(frame$USUBJID, dm$USUBJID)
  planned <- !is.null(ds$structure$visit)
  taken <- rep(FALSE, nrow(frame))
  drop <- rep(FALSE, nrow(frame))
  nearest <- function(rows, value, m) {
    near <- which(rows & !is.na(value))
    near <- near[order(frame$USUBJID[near], abs(value[near] - m[near]))]
    near[!duplicated(frame$USUBJID[near])]
  }
  edges <- timeline_edges(study)
  hub <- paste0(VISIT_HUB, ".", c("SVSTDTC", "SVENDTC"))
  # Fatal outcomes only in subjects with a death date (SDTMIG: --OUT "FATAL" with DM.DTHDTC),
  # when the source keeps them there.
  if (isTRUE(ds$links$fatal >= SHARE_TO_PROPOSE) && !is.null(ds$links$fatal_state))
    frame <- apply_state(ds, frame, ds$links$fatal_state, !is.na(as.numeric(dm$DTHDTC %||% NA)[at]), isTRUE(ds$links$fatal_all))
  for (p in Filter(function(p) p$target %in% names(dm) && p$column %in% names(frame), ds$pins)) {
    column <- p$column
    m <- as.numeric(dm[[p$target]])[at]
    if (!is.null(p$state)) frame <- apply_state(ds, frame, p$state, !is.na(m))
    # The record's visit follows its date when the date is read from the visit, or is the
    # record's main date and bounded below by the visit.
    by_visit <- column %in% ds$links$on_visit || column == main_date(ds$prefix, names(frame)) &&
      any(edges$kind == "bound" & edges$from %in% hub & edges$to == paste0(ds$name, ".", column) & edges$relation == ">=")
    rows <- !is.na(m) & (if (p$where == "-") TRUE else eval(parse(text = p$where), frame) %in% TRUE)
    near <- nearest(rows, as.numeric(frame[[column]]), m)
    if (by_visit && isTRUE(ds$structure$sv) && "VISITNUM" %in% names(frame)) {
      frame$VISITNUM[near] <- visit_on(built, ds, frame$USUBJID[near], m[near], frame$VISITNUM[near])
      frame <- redo_rules(ds, join_visits(frame, built), c("VISITNUM", grep(paste0("^", VISIT_HUB, "\\."), names(frame), value = TRUE)), near)
    }
    frame[[column]][near] <- as.Date(m[near], origin = "1970-01-01")
    taken[near] <- TRUE
    value <- as.numeric(frame[[column]])
    other <- which(rows & frame$USUBJID %in% frame$USUBJID[near] & !seq_along(rows) %in% near & !is.na(value))
    moved <- other[if (p$pick == "first") value[other] < m[other] else value[other] > m[other]]
    if (p$pick == "last" && planned) drop[moved] <- TRUE else frame[[column]][moved] <- as.Date(m[moved], origin = "1970-01-01")
    frame <- redo_rules(ds, frame, column, c(near, moved))
  }
  # Milestones: each DM date falls on its usual visit when the subject has it (without one,
  # for a date that closes the visits it ends, the last of them the subject has), or else on the
  # nearest free visit. When dates follow visit order, the subject's other visits are then
  # rescaled between these fixed dates (and shifted beyond them), so the order holds.
  milestones <- intersect(ds$links$milestones, names(dm))
  if (length(milestones)) {
    column <- main_date(ds$prefix, names(frame))
    value <- as.numeric(frame[[column]])
    monotone <- isTRUE(study$fits[[ds$name]]$nodes[[column]]$monotone)
    o <- order(frame$USUBJID, as.numeric(frame$VISITNUM))
    if (monotone) value[o] <- stats::ave(value[o], frame$USUBJID[o], FUN = function(v) replace(v, !is.na(v), sort(v[!is.na(v)])))
    fixed <- ifelse(taken, value, NA)
    for (target in milestones) {
      m <- as.numeric(dm[[target]])[at]
      on <- as.logical(stats::ave(!is.na(fixed) & !is.na(m) & fixed == m, frame$USUBJID, FUN = any))
      free <- !is.na(m) & !on & is.na(fixed) & !drop & !is.na(value)
      # Only visits that keep the fixed dates in visit order: none earlier fixed later, none later fixed earlier.
      if (monotone) free[o] <- free[o] & !stats::ave(ifelse(!is.na(fixed[o]) & fixed[o] > m[o], 1, 0), frame$USUBJID[o], FUN = cumsum) &
        !rev(stats::ave(rev(ifelse(!is.na(fixed[o]) & fixed[o] < m[o], 1, 0)), rev(frame$USUBJID[o]), FUN = cumsum))
      usual <- free & format_value(frame$VISITNUM) %in% ds$links$milestone_visits[target]
      reach <- study$fits[[ds$name]]$structure$reach
      if (!target %in% names(ds$links$milestone_visits) && paste0("DM.", target) %in% reach$closes) {
        ends <- which(free & reach$bound[format_value(frame$VISITNUM)] %in% paste0("DM.", target))
        ends <- ends[order(frame$USUBJID[ends], -reach$usual[format_value(frame$VISITNUM[ends])])]
        usual <- seq_along(free) %in% ends[!duplicated(frame$USUBJID[ends])]
      }
      near <- c(which(usual), nearest(free & !frame$USUBJID %in% frame$USUBJID[usual], value, m))
      fixed[near] <- m[near]
      # Without such a visit, the subject attended their next planned one on that date.
      on <- as.logical(stats::ave(!is.na(fixed) & !is.na(m) & fixed == m, frame$USUBJID, FUN = any))
      lack <- which(!is.na(m) & !on & !drop)
      lack <- lack[order(frame$USUBJID[lack], -as.numeric(frame$VISITNUM[lack]))]
      lack <- lack[!duplicated(frame$USUBJID[lack])]
      planned <- study$fits[[ds$name]]$structure$visits
      after <- vapply(lack, function(i) c(planned[planned > max(as.numeric(frame$VISITNUM[frame$USUBJID == frame$USUBJID[i]]))], NA)[1], 0)
      lack <- lack[!is.na(after)]
      if (length(lack)) {
        added <- frame[lack, , drop = FALSE]
        added$VISITNUM <- after[!is.na(after)]
        added[[column]] <- as.Date(m[lack], origin = "1970-01-01")
        added <- redo_rules(ds, added, c("VISITNUM", column), seq_len(nrow(added)))
        frame <- rbind(frame, added)
        n <- length(lack)
        value <- c(value, m[lack]); fixed <- c(fixed, m[lack]); drop <- c(drop, rep(FALSE, n)); taken <- c(taken, rep(TRUE, n))
        at <- c(at, at[lack])
        o <- order(frame$USUBJID, as.numeric(frame$VISITNUM))
      }
    }
    if (monotone) for (i in split(o, frame$USUBJID[o])) {
      i <- i[!is.na(value[i])]
      a <- i[!is.na(fixed[i])]
      a <- a[fixed[a] == cummax(fixed[a])]
      if (!length(a)) next
      v <- value[i]
      at_a <- findInterval(v, value[a])
      lo <- pmax(at_a, 1)
      hi <- pmin(at_a + 1, length(a))
      span <- value[a][hi] - value[a][lo]
      value[i] <- ifelse(at_a == 0, v + fixed[a][1] - value[a][1], ifelse(at_a == length(a) | span <= 0,
        v + fixed[a][lo] - value[a][lo], fixed[a][lo] + round((v - value[a][lo]) * (fixed[a][hi] - fixed[a][lo]) / span)))
      value[a] <- fixed[a]
    } else value[!is.na(fixed)] <- fixed[!is.na(fixed)]
    end <- frame[[ds$window$end %||% ""]]
    if (!is.null(end)) value <- ifelse(!is.na(end) & value > as.numeric(end), as.numeric(end), value)
    frame[[column]] <- as.Date(value, origin = "1970-01-01")
    frame <- redo_rules(ds, frame, column, seq_len(nrow(frame)))
  }
  frame[!drop, , drop = FALSE]
}

# A state value only in subjects with the state, set on the sampled state columns so that
# rules carry it: other subjects' records take the state columns of a record drawn from the
# non-state records of the dataset and, when every subject with the state has one, a subject
# with the state and no such record gets one, on a record with the keys the state's source
# records have most often.
apply_state <- function(ds, frame, state, has, all = TRUE) {
  columns <- intersect(intersect(names(state$columns), names(Filter(function(t) t == "sampled", node_types(ds)))), names(frame))
  if (!length(columns)) return(frame)
  held <- Reduce(`|`, lapply(columns, function(v) frame[[v]] %in% state$columns[[v]]))
  wrong <- which(held & !has)
  pool <- which(!held & !is.na(frame[[columns[1]]]))
  if (length(wrong) && length(pool)) {
    donor <- pool[sample.int(length(pool), length(wrong), replace = TRUE)]
    for (v in columns) frame[[v]][wrong] <- frame[[v]][donor]
  }
  rows <- integer()
  if (all) {
    keys <- intersect(intersect(ds$structure$keys, names(state$usual)), names(frame))
    usual <- Reduce(`&`, lapply(keys, function(k) frame[[k]] %in% state$usual[[k]]), rep(TRUE, nrow(frame)))
    held <- Reduce(`|`, lapply(columns, function(v) frame[[v]] %in% state$columns[[v]]))
    missing <- which(has & !stats::ave(held, frame$USUBJID, FUN = any))
    rows <- missing[order(frame$USUBJID[missing], !usual[missing])]
    rows <- rows[!duplicated(frame$USUBJID[rows])]
    for (v in columns) frame[[v]][rows] <- state$columns[[v]][1]
  }
  redo_rules(ds, frame, columns, c(wrong, rows))
}

# Rules calculated, directly or not, from the changed columns are redone for `rows`.
redo_rules <- function(ds, frame, changed, rows) {
  for (v in setdiff(intersect(generation_order(ds), descendants(ds, changed)), changed)) if (node_type(ds, v) == "rule")
    frame[[v]][rows] <- RULE_OPS[[ds$rules[[v]]$op]]$apply(ds$rules[[v]], frame)[rows]
  frame
}

# ---- Saving -------------------------------------------------------------------------

# Everything generation needs, without source records: the graphs and fits, how SUPP--
# variables unfold, and the trial design datasets, which hold no subject data.
study_bundle <- function(study, seed = 1) {
  list(datasets = study$datasets, fits = study$fits, supp = study_supp(study), design = study_design(study),
    seed = seed, created = Sys.time())
}

study_supp <- function(study) study$supp %||% study$source$supp

study_design <- function(study) study$design %||% study$source$design

# The files a submission holds: generated datasets with their SUPP-- datasets unfolded
# after each parent, then the trial design datasets as they are in the source.
output_datasets <- function(study, generated) c(unfold_supp(generated$data, study_supp(study)), study_design(study))

# SDTM files hold ISO 8601 dates and empty text for missing values.
write_outputs <- function(study, generated, directory, formats = c("xpt", "csv")) {
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  files <- character()
  data <- output_datasets(study, generated)
  for (name in names(data)) {
    x <- data[[name]]
    x[] <- lapply(x, function(v) if (is.numeric(v)) v else ifelse(is.na(v), "", format_value(v)))
    for (format in formats) {
      path <- file.path(directory, paste0(tolower(name), ".", format))
      if (format == "xpt") haven::write_xpt(x, path, version = 5, name = name) else utils::write.csv(x, path, row.names = FALSE)
      files <- c(files, path)
    }
  }
  files
}
