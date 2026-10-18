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
  out <- do.call(rbind, lapply(keys, function(k) key_implications(frame, k, values, source)))
  if (!is.null(out)) out <- out[!vapply(out$value, identical, TRUE, RARE), , drop = FALSE]
  if (!NROW(out)) return(NULL)
  rownames(out) <- NULL
  out$typed <- I(lapply(out$value, function(v) if (v == MISSING) NA else c(y[values == v], source[level_string(source) == v])[1]))
  out$via <- NA_character_
  out
}

# The implications between key `k` and `values` (see `fit_implications()`): rows of kind
# "forced" (key level gives value) and "only" (value only at key levels).
key_implications <- function(frame, k, values, source) {
  subject <- factor(frame$USUBJID, unique(frame$USUBJID))
  # Per level: the subjects that have it and the mean of their record shares.
  subjects <- function(groups) colSums(table(subject, groups) > 0)
  share <- function(groups) {
    t <- table(subject, groups)
    colMeans(t / rowSums(t))
  }
  key <- level_string(frame[[k]])
  common <- subjects_holding(key, frame$USUBJID) >= min_subjects()
  known <- unique(key[common])
  key[!common] <- RARE
  present <- table(key, values) > 0
  n_key <- subjects(key)[rownames(present)]
  p_key <- share(key)[rownames(present)]
  forced <- which(n_key >= min_subjects() & rowSums(present) == 1)
  with <- apply(present[forced, , drop = FALSE], 1, which)
  forced <- forced[share(values)[colnames(present)][with]^n_key[forced] < IMPLICATION_P]
  # Key levels a value is never at are ruled out when, by chance, some of its subjects would
  # have been there: a level alone, or all of them together (no single level of treatment
  # status or last visit is enough). A set of levels is picked after the fact, so its chance
  # is multiplied by the number of sets of as many levels: many sparse levels (a study day)
  # always leave some empty.
  n <- subjects(values)[colnames(present)]
  absent <- !present
  together <- (1 - colSums(absent * p_key))^n * choose(nrow(present), colSums(absent)) < IMPLICATION_P
  never <- absent & (outer(1 - p_key, n, `^`) < IMPLICATION_P | rep(together, each = nrow(absent)))
  only <- which(n >= min_subjects() & colSums(never) > 0)
  rows <- function(kind, level, value, levels = rep(list(known), length(value)))
    data.frame(kind = kind, key = k, level = I(level), value = I(value), known = I(levels), stringsAsFactors = FALSE)
  out <- list(if (length(forced)) rows("forced", as.list(rownames(present)[forced]), as.list(colnames(present)[with[names(forced)]])),
    if (length(only)) rows("only", lapply(only, function(j) rownames(present)[!never[, j]]), as.list(colnames(present)[only])))
  # A death feature forces its value as the source has it, however rare.
  if (grepl(DEATH_KEY, k) && !"Y" %in% rownames(present)[forced]) {
    died <- level_string(frame[[k]]) == "Y"
    v <- unique(level_string(source)[died])
    if (length(v) == 1 && mean(level_string(source) == v)^length(unique(frame$USUBJID[died])) < IMPLICATION_P)
      out <- c(out, list(rows("forced", list("Y"), list(v), list(union(known, "Y")))))
  }
  do.call(rbind, out)
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
  breaks <- function(i) {
    key <- implication_key(implications, i, keys[[implications$key[i]]])
    within <- implications$within[i]
    rows <- if (is.null(within) || is.na(within)) TRUE else tests[[within]] == implications$part[i]
    held <- key %in% implications$level[[i]]
    rows & if (implications$kind[i] == "forced") held & (if (identical(implications$value[[i]], NONE)) !no_amount(values) else s != implications$value[[i]]) else
      s %in% implications$value[[i]] & !held
  }
  Reduce(`|`, lapply(seq_len(NROW(implications)), breaks), bad)
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
