# Fitting one sampled node (see models.R for the parts it is made of).

# A node is fitted on one row per subject, per subject and visit, or per record. The model
# type is chosen once per test, so first and later records share a scale: categories for
# text and numbers with few values, otherwise numbers (on their normal scores, with the
# source's percentiles of the test). A trajectory's first record is fitted without the arm
# (it is measured before treatment); later records add the previous recorded value, and a
# number also the mean of the earlier ones (so it returns to the subject's level instead of
# drifting like a random walk). A category also keeps its implications (see
# implications.R), and one value per subject in a test where each subject has one, however
# many records of it they have.
fit_node <- function(ds, variable, frame, arm) {
  m <- node_inputs(ds, variable, frame, arm)
  fits <- lapply(split(which(m$kept), m$parts$key[m$kept]), fit_node_part, m = m)
  implications <- node_implications(ds, variable, m)
  list(parts = fits, arm = m$arm, description = node_description(m, fits, implications), implications = implications,
    constant = if (m$level == "record" && !is.null(m$s$test)) intersect(constant_tests(m$frame, m$s$test, variable), names(m$types)[m$types == "categories"]),
    missing_text = missing_result_text(ds, variable, m))
}

# What a node is fitted on: its rows and values, each record's model part, the type and
# percentiles of each test, and which records are kept (see `rare_categories()`).
node_inputs <- function(ds, variable, frame, arm) {
  node <- ds$nodes[[variable]]
  s <- ds$structure
  level <- if (ds$name == "DM") "record" else ds$levels[[variable]]
  frame <- frame[level_rows(frame, level, s), , drop = FALSE]
  y <- frame[[variable]]
  history <- if (isTRUE(node$prev)) series_history(frame, y, s) else list(previous = rep(NA_real_, nrow(frame)), earlier = rep(NA_real_, nrow(frame)))
  parts <- node_parts(ds, variable, frame, history$previous)
  # Text that reads as a number in every record of a test (VSORRES "58.0") is modelled as
  # that number and written back in the source's format.
  numbers <- parse_number(y)
  by_test <- split(seq_along(y), parts$scale)
  as_number <- vapply(by_test, function(i) is.numeric(y) || all(!is.na(numbers[i][!is.na(y[i])])), TRUE)
  value <- function(i) if (as_number[match(parts$scale[i[1]], names(by_test))]) numbers[i] else y[i]
  tree <- node$model == "tree"
  model <- if (tree) "auto" else node$model
  coarse <- coarse_values(ds, variable, y, frame)
  types <- vapply(by_test, function(i) {
    v <- value(i)
    v <- v[!is.na(v)]
    if (!is.numeric(v) || model == "categories" || (model == "auto" && length(unique(v)) <= MAX_DISCRETE_VALUES) || length(unique(v)) < 2)
      "categories" else "number"
  }, "")
  margins <- lapply(by_test, function(i) { v <- value(i); if (is.numeric(v)) number_margin(v, frame$USUBJID[i]) })
  c(list(node = node, s = s, level = level, frame = frame, y = y, parts = parts, by_test = by_test, value = value,
    arm = c(arm, paste0("DM.", arm)), predictors = intersect(node$predictors, names(frame)), tree = tree, coarse = coarse,
    types = types, margins = margins),
    history, rare_categories(by_test[types == "categories"], y, coarse, frame$USUBJID, tree))
}

# For each row of a series, its previous recorded value, and the mean of its earlier numbers.
series_history <- function(frame, y, s) {
  previous <- earlier <- rep(NA_real_, nrow(frame))
  series <- group_index(frame, c("USUBJID", setdiff(s$keys, s$visit)))
  o <- order(series, as.numeric(frame[[s$visit]]))
  last <- c(0, cummax(ifelse(is.na(y[o]), 0, seq_along(o))))[seq_along(o)]
  follows <- last > 0 & series[o][pmax(last, 1)] == series[o]
  previous[o[follows]] <- y[o][last[follows]]
  x <- parse_number(y[o])
  seen <- !is.na(x)
  sums <- stats::ave(ifelse(seen, x, 0), series[o], FUN = cumsum) - ifelse(seen, x, 0)
  counts <- stats::ave(as.numeric(seen), series[o], FUN = cumsum) - seen
  earlier[o] <- ifelse(counts > 0, sums / counts, NA)
  list(previous = previous, earlier = earlier)
}

# A category value held by fewer than min_subjects() subjects of its test is never drawn:
# its records are left out of the fit (a test with none left stays missing), and the
# implications pool it. Where a lookup derives a coarse value from the node (DSDECOD from
# DSTERM), a rare value whose coarse value is common but has no common value of its own is
# drawn as that coarse value (WITHDRAWAL BY SUBJECT, whose 22 verbatims are each rare). A
# test with too many values to model, drawn within its coarse value, keeps the records of
# every common coarse value in that model; its rare values are not drawn within it.
rare_categories <- function(tests, y, coarse, subject, tree) {
  kept <- rep(TRUE, length(y))
  within <- rep(FALSE, length(y))
  fine <- y
  for (i in tests) {
    held <- is.na(y[i]) | subjects_holding(level_string(y[i]), subject[i]) >= min_subjects()
    if (is.null(coarse) || !is.character(y)) {
      kept[i] <- held
      next
    }
    common <- subjects_holding(coarse[i], subject[i]) >= min_subjects()
    fine[i[!held]] <- ifelse(common[!held] & !coarse[i[!held]] %in% coarse[i[held]], coarse[i[!held]], RARE)
    within[i] <- !tree && length(unique(level_string(y[i]))) > MAX_MODEL_LEVELS
    kept[i] <- if (within[i[1]]) common else !fine[i] %in% RARE
  }
  list(kept = kept, within = within, fine = fine)
}

# The fit of one part: records `i`, all of one test and stage.
fit_node_part <- function(i, m) {
  follow <- grepl("|follow", m$parts$key[i[1]], fixed = TRUE)
  scale <- m$parts$scale[i[1]]
  type <- unname(m$types[match(scale, names(m$types))])
  margin <- if (type == "number" && !m$tree) m$margins[[match(scale, names(m$margins))]]
  records <- m$frame[i, , drop = FALSE]
  previous <- m$previous[i]
  records$.previous <- if (!is.null(margin)) normal_score(margin, parse_number(previous)) else if (type == "number") parse_number(previous) else previous
  records$.earlier <- if (!is.null(margin)) normal_score(margin, m$earlier[i]) else m$earlier[i]
  used <- if (follow) c(m$predictors, ".previous", if (type == "number") ".earlier") else if (isTRUE(m$node$prev)) setdiff(m$predictors, m$arm) else m$predictors
  text <- if (type == "number" && is.character(m$y)) number_format(m$y[i])
  if (m$tree) return(c(fit_tree(if (type == "number") m$value(i) else m$fine[i], records, used, type == "categories"), list(text = text)))
  info <- design_info(records, used, c("VISITNUM", m$s$test))
  if (type == "categories") return(c(fit_part(m$fine[i], design_matrix(info, records), type, design_groups(info), if (m$within[i[1]]) m$coarse[i],
    isTRUE(m$node$vary), names(Filter(function(p) p$type == "categorical", info)), records$USUBJID), list(info = info)))
  v <- m$value(i)
  v <- v[!is.na(v)]
  c(fit_part(normal_score(margin, m$value(i)), design_matrix(info, records), type, design_groups(info)),
    list(info = info, margin = margin, digits = decimals(v), grid = value_grid(v, margin[1]), text = text))
}

# Implications are found within a test: the last visit gives PROTOCOL COMPLETED to the
# disposition event, not to the subject's other DS records.
node_implications <- function(ds, variable, m) {
  keys <- implication_keys(ds, m$frame)
  if (ds$name != "DM" && m$level == "subject") keys <- keys[!record_feature(keys)]
  within <- length(unique(m$parts$scale)) > 1
  do.call(rbind, lapply(m$by_test, function(i) {
    records <- m$frame[i, , drop = FALSE]
    rare <- !m$kept[i] | m$fine[i] %in% RARE
    out <- if (m$types[match(m$parts$scale[i[1]], names(m$types))] == "categories")
      rbind(fit_implications(m$fine[i], records, keys, rare, m$y[i]), derived_implications(ds, variable, m$y[i], records, keys, m$fine[i], rare)) else
      number_implications(m$y[i], records, keys)
    if (!NROW(out)) return(NULL)
    out$within <- if (within) m$s$test else NA_character_
    out$part <- if (within) m$parts$scale[i[1]] else NA_character_
    out
  }))
}

# A number keeps the implications of its missing value (no result where the test was NOT
# DONE). A key level whose records hold no amount, only zero or missing (a placebo arm's
# drug concentrations), keeps none: an amount drawn there becomes the commoner of the two.
number_implications <- function(y, records, keys) {
  out <- fit_implications(ifelse(is.na(y), NA, "recorded"), records, keys)
  if (NROW(out)) out <- out[vapply(out$value, identical, TRUE, MISSING), , drop = FALSE]
  none <- fit_implications(ifelse(no_amount(y), NONE, "amount"), records, keys)
  if (NROW(none)) none <- none[none$kind == "forced" & vapply(none$value, identical, TRUE, NONE) &
    !paste(none$key, none$level) %in% paste(out$key, out$level), , drop = FALSE]
  if (NROW(none)) none$typed <- I(lapply(seq_len(nrow(none)), function(j) {
    at <- level_string(records[[none$key[j]]]) == none$level[[j]]
    if (mean(is.na(y[at])) >= 0.5) NA else y[at & !is.na(y)][1]
  }))
  rbind(out, none)
}

node_description <- function(m, fits, implications) {
  reduced <- sum(vapply(fits, function(p) p$type != "tree" && length(p$columns) < length(design_groups(p$info)), TRUE))
  sprintf("%s%s%s, %d %s%s%s", if (m$tree) sprintf("trees (%s leaves)", paste(range(vapply(fits, `[[`, 0L, "leaves")), collapse = "-"))
    else paste(sort(unique(m$types)), collapse = "/"),
    if (length(unique(m$parts$scale)) > 1) paste(" per", m$s$test) else "",
    if (isTRUE(m$node$prev)) " with previous values" else "", nrow(m$frame), if (m$level == "record") "records" else paste(m$level, "rows"),
    if (reduced) sprintf(", fewer predictors in %d of %d parts (few values)", reduced, length(fits)) else "",
    if (NROW(implications)) sprintf(", %d implication%s", nrow(implications), if (nrow(implications) > 1) "s" else "") else "")
}

# The text a missing --STRESN has in its test (PCSTRESC <BLQ), where every such record has
# the same one and at least min_subjects() subjects hold it.
missing_result_text <- function(ds, variable, m) {
  result <- paste0(ds$prefix, "STRESC")
  if (variable != paste0(ds$prefix, "STRESN") || !result %in% names(m$frame)) return(NULL)
  unlist(lapply(m$by_test, function(i) {
    i <- i[is.na(m$y[i])]
    w <- m$frame[[result]][i]
    if (length(w) && !anyNA(w) && length(unique(w)) == 1 && subjects_holding(w, m$frame$USUBJID[i])[1] >= min_subjects()) w[1]
  }))
}

# The values of the coarsest useful category derived from a node by an accepted lookup on
# the node alone (DSDECOD from 36 verbatim DSTERM terms, a body system from AELLT): the one
# with most values, at most MAX_MODEL_LEVELS. NULL when there is none. The source's values
# are used where it has the column and the table was not edited, as the table leaves out
# rare verbatims.
coarse_values <- function(ds, variable, y, frame) {
  rules <- Filter(function(r) r$op == "lookup" && identical(r$inputs, variable), accepted_rules(ds))
  if (!length(rules)) return(NULL)
  values <- level_string(y)
  first <- frame[!duplicated(values), , drop = FALSE]
  derived <- lapply(rules, function(r) if (!isTRUE(r$params$edited) && r$target %in% names(frame)) level_string(frame[[r$target]]) else
    level_string(RULE_OPS$lookup$apply(r, first))[match(values, level_string(first[[variable]]))])
  n <- vapply(derived, function(d) length(unique(d)), 0L)
  ok <- which(n > 1 & n <= MAX_MODEL_LEVELS)
  if (length(ok)) derived[[ok[which.max(n[ok])]]]
}
