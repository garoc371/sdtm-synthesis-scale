# How a dataset's records are structured, read from the source: the record keys, the planned
# test and visit, the series, variable levels, record ids and the link to an earlier dataset
# (see `new_dataset()` in study.R).

# Category values with the same number of records in every subject of the dataset.
fixed_categories <- function(x, name) {
  subjects <- match(x$USUBJID, unique(x$USUBJID))
  do.call(rbind, lapply(names(x), function(v) {
    values <- unique(x[[v]])
    if (name == "DM" || !is.character(x[[v]]) || anyNA(values) || !length(values) %in% 2:MAX_DEFAULT_LEVELS) return(NULL)
    records <- vapply(values, function(value) {
      n <- tabulate(subjects[x[[v]] == value], max(subjects))
      if (all(n == n[1])) n[1] else NA_integer_
    }, 0L)
    if (any(!is.na(records))) data.frame(variable = v, value = values, records = records)[!is.na(records), ]
  }))
}

# The record structure no longer draws records (the spine gives them); it names the record
# keys, the planned test and visit, and the series, which set the levels values are
# modelled at and per test. Findings (--TESTCD) and datasets with one record per subject and
# visit follow a schedule. Otherwise a category that every subject has the same number of
# records of (one DSCAT "DISPOSITION EVENT" each) is planned like a test. Everything else is
# a series (see `series_structure()`) or a count of records per subject.
record_structure <- function(study, x, name, prefix, fixed) {
  if (name == "DM") return(list(type = "subject", keys = character(), visit = NULL, test = NULL, unique = FALSE))
  visit <- if ("VISITNUM" %in% names(x)) "VISITNUM"
  test <- if (paste0(prefix, "TESTCD") %in% names(x)) paste0(prefix, "TESTCD")
  if (!is.null(test) || (!is.null(visit) && unique_share(x, visit) >= 0.99)) return(schedule_structure(study, x, name, visit, test, fixed))
  if (length(fixed)) return(list(type = "schedule", keys = fixed$variable[1], visit = NULL, test = fixed$variable[1], unique = FALSE))
  series_structure(x, prefix, visit) %||% list(type = "count", keys = character(), visit = NULL, test = NULL, unique = FALSE)
}

# Share of the records that the subject and `keys` tell apart.
unique_share <- function(x, keys) max(group_index(x, c("USUBJID", keys))) / nrow(x)

# A schedule's keys: the visit and test, then the categorical columns that best separate
# repeated records until records are unique.
schedule_structure <- function(study, x, name, visit, test, fixed) {
  variables <- names(x)
  keys <- c(visit, test)
  counts <- vapply(x, function(v) length(unique(v)), 0L)
  extra <- variables[counts >= 2 & counts <= MAX_DEFAULT_LEVELS & !vapply(x, inherits, TRUE, "Date")]
  # Supplemental qualifiers folded in from SUPP-- describe records, they never identify them;
  # --SEQ numbers records, it does not say what they are.
  folded <- unlist(lapply(Filter(function(s) identical(s$parent, name), study$source$supp), `[[`, "columns"))
  extra <- setdiff(extra[!endsWith(extra, "SEQ")], c("USUBJID", keys, folded))
  while (unique_share(x, keys) < 0.99 && length(extra)) {
    shares <- vapply(extra, function(e) unique_share(x, c(keys, e)), 0)
    if (max(shares) <= unique_share(x, keys)) break
    keys <- c(keys, extra[which.max(shares)])
    extra <- setdiff(extra, keys)
  }
  if (unique_share(x, keys) < 0.99) keys <- c(keys, naming_column(x, keys, setdiff(variables, c("USUBJID", keys, folded))))
  keys <- c(keys, determined_keys(x, keys, visit, test, fixed))
  list(type = "schedule", keys = keys, visit = visit, test = test, unique = unique_share(x, keys) == 1)
}

# A text column that, with the keys, identifies the records of a subject (99%) and whose
# values recur across subjects (TULNKID, TRLNKID: T01, NT01, NEW01) names records like a
# test, whatever its number of values. The one with the fewest values.
naming_column <- function(x, keys, columns) {
  names_records <- Filter(function(v) is.character(x[[v]]) && !endsWith(v, "SEQ") && unique_share(x, c(keys, v)) >= 0.99 &&
    max(group_index(x, c("USUBJID", v))) >= 2 * length(unique(x[[v]])), columns)
  if (length(names_records)) names_records[which.min(vapply(x[names_records], function(v) length(unique(v)), 0L))]
}

# A fixed category that the keys (visit aside) determine but the test alone does not (PCSPEC
# by PCSEQ) is a key with them.
determined_keys <- function(x, keys, visit, test, fixed) {
  groups <- function(columns) max(group_index(x, columns))
  combos <- setdiff(keys, visit)
  Filter(function(v) groups(c(combos, v)) == groups(combos) && groups(c(test, v)) > groups(test), setdiff(unique(fixed$variable), keys))
}

# Subject-level variables are constant within subject (every variable, when subjects have one
# record), visit-level within subject and visit, series-level within a series.
constant_within <- function(x, columns) {
  group <- group_index(x, columns)
  repeated <- tabulate(group) > 1
  vapply(x, function(v) {
    distinct <- tabulate(group[!duplicated(group_index(data.frame(group, v), 1:2))], length(repeated))
    any(repeated) && mean(distinct[repeated] == 1) >= 0.99
  }, TRUE)
}

# Series (SDTMIG: --SPID, or the topic variable, links the records of one event or
# medication recorded at several visits): a sponsor identifier or topic variable groups the
# records so that each group has each collection (VISITNUM, else --DTC) once, in at least
# 95% of groups, and at least 10% of groups have several records. Of these the grouping
# with the most variables constant within a group. NULL when there is none.
series_structure <- function(x, prefix, visit) {
  collection <- if (!is.null(visit)) "VISITNUM" else intersect(paste0(prefix, "DTC"), names(x))[1]
  candidates <- if (!is.na(collection)) intersect(paste0(prefix, c("SPID", "TRT", "TERM", "DECOD")), names(x))
  once <- vapply(candidates, function(k) {
    group <- group_index(x, c("USUBJID", k))
    size <- tabulate(group)
    distinct <- tabulate(group[!duplicated(group_index(x, c("USUBJID", k, collection)))], length(size))
    if (mean(distinct == size) >= 0.95 && mean(size > 1) >= 0.1) sum(constant_within(x, c("USUBJID", k))) else NA_real_
  }, 0)
  if (any(!is.na(once))) list(type = "series", keys = collection, visit = NULL, test = NULL, unique = FALSE, series = names(which.max(once)))
}

variable_levels <- function(x, name, structure) {
  levels <- rep("record", ncol(x))
  if (!is.null(structure$visit)) levels[constant_within(x, c("USUBJID", structure$visit))] <- "visit"
  if (!is.null(structure$series)) levels[constant_within(x, c("USUBJID", structure$series))] <- "series"
  levels[name == "DM" | constant_within(x, "USUBJID") | !anyDuplicated(x$USUBJID)] <- "subject"
  setNames(levels, names(x))
}

# A whole number that no subject repeats names the record (--SEQ, also when numbered in
# blocks: QS's 1001, 1002, 3001): it comes with the record. Its values recur across
# subjects and most records are in subjects with several, so it is not unique merely
# because subjects have one record.
record_ids <- function(x, name, structure) {
  subjects <- match(x$USUBJID, unique(x$USUBJID))
  several <- mean(tabulate(subjects)[subjects] > 1) >= 0.5
  if (name != "DM" && several) Filter(function(v) {
    w <- x[[v]]
    is.numeric(w) && !anyNA(w) && all(w == round(w)) && !anyDuplicated(data.frame(subjects, w)) &&
      nrow(x) >= 2 * length(unique(w))
  }, setdiff(names(x), c("USUBJID", structure$keys)))
}

# A record linked to a record of an earlier dataset (TR to its TU lesion) sees that record's
# spine and category columns in words as LINK.<VAR> (LINK.TULOC) when both were collected
# at the same visit (the lesion's measurements where it was identified); later records follow
# their previous values. Link ids and visits are spine columns, and a linked record is kept
# only where the subject has the record it links to (see `compose_records()`).
record_link <- function(study, x, name, prefix, structure) {
  link <- dataset_link(study$source$data, name)
  to <- study$datasets[[link$dataset %||% ""]]
  if (is.null(to) || !link$key %in% structure$spine || !link$by %in% to$structure$spine) return(NULL)
  y <- study$source$data[[link$dataset]]
  columns <- Filter(function(v) is.character(y[[v]]) && length(unique(stats::na.omit(y[[v]]))) %in% 2:MAX_DEFAULT_LEVELS,
    setdiff(c(to$structure$spine, intersect(paste0(to$prefix, c("CAT", "SCAT")), to$variables)), c("USUBJID", link$by)))
  copy <- linked_copies(x, y, link, to, prefix, structure)
  if (length(columns) || length(copy)) c(link, list(columns = columns, visit = intersect("VISITNUM", intersect(structure$spine, to$structure$spine)), copy = copy))
}

# A column the record shares with the record it links to (TRMETHOD and TUMETHOD) is that
# record's value, where every source record has it.
linked_copies <- function(x, y, link, to, prefix, structure) {
  variables <- names(x)
  i <- match(paste(x$USUBJID, x[[link$key]]), paste(y$USUBJID, y[[link$by]]))
  on <- !is.na(i) & !is.na(x[[link$key]])
  shared <- setdiff(variables[startsWith(variables, prefix)], c(structure$spine, link$key))
  copy <- Filter(function(v) {
    from <- y[[paste0(to$prefix, substring(v, 3))]]
    any(on) && !is.null(from) && identical(level_string(x[[v]][on]), level_string(from[i[on]]))
  }, shared)
  setNames(paste0(to$prefix, substring(copy, 3)), copy)
}

initial_nodes <- function(name, variables, structure, link) {
  node <- list(type = NA_character_, predictors = character(), model = default_node_model(), prev = FALSE, vary = FALSE)
  nodes <- setNames(rep(list(node), length(variables)), variables)
  for (v in c(structure$spine, names(link$copy))) nodes[[v]]$type <- "key"
  if ("USUBJID" %in% variables) nodes$USUBJID$type <- if (name == "DM") "identifier" else "key"
  nodes
}
