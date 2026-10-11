# The study specification: one graph per dataset, in the fixed dataset order.
#
# A node is a variable of its dataset. Its type is one of
#   key         subject key, or part of the time spine: copied from the subject's donor
#               (see spine.R): every date, visit, study day, time point and record key
#   rule        calculated by an accepted rule
#   identifier  regenerated (USUBJID, SUBJID, …)
#   sampled     drawn from a model fitted by maximum likelihood
#   blank       not generated
# Cross-dataset inputs are extra nodes: DM variables ("DM.AGE"), record counts of datasets
# generated earlier ("N.AE") and spine features ("T.PHASE"). Arrows never point into
# another dataset.

NODE_TYPES <- c(identifier = "Identifier", sampled = "Sampled", blank = "Not generated")
LEVELS <- c("subject", "visit", "series", "record")
MAX_DEFAULT_LEVELS <- 25

new_study <- function(source) {
  study <- list(source = source, datasets = list(), fits = list())
  for (name in names(source$data)) study$datasets[[name]] <- new_dataset(study, name)
  study
}

# Record structure and variable levels are read from the source. The record structure no
# longer draws records (the spine gives them); it names the record keys, the planned test
# and visit, and the series, which set the levels values are modelled at and per test.
# Findings (--TESTCD) and datasets with one record per subject and visit follow a
# schedule; findings add the categorical columns that best separate repeated records
# until records are unique. Otherwise a category that every subject has the same number
# of records of (one DSCAT "DISPOSITION EVENT" each) is planned like a test. Everything
# else is a count of records per subject. The keys are checked unique in the generated data
# only when they are unique in every source record (a source duplicate is copied with its donor).
new_dataset <- function(study, name) {
  x <- study$source$data[[name]]
  variables <- names(x)
  prefix <- domain_prefix(name, x)
  unique_share <- function(keys) max(group_index(x, c("USUBJID", keys))) / nrow(x)
  visit <- if ("VISITNUM" %in% variables) "VISITNUM"
  test <- if (paste0(prefix, "TESTCD") %in% variables) paste0(prefix, "TESTCD")
  # Category values with the same number of records in every subject of the dataset.
  subjects <- match(x$USUBJID, unique(x$USUBJID))
  fixed <- do.call(rbind, lapply(variables, function(v) {
    values <- unique(x[[v]])
    if (name == "DM" || !is.character(x[[v]]) || anyNA(values) || !length(values) %in% 2:MAX_DEFAULT_LEVELS) return(NULL)
    records <- vapply(values, function(value) {
      n <- tabulate(subjects[x[[v]] == value], max(subjects))
      if (all(n == n[1])) n[1] else NA_integer_
    }, 0L)
    if (any(!is.na(records))) data.frame(variable = v, value = values, records = records)[!is.na(records), ]
  }))
  structure <- list(type = if (name == "DM") "subject" else "count", keys = character(), visit = NULL, test = NULL, unique = FALSE)
  if (name != "DM" && is.null(test) && (is.null(visit) || unique_share(visit) < 0.99) && length(fixed)) {
    structure <- list(type = "schedule", keys = fixed$variable[1], visit = NULL, test = fixed$variable[1], unique = FALSE)
  } else if (name != "DM" && (!is.null(test) || (!is.null(visit) && unique_share(visit) >= 0.99))) {
    keys <- c(visit, test)
    counts <- vapply(x, function(v) length(unique(v)), 0L)
    extra <- variables[counts >= 2 & counts <= MAX_DEFAULT_LEVELS & !vapply(x, inherits, TRUE, "Date")]
    # Supplemental qualifiers folded in from SUPP-- describe records, they never identify them.
    folded <- unlist(lapply(Filter(function(s) identical(s$parent, name), study$source$supp), `[[`, "columns"))
    extra <- setdiff(extra, c("USUBJID", keys, folded))
    while (unique_share(keys) < 0.99 && length(extra)) {
      shares <- vapply(extra, function(e) unique_share(c(keys, e)), 0)
      if (max(shares) <= unique_share(keys)) break
      keys <- c(keys, extra[which.max(shares)])
      extra <- setdiff(extra, keys)
    }
    # A text column that, with the keys, identifies the records of a subject (99%) and whose
    # values recur across subjects (TULNKID, TRLNKID: T01, NT01, NEW01) names records like a
    # test, whatever its number of values. The one with the fewest values.
    if (unique_share(keys) < 0.99) {
      names_records <- Filter(function(v) is.character(x[[v]]) && !endsWith(v, "SEQ") && unique_share(c(keys, v)) >= 0.99 &&
        max(group_index(x, c("USUBJID", v))) >= 2 * length(unique(x[[v]])), setdiff(variables, c("USUBJID", keys, folded)))
      if (length(names_records)) keys <- c(keys, names_records[which.min(vapply(x[names_records], function(v) length(unique(v)), 0L))])
    }
    # A fixed category that the keys (visit aside) determine but the test alone does not (PCSPEC
    # by PCSEQ) is a key with them.
    groups <- function(columns) max(group_index(x, columns))
    combos <- setdiff(keys, visit)
    determined <- Filter(function(v) groups(c(combos, v)) == groups(combos) && groups(c(test, v)) > groups(test),
      setdiff(unique(fixed$variable), keys))
    keys <- c(keys, determined)
    structure <- list(type = "schedule", keys = keys, visit = visit, test = test, unique = unique_share(keys) == 1)
  }
  # Subject-level variables are constant within subject, visit-level within subject and visit,
  # series-level within a series.
  constant_within <- function(columns) {
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
  # with the most variables constant within a group.
  if (identical(structure$type, "count")) {
    collection <- if (!is.null(visit)) "VISITNUM" else intersect(paste0(prefix, "DTC"), variables)[1]
    candidates <- if (!is.na(collection)) intersect(paste0(prefix, c("SPID", "TRT", "TERM", "DECOD")), variables)
    once <- vapply(candidates, function(k) {
      group <- group_index(x, c("USUBJID", k))
      size <- tabulate(group)
      distinct <- tabulate(group[!duplicated(group_index(x, c("USUBJID", k, collection)))], length(size))
      if (mean(distinct == size) >= 0.95 && mean(size > 1) >= 0.1) sum(constant_within(c("USUBJID", k))) else NA_real_
    }, 0)
    if (any(!is.na(once))) structure <- list(type = "series", keys = collection, visit = NULL, test = NULL, unique = FALSE,
      series = names(which.max(once)))
  }
  levels <- rep("record", length(variables))
  if (!is.null(structure$visit)) levels[constant_within(c("USUBJID", structure$visit))] <- "visit"
  if (!is.null(structure$series)) levels[constant_within(c("USUBJID", structure$series))] <- "series"
  levels[name == "DM" | constant_within("USUBJID")] <- "subject"
  kinds <- vapply(x, value_kind, "")
  # A whole number that no subject repeats names the record (--SEQ, also when numbered in
  # blocks: QS's 1001, 1002, 3001): it is copied with the record. Its values recur across
  # subjects and most records are in subjects with several, so it is not unique merely
  # because subjects have one record.
  several <- mean(tabulate(subjects)[subjects] > 1) >= 0.5
  structure$id <- if (name != "DM" && several) Filter(function(v) {
    w <- x[[v]]
    is.numeric(w) && !anyNA(w) && all(w == round(w)) && !anyDuplicated(data.frame(subjects, w)) &&
      nrow(x) >= 2 * length(unique(w))
  }, setdiff(variables, c("USUBJID", structure$keys)))
  # A sponsor identifier of a series numbers the subject's series: it is part of the spine.
  spine_keys <- c(structure$keys, structure$id, if (endsWith(structure$series %||% "", "SPID")) structure$series)
  structure$spine <- spine_columns(name, prefix, variables, kinds, spine_keys)
  node <- list(type = NA_character_, predictors = character(), model = default_node_model(), prev = FALSE, vary = FALSE)
  nodes <- setNames(rep(list(node), length(variables)), variables)
  for (v in structure$spine) nodes[[v]]$type <- "key"
  if ("USUBJID" %in% variables) nodes$USUBJID$type <- if (name == "DM") "identifier" else "key"
  list(name = name, prefix = prefix, class = domain_class(name, x), variables = variables,
    labels = study$source$labels[[name]], structure = structure,
    levels = setNames(levels, variables), kinds = kinds, inputs = character(), rules = list(),
    known_dates = if (name != "DM") known_dates(x, variables[kinds[variables] == "date"]),
    nodes = nodes, order = character(), include = TRUE, fixed = fixed)
}

VISIT_HUB <- "SV"

main_date <- function(prefix, columns) intersect(paste0(prefix, c("STDTC", "DTC")), columns)[1]

# ---- Frames ---------------------------------------------------------------------------

# A dataset's records with every DM variable joined as DM.<VAR>, record counts of earlier
# datasets as N.<DATASET> and the spine features T.* (see spine.R). DM's frame has the
# subject-level spine features. The record structure travels as an attribute.
dataset_frame <- function(study, name, data = study$source$data) {
  x <- data[[name]]
  if (name == "DM") x <- cbind(x, spine_subjects(data, x$USUBJID)) else {
    subjects <- subject_inputs(study, data, name)
    x <- cbind(x, subjects[match(x$USUBJID, subjects$USUBJID), -1, drop = FALSE])
    x <- spine_record_features(x, study$datasets[[name]])
    if (identical(study$datasets[[name]]$structure$type, "series")) x <- series_frame(x, study$datasets[[name]]$structure)
  }
  rownames(x) <- NULL
  attr(x, "structure") <- study$datasets[[name]]$structure
  x
}

# A series' records carry whether it is still recorded at the subject's last collection in
# the dataset (SERIES.ONGOING): a series-level input, as an event still ongoing at the end.
SERIES_INPUTS <- "SERIES.ONGOING"

series_frame <- function(x, s) {
  if (is.null(x$.series)) x$.series <- group_index(x, c("USUBJID", s$series))
  at <- as.numeric(x[[s$keys[1]]])
  pick <- function(group) stats::ave(at, group, FUN = function(v) if (all(is.na(v))) NA else max(v, na.rm = TRUE))
  last <- pick(x$.series)
  x$SERIES.ONGOING <- as.numeric(!is.na(last) & last == pick(x$USUBJID))
  x
}

subject_inputs <- function(study, data, name) {
  dm <- data$DM
  subjects <- data.frame(USUBJID = dm$USUBJID, stringsAsFactors = FALSE)
  for (v in setdiff(names(dm), "USUBJID")) subjects[[paste0("DM.", v)]] <- dm[[v]]
  for (d in intersect(earlier_datasets(study, name), names(data)))
    subjects[[paste0("N.", d)]] <- as.numeric(tabulate(match(data[[d]]$USUBJID, dm$USUBJID), nrow(dm)))
  cbind(subjects, spine_subjects(data, dm$USUBJID))
}

# A dataset not yet in the study (while it is being read) comes after those that are.
earlier_datasets <- function(study, name) {
  datasets <- names(study$datasets)
  setdiff(datasets[seq_len(match(name, datasets, nomatch = length(datasets) + 1) - 1)], "DM")
}

input_choices <- function(study, name) {
  if (name == "DM") return(spine_subject_names(study))
  generated <- variables_of_type(study$datasets$DM, c("key", "rule", "sampled", "identifier"))
  earlier <- earlier_datasets(study, name)
  counts <- earlier[vapply(study$datasets[earlier], `[[`, TRUE, "include")]
  c(paste0("DM.", setdiff(generated, "USUBJID")), if (length(counts)) paste0("N.", counts), spine_feature_names(study, name),
    if (identical(study$datasets[[name]]$structure$type, "series")) SERIES_INPUTS)
}

# Spine features of the record (T.PHASE, T.<D>=<M>, T.<D>.KNOWN), as opposed to the subject's.
record_feature <- function(x) x == "T.PHASE" | startsWith(x, "T.") & (grepl("=", x, fixed = TRUE) | endsWith(x, ".KNOWN"))

# ---- Rules and node types --------------------------------------------------------------

# Rules are found, every other variable gets its suggested type (rule targets too, for
# when their rule is rejected) and the starting graph is built, so a dataset starts out
# complete. Datasets are prepared in order: later starting graphs use DM's arm. The spine
# is copied, so it is never a rule's target.
discover_dataset <- function(study, name) {
  ds <- study$datasets[[name]]
  ds$rules <- discover_rules(discovery_frame(study, name), setdiff(ds$variables, c("USUBJID", ds$structure$spine)), ds$prefix)
  x <- study$source$data[[name]]
  for (v in ds$variables[is.na(vapply(ds$nodes, `[[`, "", "type"))]) ds$nodes[[v]]$type <- suggest_type(ds, x, v)
  study$datasets[[name]] <- starting_graph(study, ds)
  study
}

# Rules are searched among the dataset's own variables, DM's and the spine features of the
# subject and record phase. Record counts of other datasets (N.*) would add one candidate
# key per earlier dataset, so the search would grow with the square of the number of
# datasets; so would the date indicators T.<D>=<M>.
discovery_frame <- function(study, name) {
  frame <- dataset_frame(study, name)
  structure <- attr(frame, "structure")
  frame <- frame[!grepl("^N\\.|^SERIES\\.|^\\.series$|^T\\..*=", names(frame))]
  attr(frame, "structure") <- structure
  frame
}
node_type <- function(ds, variable) {
  rule <- ds$rules[[variable]]
  if (!is.null(rule) && rule$status == "accepted") return("rule")
  ds$nodes[[variable]]$type %||% NA_character_
}

node_types <- function(ds) vapply(ds$variables, node_type, "", ds = ds)

variables_of_type <- function(ds, types) ds$variables[node_types(ds) %in% types]

accepted_rules <- function(ds) Filter(function(r) r$status == "accepted", ds$rules)

remaining_variables <- function(ds) ds$variables[!node_types(ds) %in% c("key", "rule")]

generated_variables <- function(ds) variables_of_type(ds, c("key", "rule", "sampled", "identifier"))

# Starting types, which the user can change. Text that is (nearly) unique per record is an
# identifier; free text is not generated unless an accepted rule needs it.
suggest_type <- function(ds, x, variable) {
  v <- x[[variable]]
  observed <- v[!is.na(v)]
  n_values <- length(unique(observed))
  if (variable %in% unlist(lapply(accepted_rules(ds), rule_columns))) return("sampled")
  if (!length(observed)) return("blank")
  if (is.character(v) && n_values >= 10 && n_values >= 0.99 * length(observed)) return("identifier")
  if (is.character(v) && n_values > 50 && n_values > 0.3 * length(observed)) return("blank")
  "sampled"
}

arm_variable <- function(dm) {
  arms <- intersect(c("ARMCD", "ARM"), dm$variables)
  arms[arms %in% variables_of_type(dm, c("key", "sampled"))][1]
}

# Records follow the treatment actually received when DM generates it.
treatment_variable <- function(dm) {
  actual <- intersect(c("ACTARMCD", "ACTARM"), variables_of_type(dm, c("key", "sampled", "rule")))
  if (length(actual)) actual[1] else arm_variable(dm)
}

# Nodes a node's value depends on. Inputs (DM.*, N.*, T.*) and keys have no parents here.
node_parents <- function(ds, variable) switch(node_type(ds, variable), sampled = ds$nodes[[variable]]$predictors,
  rule = rule_columns(ds$rules[[variable]]), character())

# `start` and every node reachable from it by repeatedly following `step`.
reachable <- function(start, step) {
  found <- start
  frontier <- start
  while (length(frontier)) {
    frontier <- setdiff(step(frontier), found)
    found <- union(found, frontier)
  }
  found
}

# `nodes` and every variable calculated or sampled from them.
descendants <- function(ds, nodes) reachable(nodes, function(n) Filter(function(v) any(n %in% node_parents(ds, v)), ds$variables))

generation_order <- function(ds) {
  nodes <- generated_variables(ds)
  parents <- lapply(setNames(nodes, nodes), function(v) intersect(node_parents(ds, v), nodes))
  order <- character()
  while (length(order) < length(nodes)) {
    ready <- setdiff(nodes[vapply(parents, function(p) all(p %in% order), TRUE)], order)
    if (!length(ready)) stop("Cycle among ", paste(setdiff(nodes, order), collapse = ", "))
    # Rules first: they are often calculated from the spine alone.
    order <- c(order, ready[order(node_types(ds)[ready] != "rule")])
  }
  order
}

# ---- Starting graph ---------------------------------------------------------------------

# The ordering is the given one, completed by the arm, then the remaining sampled
# variables in column order. Synthpop-style: each sampled node uses the dataset's inputs
# and every earlier node in the ordering at its level or coarser that carries information:
# numbers, or codes with few levels. DM's inputs are the subject's spine features, so the
# arm and characteristics follow the timing the subject copied (a screen failure is never
# treated); the arm is not a predictor of baseline values. Other datasets' inputs are the
# treatment, the subject's death and last visit, the record's phase and which of its dates
# it has. A record-level node also uses the visit the record was copied at (a DS term at
# the retrieval visit).
starting_graph <- function(study, ds, order = character()) {
  is_dm <- ds$name == "DM"
  arm <- if (is_dm) arm_variable(ds) else treatment_variable(study$datasets$DM)
  sampled <- variables_of_type(ds, "sampled")
  order <- unique(c(intersect(order, sampled), sampled))
  choices <- input_choices(study, ds$name)
  if (!length(ds$inputs)) ds$inputs <- intersect(if (is_dm) SPINE_SUBJECT_FEATURES else
    c(paste0("DM.", arm), "T.DIED", "T.LASTVISIT", "T.PHASE", known_date_names(ds), SERIES_INPUTS), choices)
  frame <- dataset_frame(study, ds$name)
  kinds <- vapply(frame, value_kind, "")
  few <- vapply(frame, function(v) length(unique(v)), 0L) <= MAX_DEFAULT_LEVELS
  informative <- names(frame)[kinds == "numeric" | (kinds != "date" & few)]
  s <- ds$structure
  for (i in seq_along(order)) {
    v <- order[i]
    earlier <- order[seq_len(i - 1)]
    level <- match(ds$levels[[v]], LEVELS)
    usable <- earlier[match(ds$levels[earlier], LEVELS) <= level & earlier %in% informative]
    if (is_dm) usable <- setdiff(usable, arm)
    inputs <- intersect(ds$inputs, informative)
    if (!is_dm && level == 1) inputs <- inputs[!record_feature(inputs)]
    ds$nodes[[v]]$predictors <- unique(c(inputs, usable, if (level >= 2) s$visit, if (level == 4) intersect("VISITNUM", s$spine)))
    ds$nodes[[v]]$prev <- !is.null(s$visit) && !is.null(s$test) && ds$levels[[v]] == "record"
  }
  ds$order <- order
  ds
}

# ---- Checks ----------------------------------------------------------------------------

# One row of a check table, for specification and coherence checks alike: a check
# passes when it finds no problems.
check_row <- function(dataset, check, problems = character()) {
  shown <- paste(utils::head(problems, 5), collapse = "; ")
  if (length(problems) > 5) shown <- sprintf("%s (+%d more)", shown, length(problems) - 5)
  data.frame(Dataset = dataset, Check = check, Status = if (length(problems)) "failed" else "ok",
    Problems = shown, stringsAsFactors = FALSE)
}

check_study <- function(study) {
  included <- names(study$datasets)[vapply(study$datasets, `[[`, TRUE, "include")]
  rows <- list(check_row("Study", "DM is generated first and provides every subject", if (!"DM" %in% included) "DM is excluded"))
  do.call(rbind, c(rows, lapply(included, check_dataset, study = study)))
}

# One dataset's checks. They read only the dataset, DM's graph and which earlier datasets
# are generated, so a dataset is checked where it is built.
check_dataset <- function(study, name) {
  ds <- study$datasets[[name]]
  inputs <- input_choices(study, name)
  available <- c(generated_variables(ds), inputs)
  sampled <- variables_of_type(ds, "sampled")
  unavailable <- lapply(ds$variables, function(v) setdiff(node_parents(ds, v), available))
  finer <- lapply(sampled, function(v) {
    p <- intersect(ds$nodes[[v]]$predictors, ds$variables)
    p[match(ds$levels[p], LEVELS) > match(ds$levels[[v]], LEVELS)]
  })
  rbind(
    if (name == "DM") check_row("DM", "A treatment arm variable (ARMCD or ARM) is in the spine or sampled",
      if (is.na(arm_variable(ds))) "no arm"),
    check_row(name, "Every variable has a type", ds$variables[is.na(node_types(ds))]),
    check_row(name, "Every spine column is copied from the donor", setdiff(ds$structure$spine, variables_of_type(ds, "key"))),
    check_row(name, "Every rule input and predictor is generated before use",
      paste(ds$variables, "needs", vapply(unavailable, paste, "", collapse = ", "))[lengths(unavailable) > 0]),
    check_row(name, "The graph has no cycles", tryCatch({generation_order(ds); character()}, error = conditionMessage)),
    check_row(name, "Predictors are at the node's level or coarser",
      sprintf("%s (%s level) uses %s", sampled, ds$levels[sampled], vapply(finer, paste, "", collapse = ", "))[lengths(finer) > 0]))
}

# ---- Formulas -------------------------------------------------------------------------

node_formula <- function(ds, variable, fit = NULL) {
  node <- ds$nodes[[variable]]
  predictors <- c(display_name(node$predictors), if (isTRUE(node$prev)) "previous values")
  by <- if (!is.null(ds$structure$test) && ds$levels[[variable]] == "record") paste(" | per", ds$structure$test)
  implications <- if (!is.null(fit$implications)) vapply(seq_len(nrow(fit$implications)), function(i) implication_label(fit$implications[i, ]), "")
  paste0(switch(node_type(ds, variable),
    key = if (variable == "USUBJID" && ds$name != "DM") "subject from DM" else "copied from the donor's spine",
    rule = rule_label(ds$rules[[variable]]),
    identifier = "new identifier",
    blank = "not generated",
    sampled = paste0(variable, " ~ ", if (length(predictors)) paste(predictors, collapse = " + ") else "1", by,
      if (!is.null(fit)) paste0(" · ", fit$description)),
    "not set"), if (length(implications)) paste0(" · ", utils::head(implications, 3), collapse = ""),
    if (length(implications) > 3) sprintf(" (+%d more)", length(implications) - 3))
}

record_structure_label <- function(ds) {
  s <- ds$structure
  paste0(switch(s$type,
    subject = "one record per subject",
    schedule = paste0("planned ", paste(s$keys, collapse = " × ")),
    series = paste0("series of ", s$series, " per subject, by ", s$keys),
    count = "records per subject"), "; records and timing copied from the donor's spine")
}
