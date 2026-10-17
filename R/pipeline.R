# The build: every dataset is resolved by one study-level policy, in background workers,
# and cached by content. Nothing waits for a decision; the user inspects exceptions.
#
#   load      the source is read, organised and given a starting specification in a worker
#   build     per dataset: rules found, resolved by the policy, starting graph built,
#             checked and fitted. DM goes first, as later starting graphs use its arm;
#             the others follow in parallel, largest first.
#   generate  once nothing is left to build, the study is sampled and its coherence
#             checked; again after any change.
# The main process only submits jobs and collects results, so the app answers while
# workers compute. A build is an environment changed in place by the functions below;
# `build_step()` is called on a timer (by the app) or in a loop (`run_build()`).

# The policy: found rules that reproduce at least this share of checked records are
# accepted, the others rejected. Discovery proposes nothing below SHARE_TO_PROPOSE, so the
# default accepts every proposal: a rule keeps generated records coherent.
POLICY_SHARE <- 0.9

# The files a worker needs: no Shiny.
CORE_FILES <- c("source.R", "domains.R", "supp.R", "rules.R", "spine.R", "study.R", "models.R", "generate.R", "review.R", "pipeline.R")

resolve_rules <- function(ds, share) {
  for (t in names(ds$rules)) if (identical(ds$rules[[t]]$origin, "found") && !isTRUE(ds$rules[[t]]$manual))
    ds$rules[[t]]$status <- if (isTRUE(ds$rules[[t]]$share >= share)) "accepted" else "rejected"
  ds
}

rule_statuses <- function(ds) vapply(ds$rules, `[[`, "", "status")

# After rule or type changes: inputs of accepted rules are generated, the starting graph is
# rebuilt and must have no cycle. Returns the dataset, or an error condition.
refresh_dataset <- function(study, ds) {
  inputs <- unlist(lapply(accepted_rules(ds), rule_columns))
  for (v in intersect(inputs, variables_of_type(ds, "blank"))) ds$nodes[[v]]$type <- "sampled"
  ds <- starting_graph(study, ds, ds$order)
  order <- tryCatch(generation_order(ds), error = function(e) e)
  if (inherits(order, "error")) order else ds
}

# ---- Workers ----------------------------------------------------------------------------

# Worker processes with the app's functions and one BLAS thread each: a multithreaded BLAS
# in every worker would oversubscribe the cores.
start_workers <- function(n, dir = "R") {
  if (identical(mirai::status()$connections, as.integer(n))) return(invisible(n))
  threads <- Sys.getenv(c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS"), NA)
  Sys.setenv(OPENBLAS_NUM_THREADS = 1, OMP_NUM_THREADS = 1)
  on.exit(for (v in names(threads)) if (is.na(threads[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, as.list(threads[v])))
  mirai::daemons(0)
  mirai::daemons(n)
  invisible(mirai::collect_mirai(mirai::everywhere(for (f in files) sys.source(f, globalenv()),
    files = normalizePath(file.path(dir, CORE_FILES)))))
  invisible(n)
}

content_key <- function(...) digest::digest(list(...), algo = "xxhash64")

code_version <- function(dir = "R") content_key(lapply(file.path(dir, CORE_FILES), readLines), default_node_model(), min_subjects())

cache_file <- function(dir, key) file.path(dir, paste0(key, ".rds"))

# A value computed once per key. Written under a temporary name first, so that a reader
# never sees half a file.
cached <- function(dir, key, compute) {
  if (is.null(dir)) return(compute())
  path <- cache_file(dir, key)
  if (file.exists(path)) return(readRDS(path))
  value <- compute()
  save_cached(dir, key, value)
  value
}

save_cached <- function(dir, key, value) {
  if (is.null(dir)) return(invisible())
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  temporary <- tempfile(tmpdir = dir)
  saveRDS(value, temporary)
  file.rename(temporary, cache_file(dir, key))
}

# ---- Jobs (run in workers) ----------------------------------------------------------------

load_study <- function(files, names = basename(files)) {
  source <- if (length(files) == 1 && dir.exists(files)) read_folder(files) else read_source(files, names)
  source <- organise_source(source)
  calendar <- visit_window(source$data)
  source$data <- shift_dates(source$data)
  study <- new_study(source)
  study$spine <- spine_records(study)
  study$schedule <- visit_schedule(study$source$data, study_design(study), calendar)
  study$hashes <- vapply(study$source$data, digest::digest, "", algo = "xxhash64")
  study$source_checks <- source_checks(study)
  study
}

# One dataset. Without a given graph the dataset is discovered (cached apart, since a
# policy change reuses it) and resolved by the policy; a given graph (edited by hand) is
# only refreshed when the policy changed one of its rules. Unchanged nodes keep their fit.
build_job <- function(job) {
  study <- job$study
  name <- job$name
  ds <- job$ds
  seconds <- c(discover = 0, fit = 0)
  if (is.null(ds)) {
    started <- proc.time()[[3]]
    ds <- cached(job$cache, paste0("discover-", job$discover_key), function() discover_dataset(study, name)$datasets[[name]])
    seconds[["discover"]] <- proc.time()[[3]] - started
    ds <- resolve_rules(ds, job$share)
  }
  if (job$refresh) {
    ds <- refresh_dataset(study, ds)
    if (inherits(ds, "error")) stop(conditionMessage(ds))
  }
  study$datasets[[name]] <- ds
  started <- proc.time()[[3]]
  checks <- check_dataset(study, name)
  fit <- if (!any(checks$Status == "failed")) fit_dataset(study, name, job$previous)
  seconds[["fit"]] <- proc.time()[[3]] - started
  result <- list(ds = ds, fit = fit, checks = checks, seconds = seconds)
  save_cached(job$cache, job$key, result)
  # Also under the key it has once its rules are known (see `dataset_keys()`), which a
  # later policy change or a return to the policy looks up.
  if (is.null(job$ds) && !is.null(job$cache)) {
    known <- cache_file(job$cache, content_key(job$context, rule_statuses(ds)))
    temporary <- tempfile(tmpdir = job$cache)
    if (!file.exists(known) && file.copy(cache_file(job$cache, job$key), temporary)) file.rename(temporary, known)
  }
  result
}

generate_job <- function(bundle, n, seed) {
  started <- proc.time()[[3]]
  generated <- generate_study(bundle, n, seed)
  list(generated = generated, checks = coherence_checks(bundle, generated), seconds = proc.time()[[3]] - started)
}

# ---- The build (main process) -------------------------------------------------------------

new_build <- function(workers = getOption("synth.cores", 2L), cache = getOption("synth.cache"), share = POLICY_SHARE,
                      n = 200, seed = 1, dir = "R") {
  b <- new.env()
  b$workers <- workers
  b$cache <- if (!is.null(cache)) normalizePath(cache, mustWork = FALSE)
  b$share <- share
  b$n <- n
  b$seed <- seed
  b$code <- code_version(dir)
  b$running <- list()
  b$steps <- data.frame(seconds = numeric(), what = character())
  b$submitted <- character()
  b
}

# A new study: everything from an earlier one is dropped (running jobs finish unseen).
start_load <- function(b, files, names = basename(files)) {
  for (v in c("study", "info", "generated", "coherence", "generated_key", "loaded")) b[[v]] <- NULL
  b$manual <- character()
  b$queue <- character()
  b$started <- Sys.time()
  b$generation <- NULL
  b$running <- list(load = list(kind = "load", task = mirai::mirai(load_study(files, names), files = files, names = names),
    submitted = 0))
  invisible(b)
}

elapsed <- function(b) as.numeric(difftime(Sys.time(), b$started, units = "secs"))

# The policy applies to every dataset; datasets whose accepted rules change are rebuilt.
set_policy <- function(b, share) {
  b$share <- share
  invisible(b)
}

# A dataset changed by hand: kept as given from now on (the policy still decides its rules
# not set by hand) and rebuilt alone.
set_dataset <- function(b, ds) {
  b$study$datasets[[ds$name]] <- ds
  b$manual <- union(b$manual, ds$name)
  invisible(b)
}

# Back to the policy: the dataset is discovered again (from the cache) and resolved. Its
# marks are dropped first, so that its key is the policy's and a cached build is found.
reset_dataset <- function(b, name) {
  b$manual <- setdiff(b$manual, name)
  ds <- b$study$datasets[[name]]
  for (t in names(ds$rules)) ds$rules[[t]]$manual <- NULL
  for (v in names(ds$nodes)) ds$nodes[[v]]$manual <- NULL
  b$study$datasets[[name]] <- ds
  invisible(b)
}

# What generation needs: no source records beyond the spine (see `generate_study()`).
build_bundle <- function(b) c(generation_bundle(b), list(seed = b$seed, created = Sys.time()))

set_sample <- function(b, n, seed) {
  b$n <- n
  b$seed <- seed
  invisible(b)
}

# What a worker needs for one dataset: its records, DM's and SV's (the spine features), the
# subjects and categories of other datasets for their record counts and T.<DATASET>.<CATEGORY>,
# the tests and results behind their subject results, the link ids, visits and columns of
# the records its link ids point to, and DM's graph for the arm.
job_study <- function(study, name) {
  keep <- c("DM", VISIT_HUB, name)
  earlier <- earlier_datasets(study, name)
  s <- study[c("source", "datasets")]
  other <- setdiff(names(s$source$data), keep)
  link <- s$datasets[[name]]$link
  s$source$data[other] <- lapply(other, function(d) { x <- s$source$data[[d]]; x[intersect(c("USUBJID", paste0(d, "CAT"), unlist(s$datasets[[d]]$subject_results[c("test", "result")]),
    if (identical(d, link$dataset)) c(link$by, link$visit, link$columns, link$copy)), names(x))] })
  s$datasets <- s$datasets[intersect(names(s$datasets), union(keep, earlier))]
  s$datasets[setdiff(names(s$datasets), keep)] <- lapply(s$datasets[setdiff(names(s$datasets), keep)], function(ds) ds[c("include", "subject_results")])
  s$fits <- list()
  s
}

# A dataset's build depends on its records, DM's, SV's and earlier datasets', what DM offers
# as inputs (the arm and generated variables), which earlier datasets are generated, and
# either what the policy
# decides for its found rules or the graph edited by hand. Before its rules are found, the
# policy's share stands for what it will decide; so a policy change rebuilds only the
# datasets where it changes a decision.
dataset_keys <- function(b, name) {
  study <- b$study
  ds <- study$datasets[[name]]
  dm <- study$datasets$DM
  earlier <- earlier_datasets(study, name)
  context <- list(b$code, study$hashes, if (name != "DM") list(treatment_variable(dm),
    variables_of_type(dm, c("key", "rule", "sampled", "identifier")), earlier[vapply(study$datasets[earlier], `[[`, TRUE, "include")]))
  context <- content_key(context)
  list(discover = content_key(context, "discover"), context = context,
    build = content_key(context, if (name %in% b$manual) resolve_rules(ds, b$share) else
      if (isTRUE(b$info[[name]]$found)) rule_statuses(resolve_rules(ds, b$share)) else b$share))
}

set_info <- function(b, name, ...) b$info[[name]] <- utils::modifyList(b$info[[name]], list(...))

# A result is kept under the key of what it stores: its found rules (or its graph, when
# changed by hand) are known only once it is built.
keep_result <- function(b, name, result, key, cached) {
  b$study$datasets[[name]] <- result$ds
  b$study$fits[[name]] <- result$fit
  b$checks[[name]] <- result$checks
  b$info[[name]]$found <- TRUE
  key <- dataset_keys(b, name)$build
  set_info(b, name, stage = if (is.null(result$fit)) "failed" else "built", key = key, cached = cached,
    discover = result$seconds[["discover"]], fit = result$seconds[["fit"]], finished = elapsed(b), message = "")
}

# Datasets whose build is out of date are taken from the cache or queued. Until DM has been
# built once, only DM is: the others' starting graphs need its arm and variables.
schedule <- function(b) {
  names <- names(b$study$datasets)
  if (!isTRUE(b$info$DM$found)) names <- "DM"
  running <- vapply(b$running, function(r) r$name %||% "", "")
  for (name in names) {
    ds <- b$study$datasets[[name]]
    if (!ds$include) {
      set_info(b, name, stage = "skipped", key = "")
      next
    }
    key <- dataset_keys(b, name)$build
    if (identical(b$info[[name]]$key, key) || name %in% running) next
    if (!is.null(b$cache) && file.exists(cache_file(b$cache, key))) keep_result(b, name, readRDS(cache_file(b$cache, key)), key, TRUE)
    else if (!name %in% b$queue) {
      b$queue <- c(b$queue, name)
      set_info(b, name, stage = "queued")
    }
  }
  # Hand edits first, then the largest datasets, which take longest.
  records <- vapply(b$queue, function(n) b$info[[n]]$records, 0)
  b$queue <- b$queue[order(!b$queue %in% b$manual, -records)]
}

submit <- function(b, name) {
  study <- b$study
  ds <- study$datasets[[name]]
  keys <- dataset_keys(b, name)
  manual <- name %in% b$manual
  given <- if (manual) resolve_rules(ds, b$share)
  job <- list(name = name, study = job_study(study, name), ds = given, previous = study$fits[[name]],
    refresh = !manual || !identical(rule_statuses(given), rule_statuses(ds)), share = b$share,
    key = keys$build, discover_key = keys$discover, context = keys$context, cache = b$cache)
  b$running[[paste0("build:", name)]] <- list(kind = "build", name = name, key = keys$build, submitted = elapsed(b),
    task = mirai::mirai(build_job(job), job = job))
  set_info(b, name, stage = "running", submitted = elapsed(b))
  b$submitted <- c(b$submitted, name)
}

# Generated: built datasets whose checks pass, in study order. DM is required.
generation_key <- function(b) {
  names <- names(Filter(function(i) identical(i$stage, "built"), b$info))
  if (!"DM" %in% names) return(NULL)
  content_key(names, vapply(b$info[names], `[[`, "", "key"), b$n, b$seed)
}

generation_bundle <- function(b) {
  study <- b$study
  names <- names(b$study$datasets)[vapply(names(b$study$datasets), function(n) identical(b$info[[n]]$stage, "built"), TRUE)]
  datasets <- lapply(study$datasets, function(ds) {
    ds$include <- ds$name %in% names
    ds
  })
  list(datasets = datasets, fits = study$fits[names], spine = study$spine, schedule = study$schedule, supp = study_supp(study),
    design = study_design(study), define = study$source$define)
}

idle <- function(b) !length(b$queue) && !any(vapply(b$running, `[[`, "", "kind") %in% c("load", "build"))

# One poll: collect finished jobs, bring the specification up to date, submit what fits on
# the workers, and generate once nothing is left to build. Returns whether anything changed.
build_step <- function(b) {
  started <- proc.time()[[3]]
  changed <- character()
  for (id in names(b$running)) {
    r <- b$running[[id]]
    if (mirai::unresolved(r$task)) next
    b$running[[id]] <- NULL
    value <- r$task$data
    failed <- mirai::is_mirai_error(value) || mirai::is_error_value(value)
    changed <- c(changed, id)
    if (r$kind == "load") {
      if (failed) {
        b$error <- conditionMessage(value)
        next
      }
      b$study <- value
      b$loaded <- elapsed(b)
      b$checks <- list()
      b$info <- lapply(setNames(names(value$datasets), names(value$datasets)), function(n) list(stage = "waiting", key = "",
        cached = FALSE, discover = NA_real_, fit = NA_real_, submitted = NA_real_, finished = NA_real_, message = "",
        records = nrow(value$source$data[[n]])))
    } else if (r$kind == "build" && !is.null(b$info[[r$name]])) {
      # Changed while it ran (by hand or by the policy): it is built again.
      if (!identical(r$key, dataset_keys(b, r$name)$build)) next
      if (failed) set_info(b, r$name, stage = "error", key = r$key, finished = elapsed(b), message = conditionMessage(value))
      else keep_result(b, r$name, value, r$key, FALSE)
    } else if (r$kind == "generate") {
      if (failed) b$generation_error <- conditionMessage(value) else {
        b$generated <- value$generated
        b$coherence <- with_source(value$checks, b$study$source_checks)
        b$generation_error <- NULL
      }
      b$generated_key <- r$key
      b$generation <- list(seconds = if (!failed) value$seconds, finished = elapsed(b))
    }
  }
  if (!is.null(b$study)) {
    schedule(b)
    while (length(b$queue) && sum(vapply(b$running, `[[`, "", "kind") == "build") < b$workers) {
      submit(b, b$queue[1])
      b$queue <- b$queue[-1]
      changed <- c(changed, "submit")
    }
    key <- generation_key(b)
    if (idle(b) && !is.null(key) && !identical(key, b$generated_key) && is.null(b$running$generate)) {
      bundle <- generation_bundle(b)
      b$running$generate <- list(kind = "generate", key = key, submitted = elapsed(b),
        task = mirai::mirai(generate_job(bundle, n, seed), bundle = bundle, n = b$n, seed = b$seed))
      changed <- c(changed, "generate")
    }
  }
  if (length(changed)) b$steps <- rbind(b$steps, data.frame(seconds = proc.time()[[3]] - started, what = paste(unique(changed), collapse = " ")))
  length(changed) > 0
}

build_busy <- function(b) length(b$running) > 0 || length(b$queue) > 0 ||
  (!is.null(b$study) && !is.null(generation_key(b)) && !identical(generation_key(b), b$generated_key))

run_build <- function(b, poll = 0.05) {
  repeat {
    build_step(b)
    if (!build_busy(b)) break
    Sys.sleep(poll)
  }
  invisible(b)
}

# ---- What the board shows -------------------------------------------------------------------

# One row per dataset: where its build stands and what needs a look. A dataset needs
# attention when a check of its graph or of its generated data fails.
build_status <- function(b) {
  if (is.null(b$info)) return(NULL)
  failed <- function(checks, name) if (is.null(checks)) 0L else sum(checks$Dataset == name & checks$Status == "failed")
  rows <- lapply(names(b$info), function(name) {
    i <- b$info[[name]]
    ds <- b$study$datasets[[name]]
    rules <- if (!is.null(ds)) rule_statuses(ds) else character()
    found <- vapply(ds$rules, function(r) identical(r$origin, "found"), TRUE)
    data.frame(dataset = name, class = ds$class, records = i$records, stage = i$stage, cached = i$cached,
      discover = i$discover, fit = i$fit, submitted = i$submitted, finished = i$finished, message = i$message,
      rules = sum(rules == "accepted"), found = sum(rules == "accepted" & found), rejected = sum(rules == "rejected"),
      dropped = sum(node_types(ds) %in% "blank"), manual = name %in% b$manual,
      spec_failed = failed(b$checks[[name]], name), coherence_failed = failed(b$coherence, name),
      stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Every decision the policy and the starting specification made that changes what is
# generated, with how many source records it affects: accepted rules that disagree with
# some records (generation follows the rule), rejected rules, and variables not generated.
# A rule that gives every source record its value, or a column left out that the source
# leaves empty, changes nothing: those are counted (`unchanged`), not listed.
build_decisions <- function(b) {
  rows <- lapply(b$study$datasets, function(ds) {
    rules <- Filter(function(r) r$status %in% c("accepted", "rejected"), ds$rules)
    rule_rows <- lapply(rules, function(r) data.frame(dataset = ds$name, variable = r$target,
      decision = if (r$status == "accepted") "Calculated" else "Rule rejected", detail = rule_label(r),
      share = r$share, records = if (r$status == "accepted") round(r$checked * (1 - r$share)) else r$checked,
      by = if (isTRUE(r$manual)) "hand" else "policy", stringsAsFactors = FALSE))
    x <- b$study$source$data[[ds$name]]
    blank <- setdiff(variables_of_type(ds, c("blank", "identifier")), "USUBJID")
    type_rows <- lapply(blank, function(v) data.frame(dataset = ds$name, variable = v,
      decision = if (node_type(ds, v) == "blank") "Not generated" else "New identifier", detail = summarise_values(x[[v]]),
      share = NA_real_, records = sum(nzchar(format_value(x[[v]])) & !is.na(x[[v]])), by = if (isTRUE(ds$nodes[[v]]$manual)) "hand" else "starting type",
      stringsAsFactors = FALSE))
    do.call(rbind, c(rule_rows, type_rows))
  })
  rows <- do.call(rbind, rows)
  if (is.null(rows)) return(NULL)
  listed <- rows$records > 0 | rows$by == "hand"
  out <- rows[listed, ][order(-rows$records[listed]), ]
  attr(out, "unchanged") <- rows$dataset[!listed]
  out
}
