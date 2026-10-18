# The build as reactives. Only the top server (server.R) and this file read the build `b`;
# the page modules get these reactives. `tick()` counts the polls that changed the build.

# Follows `f(b)` and invalidates its readers only when the value changes (see `changes()`).
follow_build <- function(b, tick, f) {
  changes(function() {
    tick()
    f(b)
  })
}

# What the study overview shows.
board_reactives <- function(b, tick) {
  follow <- function(f) follow_build(b, tick, f)
  # Recomputed only when a dataset's build changed.
  keys <- follow(function(b) vapply(b$info %||% list(), `[[`, "", "key"))
  list(
    stages = follow(function(b) pipeline_stages(b, build_status(b))),
    status = follow(build_status),
    parents = follow(function(b) vapply(study_supp(b$study), `[[`, "", "parent")),
    design = follow(function(b) names(study_design(b$study))),
    attention = follow(attention_state),
    schedule = follow(function(b) {
      if (!is.null(b$study$source)) {
        list(schedule = b$study$schedule, design = names(study_design(b$study)))
      }
    }),
    # Read when a download is asked for, not on every poll.
    outputs = reactive({
      tick()
      list(study = b$study, generated = b$generated, seed = b$seed)
    }),
    bundle = reactive({
      tick()
      build_bundle(b)
    }),
    ledger = reactive(built_decisions(b, keys()))
  )
}

# What the open dataset's page shows. `current`: the open dataset.
dataset_reactives <- function(b, tick, current) {
  list(
    study = reactive({
      tick()
      b$study
    }),
    share = reactive({
      tick()
      b$share
    }),
    generated = follow_build(b, tick, function(b) b$generated),
    state = follow_build(b, tick, function(b) dataset_state(b, current()))
  )
}

# The dataset's build and its failed checks.
dataset_state <- function(b, ds) {
  if (is.null(ds)) {
    return(NULL)
  }
  failed <- function(x) {
    if (!is.null(x)) x[x$Dataset == ds$name & x$Status == "failed", ]
  }
  list(
    info = b$info[[ds$name]],
    manual = ds$name %in% b$manual,
    spec = failed(b$checks[[ds$name]]),
    coherence = failed(b$coherence)
  )
}

# The decisions of the datasets built so far.
built_decisions <- function(b, keys) {
  built <- names(keys)[nzchar(keys)]
  x <- build_decisions(b)
  if (is.null(x)) return(NULL)
  unchanged <- attr(x, "unchanged")
  x <- x[x$dataset %in% built, ]
  attr(x, "unchanged") <- sum(unchanged %in% built)
  x
}
