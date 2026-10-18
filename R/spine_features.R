# Subject-level spine features for `ids`, from DM's dates, SV's visits and which datasets
# have their records.
spine_subjects <- function(data, ids) {
  dm <- data$DM[match(ids, data$DM$USUBJID), , drop = FALSE]
  date <- function(columns) {
    v <- intersect(columns, date_columns(dm))[1]
    if (is.na(v)) rep(NA_real_, length(ids)) else as.numeric(dm[[v]])
  }
  start <- date(c("RFXSTDTC", "RFSTDTC"))
  end <- date(c("RFXENDTC", "RFENDTC"))
  out <- data.frame(T.TREATED = ifelse(is.na(start), "N", ifelse(is.na(end), "STARTED", "Y")), T.DIED = ifelse(is.na(date("DTHDTC")), "N", "Y"),
    T.EXPOSED = end - start + 1, stringsAsFactors = FALSE)
  sv <- data[[VISIT_HUB]]
  last <- if (!is.null(sv$VISITNUM)) {
    sv <- sv[as.numeric(sv$VISITNUM) %in% progress_visits(sv), , drop = FALSE]
    tapply(as.numeric(sv$VISITNUM), sv$USUBJID, max, na.rm = TRUE)
  }
  out$T.LASTVISIT <- if (is.null(last)) NA_character_ else format_value(unname(last[ids]))
  others <- setdiff(names(data), "DM")
  out[sprintf("T.%s", others)] <- lapply(others, function(d) ifelse(ids %in% data[[d]]$USUBJID, "Y", "N"))
  stored <- startsWith(names(data$DM), "T.")
  categories <- if (any(stored)) data$DM[match(ids, data$DM$USUBJID), stored, drop = FALSE] else spine_categories(data, ids)
  if (length(categories)) out <- cbind(out, categories)
  out
}

# The T.<DATASET>.<CATEGORY> features. A category is not always a spine column (QSCAT follows
# from the test), so the spine keeps them with DM's records (see `spine_records()`).
spine_categories <- function(data, ids) {
  out <- lapply(setdiff(names(data), "DM"), function(d) {
    x <- data[[d]]
    category <- x[[paste0(d, "CAT")]]
    if (is.null(category)) return(list())
    levels <- sort(unique(category[!is.na(category)]))
    has <- lapply(stats::setNames(levels, sprintf("T.%s.%s", d, levels)), function(v) ids %in% x$USUBJID[category %in% v])
    # Only categories some subjects with the dataset's records lack.
    lapply(Filter(function(y) any(y != ids %in% x$USUBJID), has), ifelse, "Y", "N")
  })
  data.frame(unlist(out, recursive = FALSE), check.names = FALSE, stringsAsFactors = FALSE)
}

spine_subject_names <- function(study) c(SPINE_SUBJECT_FEATURES, paste0("T.", setdiff(names(study$source$data), "DM")),
  names(spine_categories(study$source$data, study$source$data$DM$USUBJID)))

# Record-level spine features. `x` holds the records with DM's variables joined as DM.*.
spine_record_features <- function(x, ds) {
  dates <- ds$variables[ds$kinds[ds$variables] == "date"]
  main <- main_date(ds$prefix, dates)
  d <- if (is.na(main)) rep(NA_real_, nrow(x)) else as.numeric(x[[main]])
  start <- as.numeric(x[["DM.RFSTDTC"]] %||% rep(NA, nrow(x)))
  end <- as.numeric(x[["DM.RFENDTC"]] %||% rep(NA, nrow(x)))
  x$T.PHASE <- ifelse(is.na(d) | is.na(start), "UNKNOWN", ifelse(d < start, "BEFORE", ifelse(is.na(end) | d <= end, "DURING", "AFTER")))
  for (v in ds$known_dates) x[[paste0("T.", v, ".KNOWN")]] <- ifelse(is.na(x[[v]]), "N", "Y")
  pairs <- expand.grid(v = dates, m = spine_dm_dates(x), stringsAsFactors = FALSE)
  same <- Map(function(v, m) as.numeric(x[[v]]) == as.numeric(x[[paste0("DM.", m)]]), pairs$v, pairs$m)
  x[sprintf("T.%s=%s", pairs$v, pairs$m)] <- lapply(same, function(s) ifelse(!is.na(s) & s, "Y", "N"))
  x
}

spine_dm_dates <- function(x) sub("^DM\\.", "", grep("^DM\\.", date_columns(x), value = TRUE))

# The names of the spine features a dataset's frame has (see `dataset_frame()`).
spine_feature_names <- function(study, name) {
  if (name == "DM") return(spine_subject_names(study))
  ds <- study$datasets[[name]]
  dm <- study$datasets$DM
  dates <- ds$variables[ds$kinds[ds$variables] == "date"]
  dm_dates <- dm$variables[dm$kinds[dm$variables] == "date"]
  c(spine_subject_names(study), "T.PHASE", known_date_names(ds), if (length(dates) && length(dm_dates))
    as.vector(outer(dates, dm_dates, function(v, m) paste0("T.", v, "=", m))))
}

# Dates that some records of the dataset have and others lack, set when the dataset is read.
known_dates <- function(x, dates) dates[vapply(x[dates], function(v) anyNA(v) && !all(is.na(v)), TRUE)]
known_date_names <- function(ds) if (length(ds$known_dates)) paste0("T.", ds$known_dates, ".KNOWN")

