# Reading SDTM sources, and the value conventions shared by rules, models and generation.
#
# Internally every dataset is a data.frame in which blanks are NA, dates are Date objects
# and everything else keeps its source type.

MISSING <- "(missing)"

read_folder <- function(directory) {
  files <- list.files(directory, "\\.(xpt|sas7bdat|csv|xml)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No SDTM files found in ", directory, ".")
  read_source(files)
}

# Labels come from the files, or from define.xml item definitions (OIDs of the form
# IT.<DATASET>.<VARIABLE>) when one is given, as are its domain keys and code lists for the
# SDTM rules (`read_define()`). Partial ISO dates are imputed (`impute_dates()`).
read_source <- function(files, names = basename(files)) {
  ext <- tolower(tools::file_ext(names))
  tables <- which(ext %in% c("xpt", "sas7bdat", "csv"))
  if (!length(tables)) stop("Provide .xpt, .sas7bdat or .csv files.")
  data <- lapply(tables, function(i) {
    x <- as.data.frame(switch(ext[i], xpt = haven::read_xpt(files[i]), sas7bdat = haven::read_sas(files[i]),
      csv = utils::read.csv(files[i], stringsAsFactors = FALSE, na.strings = "", check.names = FALSE)))
    setNames(x, toupper(names(x)))
  })
  names(data) <- toupper(tools::file_path_sans_ext(names[tables]))
  if (!"DM" %in% names(data)) stop("DM is required: every other dataset links to it through USUBJID.")
  labels <- lapply(data, function(x) vapply(x, function(v) attr(v, "label") %||% "", ""))
  define <- files[ext == "xml"][1]
  if (!is.na(define)) {
    items <- xml2::xml_find_all(xml2::xml_ns_strip(xml2::read_xml(define)), "//ItemDef")
    oid <- strsplit(xml2::xml_attr(items, "OID"), ".", fixed = TRUE)
    text <- xml2::xml_text(xml2::xml_find_first(items, ".//TranslatedText"))
    for (i in which(lengths(oid) == 3 & nzchar(text) & !is.na(text))) {
      if (oid[[i]][3] %in% names(labels[[oid[[i]][2]]])) labels[[oid[[i]][2]]][[oid[[i]][3]]] <- text[i]
    }
  }
  data <- lapply(data, function(x) {
    x[] <- lapply(names(x), function(name) {
      v <- x[[name]]
      attributes(v) <- NULL
      # SAS transport files are often Latin-1.
      if (is.character(v)) v[!validUTF8(v)] <- iconv(v[!validUTF8(v)], "latin1", "UTF-8")
      if (is.character(v)) v[grepl("^[ \t\r\n]*$", v)] <- NA
      v
    })
    x
  })
  list(data = impute_dates(data), labels = labels, define = if (!is.na(define)) read_define(define))
}

# ---- Dates ------------------------------------------------------------------------------

# Every --DTC column becomes a Date. A partial date is imputed: a missing day is the 15th and
# a missing month July (2013-04 gives 2013-04-15, 2013 gives 2013-07-15); without a year the
# date stays missing. The time of day is dropped. An imputed date then moves within its stated
# period, as little as needed, so that the date orders of the source hold for it too (see
# `date_orders()`): a medication's start stays on or before its end, a history date before
# the first dose, and a visit's date between the dates of the visits before and after it.
impute_dates <- function(data) {
  partial <- list()
  for (d in names(data)) for (v in grep("DTC$|DTM$", names(data[[d]]), value = TRUE)) {
    text <- substr(as.character(data[[d]][[v]]), 1, 10)
    month <- grepl("^\\d{4}-\\d{2}$", text)
    year <- grepl("^\\d{4}$", text)
    data[[d]][[v]] <- as.Date(ifelse(month, paste0(text, "-15"), ifelse(year, paste0(text, "-07-15"), text)), format = "%Y-%m-%d")
    if (!any(month | year)) next
    rows <- which(month | year)
    lo <- as.Date(ifelse(month, paste0(text, "-01"), paste0(text, "-01-01"))[rows], format = "%Y-%m-%d")
    hi <- as.Date(ifelse(month[rows], format(lo + 31, "%Y-%m-01"), format(lo + 366, "%Y-01-01")), format = "%Y-%m-%d") - 1
    partial[[d]][[v]] <- data.frame(row = rows, lo = lo, hi = hi)
  }
  if (!length(partial)) return(data)
  orders <- date_orders(data, partial)
  # Bounds may come from other imputed dates, so a few passes settle them.
  for (pass in 1:3) for (d in names(partial)) for (v in names(partial[[d]])) {
    p <- partial[[d]][[v]]
    bounds <- date_bounds(data, d, v, orders[orders$dataset == d & (orders$a == v | orders$b == v), , drop = FALSE])
    # A bound outside the stated period (an event recorded as starting in 2003, years before
    # the study) is an exception of the source that the date keeps.
    lower <- bounds$lower[p$row]
    upper <- bounds$upper[p$row]
    lower[lower > as.numeric(p$hi)] <- NA
    upper[upper < as.numeric(p$lo)] <- NA
    x <- as.numeric(data[[d]][[v]][p$row])
    data[[d]][[v]][p$row] <- as.Date(pmin(pmax(x, lower, na.rm = TRUE), upper, na.rm = TRUE))
  }
  data
}

DATE_ORDER_SHARE <- 0.99
DATE_ORDER_RECORDS <- 10

# Orders a <= b that hold on at least 99% of the source's complete dates (at least 10 records),
# for the columns that have imputed dates: two dates of a record ("record"), a record's date
# and its subject's DM date ("DM.<column>" as a or b), and a date over the visits of a subject
# ("visit": the date of a later VISITNUM is not earlier).
date_orders <- function(data, partial) {
  rows <- list()
  complete <- function(d, v) {
    ok <- !is.na(data[[d]][[v]])
    ok[partial[[d]][[v]]$row] <- FALSE
    ok
  }
  holds <- function(a, b, ok) sum(ok) >= DATE_ORDER_RECORDS && mean(a[ok] <= b[ok]) >= DATE_ORDER_SHARE
  for (d in names(partial)) {
    x <- data[[d]]
    dates <- names(Filter(function(v) inherits(v, "Date"), x))
    dm <- if (d != "DM") data$DM[match(x$USUBJID, data$DM$USUBJID), , drop = FALSE]
    dm_dates <- if (d != "DM") names(Filter(function(v) inherits(v, "Date"), dm))
    for (v in names(partial[[d]])) {
      for (w in setdiff(dates, v)) {
        ok <- complete(d, v) & complete(d, w)
        if (holds(x[[w]], x[[v]], ok)) rows[[length(rows) + 1]] <- data.frame(dataset = d, a = w, b = v, kind = "record")
        if (holds(x[[v]], x[[w]], ok)) rows[[length(rows) + 1]] <- data.frame(dataset = d, a = v, b = w, kind = "record")
      }
      for (w in dm_dates) {
        ok <- complete(d, v) & !is.na(dm[[w]])
        if (holds(dm[[w]], x[[v]], ok)) rows[[length(rows) + 1]] <- data.frame(dataset = d, a = paste0("DM.", w), b = v, kind = "record")
        if (holds(x[[v]], dm[[w]], ok)) rows[[length(rows) + 1]] <- data.frame(dataset = d, a = v, b = paste0("DM.", w), kind = "record")
      }
      if (!is.null(x$VISITNUM)) {
        ok <- complete(d, v) & !is.na(x$VISITNUM)
        bounds <- visit_bounds(x$USUBJID[ok], x$VISITNUM[ok], as.numeric(x[[v]][ok]))
        later <- !is.na(bounds$lower)
        if (sum(later) >= DATE_ORDER_RECORDS && mean(bounds$lower[later] <= as.numeric(x[[v]][ok][later])) >= DATE_ORDER_SHARE)
          rows[[length(rows) + 1]] <- data.frame(dataset = d, a = v, b = v, kind = "visit")
      }
    }
  }
  do.call(rbind, rows) %||% data.frame(dataset = character(), a = character(), b = character(), kind = character())
}

# Each record's latest date at an earlier visit of its subject and earliest date at a later one.
visit_bounds <- function(subject, visit, date) {
  o <- order(subject, visit)
  s <- subject[o]
  k <- visit[o]
  x <- date[o]
  group <- cumsum(!duplicated(data.frame(s, k)))
  first <- !duplicated(s)
  run <- function(values, f, reverse = FALSE) {
    # Per visit group: f over the group, then a running f over the subject's earlier (later) groups.
    g <- tapply(values, group, f, na.rm = TRUE)
    g[!is.finite(g)] <- NA
    gs <- s[!duplicated(group)]
    out <- if (reverse) rev(stats::ave(rev(g), rev(gs), FUN = function(z) c(NA, cumf(z, f)[-length(z)])))
      else stats::ave(g, gs, FUN = function(z) c(NA, cumf(z, f)[-length(z)]))
    out[group]
  }
  lower <- upper <- numeric(length(x))
  lower[o] <- run(x, max)
  upper[o] <- run(x, min, reverse = TRUE)
  list(lower = lower, upper = upper)
}

cumf <- function(z, f) Reduce(function(a, b) if (is.na(a)) b else if (is.na(b)) a else f(a, b), z, accumulate = TRUE)

# Lower and upper bounds of column v of dataset d on every record, from its orders.
date_bounds <- function(data, d, v, orders) {
  x <- data[[d]]
  n <- nrow(x)
  lower <- upper <- rep(NA_real_, n)
  value <- function(column) {
    if (startsWith(column, "DM.")) as.numeric(data$DM[[substring(column, 4)]][match(x$USUBJID, data$DM$USUBJID)])
    else as.numeric(x[[column]])
  }
  for (i in seq_len(nrow(orders))) {
    if (orders$kind[i] == "visit") {
      b <- visit_bounds(x$USUBJID, x$VISITNUM, as.numeric(x[[v]]))
      lower <- pmax(lower, b$lower, na.rm = TRUE)
      upper <- pmin(upper, b$upper, na.rm = TRUE)
    } else if (orders$b[i] == v) lower <- pmax(lower, value(orders$a[i]), na.rm = TRUE)
    else upper <- pmin(upper, value(orders$b[i]), na.rm = TRUE)
  }
  list(lower = lower, upper = upper)
}

as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- substr(as.character(x), 1, 10)
  x[!grepl("^\\d{4}-\\d{2}-\\d{2}$", x)] <- NA
  as.Date(x)
}

# Variable kinds drive model choice: dates, numbers, or categories (everything else).
value_kind <- function(v) {
  if (inherits(v, "Date")) "date" else if (is.numeric(v)) "numeric" else "categorical"
}

parse_number <- function(v) if (is.numeric(v)) v else suppressWarnings(as.numeric(as.character(v)))

format_value <- function(x) if (inherits(x, "Date")) format(x) else as.character(x)

level_string <- function(v) {
  s <- format_value(v)
  s[is.na(v)] <- MISSING
  s
}

decimals <- function(v) {
  v <- if (is.numeric(v)) format(v[!is.na(v)], scientific = FALSE, trim = TRUE, drop0trailing = TRUE) else
    as.character(v[!is.na(v)])
  parts <- regmatches(v, regexpr("\\.[0-9]+$", v))
  if (!length(parts)) 0L else min(max(nchar(parts) - 1L), 6L)
}

same_value <- function(a, b) {
  both_missing <- is.na(a) & is.na(b)
  if (inherits(a, "Date") || inherits(b, "Date")) {
    a <- as.numeric(a)
    b <- as.numeric(b)
  }
  number <- is.numeric(a) || is.numeric(b)
  if (number) b <- parse_number(b)
  equal <- if (number) abs(parse_number(a) - b) <= 1e-8 * pmax(1, abs(b)) else as.character(a) == as.character(b)
  both_missing | (!is.na(equal) & equal)
}

`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a

# One string per row of the given columns; missing values stay distinct from the text "NA".
key_strings <- function(x) {
  do.call(paste, c(lapply(x, function(v) { s <- format_value(v); s[is.na(v)] <- "\rNA"; s }), sep = "\r"))
}

# Integer group of each row by the given columns, 1 for every row when there are none.
group_index <- function(x, columns) {
  group <- rep(1L, nrow(x))
  for (v in x[columns]) {
    values <- unique(v)
    # The first column's codes are already numbered in order of appearance.
    if (all(group == 1L)) { group <- match(v, values); next }
    pair <- (group - 1) * length(values) + match(v, values)
    group <- match(pair, unique(pair))
  }
  group
}
