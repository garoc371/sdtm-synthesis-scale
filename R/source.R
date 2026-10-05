# Reading SDTM sources, and the value conventions shared by rules, models and generation.
#
# Internally every dataset is a data.frame in which blanks are NA, dates are Date objects
# and everything else keeps its source type.

# Fixed dataset order: subjects and their visits first, then exposure and disposition, findings,
# events and history. Visit datasets take their visits and dates from SV, so SV comes early.
DATASET_ORDER <- c("DM", "SV", "SE", "EX", "EC", "DS", "VS", "LB", "EG", "QS", "AE", "CE", "CM", "MH")
MISSING <- "(missing)"

read_folder <- function(directory) {
  files <- list.files(directory, "\\.(xpt|sas7bdat|csv|xml)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No SDTM files found in ", directory, ".")
  read_source(files)
}

# Labels come from the files, or from define.xml item definitions (OIDs of the form
# IT.<DATASET>.<VARIABLE>) when one is given. Partial ISO dates (e.g. "2013-04") cannot
# be placed on a day scale and become missing.
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
  data <- data[c(intersect(DATASET_ORDER, names(data)), sort(setdiff(names(data), DATASET_ORDER)))]
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
      if (is.character(v)) v[!nzchar(trimws(v))] <- NA
      if (grepl("DTC$|DTM$", name)) as_date(v) else v
    })
    x
  })
  list(data = data, labels = labels)
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

level_string <- function(v) ifelse(is.na(v), MISSING, format_value(v))

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
  do.call(paste, c(lapply(x, function(v) ifelse(is.na(v), "\rNA", format_value(v))), sep = "\r"))
}

# Integer group of each row by the given columns, 1 for every row when there are none.
group_index <- function(x, columns) {
  group <- rep(1L, nrow(x))
  for (v in x[columns]) {
    values <- unique(v)
    pair <- (group - 1) * length(values) + match(v, values)
    group <- match(pair, unique(pair))
  }
  group
}
