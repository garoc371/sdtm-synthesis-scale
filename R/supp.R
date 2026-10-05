# SUPP-- datasets: folded into their parent as one column per QNAM before the study is
# built, and unfolded again after generation.
#
# `fold_supp()` leaves `source$supp`, one entry per SUPP dataset, which is all
# `unfold_supp()` and `supp_checks()` need to rebuild and check the dataset.

# Most common value (first seen on ties); NA counts as a value.
supp_common <- function(x) {
  if (!length(x)) return(NA_character_)
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

# Values as text in IDVARVAL and QVAL: ISO dates, whole numbers without decimals.
supp_text <- function(v) {
  if (inherits(v, "Date")) return(format(v))
  if (!is.numeric(v)) return(as.character(v))
  s <- as.character(v)
  whole <- !is.na(v) & is.finite(v) & v == round(v) & abs(v) < 1e15
  s[whole] <- sprintf("%.0f", v[whole] + 0)
  long <- !is.na(s) & grepl("e", s, fixed = TRUE)
  s[long] <- vapply(v[long], format, "", digits = 15, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
  s
}

# IDVARVAL of the SUPP records as the text of the parent column they point at.
supp_idval <- function(text, column) {
  text <- trimws(as.character(text))
  text[!nzchar(text)] <- NA
  if (is.numeric(column) || inherits(column, "Date")) {
    value <- if (inherits(column, "Date")) as_date(text) else parse_number(text)
    text[!is.na(value)] <- supp_text(value[!is.na(value)])
  }
  text
}

# One string per record from subject and IDVAR value; NA where either is missing.
supp_key <- function(usubjid, id) {
  key <- paste(usubjid, id, sep = "\r")
  key[is.na(usubjid) | is.na(id)] <- NA
  key
}

# Keys of the parent records for one IDVAR (NA: the subject); all NA when the parent lacks it.
supp_parent_keys <- function(parent, idvar) {
  id <- if (is.na(idvar)) "" else if (idvar %in% names(parent)) trimws(supp_text(parent[[idvar]])) else NA_character_
  supp_key(parent$USUBJID %||% rep(NA_character_, nrow(parent)), id)
}

# Keys of SUPP records for one IDVAR, comparable with `supp_parent_keys()`.
supp_row_keys <- function(parent, usubjid, idvar, idvarval) {
  supp_key(usubjid, if (is.na(idvar)) "" else supp_idval(idvarval, parent[[idvar]]))
}

NUMBER_TEXT <- "^\\s*[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?\\s*$"

# Date for --DTC and --DTM names, number when every value is one, text otherwise.
supp_kind <- function(qnam, value) {
  value <- value[!is.na(value)]
  if (grepl("(DTC|DTM)$", qnam)) "date"
  else if (length(value) && all(grepl(NUMBER_TEXT, value)) && all(is.finite(parse_number(value)))) "numeric"
  else "character"
}

# Folds one SUPP dataset into its parent. Records that match no parent record, or that use
# another IDVAR than the most common of their QNAM, are orphans; of repeated records of a
# QNAM on one parent record the first is kept; records without a QVAL carry no value and
# are dropped (`empty`).
supp_fold_one <- function(parent, x) {
  column <- function(v) x[[v]] %||% rep(NA_character_, nrow(x))
  qnam <- column("QNAM")
  idvar <- column("IDVAR")
  usubjid <- column("USUBJID")
  idvarval <- column("IDVARVAL")
  qval <- supp_text(column("QVAL"))
  qlabel <- column("QLABEL")
  qorig <- column("QORIG")
  qeval <- column("QEVAL")
  empty <- is.na(qval)
  rows <- split(seq_len(nrow(x)), factor(ifelse(empty, NA, qnam), levels = unique(qnam[!is.na(qnam) & !empty])))
  named <- function() stats::setNames(character(), character())
  columns <- idvars <- labels <- origins <- evals <- kinds <- named()
  new <- list()
  cache <- list()
  orphans <- sum(is.na(qnam) & !empty)
  duplicates <- 0L
  for (q in names(rows)) {
    ix <- rows[[q]]
    id <- supp_common(idvar[ix])
    label <- if (is.na(id)) "\rsubject" else id
    if (is.null(cache[[label]])) cache[[label]] <- supp_parent_keys(parent, id)
    pk <- cache[[label]]
    sk <- supp_row_keys(parent, usubjid[ix], id, idvarval[ix])
    sk[match(idvar[ix], id, 0L) == 0L] <- NA
    hit <- !is.na(match(sk, pk, incomparables = NA))
    orphans <- orphans + sum(!hit)
    first <- !duplicated(sk[hit])
    duplicates <- duplicates + sum(!first)
    if (!any(first)) next
    value <- qval[ix[hit][first]][match(pk, sk[hit][first], incomparables = NA)]
    kind <- supp_kind(q, value)
    name <- q
    taken <- c(names(parent), names(new))
    if (name %in% taken) name <- paste0("Q.", q)
    if (name %in% taken) name <- make.unique(c(taken, name))[length(taken) + 1]
    new[[name]] <- switch(kind, date = as_date(value), numeric = parse_number(value), value)
    columns[q] <- name
    idvars[q] <- id
    labels[q] <- supp_common(qlabel[ix][!is.na(qlabel[ix])])
    origins[q] <- supp_common(qorig[ix])
    evals[q] <- supp_common(qeval[ix])
    kinds[q] <- kind
  }
  if (length(new)) parent[names(new)] <- new
  shown <- labels
  shown[is.na(shown)] <- ""
  names(shown) <- unname(columns)
  list(parent = parent, labels = shown,
    entry = list(rdomain = supp_common(column("RDOMAIN")), columns = columns, idvar = idvars, qlabel = labels,
      qorig = origins, qeval = evals, kind = kinds, variables = names(x), orphans = as.integer(orphans),
      duplicates = as.integer(duplicates), empty = as.integer(sum(empty))))
}

# Pivots every SUPP dataset onto its parent (SUPPxx -> xx, else the RDOMAIN dataset) and
# records how to unfold it. Datasets without a parent are left as they are.
fold_supp <- function(source) {
  source$supp <- list()
  for (name in grep("^SUPP", names(source$data), value = TRUE)) {
    x <- source$data[[name]]
    parent <- sub("^SUPP", "", name)
    if (!parent %in% names(source$data)) parent <- supp_common(x$RDOMAIN[!is.na(x$RDOMAIN)])
    if (is.na(parent) || !parent %in% names(source$data) || startsWith(parent, "SUPP") ||
        !all(c("USUBJID", "QNAM", "QVAL") %in% names(x))) next
    folded <- supp_fold_one(source$data[[parent]], x)
    source$data[[parent]] <- folded$parent
    source$labels[[parent]] <- c(source$labels[[parent]], folded$labels)
    source$data[[name]] <- NULL
    source$labels[[name]] <- NULL
    source$supp[[name]] <- c(list(parent = parent), folded$entry)
  }
  source
}

# The SUPP records of one QNAM: one per distinct subject (and IDVAR value) whose folded value is not missing.
supp_records <- function(parent, s, q) {
  col <- s$columns[[q]]
  idvar <- s$idvar[[q]]
  if (!col %in% names(parent) || (!is.na(idvar) && !idvar %in% names(parent))) return(NULL)
  id <- if (is.na(idvar)) "" else trimws(supp_text(parent[[idvar]]))
  usubjid <- parent$USUBJID %||% rep(NA_character_, nrow(parent))
  key <- supp_key(usubjid, id)
  i <- which(!is.na(parent[[col]]) & !is.na(key))
  i <- i[!duplicated(key[i])]
  n <- length(i)
  data.frame(STUDYID = if ("STUDYID" %in% names(parent)) as.character(parent$STUDYID[i]) else rep(NA_character_, n),
    RDOMAIN = rep(s$rdomain, n), USUBJID = usubjid[i], IDVAR = rep(idvar, n),
    IDVARVAL = if (is.na(idvar)) rep(NA_character_, n) else id[i], QNAM = rep(q, n),
    QLABEL = rep(s$qlabel[[q]], n), QVAL = supp_text(parent[[col]][i]), QORIG = rep(s$qorig[[q]], n),
    QEVAL = rep(s$qeval[[q]], n), stringsAsFactors = FALSE)
}

# Rebuilds the SUPP datasets from the folded columns and puts them right after their parent.
unfold_supp <- function(data, supp) {
  built <- list()
  for (name in names(supp)) {
    s <- supp[[name]]
    parent <- data[[s$parent]]
    if (is.null(parent)) next
    pieces <- lapply(names(s$columns), function(q) supp_records(parent, s, q))
    out <- do.call(rbind, pieces[!vapply(pieces, is.null, TRUE)])
    if (is.null(out)) out <- data.frame(STUDYID = character(), RDOMAIN = character(), USUBJID = character(),
      IDVAR = character(), IDVARVAL = character(), QNAM = character(), QLABEL = character(), QVAL = character(),
      QORIG = character(), QEVAL = character())
    for (v in setdiff(s$variables, names(out))) out[[v]] <- rep(NA_character_, nrow(out))
    number <- ifelse(grepl("SEQ$", out$IDVAR), parse_number(out$IDVARVAL), NA_real_)
    out <- out[order(out$USUBJID, is.na(number), number, out$IDVARVAL, out$QNAM, method = "radix"), s$variables, drop = FALSE]
    rownames(out) <- NULL
    built[[name]] <- out
    data[[s$parent]] <- parent[setdiff(names(parent), s$columns)]
  }
  data <- data[setdiff(names(data), names(built))]
  result <- list()
  for (name in names(data)) {
    result[[name]] <- data[[name]]
    for (s in names(built)[vapply(supp[names(built)], `[[`, "", "parent") == name]) result[[s]] <- built[[s]]
  }
  result
}

# Each SUPP record points to a parent record, and each QNAM occurs once per parent record.
supp_checks <- function(data, supp) {
  rows <- list()
  for (name in names(supp)) {
    s <- supp[[name]]
    x <- data[[name]]
    parent <- data[[s$parent]]
    if (is.null(x) || is.null(parent)) next
    column <- function(v) x[[v]] %||% rep(NA_character_, nrow(x))
    matched <- rep(FALSE, nrow(x))
    group <- match(column("IDVAR"), unique(column("IDVAR")))
    for (g in unique(group)) {
      at <- which(group == g)
      idvar <- column("IDVAR")[at[1]]
      pk <- supp_parent_keys(parent, idvar)
      sk <- supp_row_keys(parent, column("USUBJID")[at], idvar, column("IDVARVAL")[at])
      matched[at] <- !is.na(match(sk, pk, incomparables = NA))
    }
    rows <- c(rows, list(
      check_row(name, sprintf("Every %s record points to a %s record", name, s$parent), which(!matched)),
      check_row(name, "QNAM is unique per parent record",
        which(duplicated(key_strings(list(column("USUBJID"), column("IDVAR"), column("IDVARVAL"), column("QNAM"))))))))
  }
  if (length(rows)) do.call(rbind, rows) else check_row("", "")[0, ]
}
