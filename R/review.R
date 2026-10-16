# Review of generated data: SDTM rules that must hold exactly, and comparisons with the
# source that show how closely the models reproduce it.

# ---- SDTM rules -------------------------------------------------------------------------

# A fixed list of SDTMIG rules, read from the output datasets (SUPP-- unfolded, trial design
# included) and the define.xml's domain keys and code lists alone. Nothing comes from the
# fitted specification, so the list does not restate the generator, and it runs on source
# and synthetic data alike. Dates are compared for order only, never for their values.
coherence_checks <- function(study, generated) sdtm_rules(output_datasets(study, generated), study$define)

# The same rules on the source, read once on loading: a generated record that breaks a rule
# the source breaks as often follows its donor, and only the others point to the generator.
source_checks <- function(study) {
  given <- list(supp = study_supp(study), design = study_design(study), define = study$source$define)
  coherence_checks(given, list(data = study$source$data))
}

# Each rule's category, with what it checks, and where to look when the generated data break
# a rule more often than the source.
CHECK_CATEGORIES <- list(
  keys = list(label = "Keys", about = "one record per subject in DM, per domain key of the define.xml and per --SEQ within a subject",
    fix = "Open the dataset's Variables tab: a key column that is sampled or not generated, rather than copied from the donor, can repeat."),
  subjects = list(label = "Subjects", about = "every record's subject is in DM",
    fix = "No setting changes this: subjects are copied from DM. Report it with the examples."),
  dates = list(label = "Date order", about = "a record's start date is on or before its end date",
    fix = "No setting changes this: dates are copied from the donor. Report it with the examples."),
  days = list(label = "Study days", about = "--DY is the date minus RFSTDTC, plus one from day 1 on (there is no day 0)",
    fix = "Permuting the visit gaps moves dates but keeps a study day that is planned per visit, so the two drift apart; synth.permute_gaps = FALSE keeps the donor's days. Otherwise report it with the examples."),
  exposure = list(label = "Exposure", about = "EX dates fall within the subject's first and last exposure dates",
    fix = "No setting changes this: dates are copied from the donor. Report it with the examples."),
  death = list(label = "Death", about = "DTHFL is Y exactly when DTHDTC is given",
    fix = "Open DM and look at how DTHFL is made (Rules and Variables tabs)."),
  links = list(label = "Parent records", about = "SUPP-- and RELREC records point to a record that exists",
    fix = "No setting changes this: the links are made when the data are written. Report it with the examples."),
  codes = list(label = "Code lists", about = "values are in the define.xml's code list for the variable",
    fix = "Open the dataset and look at how the column is made (Variables tab): sampled values and rule results come from the source.")
)

# A broken rule counts as inherited when the source breaks it too and the generated share of
# records breaking it is at most this much above the source's.
INHERITED_MARGIN <- 0.05

# Generated checks with the source's result for the same rule: SourceFailed is NA when the
# source has nothing to check.
with_source <- function(checks, source) {
  i <- match(paste(checks$Dataset, checks$Check), paste(source$Dataset, source$Check))
  checks$SourceChecked <- source$Checked[i]
  checks$SourceFailed <- source$Failed[i]
  checks$SourceProblems <- source$Problems[i]
  share <- checks$Failed / checks$Checked
  source_share <- checks$SourceFailed / checks$SourceChecked
  checks$Inherited <- checks$Status == "failed" & !is.na(i) & checks$SourceFailed > 0 &
    share <= source_share + INHERITED_MARGIN
  checks
}

# Domain keys (def:DomainKeys) and enumerated code lists per dataset and variable. Code lists
# without items name an external dictionary (MedDRA, WHODrug) and are left out.
read_define <- function(file) {
  xml <- xml2::xml_ns_strip(xml2::read_xml(file))
  attr_of <- function(node, name) {
    value <- xml2::xml_attr(node, name)
    ifelse(is.na(value), xml2::xml_attr(node, paste0("def:", name)), value)
  }
  groups <- xml2::xml_find_all(xml, "//ItemGroupDef")
  names <- xml2::xml_attr(groups, "Name")
  keys <- stats::setNames(lapply(attr_of(groups, "DomainKeys"), function(k) if (!is.na(k)) trimws(strsplit(k, ",")[[1]])), names)
  items <- xml2::xml_find_all(xml, "//ItemDef[CodeListRef]")
  item_list <- stats::setNames(xml2::xml_attr(xml2::xml_find_first(items, "CodeListRef"), "CodeListOID"), xml2::xml_attr(items, "OID"))
  item_name <- stats::setNames(xml2::xml_attr(items, "Name"), xml2::xml_attr(items, "OID"))
  lists <- xml2::xml_find_all(xml, "//CodeList")
  codes <- stats::setNames(lapply(lists, function(l) xml2::xml_attr(xml2::xml_find_all(l, "CodeListItem|EnumeratedItem"), "CodedValue")),
    xml2::xml_attr(lists, "OID"))
  numeric <- stats::setNames(xml2::xml_attr(lists, "DataType") %in% c("integer", "float"), xml2::xml_attr(lists, "OID"))
  codelists <- do.call(rbind, lapply(seq_along(groups), function(i) {
    refs <- xml2::xml_attr(xml2::xml_find_all(groups[[i]], "ItemRef"), "ItemOID")
    refs <- refs[refs %in% names(item_list) & item_list[refs] %in% names(codes)]
    refs <- refs[lengths(codes[item_list[refs]]) > 0]
    if (length(refs)) data.frame(dataset = names[i], variable = unname(item_name[refs]), codelist = unname(item_list[refs]))
  }))
  list(keys = Filter(length, keys), codelists = codelists, codes = codes, numeric = numeric)
}

# One row per rule and dataset: records checked, records failing and the first examples.
sdtm_rules <- function(data, define = NULL) {
  as_day <- function(x) if (inherits(x, "Date")) x else {
    x <- substr(as.character(x), 1, 10)
    as.Date(ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA))
  }
  text <- function(x) ifelse(is.na(x), "", trimws(as.character(x)))
  # Keys compare the values as given, partial dates included.
  key_text <- lapply(data, function(x) as.data.frame(lapply(x, text), check.names = FALSE))
  data <- lapply(data, function(x) {
    x <- as.data.frame(x)
    for (v in grep("DTC$", names(x), value = TRUE)) x[[v]] <- as_day(x[[v]])
    x
  })
  dm <- data$DM
  at_dm <- function(x, v) if (v %in% names(dm)) dm[[v]][match(x$USUBJID, dm$USUBJID)] else rep(NA, nrow(x))
  rows <- list()
  # ok: TRUE kept, FALSE broken, NA not checked; detail names each record.
  add <- function(dataset, category, check, ok, detail) {
    if (!any(!is.na(ok))) return()
    bad <- which(!is.na(ok) & !ok)
    shown <- paste(utils::head(detail[bad], 3), collapse = "; ")
    if (length(bad) > 3) shown <- sprintf("%s (+%d more)", shown, length(bad) - 3)
    rows[[length(rows) + 1]] <<- data.frame(Dataset = dataset, Category = category, Check = check, Status = if (length(bad)) "failed" else "ok",
      Problems = shown, Checked = sum(!is.na(ok)), Failed = length(bad), stringsAsFactors = FALSE)
  }
  subject <- function(x) if ("USUBJID" %in% names(x)) x$USUBJID else rep("", nrow(x))
  for (name in names(data)) {
    x <- data[[name]]
    if (!nrow(x)) next
    who <- subject(x)
    # Keys: one DM record per subject, every subject in DM, the define.xml's domain keys,
    # and --SEQ unique within a subject.
    if (name == "DM") add(name, "keys", "One record per subject", !duplicated(x$USUBJID), paste(who, "has more than one record"))
    else if ("USUBJID" %in% names(x) && !is.null(dm)) add(name, "subjects", "Subject is in DM", ifelse(is.na(x$USUBJID), NA, x$USUBJID %in% dm$USUBJID), paste(who, "is not in DM"))
    keys <- define$keys[[name]]
    if (length(keys) > 1 && all(keys %in% names(x))) {
      keys <- setdiff(keys, "STUDYID")
      k <- do.call(paste, c(key_text[[name]][keys], sep = " / "))
      named <- do.call(paste, c(Map(function(v, x) paste(v, ifelse(is.na(x) | x == "", "(empty)", x)), keys, key_text[[name]][keys]), sep = ", "))
      add(name, "keys", paste("One record per", paste(keys, collapse = ", ")), !duplicated(k), paste(named, "occurs again"))
    }
    if ("USUBJID" %in% names(x)) for (v in grep("^[A-Z]{2}SEQ$", names(x), value = TRUE))
      add(name, "keys", paste(v, "unique within a subject"), !is.na(x[[v]]) & !duplicated(x[c("USUBJID", v)]), paste(who, v, x[[v]], "occurs again"))
    # Order of dates within a record.
    for (start in grep("STDTC$", names(x), value = TRUE)) {
      end <- sub("STDTC$", "ENDTC", start)
      if (end %in% names(x)) add(name, "dates", sprintf("%s on or before %s", start, end), ifelse(is.na(x[[start]]) | is.na(x[[end]]), NA, x[[start]] <= x[[end]]),
        sprintf("%s %s %s after %s %s", who, start, x[[start]], end, x[[end]]))
    }
    # Study days: date minus RFSTDTC, plus one on or after it (no day 0).
    for (v in grep("^[A-Z]{2}(ST|EN)?DY$", names(x), value = TRUE)) {
      date <- sub("DY$", "DTC", v)
      if (!date %in% names(x) || !"RFSTDTC" %in% names(dm)) next
      days <- as.numeric(x[[date]] - at_dm(x, "RFSTDTC"))
      expected <- days + (days >= 0)
      add(name, "days", sprintf("%s matches %s and RFSTDTC", v, date), ifelse(is.na(x[[v]]) | is.na(days), NA, x[[v]] == expected),
        sprintf("%s %s %s, dates give %s", who, v, x[[v]], expected))
    }
  }
  # Exposure within the subject's first and last exposure dates.
  ex <- data$EX
  if (!is.null(ex) && all(c("RFXSTDTC", "RFXENDTC") %in% names(dm))) {
    first <- at_dm(ex, "RFXSTDTC"); last <- at_dm(ex, "RFXENDTC")
    within <- function(d) ifelse(is.na(d), NA, (is.na(first) | d >= first) & (is.na(last) | d <= last) & !(is.na(first) & is.na(last)))
    ok <- Reduce(function(a, b) ifelse(is.na(a), b, ifelse(is.na(b), a, a & b)), lapply(intersect(c("EXSTDTC", "EXENDTC"), names(ex)), function(v) within(ex[[v]])))
    add("EX", "exposure", "EX dates within RFXSTDTC to RFXENDTC", ok, sprintf("%s EX %s to %s, RFX %s to %s", ex$USUBJID, text(ex$EXSTDTC), text(ex$EXENDTC), text(first), text(last)))
  }
  if (!is.null(dm) && all(c("DTHFL", "DTHDTC") %in% names(dm)))
    add("DM", "death", "DTHFL = Y exactly when DTHDTC is given", (dm$DTHFL %in% "Y") == !is.na(dm$DTHDTC),
      sprintf("%s DTHFL %s, DTHDTC %s", dm$USUBJID, text(dm$DTHFL), text(dm$DTHDTC)))
  # SUPP-- and RELREC records name their parent by RDOMAIN, USUBJID and IDVAR = IDVARVAL.
  for (name in c(grep("^SUPP", names(data), value = TRUE), intersect("RELREC", names(data)))) {
    s <- data[[name]]
    if (!nrow(s) || !"USUBJID" %in% names(s)) next
    domain <- if ("RDOMAIN" %in% names(s)) s$RDOMAIN else rep(sub("^SUPP", "", name), nrow(s))
    idvar <- if ("IDVAR" %in% names(s)) s$IDVAR else rep(NA, nrow(s))
    value <- trimws(as.character(if ("IDVARVAL" %in% names(s)) s$IDVARVAL else rep(NA, nrow(s))))
    ok <- rep(NA, nrow(s))
    for (g in unique(paste(domain, idvar))) {
      i <- which(paste(domain, idvar) == g & !is.na(s$USUBJID))
      if (!length(i)) next
      p <- data[[domain[i[1]]]] %||% data[[sub("^SUPP", "", name)]]
      v <- idvar[i[1]]
      ok[i] <- if (is.null(p)) FALSE else if (is.na(v) || !nzchar(v)) s$USUBJID[i] %in% p$USUBJID
        else if (!v %in% names(p)) FALSE
        else {
          parent <- p[[v]]
          given <- if (is.numeric(parent)) suppressWarnings(as.numeric(value[i])) else if (inherits(parent, "Date")) as_day(value[i]) else value[i]
          paste(s$USUBJID[i], given) %in% paste(p$USUBJID, if (is.character(parent)) trimws(parent) else parent)
        }
    }
    add(name, "links", "Points to an existing parent record", ok, sprintf("%s %s %s = %s not found", s$USUBJID, domain, idvar, value))
  }
  # Controlled terminology: values in the define.xml code list, where the source names one.
  cl <- define$codelists
  for (i in seq_len(NROW(cl))) {
    x <- data[[cl$dataset[i]]]
    v <- cl$variable[i]
    if (is.null(x) || !v %in% names(x)) next
    codes <- define$codes[[cl$codelist[i]]]
    y <- x[[v]]
    ok <- if (isTRUE(define$numeric[[cl$codelist[i]]])) suppressWarnings(as.numeric(as.character(y))) %in% suppressWarnings(as.numeric(codes))
      else trimws(as.character(y)) %in% codes
    add(cl$dataset[i], "codes", sprintf("%s in code list %s", v, cl$codelist[i]), ifelse(is.na(y), NA, ok), sprintf("%s %s = %s", subject(x), v, text(y)))
  }
  if (length(rows)) do.call(rbind, rows)
  else data.frame(Dataset = character(), Category = character(), Check = character(), Status = character(), Problems = character(), Checked = integer(), Failed = integer())
}

# ---- Comparisons ----------------------------------------------------------------------

summarise_values <- function(v) {
  missing <- mean(is.na(v))
  observed <- v[!is.na(v)]
  if (!length(observed)) return("all missing")
  text <- if (inherits(v, "Date")) sprintf("median %s (%s to %s)", format(stats::median(observed)),
    format(min(observed)), format(max(observed)))
  else if (is.numeric(v)) sprintf("mean %s, SD %s", signif(mean(observed), 3), signif(stats::sd(observed), 3))
  else {
    counts <- sort(table(observed), decreasing = TRUE)
    top <- utils::head(counts, 3)
    paste0(paste(sprintf("%s %.0f%%", names(top), 100 * top / length(observed)), collapse = ", "),
      if (length(counts) > 3) sprintf(" (+%d more)", length(counts) - 3) else "")
  }
  if (missing > 0) paste0(text, sprintf("; %.0f%% missing", 100 * missing)) else text
}

compare_marginals <- function(study, source, generated, name) {
  ds <- study$datasets[[name]]
  variables <- ds$variables[vapply(ds$variables, function(v) node_type(ds, v) %in% c("sampled", "rule", "key"), TRUE)]
  data.frame(Variable = variables, Type = vapply(variables, function(v) node_type(ds, v), ""),
    Source = vapply(variables, function(v) summarise_values(source[[v]]), ""),
    Synthetic = vapply(variables, function(v) summarise_values(generated[[v]]), ""),
    Records = sprintf("%d / %d", nrow(source), nrow(generated)), stringsAsFactors = FALSE, row.names = NULL)
}

# Mean value per visit and arm for one test.
trajectory_data <- function(study, data, name, variable, test, arm) {
  ds <- study$datasets[[name]]
  s <- ds$structure
  x <- data[[name]]
  if (!is.null(s$test)) x <- x[format_value(x[[s$test]]) == test, , drop = FALSE]
  value <- parse_number(x[[variable]])
  group <- if (!is.na(arm)) level_string(data$DM[[arm]][match(x$USUBJID, data$DM$USUBJID)]) else rep("all", nrow(x))
  keep <- !is.na(value) & !is.na(x[[s$visit]])
  if (!any(keep)) return(NULL)
  out <- stats::aggregate(list(mean = value[keep]), list(visit = as.numeric(x[[s$visit]][keep]), arm = group[keep]), mean)
  out$n <- stats::aggregate(value[keep], list(as.numeric(x[[s$visit]][keep]), group[keep]), length)$x
  out
}

# Records per subject by arm, for event and history datasets.
event_rate_data <- function(data, name, arm) {
  dm <- data$DM
  counts <- tabulate(match(data[[name]]$USUBJID, dm$USUBJID), nrow(dm))
  group <- if (!is.na(arm)) level_string(dm[[arm]]) else rep("all", nrow(dm))
  stats::aggregate(list(rate = counts), list(arm = group), mean)
}
