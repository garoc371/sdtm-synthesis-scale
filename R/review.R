# Review of generated data: SDTM rules that must hold exactly, and comparisons with the
# source that show how closely the models reproduce it.

# ---- SDTM rules -------------------------------------------------------------------------

# A fixed list of SDTMIG rules, read from the output datasets (SUPP-- unfolded, trial design
# included) and the define.xml's domain keys and code lists alone. Nothing comes from the
# fitted specification, so the list does not restate the generator, and it runs on source
# and synthetic data alike. Dates are compared for order only, never for their values.
coherence_checks <- function(study, generated) sdtm_rules(output_datasets(study, generated), study$define)

# The same rules on the source, read once on loading: a rule is run on the generated data only
# where the source keeps it in every record it has for it.
source_checks <- function(study) {
  given <- list(supp = study_supp(study), design = study_design(study), define = study$source$define)
  coherence_checks(given, list(data = study$source$data))
}

# Each rule's category, with what it checks, and where to look when the generated data break
# a rule more often than the source.
CHECK_CATEGORIES <- list(
  keys = list(label = "Keys", about = "one record per subject in DM, per domain key of the define.xml and per --SEQ within a subject",
    fix = "Open the dataset's Variables tab: a key column that is sampled or not generated, rather than sampled with the spine, can repeat."),
  subjects = list(label = "Subjects", about = "every record's subject is in DM",
    fix = "No setting changes this: subjects are copied from DM. Report it with the examples."),
  dates = list(label = "Date order", about = "a record's start date is on or before its end date, and no date is after the subject's death",
    fix = "No setting changes this: a record's dates keep their source order when placed on the visit days. Report it with the examples."),
  days = list(label = "Study days", about = "--DY is the date minus RFSTDTC, plus one from day 1 on (there is no day 0)",
    fix = "No setting changes this: study days are calculated from each date and RFSTDTC after the visit days are drawn. Report it with the examples."),
  exposure = list(label = "Exposure", about = "EX dates fall within the subject's first and last exposure dates, and RFXSTDTC and RFXENDTC are EX's first and last",
    fix = "No setting changes this: EX dates are placed on the visit days and RFXSTDTC and RFXENDTC read from EX. Report it with the examples."),
  death = list(label = "Death", about = "DTHFL is Y exactly when DTHDTC is given and when DS has a DEATH record",
    fix = "Open DM and look at how DTHFL is made (Rules and Variables tabs)."),
  links = list(label = "Parent records", about = "SUPP-- and RELREC records point to a record that exists, and --LNKID and --LNKGRP match a record of the subject in another dataset, and a subject with PC records has EX records and one with PP records PC",
    fix = "No setting changes this: the links are made when the data are written. Report it with the examples."),
  visits = list(label = "Visits", about = "a record's visit is one of the subject's SV visits, with its name and dates; VISITDY is TV's; visits start in the order of VISITNUM",
    fix = "No setting changes this: visits and their days come from the time spine. Report it with the examples."),
  names = list(label = "Names", about = "a code and its name pair one to one (--TESTCD and --TEST, ARMCD and ARM, VISITNUM and VISIT, ...), a dictionary term has one body system and a test one standard unit",
    fix = "Open the dataset and look at how the name or unit column is made (Rules and Variables tabs)."),
  results = list(label = "Results", about = "--STRESN is --STRESC as a number, a test not done has no result, and --NRIND is LOW, NORMAL or HIGH as the result is below, within or above its reference range (--STNRLO to --STNRHI, or --ORNRLO to --ORNRHI)",
    fix = "Open the dataset and look at how the result columns are made (Rules and Variables tabs)."),
  design = list(label = "Trial design", about = "arms are TA's and elements TE's, each subject's elements follow the arm's path in TA one after the other, and EPOCH is that of the element holding the date",
    fix = "No setting changes this: elements and epochs come from the time spine. Report it with the examples."),
  flags = list(label = "Flags", about = "AESER is Y exactly when a serious criterion is, AEOUT is FATAL exactly when AESDTH is Y, and --BLFL and --LOBXFL are Y on at most one record per subject and test, on or before RFXSTDTC",
    fix = "Open the dataset and look at how the flag columns are made (Rules and Variables tabs)."),
  codes = list(label = "Code lists", about = "values are in the define.xml's code list for the variable",
    fix = "Open the dataset and look at how the column is made (Variables tab): sampled values and rule results come from the source.")
)

# The generated checks of the rules the source keeps, with the number of source records
# checked; the rules the source breaks are left out, and kept as the attribute "not_run".
with_source <- function(checks, source) {
  kept <- source[source$Failed == 0, ]
  i <- match(paste(checks$Dataset, checks$Check), paste(kept$Dataset, kept$Check))
  checks$SourceChecked <- kept$Checked[i]
  out <- checks[!is.na(i), , drop = FALSE]
  attr(out, "not_run") <- source[source$Failed > 0, c("Dataset", "Category", "Check", "Failed", "Checked")]
  out
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
  structure_rules(data, add, at_dm, text)
  if (length(rows)) do.call(rbind, rows)
  else data.frame(Dataset = character(), Category = character(), Check = character(), Status = character(), Problems = character(), Checked = integer(), Failed = integer())
}

# Rules on the study's structure, from SDTMIG's general assumptions and the trial design
# datasets: visits as in SV and TV, one name per code, one standard unit per test, results,
# the arm's path of elements in TA, EPOCH from the subject's elements, --LNKID and --LNKGRP
# between datasets, PC to EX and PP to PC, baseline flags, and flags that name the same fact twice. `add`,
# `at_dm` and `text` are sdtm_rules()'s.
structure_rules <- function(data, add, at_dm, text) {
  dm <- data$DM; sv <- data$SV; tv <- data$TV; ta <- data$TA; te <- data$TE; se <- data$SE
  # A record's collection date (--DTC, else --STDTC) and its start (--STDTC, else --DTC).
  date_of <- function(x, order) {
    v <- unlist(lapply(order, function(s) grep(sprintf("^[A-Z]{2}%sDTC$", s), names(x), value = TRUE)))[1]
    if (is.na(v)) rep(as.Date(NA), nrow(x)) else x[[v]]
  }
  # TRUE where the record's name is its code's most common one, and its code the name's.
  same_pair <- function(a, b, one_way = FALSE) {
    a <- text(a); b <- text(b)
    ok <- nzchar(a) & nzchar(b)
    if (!any(ok)) return(rep(NA, length(a)))
    top <- function(k, v) { t <- table(k[ok], v[ok]); stats::setNames(colnames(t)[max.col(t, "first")], rownames(t)) }
    keep <- b == top(a, b)[a]
    if (!one_way) keep <- keep & a == top(b, a)[b]
    ifelse(ok, keep, NA)
  }
  arm_of <- function(x) { a <- text(at_dm(x, "ACTARMCD")); ifelse(nzchar(a), a, text(at_dm(x, "ARMCD"))) }
  for (name in names(data)) {
    x <- data[[name]]
    if (!nrow(x)) next
    who <- if ("USUBJID" %in% names(x)) x$USUBJID else rep("", nrow(x))
    # Names: a code and its name pair one to one (--TESTCD and --TEST, ARMCD and ARM, ...);
    # a dictionary term has one body system, and a test one standard unit.
    p <- function(s) grep(sprintf("^[A-Z]{2}%s$", s), names(x), value = TRUE)[1]
    pairs <- c(lapply(grep("CD$", names(x), value = TRUE), function(v) c(v, sub("CD$", "", v))),
      list(c("VISITNUM", "VISIT"), c(p("TPTNUM"), p("TPT")), c("ETCD", "ELEMENT"), c("QNAM", "QLABEL"),
        c(p("PTCD"), p("DECOD")), c(p("BDSYCD"), p("BODSYS"))))
    for (v in Filter(function(v) all(v %in% names(x)), pairs))
      add(name, "names", sprintf("%s and %s pair one to one", v[1], v[2]), same_pair(x[[v[1]]], x[[v[2]]]),
        sprintf("%s %s = %s with %s = %s", who, v[1], text(x[[v[1]]]), v[2], text(x[[v[2]]])))
    for (v in Filter(function(v) all(v %in% names(x)), list(c(p("DECOD"), p("BODSYS")), c(p("TESTCD"), p("STRESU")))))
      add(name, "names", sprintf("One %s per %s", v[2], v[1]), same_pair(x[[v[1]]], x[[v[2]]], one_way = TRUE),
        sprintf("%s %s = %s with %s = %s", who, v[1], text(x[[v[1]]]), v[2], text(x[[v[2]]])))
    # Results: --STRESN is --STRESC as a number; nothing is reported for a test not done.
    for (n in p("STRESN")) if (!is.na(n) && sub("N$", "C", n) %in% names(x)) {
      given <- suppressWarnings(as.numeric(text(x[[sub("N$", "C", n)]])))
      number <- suppressWarnings(as.numeric(x[[n]]))
      add(name, "results", sprintf("%s is %s as a number", n, sub("N$", "C", n)),
        ifelse(is.na(given) & is.na(number), NA, !is.na(given) & !is.na(number) & abs(given - number) <= 1e-8 * pmax(1, abs(given))),
        sprintf("%s %s = %s, %s = %s", who, sub("N$", "C", n), text(given), n, text(number)))
    }
    for (stat in p("STAT")) if (!is.na(stat)) {
      not_done <- text(x[[stat]]) == "NOT DONE"
      for (v in intersect(paste0(substr(stat, 1, 2), c("ORRES", "REASND")), names(x))) {
        given <- nzchar(text(x[[v]]))
        ok <- if (endsWith(v, "ORRES")) ifelse(not_done, !given, NA) else ifelse(given, not_done, NA)
        add(name, "results", if (endsWith(v, "ORRES")) sprintf("%s empty when %s = NOT DONE", v, stat) else sprintf("%s only when %s = NOT DONE", v, stat),
          ok, sprintf("%s %s = %s, %s = %s", who, stat, text(x[[stat]]), v, text(x[[v]])))
      }
    }
    # --NRIND is where the result falls in its reference range, in standard units, or in
    # original ones where given (SDTMIG allows either); run where --NRIND and the standard
    # result and limits are all given.
    flag_of <- function(v) if (!anyNA(v)) {
      y <- lapply(x[v], function(z) suppressWarnings(as.numeric(text(z))))
      ifelse(y[[1]] < y[[2]], "LOW", ifelse(y[[1]] > y[[3]], "HIGH", "NORMAL"))
    }
    standard <- flag_of(c(p("STRESN"), p("STNRLO"), p("STNRHI")))
    if (!is.na(p("NRIND")) && !is.null(standard)) {
      original <- flag_of(c(p("ORRES"), p("ORNRLO"), p("ORNRHI"))) %||% rep(NA, nrow(x))
      given <- toupper(text(x[[p("NRIND")]]))
      add(name, "results", sprintf("%s agrees with the reference range", p("NRIND")),
        ifelse(nzchar(given) & !is.na(standard), given == standard | given %in% original, NA),
        sprintf("%s %s = %s, standard result gives %s, original %s", who, p("NRIND"), given, standard, text(original)))
    }
    if (!"USUBJID" %in% names(x)) next
    # Baseline: --BLFL and --LOBXFL are Y on at most one record per subject and test, on or
    # before the first exposure.
    test <- c("USUBJID", stats::na.omit(c(p("TESTCD"), p("CAT"), p("SPEC"), p("LOC"), p("POS"))))
    for (flag in stats::na.omit(c(p("BLFL"), p("LOBXFL")))) if (!is.na(p("TESTCD"))) {
      y <- x[[flag]] %in% "Y"
      k <- do.call(paste, c(lapply(x[test], text), sep = " / "))
      add(name, "flags", sprintf("%s = Y once per %s", flag, paste(test, collapse = ", ")), ifelse(y, !k %in% k[y][duplicated(k[y])], NA),
        sprintf("%s has more than one %s = Y", k, flag))
      d <- date_of(x, c("", "ST")); first <- at_dm(x, "RFXSTDTC")
      add(name, "flags", sprintf("%s = Y on or before RFXSTDTC", flag), ifelse(y & !is.na(d) & !is.na(first), d <= first, NA),
        sprintf("%s %s on %s, RFXSTDTC %s", who, flag, text(d), text(first)))
    }
    # Visits: a record's visit is one of the subject's SV visits, with its name and dates.
    if ("VISITNUM" %in% names(x) && !is.null(sv) && !name %in% c("SV", "TV")) {
      at <- match(paste(x$USUBJID, x$VISITNUM), paste(sv$USUBJID, sv$VISITNUM))
      add(name, "visits", "Visit is one of the subject's SV visits",
        ifelse(is.na(x$VISITNUM), NA, !is.na(at) & (!"VISIT" %in% names(x) | text(x$VISIT) == text(sv$VISIT[at]))),
        sprintf("%s VISITNUM %s (%s) not in SV", who, text(x$VISITNUM), if ("VISIT" %in% names(x)) text(x$VISIT) else ""))
      d <- date_of(x, c("", "ST"))
      first <- sv$SVSTDTC[at]
      last <- if ("SVENDTC" %in% names(sv)) sv$SVENDTC[at] else first
      last[is.na(last)] <- first[is.na(last)]
      add(name, "visits", "Date within the SV visit", ifelse(is.na(at) | is.na(d) | is.na(first), NA, d >= first & d <= last),
        sprintf("%s %s on %s, visit %s to %s", who, text(x$VISITNUM), text(d), text(first), text(last)))
    }
    # VISITDY of a planned visit is TV's, per arm where TV gives arms.
    if (!is.null(tv) && all(c("VISITNUM", "VISITDY") %in% names(x)) && all(c("VISITNUM", "VISITDY") %in% names(tv))) {
      by_arm <- "ARMCD" %in% names(tv) && any(nzchar(text(tv$ARMCD)))
      at <- match(if (by_arm) paste(arm_of(x), x$VISITNUM) else text(x$VISITNUM), if (by_arm) paste(tv$ARMCD, tv$VISITNUM) else text(tv$VISITNUM))
      add(name, "visits", "VISITDY as in TV", ifelse(is.na(at) | is.na(x$VISITDY), NA, x$VISITDY == tv$VISITDY[at]),
        sprintf("%s VISITNUM %s VISITDY %s, TV %s", who, text(x$VISITNUM), text(x$VISITDY), text(tv$VISITDY[at])))
    }
    # No record dated after the subject's death.
    if (!is.null(dm) && "DTHDTC" %in% names(dm)) {
      dates <- grep("DTC$", names(x), value = TRUE)
      if (length(dates)) {
        latest <- suppressWarnings(do.call(pmax, c(lapply(dates, function(v) x[[v]]), na.rm = TRUE)))
        death <- at_dm(x, "DTHDTC")
        add(name, "dates", "Dates on or before DTHDTC", ifelse(is.na(latest) | is.na(death), NA, latest <= death),
          sprintf("%s date %s after death %s", who, text(latest), text(death)))
      }
    }
    # EPOCH is the epoch, in TA, of the subject's SE element holding the record's start.
    if ("EPOCH" %in% names(x) && !is.null(se) && !is.null(ta) && !name %in% c("SE", "TA") && all(c("SESTDTC", "ETCD") %in% names(se))) {
      d <- date_of(x, c("ST", ""))
      end <- if ("SEENDTC" %in% names(se)) se$SEENDTC else se$SESTDTC
      end[is.na(end)] <- as.Date("9999-12-31")
      element <- data.frame(USUBJID = se$USUBJID, start = se$SESTDTC, end = end,
        epoch = toupper(text(stats::setNames(ta$EPOCH, paste(ta$ARMCD, ta$ETCD))[paste(arm_of(se), se$ETCD)])))
      m <- merge(data.frame(i = seq_len(nrow(x)), USUBJID = x$USUBJID, d = d), element)
      m <- m[!is.na(m$d) & !is.na(m$start) & m$d >= m$start & m$d <= m$end, ]
      held <- split(m$epoch, m$i)[as.character(seq_len(nrow(x)))]
      ok <- vapply(seq_len(nrow(x)), function(i) {
        h <- held[[i]]
        if (is.na(d[i]) || !nzchar(text(x$EPOCH[i])) || any(!nzchar(h))) NA else toupper(text(x$EPOCH[i])) %in% h
      }, NA)
      add(name, "design", "EPOCH of the SE element holding the date", ok, sprintf("%s EPOCH %s on %s", who, text(x$EPOCH), text(d)))
    }
  }
  # Trial design: arms are TA's, elements TE's, and each subject's planned elements follow
  # the arm's path in TA order, one after the other.
  if (!is.null(ta) && !is.null(dm) && "ARMCD" %in% names(dm))
    add("DM", "design", "ARMCD is an arm of TA or a screen failure", ifelse(is.na(dm$ARMCD), NA, dm$ARMCD %in% ta$ARMCD | toupper(dm$ARMCD) %in% c("SCRNFAIL", "NOTASSGN")),
      sprintf("%s ARMCD %s", dm$USUBJID, text(dm$ARMCD)))
  if (!is.null(se) && nrow(se) && all(c("USUBJID", "ETCD", "SESTDTC") %in% names(se))) {
    if (!is.null(te)) add("SE", "design", "ETCD is an element of TE", ifelse(is.na(se$ETCD), NA, se$ETCD %in% te$ETCD), sprintf("%s ETCD %s", se$USUBJID, text(se$ETCD)))
    o <- order(se$USUBJID, se$SESTDTC, if ("SESEQ" %in% names(se)) se$SESEQ else seq_len(nrow(se)))
    if (!is.null(ta) && all(c("ARMCD", "ETCD", "TAETORD") %in% names(ta))) {
      path <- lapply(split(ta[order(ta$TAETORD), ], ta$ARMCD[order(ta$TAETORD)]), function(a) text(a$ETCD))
      arm <- arm_of(se)
      ok <- rep(NA, nrow(se))
      for (s in split(o, se$USUBJID[o])) {
        p <- path[[arm[s[1]]]]
        planned <- s[text(se$ETCD[s]) %in% p]
        if (!length(planned)) next
        expected <- p[seq_along(planned)]
        ok[planned] <- !is.na(expected) & text(se$ETCD[planned]) == expected
      }
      add("SE", "design", "Elements follow the arm's path in TA", ok, sprintf("%s arm %s element %s", se$USUBJID, arm, text(se$ETCD)))
    }
    if ("SEENDTC" %in% names(se)) {
      same <- c(se$USUBJID[o][-1] == se$USUBJID[o][-length(o)], FALSE)
      following <- c(se$SESTDTC[o][-1], as.Date(NA))
      following[!same] <- NA
      ok <- rep(NA, nrow(se))
      ok[o] <- ifelse(is.na(following) | is.na(se$SEENDTC[o]), NA, se$SEENDTC[o] == following)
      add("SE", "design", "SEENDTC is the next element's SESTDTC", ok, sprintf("%s %s ends %s", se$USUBJID, text(se$ETCD), text(se$SEENDTC)))
    }
  }
  # SV: visits start in the order of VISITNUM.
  if (!is.null(sv) && nrow(sv) && all(c("USUBJID", "VISITNUM", "SVSTDTC") %in% names(sv))) {
    o <- order(sv$USUBJID, sv$VISITNUM)
    d <- as.numeric(sv$SVSTDTC[o])
    before <- stats::ave(ifelse(is.na(d), -Inf, d), sv$USUBJID[o], FUN = function(z) c(-Inf, utils::head(cummax(z), -1)))
    ok <- rep(NA, nrow(sv))
    ok[o] <- ifelse(is.na(d), NA, d >= before)
    add("SV", "visits", "Visits start in the order of VISITNUM", ok, sprintf("%s VISITNUM %s starts %s", sv$USUBJID, text(sv$VISITNUM), text(sv$SVSTDTC)))
  }
  # Links: --LNKID and --LNKGRP values match a record of the same subject in another dataset.
  for (suffix in c("LNKID", "LNKGRP")) {
    column <- unlist(lapply(names(data), function(n) { v <- grep(sprintf("^[A-Z]{2}%s$", suffix), names(data[[n]]), value = TRUE)[1]
      if (!is.na(v) && "USUBJID" %in% names(data[[n]])) stats::setNames(v, n) }))
    for (n in names(column)) {
      others <- setdiff(names(column), n)
      if (!length(others)) next
      x <- data[[n]]
      id <- text(x[[column[[n]]]])
      known <- unlist(lapply(others, function(o) paste(data[[o]]$USUBJID, text(data[[o]][[column[[o]]]]))))
      add(n, "links", sprintf("%s matches a record in %s", column[[n]], paste(others, collapse = ", ")), ifelse(nzchar(id), paste(x$USUBJID, id) %in% known, NA),
        sprintf("%s %s = %s", x$USUBJID, column[[n]], id))
    }
  }
  # Pharmacokinetics: concentrations follow a dose and parameters are derived from
  # concentrations, so a subject with PC records has EX records, and one with PP records PC.
  for (v in list(c("PC", "EX"), c("PP", "PC"))) {
    x <- data[[v[1]]]; y <- data[[v[2]]]
    if (!is.null(x) && !is.null(y) && nrow(x) && "USUBJID" %in% names(x) && "USUBJID" %in% names(y))
      add(v[1], "links", sprintf("Subject has %s records", v[2]), ifelse(is.na(x$USUBJID), NA, x$USUBJID %in% y$USUBJID),
        sprintf("%s has no %s records", x$USUBJID, v[2]))
  }
  # Flags naming the same fact twice.
  ae <- data$AE
  if (!is.null(ae) && nrow(ae)) {
    criteria <- intersect(c("AESCAN", "AESCONG", "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE", "AESOD"), names(ae))
    if ("AESER" %in% names(ae) && length(criteria)) {
      any_yes <- Reduce(`|`, lapply(criteria, function(v) ae[[v]] %in% "Y"))
      add("AE", "flags", "AESER = Y exactly when a serious criterion is Y", ifelse(is.na(ae$AESER), NA, (ae$AESER == "Y") == any_yes),
        sprintf("%s AESER %s, criteria %s", ae$USUBJID, text(ae$AESER), ifelse(any_yes, "Y", "none")))
    }
    if (all(c("AEOUT", "AESDTH") %in% names(ae)))
      add("AE", "flags", "AEOUT = FATAL exactly when AESDTH = Y", ifelse(is.na(ae$AEOUT) & is.na(ae$AESDTH), NA, (ae$AEOUT %in% "FATAL") == (ae$AESDTH %in% "Y")),
        sprintf("%s AEOUT %s, AESDTH %s", ae$USUBJID, text(ae$AEOUT), text(ae$AESDTH)))
  }
  ds <- data$DS
  if (!is.null(dm) && "DTHFL" %in% names(dm) && !is.null(ds) && "DSDECOD" %in% names(ds))
    add("DM", "death", "DTHFL = Y exactly when DS has a DEATH record", (dm$DTHFL %in% "Y") == (dm$USUBJID %in% ds$USUBJID[ds$DSDECOD %in% "DEATH"]),
      sprintf("%s DTHFL %s", dm$USUBJID, text(dm$DTHFL)))
  # RFXSTDTC and RFXENDTC are the first and last exposure dates in EX.
  ex <- data$EX
  if (!is.null(dm) && !is.null(ex) && nrow(ex) && "EXSTDTC" %in% names(ex)) {
    end <- if ("EXENDTC" %in% names(ex)) ifelse(is.na(ex$EXENDTC), as.numeric(ex$EXSTDTC), as.numeric(ex$EXENDTC)) else as.numeric(ex$EXSTDTC)
    for (v in list(c("RFXSTDTC", "first EXSTDTC"), c("RFXENDTC", "last EXENDTC"))) if (v[1] %in% names(dm)) {
      f <- if (v[1] == "RFXSTDTC") min else max
      edge <- suppressWarnings(tapply(if (v[1] == "RFXSTDTC") as.numeric(ex$EXSTDTC) else end, ex$USUBJID, f, na.rm = TRUE))
      edge[!is.finite(edge)] <- NA
      e <- unname(edge[match(dm$USUBJID, names(edge))])
      given <- as.numeric(dm[[v[1]]])
      add("DM", "exposure", sprintf("%s is the %s", v[1], v[2]), ifelse(is.na(given) & is.na(e), NA, !is.na(given) & !is.na(e) & given == e),
        sprintf("%s %s %s, EX %s", dm$USUBJID, v[1], text(dm[[v[1]]]), text(as.Date(e))))
    }
  }
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
