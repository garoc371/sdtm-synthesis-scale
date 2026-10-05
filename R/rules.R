# Deterministic rules: a fixed set of operations, each judged by the share of source
# records it reproduces.
#
# A record only counts as evidence ("checked") when it could contradict the rule: the
# group its learned parameters are keyed by holds at least two records, and for arithmetic
# rules the target or the result is present. A key seen once is reproduced by any table,
# so with small samples a nearly unique key would otherwise "explain" every target. Rules
# are proposed when they reproduce SHARE_TO_PROPOSE of at least two checked records, and
# EXPLAINED_TO_PROPOSE of the records the most common value gets wrong: otherwise a skewed
# target (99% "N") would appear determined by almost anything.

SHARE_TO_PROPOSE <- 0.9
EXPLAINED_TO_PROPOSE <- 0.5

# Discovery cost must not grow with the records: above DISCOVERY_RECORDS the candidates are
# searched on whole subjects totalling about that many records, and only the best
# DISCOVERY_SHORTLIST per target are verified (and their tables learned) on every record.
# A sample's shares wobble, so the search keeps candidates DISCOVERY_SLACK short of the
# thresholds; the verification applies them exactly. The options synth.discovery_records /
# synth.discovery_shortlist override the first two (Inf records searches everything).
DISCOVERY_RECORDS <- 5000
DISCOVERY_SHORTLIST <- 5
DISCOVERY_SLACK <- 0.05

EXPRESSION_FUNCTIONS <- c("+", "-", "*", "/", "^", "(", "{", "sqrt", "log", "exp", "abs",
  "round", "floor", "ceiling", "pmin", "pmax", "ifelse", ">", "<", ">=", "<=", "==", "!=",
  "&", "|", "!", "%in%", "is.na", "c", "paste", "paste0", "substr", "toupper", "tolower",
  "nchar", "as.numeric", "as.character", "as.Date", "as.integer")

# Each operation says which inputs discovery tries for a target (`find`, from column kinds
# and the dataset structure only), what it learns from the source (`learn`), how it
# calculates (`apply`) and how it reads (`label`). `groups` are the columns its learned
# table is keyed by; `paired` operations only check records where the target or the result
# is present. List order is the preference between equally good rules.
RULE_OPS <- list(
  constant = list(name = "Constant", help = "No inputs.",
    find = function(target, info) if (info$values[[target]] == 1) list(character()),
    learn = function(rule, frame) list(value = most_common(frame[[rule$target]])),
    apply = function(rule, frame) rep(rule$params$value, nrow(frame)),
    label = function(rule, input) sprintf("Constant \"%s\"", format_value(rule$params$value))),
  # In the source, the target only has to number each subject's records 1..n in some order;
  # generated records are numbered after sorting.
  sequence = list(name = "Sequence number", help = "No inputs: 1, 2, … within subject after sorting.",
    find = function(target, info) if (info$kinds[[target]] == "numeric") list(character()),
    apply = function(rule, frame) as.numeric(stats::ave(frame[[rule$target]] %||% seq_len(nrow(frame)),
      frame$USUBJID, FUN = function(s) rank(s, ties.method = "first"))),
    label = function(rule, input) "Numbered 1, 2, … within subject"),
  # SDTM study days have no day 0: the reference date is day 1.
  study_day = list(name = "Study day", help = "Two inputs: the date, then the reference date.", paired = TRUE,
    find = function(target, info) if (info$kinds[[target]] == "numeric") date_pairs(info$record_dates, info$dm_dates),
    apply = function(rule, frame) {
      gap <- as.numeric(frame[[rule$inputs[1]]] - frame[[rule$inputs[2]]])
      gap + (gap >= 0)
    },
    label = function(rule, input) sprintf("Study day of %s from %s", input[1], input[2])),
  # The source's label for each class; SDTM terms for classes it never shows.
  range_flag = list(name = "Range flag", help = "Three inputs: value, lower limit, upper limit.", paired = TRUE,
    find = function(target, info) if (info$values[[target]] %in% 2:4) info$ranges,
    learn = function(rule, frame) {
      class <- range_class(rule, frame)
      y <- frame[[rule$target]]
      labels <- vapply(c(low = "low", normal = "normal", high = "high", none = "none", missing = "missing"),
        function(k) as.character(most_common(y[class %in% k])), "")
      defaults <- c(low = "LOW", normal = "NORMAL", high = "HIGH", none = NA, missing = NA)
      list(labels = ifelse(is.na(labels), defaults, labels))
    },
    apply = function(rule, frame) unname(rule$params$labels[range_class(rule, frame)]),
    label = function(rule, input) sprintf("%s against %s–%s", input[1], input[2], input[3])),
  # The last record per subject and test dated on or before the reference: a baseline flag.
  record_flag = list(name = "Record selection",
    help = "Two inputs: the record date, then the reference date. Flags the last record per subject and test on or before the reference.",
    find = function(target, info) if (info$values[[target]] == 1 && info$missing[[target]])
      date_pairs(info$record_dates, info$dm_dates),
    learn = function(rule, frame) {
      s <- attr(frame, "structure")
      list(value = most_common(frame[[rule$target]]), by = c("USUBJID", setdiff(s$keys, s$visit)))
    },
    apply = function(rule, frame) {
      date <- frame[[rule$inputs[1]]]
      reference <- frame[[rule$inputs[2]]]
      eligible <- which(!is.na(date) & !is.na(reference) & date <= reference)
      group <- group_index(frame, rule$params$by)
      latest <- eligible[order(group[eligible], as.numeric(date[eligible]), decreasing = TRUE)]
      values <- rep(NA, nrow(frame))
      values[latest[!duplicated(group[latest])]] <- rule$params$value
      values
    },
    label = function(rule, input) sprintf("\"%s\" on the last record per %s with %s on or before %s",
      rule$params$value, paste(display_name(rule$params$by), collapse = ", "), input[1], input[2])),
  date_offset = list(name = "Date offset", help = "One date input.", paired = TRUE,
    find = function(target, info) if (info$kinds[[target]] == "date") as.list(setdiff(info$dates, target)),
    learn = function(rule, frame) {
      gap <- as.numeric(frame[[rule$target]] - frame[[rule$inputs]])
      list(days = if (all(is.na(gap))) 0 else most_common(gap))
    },
    apply = function(rule, frame) frame[[rule$inputs]] + rule$params$days,
    label = function(rule, input) if (rule$params$days == 0) paste("Same date as", input) else
      sprintf("%s %+d days", input, rule$params$days)),
  # Per test: y = x × factor (+ offset when a factor alone does not reproduce the group,
  # e.g. Celsius to Fahrenheit), rounded to the target's decimals. A pure factor is tried
  # first, then a fitted line, each with the fewest significant digits that reproduce the
  # group best, so 1.8 is found rather than 1.80012. Inputs are numbers, so newly sampled
  # values convert too.
  convert = list(name = "Unit conversion", help = "One numeric input; factor (and offset if needed) are learned per test.",
    paired = TRUE, groups = function(rule) rule$params$by,
    find = function(target, info) if (info$numeric_like[[target]]) info$conversions[[target]],
    learn = function(rule, frame) {
      by <- attr(frame, "structure")$test
      x <- parse_number(frame[[rule$inputs]])
      y <- frame[[rule$target]]
      y_number <- parse_number(y)
      group <- group_index(frame, by)
      rows <- lapply(split(seq_along(y), group), function(i) {
        digits <- decimals(y[i])
        ok <- i[is.finite(x[i]) & is.finite(y_number[i]) & x[i] != 0]
        share <- function(k) mean(abs(round(x[ok] * k[1] + k[2], digits) - y_number[ok]) <= 1e-8 * pmax(1, abs(y_number[ok])))
        exact <- c(if (length(ok)) stats::median(y_number[ok] / x[ok]) else 1, 0)
        if (length(unique(x[ok])) > 1 && share(exact) < 0.9) exact <- unname(stats::coef(stats::lm(y_number[ok] ~ x[ok])))[2:1]
        options <- lapply(3:6, function(d) signif(exact, d))
        shares <- vapply(options, share, 0)
        best <- options[[which(shares >= max(shares) - 1e-9)[1]]]
        text <- y[i][!is.na(y_number[i])]
        # Text results keep trailing zeros ("5.10") only when the source does.
        fixed <- is.character(y) && digits > 0 && length(text) && mean(grepl(sprintf("\\.[0-9]{%d}$", digits), text)) >= 0.5
        data.frame(factor = best[1], offset = best[2], digits = digits, fixed = fixed)
      })
      list(by = by, text = is.character(y), table = cbind(frame[match(seq_along(rows), group), by, drop = FALSE], do.call(rbind, rows), row.names = NULL))
    },
    apply = function(rule, frame) {
      p <- rule$params
      index <- if (is.null(p$by)) rep(1L, nrow(frame)) else match(format_value(frame[[p$by]]), format_value(p$table[[p$by]]))
      digits <- p$table$digits[index]
      values <- round(parse_number(frame[[rule$inputs]]) * p$table$factor[index] + p$table$offset[index], digits)
      if (!isTRUE(p$text)) return(values)
      text <- sprintf("%.*f", ifelse(is.na(digits), 0L, digits), values)
      trim <- !p$table$fixed[index] & grepl(".", text, fixed = TRUE)
      text[trim] <- sub("\\.?0+$", "", text[trim])
      ifelse(is.na(values), NA, text)
    },
    label = function(rule, input) {
      p <- rule$params
      offset <- any(p$table$offset != 0)
      if (is.null(p$by) && isTRUE(all.equal(p$table$factor, 1)) && !offset) return(paste("Copy of", input))
      paste0(input, " × factor", if (offset) " + offset", if (!is.null(p$by)) paste(" by", p$by))
    }),
  lookup = list(name = "Lookup", help = "One or more key variables; the table is learned from the source.",
    groups = function(rule) rule$inputs,
    find = function(target, info) {
      y <- group_index(info$frame, target)
      good <- vapply(info$key_groups, function(group) {
        fit <- lookup_fit(group, y)
        fit$checked >= 2 && isTRUE(fit$share >= SHARE_TO_PROPOSE - info$slack) && isTRUE(fit$explained >= EXPLAINED_TO_PROPOSE - info$slack)
      }, TRUE)
      info$keys[good]
    },
    learn = function(rule, frame) {
      fit <- lookup_fit(group_index(frame, rule$inputs), group_index(frame, rule$target))
      table <- frame[fit$first, rule$inputs, drop = FALSE]
      table$value <- frame[[rule$target]][fit$first]
      rownames(table) <- NULL
      list(table = table)
    },
    apply = function(rule, frame) {
      table <- rule$params$table
      table$value[match(key_strings(frame[rule$inputs]), key_strings(table[rule$inputs]))]
    },
    label = function(rule, input) paste("Lookup by", paste(input, collapse = " and "))),
  # User expressions may only call the functions in EXPRESSION_FUNCTIONS.
  expression = list(name = "R expression", help = "Use variable names directly. Inputs are found from the expression.",
    paired = TRUE, find = function(target, info) NULL,
    apply = function(rule, frame) {
      expression <- parse(text = rule$params$text)
      unsupported <- setdiff(setdiff(all.names(expression), all.vars(expression)), EXPRESSION_FUNCTIONS)
      if (length(unsupported)) stop("Unsupported function in expression: ", paste(unsupported, collapse = ", "))
      values <- eval(expression[[1]], envir = as.list(frame), enclos = baseenv())
      if (length(values) == 1) rep(values, nrow(frame)) else values
    },
    label = function(rule, input) rule$params$text))

# Rules the SDTM standard defines, known before the data are read. "--" is the dataset
# prefix (AE in AE) and "DM." a DM variable; "-" means no inputs, and an expression lists
# its own. A standard rule is one rule named in advance rather than the best of many tried,
# so it is accepted when it reproduces SHARE_TO_PROPOSE of its checked records, without the
# search's test against the most common value. The first rule that holds for a target wins;
# the search covers the targets left.
STANDARD_RULES <- utils::read.table(header = TRUE, stringsAsFactors = FALSE, text = "
  target    op           inputs
  STUDYID   constant     -
  DOMAIN    constant     -
  --SEQ     sequence     -
  VISIT     lookup       VISITNUM
  VISITDY   lookup       VISITNUM
  ARM       lookup       ARMCD
  ACTARM    lookup       ACTARMCD
  DTHFL     expression   'ifelse(is.na(DTHDTC), NA, \"Y\")'
  --DY      study_day    --DTC,DM.RFSTDTC
  --STDY    study_day    --STDTC,DM.RFSTDTC
  --ENDY    study_day    --ENDTC,DM.RFSTDTC
  --TEST    lookup       --TESTCD
  --CAT     lookup       --TESTCD
  --SCAT    lookup       --TESTCD
  --STRESU  lookup       --TESTCD
  --STRESC  convert      --STRESN
  --ORRES   convert      --STRESN
  --ORNRLO  convert      --STNRLO
  --ORNRHI  convert      --STNRHI
  --NRIND   range_flag   --STRESN,--STNRLO,--STNRHI
  --BLFL    record_flag  --DTC,DM.RFSTDTC
  --STAT    expression   'ifelse(is.na(--ORRES), \"NOT DONE\", NA)'
  --TPTNUM  lookup       --TPT
  --ELTM    lookup       --TPT
  --TPTREF  lookup       --TPT
  --LLT     lookup       --TERM
  --DECOD   lookup       --LLT
  --DECOD   lookup       --TERM
  --DECOD   lookup       --TRT
  --LLTCD   lookup       --LLT
  --PTCD    lookup       --DECOD
  --HLT     lookup       --DECOD
  --HLTCD   lookup       --HLT
  --HLGT    lookup       --HLT
  --HLGTCD  lookup       --HLGT
  --BODSYS  lookup       --DECOD
  --BDSYCD  lookup       --BODSYS
  --SOC     lookup       --BODSYS
  --SOCCD   lookup       --SOC
  --CLAS    lookup       --DECOD
  --CLASCD  lookup       --CLAS
")

new_rule <- function(target, op, inputs = character(), params = list(), origin = "found") {
  list(target = target, op = op, inputs = inputs, params = params, origin = origin,
    status = "proposed", share = NA_real_, checked = 0L, explained = NA_real_, by_test = NULL)
}

# Columns a rule reads: its inputs, plus the columns its table is keyed by.
rule_columns <- function(rule) unique(c(rule$inputs, rule$params$by))

rule_label <- function(rule) RULE_OPS[[rule$op]]$label(rule, display_name(rule$inputs))

display_name <- function(x) sub("^DM\\.", "", sub("^N\\.(.*)$", "number of \\1 records", x))

# Learn the rule's parameters from the frame (unless the user edited them), apply it, and
# record the share of checked records it reproduces, overall and per test.
evaluate_rule <- function(rule, frame) {
  op <- RULE_OPS[[rule$op]]
  y <- frame[[rule$target]]
  rule$error <- NULL
  values <- tryCatch({
    missing <- setdiff(rule_columns(rule), names(frame))
    if (length(missing)) stop("Missing input: ", paste(missing, collapse = ", "))
    if (!isTRUE(rule$params$edited) && !is.null(op$learn)) rule$params <- utils::modifyList(rule$params, op$learn(rule, frame))
    values <- op$apply(rule, frame)
    if (length(values) != nrow(frame)) stop("The rule must return one value per record.")
    values
  }, error = function(e) {
    rule$error <<- conditionMessage(e)
    rep(NA, nrow(frame))
  })
  match <- same_value(values, y) & is.null(rule$error)
  group <- group_index(frame, if (!is.null(op$groups)) op$groups(rule))
  checked <- tabulate(group)[group] >= 2
  if (isTRUE(op$paired)) checked <- checked & !(is.na(values) & is.na(y))
  rule$share <- if (any(checked)) mean(match[checked]) else NA_real_
  rule$checked <- sum(checked)
  code <- match(y, unique(y))[checked]
  rule$explained <- explained(rule$share, max(tabulate(code), 0) / length(code))
  test <- attr(frame, "structure")$test
  tests <- if (!is.null(test)) format_value(frame[[test]][checked])
  rule$by_test <- if (length(tests)) data.frame(test = names(table(tests)), records = as.vector(table(tests)),
    share = as.vector(tapply(match[checked], tests, mean)))
  rule
}

# The most common value, keeping its type (a date stays a date).
most_common <- function(y) {
  observed <- y[!is.na(y)]
  if (!length(observed)) return(NA)
  text <- format_value(observed)
  counts <- table(text)
  observed[match(names(counts)[which.max(counts)], text)]
}

# Every ordered pair of distinct columns, one from each set.
date_pairs <- function(first, second) {
  grid <- expand.grid(first = first, second = second, stringsAsFactors = FALSE)
  grid <- grid[grid$first != grid$second, ]
  unname(Map(c, grid$first, grid$second))
}

range_class <- function(rule, frame) {
  x <- lapply(rule$inputs, function(name) parse_number(frame[[name]]))
  class <- ifelse(!is.na(x[[2]]) & x[[1]] < x[[2]], "low", ifelse(!is.na(x[[3]]) & x[[1]] > x[[3]], "high", "normal"))
  class[is.na(x[[2]]) & is.na(x[[3]])] <- "none"
  class[is.na(x[[1]])] <- "missing"
  class
}

# Share of the records the most common value (`base` of them) gets wrong that a rule reproduces.
explained <- function(share, base) if (isTRUE(base < 1)) (share - base) / (1 - base) else share

# For each key group, its most common target code. `first` is the first record holding
# that value; the shares count only groups with at least two records.
lookup_fit <- function(group, y_code) {
  pair <- (group - 1) * (max(y_code) + 1) + y_code
  unique_pair <- unique(pair)
  first <- match(unique_pair, pair)
  count <- tabulate(match(pair, unique_pair), length(unique_pair))
  pair_group <- group[first]
  best <- order(pair_group, -count)
  best <- best[!duplicated(pair_group[best])]
  size <- tabulate(group)
  checked_group <- size >= 2
  total <- sum(size[checked_group])
  matched <- sum(count[best][checked_group[pair_group[best]]])
  share <- if (total) matched / total else NA
  list(first = first[best], share = share, checked = total,
    explained = explained(share, if (total) max(tabulate(y_code[checked_group[group]])) / total))
}

# ---- Discovery -------------------------------------------------------------------------

# Records of randomly chosen whole subjects (sequence and record-selection rules work within
# subject) until `cap` records are reached; the whole frame when it is not larger. The
# choice is fixed, and leaves the user's random numbers where they were.
discovery_sample <- function(frame, cap) {
  if (nrow(frame) <= cap) return(frame)
  subject <- match(frame$USUBJID, unique(frame$USUBJID))
  seed <- get0(".Random.seed", globalenv(), inherits = FALSE)
  on.exit(if (is.null(seed)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", seed, globalenv()))
  set.seed(1, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  chosen <- sample(max(subject))
  chosen <- chosen[seq_len(which(cumsum(tabulate(subject)[chosen]) >= cap)[1])]
  sampled <- frame[subject %in% chosen, , drop = FALSE]
  rownames(sampled) <- NULL
  attr(sampled, "structure") <- attr(frame, "structure")
  sampled
}

# Rules for a dataset frame. `targets` are the dataset's own variables; every frame column
# may serve as an input. Standard rules that hold are accepted first; the other targets are
# searched. Search candidates per target are ranked by share, then operation preference,
# then checked records and fewer inputs, and are proposed for review. Above DISCOVERY_RECORDS
# records the search runs on a sample of subjects and the shortlisted candidates are then
# checked, and their tables learned, on every record.
#
# Mutual rules (ARMCD from ARM and ARM from ARMCD) cannot both hold in a generator. Later
# columns choose first, since SDTM lists derived variables after their sources; a target
# whose best rule would close a cycle takes its next candidate, or none.
discover_rules <- function(frame, targets, name) {
  targets <- targets[vapply(frame[targets], function(v) any(!is.na(v)), TRUE)]
  if (nrow(frame) < 2 || !length(targets)) return(list())
  chosen <- list()
  for (i in seq_len(nrow(STANDARD_RULES))) {
    row <- lapply(STANDARD_RULES[i, ], function(x) {
      x <- gsub("--", name, x, fixed = TRUE)
      if (name == "DM") gsub("DM.", "", x, fixed = TRUE) else x
    })
    expression <- row$op == "expression"
    inputs <- if (expression) all.vars(parse(text = row$inputs)) else setdiff(strsplit(row$inputs, ",")[[1]], "-")
    if (!row$target %in% setdiff(targets, names(chosen)) || !all(inputs %in% names(frame))) next
    rule <- evaluate_rule(new_rule(row$target, row$op, inputs, if (expression) list(text = row$inputs) else list(), "standard"), frame)
    rule$status <- "accepted"
    if (isTRUE(rule$share >= SHARE_TO_PROPOSE) && rule$checked >= 2) chosen[[row$target]] <- rule
  }
  searched <- setdiff(targets, names(chosen))
  # What a column is (kind, number of values, record numbering) is read from every record,
  # which is cheap: a rare value or a few unnumbered subjects must not change it. The
  # candidates are searched on `frame`, the sample.
  full <- frame
  frame <- discovery_sample(full, getOption("synth.discovery_records", DISCOVERY_RECORDS))
  sampled <- nrow(frame) < nrow(full)
  s <- attr(frame, "structure")
  columns <- names(frame)
  kinds <- vapply(full, value_kind, "")
  count_values <- function(x) vapply(x, function(v) length(unique(v[!is.na(v)])), 0L)
  values <- count_values(full)
  numeric_like <- vapply(full, function(v) is.numeric(v) || mean(!is.na(parse_number(unique(v[!is.na(v)])))) >= 0.9, TRUE)
  # Arithmetic inputs are the record's own numbers (not DM.* or N.*).
  numbers <- columns[kinds == "numeric" & !grepl("^(DM|N)\\.", columns)]
  # Lookup keys are categories shared by subjects: a table keyed by a measured value or by
  # a subject's own code (SUBJID) cannot cover newly generated values, and one keyed by a
  # record number (--SEQ, numbering each subject's records 1..n) only reflects the sort order.
  subject_values <- vapply(columns, function(v) max(group_index(frame, c("USUBJID", v))), 0L)
  numbered <- vapply(full, function(v) is.numeric(v) && !anyNA(v) &&
    all(stats::ave(v, full$USUBJID, FUN = function(s) rank(s, ties.method = "first")) == v), TRUE)
  # Subjects per value are compared with the values the sample shows, not with the whole.
  is_key <- kinds != "date" & !(numeric_like & values > 50) & values >= 2 &
    subject_values >= 2 * (if (sampled) count_values(frame) else values) & columns != "USUBJID" & !numbered
  keys <- as.list(columns[is_key])
  # Per-test tables that also differ by a subject characteristic (ranges by sex).
  if (!is.null(s$test)) keys <- c(keys, lapply(grep("^DM\\.", columns[is_key], value = TRUE), function(k) c(s$test, k)))
  limits <- Filter(function(p) {
    lo <- frame[[p[1]]]
    hi <- frame[[p[2]]]
    both <- !is.na(lo) & !is.na(hi)
    sum(both) >= 2 && all(lo[both] <= hi[both]) && any(lo[both] < hi[both])
  }, date_pairs(numbers, numbers))
  ranges <- Filter(function(r) mean(frame[[r[1]]] >= frame[[r[2]]] & frame[[r[1]]] <= frame[[r[3]]], na.rm = TRUE) >= 0.5,
    unlist(lapply(limits, function(p) lapply(setdiff(numbers, p), c, p)), recursive = FALSE))
  coded <- columns[is_key & values < 20]
  conversions <- lapply(setNames(searched, searched), function(target) {
    parsed <- !is.na(parse_number(frame[[target]]))
    usable <- vapply(numbers, function(x) sum(!is.na(frame[[x]]) & parsed) >= 2, TRUE)
    as.list(setdiff(numbers[usable], c(target, coded)))
  })
  info <- list(frame = frame, kinds = kinds, values = values, numeric_like = numeric_like,
    missing = vapply(full, anyNA, TRUE), keys = keys, key_groups = lapply(keys, group_index, x = frame),
    dates = columns[kinds == "date"], dm_dates = grep("^DM\\.", columns[kinds == "date"], value = TRUE),
    record_dates = grep("^(DM|SV)\\.", columns[kinds == "date"], value = TRUE, invert = TRUE),
    ranges = ranges, conversions = conversions, slack = if (sampled) DISCOVERY_SLACK else 0)
  proposed <- function(r, slack = 0) isTRUE(r$share >= SHARE_TO_PROPOSE - slack && r$explained >= EXPLAINED_TO_PROPOSE - slack) && r$checked >= 2
  # `rows` coarsens the checked records to a share of the frame: a sample's counts differ by
  # a few records between candidates that tie on every record, and enumeration order then decides.
  ranked <- function(options, rows = NULL) {
    checked <- vapply(options, `[[`, 0, "checked")
    order(-round(vapply(options, `[[`, 0, "share"), 3), match(vapply(options, `[[`, "", "op"), names(RULE_OPS)),
      -if (is.null(rows)) checked else round(checked / rows, 2), lengths(lapply(options, `[[`, "inputs")))
  }
  candidates <- list()
  for (target in searched) for (op in names(RULE_OPS)) {
    inputs <- Filter(function(i) !target %in% i, RULE_OPS[[op]]$find(target, info))
    found <- lapply(inputs, function(i) evaluate_rule(new_rule(target, op, i), frame))
    candidates <- c(candidates, Filter(function(r) proposed(r, info$slack), found))
  }
  # The sample only shortlists: its best candidates per target are evaluated again on every
  # record, so shares, checked records and learned tables (lookups) are the whole data's.
  if (sampled) candidates <- unlist(lapply(searched, function(target) {
    options <- Filter(function(r) r$target == target, candidates)
    shortlist <- sort(utils::head(ranked(options, nrow(frame)), getOption("synth.discovery_shortlist", DISCOVERY_SHORTLIST)))
    Filter(proposed, lapply(options[shortlist], function(r) evaluate_rule(new_rule(target, r$op, r$inputs), full)))
  }), recursive = FALSE)
  for (target in rev(searched)) {
    options <- Filter(function(r) r$target == target, candidates)
    chosen[[target]] <- Find(function(r) !target %in% reachable(rule_columns(r),
      function(v) unlist(lapply(chosen[intersect(v, names(chosen))], rule_columns))), options[ranked(options)])
  }
  chosen[intersect(targets, names(chosen))]
}
