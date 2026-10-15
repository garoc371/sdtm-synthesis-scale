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
#
# A lookup table is scored on subjects it was not learned from: learned on half of the
# subjects, checked on the other half, and the other way round. Otherwise a table of many
# keys memorises which keys happened to carry a rare value (the AE terms that were
# hospitalised) and reproduces the source without predicting anything.

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
        # A test without a number on both sides (all "NEGATIVE") keeps the plain copy.
        shares <- vapply(options, share, 0)
        shares[is.na(shares)] <- 0
        best <- options[[which(shares >= max(shares) - 1e-9)[1]]]
        text <- y[i][!is.na(y_number[i])]
        # Text results keep trailing zeros ("5.10") only when the source has any; without them
        # ("1.01" beside "1.003") the shortest form is written.
        fixed <- is.character(y) && digits > 0 && any(grepl("\\.[0-9]*0$", text))
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
      # A result that is not a number ("PRESENT", "<5") is carried over as written.
      ifelse(is.na(values), as.character(frame[[rule$inputs]]), text)
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
      subject <- record_subject(info$frame)
      good <- vapply(info$key_groups, function(group) {
        fit <- lookup_fit(group, y, subject)
        fit$checked >= 2 && isTRUE(fit$share >= SHARE_TO_PROPOSE - info$slack) && isTRUE(fit$explained >= EXPLAINED_TO_PROPOSE - info$slack)
      }, TRUE)
      info$keys[good]
    },
    learn = function(rule, frame) {
      group <- group_index(frame, rule$inputs)
      y <- group_index(frame, rule$target)
      first <- lookup_rows(group, y)
      subject <- record_subject(frame)
      # A key and value held together by fewer than min_subjects() subjects is not kept, unless
      # the key is only on records dated on the death date and every subject who died holds
      # them (DEATH; not a cause of death), or is a death feature (DTHFL, see DEATH_KEY). A
      # value of at least min_subjects() subjects whose keys are all dropped becomes its own
      # key, as a verbatim too rare to draw is drawn as its coded value (see `fit_node()`).
      death <- death_records(frame, rule$inputs)
      died <- length(unique(subject[death]))
      pair <- subjects_holding(paste(group, y), subject)[first]
      held <- pair >= min_subjects() | tapply(death, group, all)[as.character(group[first])] %in% TRUE &
        (all(grepl(DEATH_KEY, rule$inputs)) | pair == died & died > 1)
      own <- first[!held & subjects_holding(y, subject)[first] >= min_subjects() & !y[first] %in% y[first[held]]]
      own <- own[!duplicated(y[own])]
      first <- first[held]
      table <- frame[first, rule$inputs, drop = FALSE]
      table$value <- frame[[rule$target]][first]
      if (length(own) && length(rule$inputs) == 1 && is.character(frame[[rule$inputs]]) && is.character(table$value)) {
        own <- own[!frame[[rule$target]][own] %in% table[[rule$inputs]]]
        table <- rbind(table, stats::setNames(data.frame(frame[[rule$target]][own], frame[[rule$target]][own], stringsAsFactors = FALSE), c(rule$inputs, "value")))
      }
      rownames(table) <- NULL
      list(table = table)
    },
    apply = function(rule, frame) {
      table <- rule$params$table
      table$value[match(key_strings(frame[rule$inputs]), key_strings(table[rule$inputs]))]
    },
    held_out = function(rule, frame) {
      group <- group_index(frame, rule$inputs)
      y <- group_index(frame, rule$target)
      predicted <- held_out_prediction(group, y, record_subject(frame))
      list(match = !is.na(predicted) & predicted == y, checked = !is.na(predicted))
    },
    label = function(rule, input) paste("Lookup by", paste(input, collapse = " and "))),
  # Some tests are calculated from the other records of their group (see `derivation_groups()`):
  # a total from its items, a half-life from the rate constant, an overall response from the
  # target and non-target responses. The variable keeps its own rule or model for the other
  # tests, and for records whose group lacks the inputs. Found only, kept beside the
  # variable's own rule (`group_rule_name()`).
  group = list(name = "From other records", help = "Found only: the test, then the columns of the group.",
    find = function(target, info) NULL,
    learn = function(rule, frame) list(derivations = find_derivations(frame, rule$target, rule$inputs[1], rule$inputs[-1], rule$params$skip)),
    apply = function(rule, frame) {
      y <- frame[[rule$target]]
      tests <- format_value(frame[[rule$inputs[1]]])
      group <- group_index(frame, rule$inputs[-1])
      for (d in rule$params$derivations) {
        r <- derive(d, y, tests, group)
        hit <- !is.na(r$value)
        y[r$rows[hit]] <- if (d$kind == "lookup") r$value[hit] else if (is.numeric(y)) round(r$value[hit], d$digits) else
          number_text(r$value[hit], d$fixed, d$digits)
      }
      y
    },
    held_out = function(rule, frame) {
      match <- checked <- rep(FALSE, nrow(frame))
      y <- frame[[rule$target]]
      tests <- format_value(frame[[rule$inputs[1]]])
      group <- group_index(frame, rule$inputs[-1])
      subject <- record_subject(frame)
      for (d in rule$params$derivations) {
        r <- derive(d, y, tests, group, subject)
        checked[r$rows] <- !is.na(r$value)
        match[r$rows] <- if (d$kind == "lookup") !is.na(r$value) & r$value == level_string(y[r$rows]) else
          close_number(r$value, parse_number(y[r$rows]), d$digits)
      }
      list(match = match, checked = checked)
    },
    label = function(rule, input) {
      text <- vapply(rule$params$derivations, derivation_label, "")
      paste0("Per ", paste(input[-1], collapse = ", "), ": ", paste(utils::head(text, 3), collapse = "; "),
        if (length(text) > 3) sprintf("; %d more tests", length(text) - 3))
    }),
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
  if (!is.null(op$held_out) && is.null(rule$error) && !isTRUE(rule$params$edited)) {
    held <- op$held_out(rule, frame)
    match <- held$match
    checked <- held$checked
  }
  rule$share <- if (any(checked)) mean(match[checked]) else NA_real_
  rule$checked <- sum(checked)
  code <- match(y, unique(y))[checked]
  rule$explained <- explained(rule$share, max(tabulate(code), 0) / length(code))
  test <- attr(frame, "structure")$test
  tests <- if (!is.null(test)) format_value(frame[[test]][checked])
  counts <- if (length(tests)) table(tests)
  rule$by_test <- if (length(tests)) data.frame(test = names(counts), records = as.vector(counts),
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

# Subject number of each record; each record its own subject in a dataset without subjects.
record_subject <- function(frame) if ("USUBJID" %in% names(frame)) group_index(frame, "USUBJID") else seq_len(nrow(frame))

# For each record, the most common target code of its key group among the other half of the
# subjects (odd and even subject numbers); NA when no subject of the other half has the key.
held_out_prediction <- function(group, y_code, subject) {
  half <- subject %% 2 == 0
  predicted <- rep(NA_integer_, length(group))
  for (side in c(TRUE, FALSE)) {
    from <- half != side
    pair <- (group[from] - 1) * (max(y_code) + 1) + y_code[from]
    values <- sort(unique(pair))
    count <- tabulate(match(pair, values), length(values))
    pair <- values
    best <- order(pair %/% (max(y_code) + 1), -count)
    best <- best[!duplicated(pair[best] %/% (max(y_code) + 1))]
    lookup <- rep(NA_integer_, max(group))
    lookup[pair[best] %/% (max(y_code) + 1) + 1] <- as.integer(pair[best] %% (max(y_code) + 1))
    predicted[half == side] <- lookup[group[half == side]]
  }
  predicted
}

# For each key group, the first record holding its most common target code.
lookup_rows <- function(group, y_code) {
  pair <- (group - 1) * (max(y_code) + 1) + y_code
  unique_pair <- unique(pair)
  first <- match(unique_pair, pair)
  count <- tabulate(match(pair, unique_pair), length(unique_pair))
  pair_group <- group[first]
  best <- order(pair_group, -count)
  first[best[!duplicated(pair_group[best])]]
}

# Shares of the records predicted from the other half of the subjects.
lookup_fit <- function(group, y_code, subject) {
  predicted <- held_out_prediction(group, y_code, subject)
  checked <- !is.na(predicted)
  total <- sum(checked)
  share <- if (total) mean(predicted[checked] == y_code[checked]) else NA
  list(share = share, checked = total, explained = explained(share, if (total) max(tabulate(y_code[checked])) / total))
}

# ---- Calculations from other records ----------------------------------------------------

# A test is calculated from the other tests of its group when the group's records give it
# for SHARE_TO_PROPOSE of at least DERIVATION_RECORDS records. Lookups read tests with at
# most DERIVATION_LEVELS values, and use at most DERIVATION_INPUTS of them.
DERIVATION_RECORDS <- 10
DERIVATION_LEVELS <- 20
DERIVATION_INPUTS <- 3

# A rule calculating some tests of a variable from other records is kept beside the
# variable's own rule, under this name.
group_rule_name <- function(variable) paste(variable, "from other records")

group_rule <- function(ds, variable) {
  rule <- ds$rules[[group_rule_name(variable)]]
  if (!is.null(rule) && rule$status == "accepted") rule
}

# A rule's values, with the tests the variable's group rule calculates replaced.
rule_values <- function(ds, rule, frame) {
  values <- RULE_OPS[[rule$op]]$apply(rule, frame)
  group <- group_rule(ds, rule$target)
  if (rule$op == "group" || is.null(group)) return(values)
  frame[[rule$target]] <- values
  RULE_OPS$group$apply(group, frame)
}

# The records of a group: the subject's records at one visit, and of one category, specimen,
# lesion group and evaluator where the dataset has these. A column recorded for some tests
# and never for others (RSLNKGRP only on the overall response) would split the group.
derivation_groups <- function(frame, target, prefix) {
  s <- attr(frame, "structure")
  tests <- format_value(frame[[s$test]])
  columns <- intersect(c("USUBJID", s$visit, paste0(prefix, c("CAT", "SPEC", "LNKGRP", "EVAL", "EVALID"))), setdiff(names(frame), c(target, s$test)))
  Filter(function(v) {
    present <- tapply(!is.na(frame[[v]]), tests, mean)
    !(min(present) == 0 && max(present) > 0)
  }, columns)
}

# Numbers written with `digits` decimals agree when they round alike; numbers calculated
# from rounded inputs (a half-life from a rounded rate constant) agree to 1e-6.
close_number <- function(value, y, digits) {
  ok <- abs(value - y) <= pmax(0.5 * 10^-digits, 1e-6 * abs(y)) + 1e-9
  !is.na(ok) & ok
}

# Per group: the sum of the numbers of the records whose test is in `inputs`, how many
# there are, and how many records.
group_numbers <- function(x, tests, inputs, group, n) {
  records <- which(tests %in% inputs)
  use <- records[!is.na(x[records])]
  sum <- numeric(n)
  if (length(use)) {
    s <- rowsum(x[use], group[use])
    sum[as.integer(rownames(s))] <- s[, 1]
  }
  list(sum = sum, count = tabulate(group[use], n), records = tabulate(group[records], n))
}

# Per group, the value of each input test (a column per test); NA where the group has more
# than one record of the test (a lesion group's diameters).
group_values <- function(y, tests, inputs, group, n) {
  t <- match(tests, inputs)
  i <- which(!is.na(t))
  cell <- group[i] + (t[i] - 1) * n
  out <- matrix(MISSING, n, length(inputs), dimnames = list(NULL, inputs))
  out[cell] <- level_string(y[i])
  out[cell[duplicated(cell)]] <- NA
  out
}

# One key per row of group values; NA when one is ambiguous or all are missing.
values_key <- function(values) {
  key <- do.call(paste, c(lapply(seq_len(ncol(values)), function(k) values[, k]), sep = "\r"))
  key[rowSums(is.na(values)) > 0 | rowSums(values == MISSING, na.rm = TRUE) == ncol(values)] <- NA
  key
}

# The value each record's key gives among the other half of the subjects (see
# `held_out_prediction()`); NA where it gives none, or a missing value.
held_out_lookup <- function(key, level, subject) {
  value <- rep(NA_character_, length(key))
  ok <- !is.na(key)
  if (!any(ok)) return(value)
  levels <- unique(level[ok])
  value[ok] <- levels[held_out_prediction(match(key[ok], unique(key[ok])), match(level[ok], levels), subject[ok])]
  value[value %in% MISSING] <- NA
  value
}

# The derivation's value for each record of its test (`rows`), NA where its group lacks the
# inputs. With `subject`, a lookup gives the value as text, learned from the other half of
# the subjects.
derive <- function(d, y, tests, group, subject = NULL) {
  rows <- which(tests == d$test)
  n <- max(group, 0L)
  g <- group[rows]
  if (d$kind == "lookup") {
    key <- values_key(group_values(y, tests, d$inputs, group, n)[g, , drop = FALSE])
    return(list(rows = rows, value = if (is.null(subject)) d$values[match(key, d$keys)] else
      held_out_lookup(key, level_string(y[rows]), subject[rows])))
  }
  numbers <- group_numbers(parse_number(y), tests, d$inputs, group, n)
  x <- numbers$sum[g]
  value <- switch(d$kind, sum = x, linear = d$a * x + d$b, ratio = d$a / x)
  value[!is.finite(value) | if (d$kind == "sum") numbers$count[g] == 0 else numbers$count[g] != 1 | numbers$records[g] != 1] <- NA
  list(rows = rows, value = value)
}

derivation_label <- function(d) {
  inputs <- if (length(d$inputs) > 3) sprintf("%s, … (%d tests)", paste(d$inputs[1:3], collapse = ", "), length(d$inputs)) else
    paste(d$inputs, collapse = ", ")
  paste(d$test, switch(d$kind,
    sum = paste("= sum of", inputs),
    linear = if (d$a == 1 && d$b == 0) paste("= copy of", inputs) else sprintf("= %s × %s%s", format(d$a), inputs,
      if (d$b != 0) sprintf(" %s %s", if (d$b > 0) "+" else "−", format(abs(d$b))) else ""),
    ratio = sprintf("= %s / %s", format(d$a), inputs),
    lookup = sprintf("by %s (%d values)", inputs, length(d$keys))))
}

# Calculations of tests from the other records of their group, searched per test (except
# `skip`, the tests the variable's own rule gives): the sum of some other tests (least squares
# picks the tests, then single tests are added or dropped while that helps), a line through
# or ratio to one other test, or a lookup by other tests' values. Each is scored by the records
# it gives right less SHARE_TO_PROPOSE / (1 - SHARE_TO_PROPOSE) times those it gives wrong;
# lookups on subjects they were not learned from. A test whose calculation would use itself
# through others takes none; the calculations scoring highest choose first, then later tests.
find_derivations <- function(frame, target, test, groups, skip = NULL) {
  y <- frame[[target]]
  if (inherits(y, "Date") || !test %in% names(frame)) return(list())
  tests <- format_value(frame[[test]])
  group <- group_index(frame, groups)
  subject <- record_subject(frame)
  x <- parse_number(y)
  level <- level_string(y)
  codes <- unique(tests[!is.na(tests)])
  n <- max(group)
  t <- match(tests, codes)
  cell <- group + (t - 1) * n
  number <- which(!is.na(x) & !is.na(t))
  sums <- matrix(0, n, length(codes), dimnames = list(NULL, codes))
  if (length(number)) {
    s <- rowsum(x[number], cell[number])
    sums[as.integer(rownames(s))] <- s[, 1]
  }
  counts <- matrix(tabulate(cell[number], n * length(codes)), n, dimnames = list(NULL, codes))
  records <- matrix(tabulate(cell[!is.na(t)], n * length(codes)), n, dimnames = list(NULL, codes))
  values <- group_values(y, tests, codes, group, n)
  weight <- SHARE_TO_PROPOSE / (1 - SHARE_TO_PROPOSE)
  levels <- tapply(level, factor(t, seq_along(codes)), function(v) length(unique(v)))
  names(levels) <- codes
  found <- list()
  for (j in setdiff(codes, skip)) {
    rows <- which(tests == j)
    if (sum(!is.na(y[rows])) < DERIVATION_RECORDS || length(unique(y[rows][!is.na(y[rows])])) < 2) next
    g <- group[rows]
    yn <- x[rows]
    others <- setdiff(codes, j)
    digits <- decimals(y[rows])
    best <- NULL
    keep <- function(d, value, match) {
      checked <- !is.na(value)
      score <- sum(match & checked) - weight * sum(!match & checked)
      share <- if (any(checked)) mean(match[checked]) else 0
      common <- if (any(checked)) max(table(level[rows][checked])) / sum(checked) else 1
      if (sum(checked) >= DERIVATION_RECORDS && share >= SHARE_TO_PROPOSE && explained(share, common) >= EXPLAINED_TO_PROPOSE &&
        (is.null(best) || score > best$score)) best <<- c(d, list(score = score))
    }
    numeric <- sum(!is.na(yn)) >= DERIVATION_RECORDS
    if (numeric && length(others)) {
      S <- sums[g, others, drop = FALSE]
      C <- counts[g, others, drop = FALSE]
      # A sum of other tests.
      ok <- !is.na(yn)
      used <- others[colSums(C[ok, , drop = FALSE]) > 0]
      if (length(used) && sum(ok) >= 3 * length(used)) {
        beta <- qr.coef(qr(S[ok, used, drop = FALSE]), yn[ok])
        chosen <- others %in% used[!is.na(beta) & beta > 0.5]
        objective <- function(pred, count) {
          match <- close_number(pred, yn, digits)
          colSums(match & count > 0) - weight * colSums(!match & count > 0)
        }
        current <- objective(matrix(S %*% chosen), matrix(C %*% chosen))
        repeat {
          sign <- ifelse(chosen, -1, 1)
          options <- objective(as.vector(S %*% chosen) + sweep(S, 2, sign, `*`), as.vector(C %*% chosen) + sweep(C, 2, sign, `*`))
          if (max(options) <= current) break
          chosen[which.max(options)] <- !chosen[which.max(options)]
          current <- max(options)
        }
        if (any(chosen)) {
          d <- list(test = j, kind = "sum", inputs = others[chosen], digits = digits)
          value <- ifelse(as.vector(C %*% chosen) > 0, as.vector(S %*% chosen), NA)
          keep(d, value, close_number(value, yn, digits))
        }
      }
      # A line through, or a ratio to, one other test with one number in the group.
      for (k in others) {
        one <- C[, k] == 1 & records[g, k] == 1
        input <- ifelse(one, S[, k], NA)
        both <- which(!is.na(input) & !is.na(yn))
        if (length(both) < DERIVATION_RECORDS) next
        options <- list(c(1, 0))
        if (length(unique(input[both])) > 1) {
          line <- stats::lm.fit(cbind(input[both], 1), yn[both])$coefficients
          near <- both[abs(yn[both] - cbind(input[both], 1) %*% line) <= stats::median(abs(yn[both] - cbind(input[both], 1) %*% line))]
          if (length(unique(input[near])) > 1) line <- stats::lm.fit(cbind(input[near], 1), yn[near])$coefficients
          options <- c(options, list(unname(line)))
        }
        nonzero <- both[input[both] != 0]
        ratio <- if (length(nonzero)) stats::median(yn[nonzero] * input[nonzero])
        for (kind in c("linear", "ratio")) for (o in if (kind == "linear") options else if (!is.null(ratio)) list(ratio)) {
          if (anyNA(o)) next
          raw <- if (kind == "linear") o[1] * input[both] + o[2] else o[1] / input[both]
          if (!isTRUE(mean(abs(raw - yn[both]) <= 1e-3 * pmax(abs(yn[both]), 10^-digits), na.rm = TRUE) >= 0.5)) next
          for (p in 3:6) {
            a <- signif(o[1], p)
            b <- if (kind == "linear") signif(o[2], p) else 0
            value <- if (kind == "linear") a * input + b else ifelse(input != 0, a / input, NA)
            keep(list(test = j, kind = kind, inputs = k, a = a, b = b, digits = digits), value, close_number(value, yn, digits))
          }
        }
      }
    }
    # A lookup by other tests' values.
    candidates <- others[levels[others] <= DERIVATION_LEVELS]
    candidates <- candidates[colSums(values[g, candidates, drop = FALSE] != MISSING, na.rm = TRUE) >= DERIVATION_RECORDS]
    if (levels[[j]] <= 2.5 * DERIVATION_LEVELS && length(candidates)) {
      level_y <- level[rows]
      score <- function(inputs) {
        value <- held_out_lookup(values_key(values[g, inputs, drop = FALSE]), level_y, subject[rows])
        match <- !is.na(value) & value == level_y
        list(score = sum(match) - weight * sum(!is.na(value) & !match), value = value, match = match)
      }
      # Inputs are added one at a time, the best scoring first; a key of one input may be
      # wrong too often to pass, and right with a second. The best prefix is kept.
      inputs <- character()
      while (length(inputs) < DERIVATION_INPUTS && length(rest <- setdiff(candidates, inputs))) {
        options <- lapply(rest, function(k) score(c(inputs, k)))
        fit <- options[[which.max(vapply(options, `[[`, 0, "score"))]]
        inputs <- c(inputs, rest[which.max(vapply(options, `[[`, 0, "score"))])
        key <- values_key(values[g, inputs, drop = FALSE])
        keyed <- which(!is.na(key))
        first <- keyed[lookup_rows(match(key[keyed], unique(key[keyed])), match(level_y[keyed], unique(level_y[keyed])))]
        first <- first[!is.na(y[rows][first]) & subjects_holding(paste(key, level_y), subject[rows])[first] >= min_subjects()]
        keep(list(test = j, kind = "lookup", inputs = inputs, keys = key[first], values = y[rows][first], digits = digits), fit$value, fit$match)
      }
    }
    if (!is.null(best)) found[[j]] <- best
  }
  # Calculations from fewer tests choose first, then those giving more records right, then
  # later tests; one that would use its own test is dropped.
  kept <- list()
  for (j in names(found)[order(lengths(lapply(found, `[[`, "inputs")), -vapply(found, `[[`, 0, "score"), -match(names(found), codes))]) {
    if (!j %in% reachable(found[[j]]$inputs, function(v) unlist(lapply(kept[intersect(v, names(kept))], `[[`, "inputs"))))
      kept[[j]] <- found[[j]]
  }
  # Calculated in order: inputs first.
  order <- character()
  while (length(order) < length(kept)) {
    ready <- setdiff(names(kept)[vapply(kept, function(d) all(!d$inputs %in% setdiff(names(kept), order)), TRUE)], order)
    order <- c(order, ready)
  }
  lapply(kept[order], function(d) {
    d$score <- NULL
    d$fixed <- if (is.character(y) && d$kind != "lookup") number_format(y[tests %in% d$test & !is.na(x)])
    d
  })
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
# may serve as an input. Every target is searched; candidates per target are ranked by share, then operation preference,
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
  # A number with more values than a category model keeps is drawn anew (DM.AGE), unless it
  # is copied with the spine (VISITNUM, T.*) or counts records (N.*).
  drawn <- vapply(full, is.numeric, TRUE) & values > MAX_DISCRETE_VALUES & !columns %in% s$spine & !grepl("^(T|N)\\.", columns)
  subject_values <- vapply(columns, function(v) max(group_index(frame, c("USUBJID", v))), 0L)
  numbered <- vapply(full, function(v) is.numeric(v) && !anyNA(v) &&
    all(stats::ave(v, full$USUBJID, FUN = function(s) rank(s, ties.method = "first")) == v), TRUE)
  # Subjects per value are compared with the values the sample shows, not with the whole.
  is_key <- kinds != "date" & !(numeric_like & values > 50) & !drawn & values >= 2 &
    subject_values >= 2 * (if (sampled) count_values(frame) else values) & columns != "USUBJID" & !numbered
  keys <- as.list(columns[is_key])
  # Per-test and per-visit tables that also differ by a subject characteristic (ranges by
  # sex, the dose by arm and visit).
  for (by in intersect(c(s$test, s$visit), columns))
    keys <- c(keys, lapply(grep("^DM\\.", columns[is_key], value = TRUE), function(k) c(by, k)))
  # Values set by the schedule: a table by all the record keys (RSLNKGRP by visit, test and
  # evaluator).
  if (length(s$keys) > 1 && all(s$keys %in% columns)) keys <- c(keys, list(s$keys))
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
    # Inputs are columns of numbers, or text results that are mostly numbers (TRORRES).
    inputs <- columns[numeric_like & kinds != "date" & !grepl("^(DM|N)\\.", columns)]
    usable <- vapply(inputs, function(x) sum(!is.na(parse_number(frame[[x]])) & parsed) >= 2, TRUE)
    as.list(setdiff(inputs[usable], c(target, coded)))
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
  rules <- chosen[intersect(targets, names(chosen))]
  # Tests calculated from the other records of their group, searched on every record, except
  # the tests the variable's own rule already gives. The group must not be calculated from
  # the variable.
  if (!is.null(s$test) && s$test %in% columns) for (target in setdiff(targets, s$test)) {
    groups <- derivation_groups(full, target, name)
    if (target %in% reachable(groups, function(v) unlist(lapply(rules[intersect(v, names(rules))], rule_columns)))) next
    base <- rules[[target]]$by_test
    rule <- evaluate_rule(new_rule(target, "group", c(s$test, groups), list(skip = base$test[base$share >= SHARE_TO_PROPOSE])), full)
    if (length(rule$params$derivations) && proposed(rule)) rules[[group_rule_name(target)]] <- rule
  }
  rules
}
