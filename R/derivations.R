# Calculations of tests from the other records of their group (see the group operation in
# rules.R): a total from its items, a half-life from the rate constant, an overall response
# from the target and non-target responses.

# A test is calculated from the other tests of its group when the group's records give it
# for SHARE_TO_PROPOSE of at least DERIVATION_RECORDS records. Lookups read tests with at
# most DERIVATION_LEVELS values, and use at most DERIVATION_INPUTS of them.
DERIVATION_RECORDS <- 10
DERIVATION_LEVELS <- 20
DERIVATION_INPUTS <- 3

# A rule calculating some tests of a variable from other records is kept beside the
# variable's own rule, under this name.
group_rule_name <- function(variable) sprintf("%s from other records", variable)

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
# `skip`, the tests the variable's own rule gives): the sum of some other tests, a line
# through or ratio to one other test, or a lookup by other tests' values. A test whose
# calculation would use itself through others takes none; the calculations scoring highest
# choose first, then later tests.
find_derivations <- function(frame, target, test, groups, skip = NULL) {
  y <- frame[[target]]
  if (inherits(y, "Date") || !test %in% names(frame)) return(list())
  tables <- derivation_tables(frame, target, test, groups)
  found <- Filter(Negate(is.null), lapply(setNames(nm = setdiff(tables$codes, skip)), test_derivation, tables = tables))
  kept <- acyclic_derivations(found, tables$codes)
  lapply(kept[calculation_order(kept)], function(d) {
    d$score <- NULL
    d$fixed <- if (is.character(y) && d$kind != "lookup") number_format(y[tables$tests %in% d$test & !is.na(tables$x)])
    d
  })
}

# Per group (rows) and test (columns): the sum of the numbers, how many there are, how many
# records, and the value (see `group_values()`); per test, its number of values.
derivation_tables <- function(frame, target, test, groups) {
  y <- frame[[target]]
  tests <- format_value(frame[[test]])
  group <- group_index(frame, groups)
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
  per_cell <- function(i) matrix(tabulate(cell[i], n * length(codes)), n, dimnames = list(NULL, codes))
  levels <- tapply(level, factor(t, seq_along(codes)), function(v) length(unique(v)))
  names(levels) <- codes
  list(y = y, x = x, level = level, tests = tests, group = group, subject = record_subject(frame), codes = codes,
    sums = sums, counts = per_cell(number), records = per_cell(!is.na(t)), values = group_values(y, tests, codes, group, n),
    levels = levels)
}

# The best calculation of test `code`: of the candidates (a sum, lines and ratios, lookups,
# in that order) that pass, the first scoring highest. NULL when the test has too few
# records or values, or no candidate passes.
test_derivation <- function(code, tables) {
  rows <- which(tables$tests == code)
  y <- tables$y[rows]
  if (sum(!is.na(y)) < DERIVATION_RECORDS || length(unique(y[!is.na(y)])) < 2) return(NULL)
  g <- tables$group[rows]
  others <- setdiff(tables$codes, code)
  test <- list(code = code, y = y, number = tables$x[rows], level = tables$level[rows], subject = tables$subject[rows],
    digits = decimals(y), others = others, levels = tables$levels, sums = tables$sums[g, others, drop = FALSE],
    counts = tables$counts[g, others, drop = FALSE], records = tables$records[g, , drop = FALSE],
    values = tables$values[g, , drop = FALSE])
  numeric <- sum(!is.na(test$number)) >= DERIVATION_RECORDS && length(others)
  candidates <- c(if (numeric) c(sum_derivation(test), unlist(lapply(others, line_derivations, test = test), recursive = FALSE)),
    lookup_derivations(test))
  scores <- vapply(candidates, derivation_score, 0, level = test$level)
  if (!length(scores) || all(is.na(scores))) return(NULL)
  best <- which.max(scores)
  c(candidates[[best]]$derivation, list(score = scores[[best]]))
}

# A record given wrong costs this many given right.
wrong_weight <- function() SHARE_TO_PROPOSE / (1 - SHARE_TO_PROPOSE)

candidate <- function(derivation, value, match) list(derivation = derivation, value = value, match = match)

# The records a candidate gives right less wrong_weight() times those it gives wrong; NA
# unless it gives at least DERIVATION_RECORDS, SHARE_TO_PROPOSE of them right, and explains
# EXPLAINED_TO_PROPOSE of what the most common value gets wrong.
derivation_score <- function(candidate, level) {
  checked <- !is.na(candidate$value)
  match <- candidate$match
  share <- if (any(checked)) mean(match[checked]) else 0
  common <- if (any(checked)) max(table(level[checked])) / sum(checked) else 1
  if (sum(checked) >= DERIVATION_RECORDS && share >= SHARE_TO_PROPOSE && explained(share, common) >= EXPLAINED_TO_PROPOSE)
    sum(match & checked) - wrong_weight() * sum(!match & checked) else NA_real_
}

# A sum of other tests: least squares picks the tests, then single tests are added or
# dropped while that helps.
sum_derivation <- function(test) {
  S <- test$sums
  C <- test$counts
  yn <- test$number
  ok <- !is.na(yn)
  used <- test$others[colSums(C[ok, , drop = FALSE]) > 0]
  if (!length(used) || sum(ok) < 3 * length(used)) return(list())
  beta <- qr.coef(qr(S[ok, used, drop = FALSE]), yn[ok])
  chosen <- test$others %in% used[!is.na(beta) & beta > 0.5]
  objective <- function(pred, count) {
    match <- close_number(pred, yn, test$digits)
    colSums(match & count > 0) - wrong_weight() * colSums(!match & count > 0)
  }
  current <- objective(matrix(S %*% chosen), matrix(C %*% chosen))
  repeat {
    sign <- ifelse(chosen, -1, 1)
    options <- objective(as.vector(S %*% chosen) + sweep(S, 2, sign, `*`), as.vector(C %*% chosen) + sweep(C, 2, sign, `*`))
    if (max(options) <= current) break
    chosen[which.max(options)] <- !chosen[which.max(options)]
    current <- max(options)
  }
  if (!any(chosen)) return(list())
  value <- ifelse(as.vector(C %*% chosen) > 0, as.vector(S %*% chosen), NA)
  list(candidate(list(test = test$code, kind = "sum", inputs = test$others[chosen], digits = test$digits), value,
    close_number(value, yn, test$digits)))
}

# A line through, or a ratio to, test `k` where the group has one number of it: a plain copy,
# the line fitted to the records nearest it, and the median ratio, each kept when it nearly
# gives half the records and rounded to 3 to 6 significant digits.
line_derivations <- function(k, test) {
  yn <- test$number
  digits <- test$digits
  input <- ifelse(test$counts[, k] == 1 & test$records[, k] == 1, test$sums[, k], NA)
  both <- which(!is.na(input) & !is.na(yn))
  if (length(both) < DERIVATION_RECORDS) return(list())
  fits <- list(list(kind = "linear", a = 1, b = 0))
  if (length(unique(input[both])) > 1) {
    line <- stats::lm.fit(cbind(input[both], 1), yn[both])$coefficients
    near <- both[abs(yn[both] - cbind(input[both], 1) %*% line) <= stats::median(abs(yn[both] - cbind(input[both], 1) %*% line))]
    if (length(unique(input[near])) > 1) line <- stats::lm.fit(cbind(input[near], 1), yn[near])$coefficients
    fits <- c(fits, list(list(kind = "linear", a = unname(line[1]), b = unname(line[2]))))
  }
  nonzero <- both[input[both] != 0]
  if (length(nonzero)) fits <- c(fits, list(list(kind = "ratio", a = stats::median(yn[nonzero] * input[nonzero]), b = 0)))
  unlist(lapply(fits, function(f) {
    linear <- f$kind == "linear"
    if (anyNA(c(f$a, f$b))) return(list())
    raw <- if (linear) f$a * input[both] + f$b else f$a / input[both]
    if (!isTRUE(mean(abs(raw - yn[both]) <= 1e-3 * pmax(abs(yn[both]), 10^-digits), na.rm = TRUE) >= 0.5)) return(list())
    lapply(3:6, function(p) {
      a <- signif(f$a, p)
      b <- signif(f$b, p)
      value <- if (linear) a * input + b else ifelse(input != 0, a / input, NA)
      candidate(list(test = test$code, kind = f$kind, inputs = k, a = a, b = b, digits = digits), value, close_number(value, yn, digits))
    })
  }), recursive = FALSE)
}

# Lookups by other tests' values (tests with at most DERIVATION_LEVELS values), scored on
# subjects they were not learned from. Inputs are added one at a time, the best scoring
# first; a key of one input may be wrong too often to pass, and right with a second. Each
# set of inputs so far is a candidate.
lookup_derivations <- function(test) {
  inputs <- test$others[test$levels[test$others] <= DERIVATION_LEVELS]
  inputs <- inputs[colSums(test$values[, inputs, drop = FALSE] != MISSING, na.rm = TRUE) >= DERIVATION_RECORDS]
  if (test$levels[[test$code]] > 2.5 * DERIVATION_LEVELS || !length(inputs)) return(list())
  key <- function(inputs) values_key(test$values[, inputs, drop = FALSE])
  score <- function(inputs) {
    value <- held_out_lookup(key(inputs), test$level, test$subject)
    match <- !is.na(value) & value == test$level
    list(score = sum(match) - wrong_weight() * sum(!is.na(value) & !match), value = value, match = match)
  }
  out <- list()
  chosen <- character()
  while (length(chosen) < DERIVATION_INPUTS && length(rest <- setdiff(inputs, chosen))) {
    options <- lapply(rest, function(k) score(c(chosen, k)))
    best <- which.max(vapply(options, `[[`, 0, "score"))
    chosen <- c(chosen, rest[best])
    out <- c(out, list(candidate(lookup_table(test, key(chosen), chosen), options[[best]]$value, options[[best]]$match)))
  }
  out
}

# A lookup's table: per key, the first record holding its most common value, where at least
# min_subjects() subjects hold the key and value together.
lookup_table <- function(test, key, inputs) {
  keyed <- which(!is.na(key))
  first <- keyed[lookup_rows(match(key[keyed], unique(key[keyed])), match(test$level[keyed], unique(test$level[keyed])))]
  first <- first[!is.na(test$y[first]) & subjects_holding(paste(key, test$level), test$subject)[first] >= min_subjects()]
  list(test = test$code, kind = "lookup", inputs = inputs, keys = key[first], values = test$y[first], digits = test$digits)
}

# Calculations from fewer tests choose first, then those giving more records right, then
# later tests; one that would use its own test is dropped.
acyclic_derivations <- function(found, codes) {
  kept <- list()
  for (j in names(found)[order(lengths(lapply(found, `[[`, "inputs")), -vapply(found, `[[`, 0, "score"), -match(names(found), codes))])
    if (!closes_cycle(j, found[[j]]$inputs, kept, function(d) d$inputs)) kept[[j]] <- found[[j]]
  kept
}

# The tests in calculation order: each after the tests it reads.
calculation_order <- function(kept) {
  order <- character()
  while (length(order) < length(kept)) {
    ready <- setdiff(names(kept)[vapply(kept, function(d) all(!d$inputs %in% setdiff(names(kept), order)), TRUE)], order)
    order <- c(order, ready)
  }
  order
}

# Whether `target` is reached from `inputs` through what the `kept` items read (`reads`).
closes_cycle <- function(target, inputs, kept, reads) {
  target %in% reachable(inputs, function(v) unlist(lapply(kept[intersect(v, names(kept))], reads)))
}
