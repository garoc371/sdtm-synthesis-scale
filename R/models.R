# Maximum-likelihood models for sampled nodes and record structures.
#
# A fitted node is a set of parts, one per test (findings), stage (the first record of a
# series, or a later one given the previous value) and, for dates, whether the anchor date
# is known. Each part models one value:
#   categories          multinomial over the observed values, missing being a value
#   normal / lognormal  whether a value is recorded, then a linear model of the value
#   normrank            the same on normal scores of the value's ranks, drawn back through its
#                       quantiles at 5% steps (dates, whose gaps are rarely normal)
# Dates are day gaps from their anchor date, or day numbers without one. Fits keep
# coefficients and summaries only, never source rows.

MIN_LEVEL_COUNT <- 5
MAX_MODEL_LEVELS <- 30
MAX_DISCRETE_VALUES <- 10
RANK_STEPS <- seq(0, 1, 0.05)
NODE_MODELS <- c(auto = "Automatic", normal = "Normal", lognormal = "Log-normal", categories = "Categories")

# ---- Design matrices ------------------------------------------------------------------

# Numeric predictors are standardised (missing values sit at the mean); categorical ones
# use the most common level as reference. Rare and unseen levels share the reference.
design_info <- function(frame, predictors, categorical = character()) {
  lapply(setNames(predictors, predictors), function(p) {
    v <- frame[[p]]
    if (inherits(v, "Date")) v <- as.numeric(v)
    if (is.numeric(v) && !p %in% categorical) {
      center <- mean(v, na.rm = TRUE)
      scale <- stats::sd(v, na.rm = TRUE)
      list(type = "numeric", center = if (is.finite(center)) center else 0,
        scale = if (is.finite(scale) && scale > 0) scale else 1)
    } else {
      counts <- sort(table(level_string(v)), decreasing = TRUE)
      list(type = "categorical", levels = names(counts)[seq_along(counts) == 1 | counts >= MIN_LEVEL_COUNT])
    }
  })
}

design_matrix <- function(info, frame) {
  columns <- list(`(Intercept)` = rep(1, nrow(frame)))
  for (p in names(info)) {
    i <- info[[p]]
    if (i$type == "numeric") {
      z <- (as.numeric(frame[[p]]) - i$center) / i$scale
      columns[[p]] <- ifelse(is.na(z), 0, z)
    } else {
      s <- level_string(frame[[p]])
      for (l in i$levels[-1]) columns[[paste0(p, "=", l)]] <- as.numeric(s == l)
    }
  }
  do.call(cbind, columns)
}

# ---- Fits ------------------------------------------------------------------------------

# Coefficients of codes 2..k against code 1, one row per code, with standard errors of
# the same shape. Without predictors, or with too many values to model, the estimates are
# the observed log odds; identical design rows are otherwise fitted once, weighted.
fit_multinomial <- function(X, codes, k) {
  zero <- matrix(0, k - 1, ncol(X), dimnames = list(NULL, colnames(X)))
  if (k == 1) return(list(coef = zero, se = zero))
  if (ncol(X) == 1 || k > MAX_MODEL_LEVELS) {
    n <- tabulate(codes, k)
    coef <- se <- zero
    coef[, 1] <- log(n[-1] / n[1])
    se[, 1] <- sqrt(1 / n[-1] + 1 / n[1])
    return(list(coef = coef, se = se))
  }
  key <- do.call(paste, c(as.data.frame(X), list(codes), sep = "\r"))
  rows <- which(!duplicated(key))
  data <- data.frame(.y = factor(codes[rows], levels = seq_len(k)), X[rows, -1, drop = FALSE])
  names(data)[-1] <- paste0("x", seq_len(ncol(X) - 1))
  fit <- nnet::multinom(.y ~ ., data = data, weights = tabulate(match(key, key[rows])), Hess = TRUE,
    trace = FALSE, MaxNWts = 1e6, maxit = 200)
  coef <- matrix(stats::coef(fit), k - 1, dimnames = dimnames(zero))
  coef[is.na(coef)] <- 0
  se <- tryCatch(matrix(sqrt(pmax(diag(solve(fit$Hessian)), 0)), k - 1, byrow = TRUE, dimnames = dimnames(zero)),
    error = function(e) zero)
  list(coef = coef, se = se)
}

# One model part. Predictors are used only with at least 10 values per coefficient; for
# categories, 10 values outside the most common one (a rare value is not learnt per level).
fit_part <- function(y, X, type) {
  observed <- !is.na(y)
  if (!any(observed)) type <- "categories"
  n <- if (type == "categories") length(y) - max(table(level_string(y))) else sum(observed)
  columns <- if (n >= 10 * ncol(X)) seq_len(ncol(X)) else 1L
  X <- X[, columns, drop = FALSE]
  if (type == "categories") {
    values <- level_string(y)
    levels <- names(sort(table(values), decreasing = TRUE))
    return(c(fit_multinomial(X, match(values, levels), length(levels)),
      list(type = type, columns = columns, levels = levels, numeric = is.numeric(y))))
  }
  knots <- if (type == "normrank") unname(stats::quantile(y[observed], RANK_STEPS))
  z <- switch(type, lognormal = log(y[observed]), normrank = stats::qnorm((rank(y[observed]) - 0.5) / sum(observed)), y[observed])
  X_obs <- X[observed, , drop = FALSE]
  fit <- stats::lm.fit(X_obs, z)
  sigma <- sqrt(sum(fit$residuals^2) / max(1, length(z) - fit$rank))
  variance <- tryCatch(diag(solve(crossprod(X_obs))), error = function(e) rep(0, ncol(X)))
  list(type = type, columns = columns, sigma = sigma,
    coef = matrix(ifelse(is.na(fit$coefficients), 0, fit$coefficients), 1, dimnames = list(NULL, colnames(X))),
    se = matrix(sigma * sqrt(pmax(variance, 0)), 1, dimnames = list(NULL, colnames(X))),
    recorded = if (!all(observed)) fit_multinomial(X, 2L - observed, 2L),
    digits = decimals(y[observed]), range = range(y[observed]), knots = knots)
}

model_scale <- function(type, x) if (identical(type, "lognormal")) log(pmax(x, 1e-8)) else x

# The model part of each record: its test, whether it follows an earlier value of its
# series, and for dates whether the anchor date (the first predictor) is known. A date's
# model value is its gap from the anchor, or its day number without one (offset 0).
node_parts <- function(ds, variable, frame, previous) {
  s <- ds$structure
  anchor <- ds$nodes[[variable]]$predictors[1]
  anchor <- if (ds$kinds[[variable]] == "date" && !is.na(anchor) && inherits(frame[[anchor]], "Date")) anchor
  offset <- if (is.null(anchor)) rep(NA_real_, nrow(frame)) else as.numeric(frame[[anchor]])
  test <- if (!is.null(s$test) && ds$levels[[variable]] == "record") format_value(frame[[s$test]]) else rep("", nrow(frame))
  list(anchor = anchor, offset = ifelse(is.na(offset), 0, offset),
    key = paste(test, ifelse(is.na(previous), "first", "follow"), ifelse(is.na(offset), "", "anchored"), sep = "|"))
}

# A node is fitted on one row per subject, per subject and visit, or per record. The model
# type is chosen once per test (and anchor state), so first and later records share a
# scale: categories for text and numbers with few values, otherwise normal, or log-normal
# when positive and skewed. A trajectory's first record is fitted without the arm (it is
# measured before treatment); later records add the previous recorded value.
fit_node <- function(ds, variable, frame, arm) {
  node <- ds$nodes[[variable]]
  s <- ds$structure
  level <- if (ds$name == "DM") "record" else ds$levels[[variable]]
  frame <- frame[level_rows(frame, level, s), , drop = FALSE]
  y <- frame[[variable]]
  previous <- rep(NA_real_, nrow(frame))
  if (isTRUE(node$prev)) {
    series <- group_index(frame, c("USUBJID", setdiff(s$keys, s$visit)))
    o <- order(series, as.numeric(frame[[s$visit]]))
    last <- c(0, cummax(ifelse(is.na(y[o]), 0, seq_along(o))))[seq_along(o)]
    follows <- last > 0 & series[o][pmax(last, 1)] == series[o]
    previous[o[follows]] <- y[o][last[follows]]
  }
  dates <- if (ds$kinds[[variable]] == "date") setdiff(names(Filter(function(v) inherits(v, "Date"), frame)), variable)
  ordered_dates <- function(sign) dates[vapply(frame[dates], function(d) isTRUE(mean(sign * as.numeric(y - d) >= 0, na.rm = TRUE) >= 0.99), TRUE)]
  after <- ordered_dates(1)
  before <- ordered_dates(-1)
  # A date that follows visit order within the subject in 99% of source steps.
  monotone <- !is.null(dates) && !is.null(s$visit) && level != "subject" && {
    o <- order(group_index(frame, visit_series(s, level)), as.numeric(frame[[s$visit]]))
    same <- group_index(frame, visit_series(s, level))[o]
    step <- diff(as.numeric(y[o]))[same[-1] == same[-length(same)]]
    isTRUE(mean(step >= 0, na.rm = TRUE) >= 0.99)
  }
  parts <- node_parts(ds, variable, frame, previous)
  if (ds$kinds[[variable]] == "date") y <- as.numeric(y) - parts$offset
  arm <- c(arm, paste0("DM.", arm))
  predictors <- setdiff(intersect(node$predictors, names(frame)), parts$anchor)
  scale <- sub("\\|(first|follow)\\|", "|", parts$key)
  types <- vapply(split(y, scale), function(v) {
    v <- v[!is.na(v)]
    if (!is.numeric(v) || node$model == "categories" || (node$model == "auto" && length(unique(v)) <= MAX_DISCRETE_VALUES))
      return("categories")
    skew <- if (length(v) > 2 && stats::sd(v) > 0) mean((v - mean(v))^3) / stats::sd(v)^3 else 0
    if (node$model == "auto" && ds$kinds[[variable]] == "date") return("normrank")
    if (all(v > 0) && (node$model == "lognormal" || (node$model == "auto" && skew > 1))) "lognormal" else "normal"
  }, "")
  fits <- lapply(split(seq_along(y), parts$key), function(i) {
    follow <- grepl("|follow|", parts$key[i[1]], fixed = TRUE)
    type <- types[[scale[i[1]]]]
    records <- frame[i, , drop = FALSE]
    records$.previous <- model_scale(type, previous[i])
    used <- if (follow) c(predictors, ".previous") else if (isTRUE(node$prev)) setdiff(predictors, arm) else predictors
    info <- design_info(records, used, c(s$visit, s$test))
    c(fit_part(y[i], design_matrix(info, records), type), list(info = info))
  })
  description <- sprintf("%s%s%s%s, %d %s", paste(sort(unique(types)), collapse = "/"),
    if (length(unique(parts$key)) > 1 && !is.null(s$test) && level == "record") paste(" per", s$test) else "",
    if (!is.null(parts$anchor)) paste(" of days from", display_name(parts$anchor)) else "",
    if (isTRUE(node$prev)) " with previous value" else "", nrow(frame), if (level == "record") "records" else paste(level, "rows"))
  list(parts = fits, arm = arm, description = paste0(description, if (monotone) ", in visit order"), after = after,
    before = before, monotone = monotone)
}

visit_series <- function(s, level) c("USUBJID", if (level == "record") setdiff(s$keys, s$visit))

# A node's rows: one per subject, per subject and visit, per series, or every record.
level_rows <- function(frame, level, s) {
  key <- level_key(level, s)
  if (is.null(key)) seq_len(nrow(frame)) else which(!duplicated(frame[key]))
}

level_key <- function(level, s) switch(level, subject = "USUBJID", visit = c("USUBJID", s$visit), series = c("USUBJID", ".series"), record = NULL)

# ---- Drawing --------------------------------------------------------------------------

# Codes 1..k drawn from the multinomial probabilities of each row of linear predictors.
softmax_draw <- function(eta) {
  eta <- cbind(0, eta)
  p <- exp(eta - apply(eta, 1, max))
  cumulative <- p %*% upper.tri(diag(ncol(p)), diag = TRUE)
  pmin(1L + rowSums(cumulative < stats::runif(nrow(p)) * rowSums(p)), ncol(p))
}

draw_records <- function(ds, variable, fit, frame, previous) {
  parts <- node_parts(ds, variable, frame, previous)
  values <- rep(NA, nrow(frame))
  for (key in intersect(unique(parts$key), names(fit$parts))) {
    p <- fit$parts[[key]]
    i <- which(parts$key == key)
    records <- frame[i, , drop = FALSE]
    records$.previous <- model_scale(p$type, previous[i])
    X <- design_matrix(p$info, records)[, p$columns, drop = FALSE]
    eta <- X %*% t(p$coef)
    if (p$type == "categories") {
      drawn <- p$levels[softmax_draw(eta)]
      drawn[drawn == MISSING] <- NA
      if (p$numeric) drawn <- as.numeric(drawn)
    } else {
      drawn <- stats::rnorm(length(i), drop(eta), p$sigma)
      if (p$type == "lognormal") drawn <- exp(drawn)
      if (p$type == "normrank") drawn <- stats::approx(RANK_STEPS, p$knots, stats::pnorm(drawn))$y
      drawn <- round(pmin(pmax(drawn, p$range[1]), p$range[2]), p$digits)
      if (!is.null(p$recorded)) drawn[softmax_draw(X %*% t(p$recorded$coef)) == 2] <- NA
    }
    values[i] <- if (is.numeric(drawn)) drawn + parts$offset[i] else drawn
  }
  values
}

# Draws a node at its level, visit by visit when it depends on the previous value, and
# copies each value to every record of that level. A varying arm effect draws the arm
# coefficients once from N(estimate, SE). Dates outside the observation window, or out of
# the order they keep with already drawn dates in 99% of source records, are redrawn a
# few times and then moved to the nearest allowed date; except that a record planned at a
# visit (not drawn by reach) whose main date is still too late is marked "beyond" (it did
# not take place).
draw_node <- function(ds, variable, fit, frame) {
  node <- ds$nodes[[variable]]
  s <- ds$structure
  level <- if (ds$name == "DM") "record" else ds$levels[[variable]]
  rows <- level_rows(frame, level, s)
  part <- frame[rows, , drop = FALSE]
  if (isTRUE(node$vary)) fit$parts <- lapply(fit$parts, function(p) {
    arm <- sub("=.*$", "", colnames(p$coef)) %in% fit$arm
    p$coef[, arm] <- stats::rnorm(length(p$coef[, arm]), p$coef[, arm], p$se[, arm])
    p
  })
  series <- if (isTRUE(node$prev)) group_index(part, c("USUBJID", setdiff(s$keys, s$visit))) else rep(1L, nrow(part))
  rank <- if (isTRUE(node$prev)) stats::ave(as.numeric(part[[s$visit]]), series, FUN = function(v) rank(v, ties.method = "first")) else
    rep(1, nrow(part))
  values <- rep(NA, nrow(part))
  last <- rep(NA_real_, max(0, series))
  for (r in sort(unique(rank))) {
    i <- which(rank == r)
    values[i] <- draw_records(ds, variable, fit, part[i, , drop = FALSE], if (isTRUE(node$prev)) last[series[i]] else rep(NA, length(i)))
    if (isTRUE(node$prev)) last[series[i][!is.na(values[i])]] <- values[i][!is.na(values[i])]
  }
  if (ds$kinds[[variable]] == "date") {
    bound <- function(names, pick, none) do.call(pick, c(lapply(names, function(n) as.numeric(part[[n]] %||% NA)), none, na.rm = TRUE))
    start <- bound(c(ds$window$start, fit$after), pmax, -Inf)
    end <- bound(c(ds$window$end, fit$before), pmin, Inf)
    for (attempt in 1:10) {
      bad <- which(!is.na(values) & (values < start | values > end))
      if (!length(bad)) break
      values[bad] <- draw_records(ds, variable, fit, part[bad, , drop = FALSE], rep(NA, length(bad)))
    }
    # Visits drawn by reach are within the window already: their dates are moved into it.
    planned <- s$type == "schedule" && !is.null(s$visit) && is.null(s$reach) && level != "subject" &&
      identical(variable, main_date(ds$prefix, ds$variables))
    late <- planned & !is.na(values) & values > end
    values[late] <- NA
    values <- pmin(pmax(values, start), end)
    # Dates are put in visit order within the subject; bounds that differ by record apply again.
    if (isTRUE(fit$monotone)) {
      series <- group_index(part, visit_series(s, level))
      o <- order(series, as.numeric(part[[s$visit]]))
      values[o] <- stats::ave(values[o], series[o], FUN = function(v) replace(v, !is.na(v), sort(v[!is.na(v)])))
      values <- pmin(pmax(values, start), end)
    }
    values <- structure(as.Date(values, origin = "1970-01-01"), beyond = late)
  }
  if (level == "record") return(values)
  key <- level_key(level, s)
  row <- match(key_strings(frame[key]), key_strings(part[key]))
  structure(values[row], beyond = attr(values, "beyond")[row])
}

# ---- Record structures -----------------------------------------------------------------

# How many records each subject has, and with which record keys. `subjects` holds one row
# per subject with the DM inputs; `arm` is the arm's input column.
RECORD_STRUCTURES <- list(
  # By arm: the share of subjects with records at all and, among them, the share attending
  # each visit; given attendance, the distribution of record counts for each combination
  # of the other keys (zero included). Attendance is given that a subject has records, so
  # that a visit every subject with records attends is attended by every generated one.
  schedule = list(
    fit = function(x, subjects, arm, s, visits = NULL) {
      group <- if (isTRUE(s$by_arm) && arm %in% names(subjects)) level_string(subjects[[arm]]) else rep("all", nrow(subjects))
      x <- x[x$USUBJID %in% subjects$USUBJID, , drop = FALSE]
      others <- setdiff(s$keys, s$visit)
      combo <- group_index(x, others)
      visit <- if (is.null(s$visit)) rep(".", nrow(x)) else format_value(x[[s$visit]])
      attended <- unique(data.frame(USUBJID = x$USUBJID, visit = visit, stringsAsFactors = FALSE))
      attended$arm <- group[match(attended$USUBJID, subjects$USUBJID)]
      slots <- merge(attended, data.frame(combo = seq_len(max(combo, 1))))
      slots$records <- tabulate(match(paste(x$USUBJID, visit, combo), paste(slots$USUBJID, slots$visit, slots$combo)), nrow(slots))
      if (isTRUE(s$unique)) slots$records <- pmin(slots$records, 1)
      counts <- stats::aggregate(list(n = slots$records), slots[c("arm", "visit", "combo", "records")], length)
      counts$p <- counts$n / stats::ave(counts$n, counts$arm, counts$visit, counts$combo, FUN = sum)
      attendance <- as.data.frame(table(arm = attended$arm, visit = attended$visit), stringsAsFactors = FALSE)
      members <- tapply(subjects$USUBJID %in% x$USUBJID, group, sum)
      attendance$p <- attendance$Freq / pmax(members[attendance$arm], 1)
      # With SV, attendance is given that the subject attended the visit at all.
      if (!is.null(visits)) {
        at_visit <- visits[visits$USUBJID %in% x$USUBJID, ]
        at_visit <- table(paste(group[match(at_visit$USUBJID, subjects$USUBJID)], at_visit$visit, sep = "\r"))
        attendance$p <- attendance$Freq / pmax(at_visit[paste(attendance$arm, attendance$visit, sep = "\r")], attendance$Freq, 1, na.rm = TRUE)
      }
      # And given the visit's position (see `visit_position()`), shrunk towards the visit's attendance by five subjects' worth of it.
      cells <- NULL
      if (!is.null(visits) && is.null(s$reach)) {
        at_visit <- visit_position(visits[visits$USUBJID %in% x$USUBJID, ], subjects)
        at_visit$arm <- group[match(at_visit$USUBJID, subjects$USUBJID)]
        at_visit$came <- paste(at_visit$USUBJID, at_visit$visit) %in% paste(attended$USUBJID, attended$visit)
        key <- paste(at_visit$arm, at_visit$visit, at_visit$position, sep = "\r")
        prior <- attendance$p[match(paste(at_visit$arm, at_visit$visit), paste(attendance$arm, attendance$visit))]
        cells <- (tapply(at_visit$came, key, sum) + 5 * tapply(prior, key, mean)) / (table(key) + 5)
        cells <- setNames(as.vector(cells), names(cells))
      }
      # With a reach, attendance is given that the subject's window lasts until the visit's
      # early day: the earliest day after entry any of its attenders comes. The window
      # ends at the end of participation or, for a visit whose known attenders (90% of them)
      # all come by an earlier DM date, at the earliest such date (a visit of the reference
      # period ends with it). Leaving the study, by the window, ends the visits.
      reach <- NULL
      if (!is.null(s$reach)) {
        at <- match(x$USUBJID, subjects$USUBJID)
        date <- as.numeric(x[[s$reach[["date"]]]])
        day <- date - as.numeric(subjects[[s$reach[["start"]]]])[at]
        early <- tapply(day, visit, function(d) if (all(is.na(d))) NA else min(d, na.rm = TRUE))
        ends <- grep("^DM\\.", names(Filter(function(v) inherits(v, "Date"), subjects)), value = TRUE)
        bound <- vapply(names(early), function(v) {
          rows <- visit == v
          gap <- vapply(ends, function(e) {
            g <- as.numeric(subjects[[e]])[at[rows]] - date[rows]
            if (mean(!is.na(g)) >= SHARE_TO_PROPOSE && mean(g >= 0, na.rm = TRUE) >= 0.99) stats::median(g, na.rm = TRUE) else NA
          }, 0)
          if (all(is.na(gap))) s$reach[["end"]] else ends[which.min(gap)]
        }, "")
        # A DM date closes the visits it ends when it is the date of the subject's last of them.
        closes <- vapply(unique(bound), function(e) {
          rows <- bound[visit] %in% e & !is.na(date)
          last <- tapply(date[rows], x$USUBJID[rows], max)
          mean(last == as.numeric(subjects[[e]])[match(names(last), subjects$USUBJID)], na.rm = TRUE)
        }, 0) >= SHARE_TO_PROPOSE
        reach <- list(day = early, bound = bound, closes = unique(bound)[closes %in% TRUE])
        reached <- function(id, v) visit_reached(reach, subjects, s, id, v)
        member <- subjects$USUBJID[subjects$USUBJID %in% x$USUBJID]
        cell <- paste(attendance$arm, attendance$visit, sep = "\r")
        came <- attended[reached(attended$USUBJID, attended$visit), , drop = FALSE]
        open <- vapply(seq_len(nrow(attendance)), function(i)
          sum(group[match(member, subjects$USUBJID)] == attendance$arm[i] & reached(member, attendance$visit[i])), 0)
        attendance$p <- as.vector(table(factor(paste(came$arm, came$visit, sep = "\r"), levels = cell))) / pmax(open, 1)
        # The visits that end some subject's visits, in the order of their usual day.
        last <- order(x$USUBJID, day, decreasing = TRUE)
        reach$ends <- unique(visit[last][!duplicated(x$USUBJID[last])])
        reach$usual <- tapply(day, visit, stats::median, na.rm = TRUE)
      }
      list(members = members / as.vector(table(group)[names(members)]), attendance = attendance[c("arm", "visit", "p")], reach = reach, cells = cells,
        counts = counts[c("arm", "visit", "combo", "records", "p")], combos = x[!duplicated(combo), others, drop = FALSE],
        visits = if (!is.null(s$visit)) sort(unique(x[[s$visit]])),
        description = sprintf("%d key combinations over %d visits", max(combo, 1), length(unique(visit))))
    },
    draw = function(fit, subjects, arm, s, visits = NULL) {
      group <- if (isTRUE(s$by_arm) && arm %in% names(subjects)) level_string(subjects[[arm]]) else rep("all", nrow(subjects))
      member <- fit$members[group]
      if (any(member > 0 & member < 1, na.rm = TRUE)) {
        keep <- which(stats::runif(length(group)) < member)
        subjects <- subjects[keep, , drop = FALSE]
        group <- group[keep]
      }
      slots <- merge(data.frame(USUBJID = subjects$USUBJID, arm = group, stringsAsFactors = FALSE), fit$attendance, by = "arm")
      if (!is.null(fit$reach)) {
        slots <- slots[visit_reached(fit$reach, subjects, s, slots$USUBJID, slots$visit), , drop = FALSE]
      }
      if (!is.null(visits)) slots <- slots[paste(slots$USUBJID, slots$visit) %in% paste(visits$USUBJID, visits$visit), , drop = FALSE]
      if (!is.null(fit$cells) && nrow(slots)) {
        position <- visit_position(visits, subjects)
        position <- position$position[match(paste(slots$USUBJID, slots$visit), paste(position$USUBJID, position$visit))]
        p <- fit$cells[paste(slots$arm, slots$visit, position, sep = "\r")]
        slots$p <- ifelse(is.na(p), slots$p, p)
      }
      # A member of the dataset has at least one visit: without any, their likeliest.
      came <- stats::runif(nrow(slots)) < slots$p
      none <- !slots$USUBJID %in% slots$USUBJID[came]
      first <- which(none)[order(slots$USUBJID[none], -slots$p[none])]
      came[first[!duplicated(slots$USUBJID[first])]] <- TRUE
      # A subject's visits end at a visit that ends some subject's visits in the source.
      if (!is.null(fit$reach$ends)) {
        o <- order(slots$USUBJID, -fit$reach$usual[slots$visit])
        ending <- came[o] & slots$visit[o] %in% fit$reach$ends
        reached <- stats::ave(as.numeric(ending), slots$USUBJID[o], FUN = cumsum) > 0
        came[o] <- came[o] & (reached | !stats::ave(ending, slots$USUBJID[o], FUN = any))
      }
      slots <- slots[came, c("USUBJID", "arm", "visit")]
      options <- merge(slots, fit$counts, by = c("arm", "visit"))
      options <- options[order(options$USUBJID, options$visit, options$combo, options$records), ]
      cell <- group_index(options, c("USUBJID", "visit", "combo"))
      hit <- which(stats::runif(max(0, cell))[cell] < stats::ave(options$p, cell, FUN = cumsum))
      chosen <- options[hit[!duplicated(cell[hit])], ]
      chosen <- chosen[rep(seq_len(nrow(chosen)), chosen$records), ]
      records <- data.frame(USUBJID = chosen$USUBJID, stringsAsFactors = FALSE)
      if (!is.null(s$visit)) records[[s$visit]] <- fit$visits[match(chosen$visit, format_value(fit$visits))]
      cbind(records, fit$combos[chosen$combo, , drop = FALSE], row.names = NULL)
    }),
  # Records per subject: Poisson, or negative binomial when overdispersed.
  count = list(
    fit = function(x, subjects, arm, s, visits = NULL) fit_count(tabulate(match(x$USUBJID, subjects$USUBJID), nrow(subjects)), subjects, s),
    draw = function(fit, subjects, arm, s, visits = NULL)
      data.frame(USUBJID = rep(subjects$USUBJID, draw_count(fit, subjects)), stringsAsFactors = FALSE)),
  # Series per subject, counted like records; each series is recorded at consecutive
  # collection visits of the subject (their SV visits at the dataset's visit numbers). It
  # starts at a visit at the rate series start there per series of subjects attending it, by
  # visit and reference period, and after its j-th record stops at the discrete hazard of
  # stopping then, by j and the next visit's reference period. A series still recorded at the
  # subject's last collection visit is censored there.
  series = list(
    fit = function(x, subjects, arm, s, visits = NULL) {
      own <- collection_visits(visits, subjects, s)
      visit <- if (s$keys == "VISITNUM") format_value(x$VISITNUM) else
        own$visit[match(paste(x$USUBJID, as.numeric(x[[s$keys]])), paste(own$USUBJID, as.numeric(own$date)))]
      k <- own$k[match(paste(x$USUBJID, visit), paste(own$USUBJID, own$visit))]
      x <- data.frame(USUBJID = x$USUBJID, series = group_index(x, c("USUBJID", s$series)), k = k, stringsAsFactors = FALSE)[!is.na(k), ]
      runs <- stats::aggregate(list(start = x$k, end = x$k), list(USUBJID = x$USUBJID, series = x$series), function(v) v)
      runs$start <- vapply(runs$start, min, 0)
      runs$end <- vapply(runs$end, max, 0)
      total <- tapply(own$k, own$USUBJID, max)
      cell <- function(u, k) own$cell[match(paste(u, k), paste(own$USUBJID, own$k))]
      period <- function(u, k) own$period[match(paste(u, k), paste(own$USUBJID, own$k))]
      # Starts per cell over the series of subjects attending it.
      per <- table(runs$USUBJID)
      exposure <- tapply(as.numeric(per[own$USUBJID]), own$cell, sum, na.rm = TRUE)
      starts <- table(cell(runs$USUBJID, runs$start))
      rate <- as.numeric(starts[names(exposure)]) / exposure
      rate[is.na(rate)] <- 0
      # Steps: after record j, the series continues (0) or stops (1) before the next visit.
      steps <- do.call(rbind, lapply(seq_len(nrow(runs)), function(i) {
        r <- runs[i, ]
        j <- seq_len(r$end - r$start + 1)
        more <- r$start + j <= total[[r$USUBJID]]
        if (!any(more)) return(NULL)
        data.frame(j = pmin(j, 10), period = period(r$USUBJID, r$start + j), stop = as.numeric(j == max(j)))[more, ]
      }))
      hazard <- tapply(steps$stop, paste(steps$j, steps$period), mean)
      counts <- as.numeric(per[subjects$USUBJID])
      count <- fit_count(ifelse(is.na(counts), 0, counts), subjects, s)
      # Share of subjects with records recorded at their first and at their last collection visit.
      last <- tapply(runs$end, runs$USUBJID, max)
      ends <- c(first = mean(tapply(runs$start, runs$USUBJID, min) == 1), last = mean(last == total[names(last)]))
      list(count = count, rate = rate, hazard = hazard, hazard_j = tapply(steps$stop, steps$j, mean), ends = ends,
        description = sprintf("series: %s; %.1f records per series; %.0f%% of subjects recorded at their first collection visit, %.0f%% at their last",
          count$description, nrow(x) / nrow(runs), 100 * ends[["first"]], 100 * ends[["last"]]))
    },
    draw = function(fit, subjects, arm, s, visits = NULL) {
      own <- collection_visits(visits, subjects, s)
      n <- draw_count(fit$count, subjects)
      ids <- rep(subjects$USUBJID, n)
      rows <- split(seq_len(nrow(own)), own$USUBJID)
      out <- lapply(seq_along(ids), function(i) {
        r <- rows[[ids[i]]]
        if (!length(r)) return(NULL)
        w <- fit$rate[own$cell[r]]
        w[is.na(w)] <- 0
        start <- if (sum(w) > 0) r[sample.int(length(r), 1, prob = w)] else r[1]
        end <- start
        while (end < max(r)) {
          j <- as.character(min(end - start + 1, 10))
          h <- fit$hazard[paste(j, own$period[end + 1])]
          if (is.na(h)) h <- fit$hazard_j[j]
          if (is.na(h) || stats::runif(1) < h) break
          end <- end + 1
        }
        data.frame(USUBJID = ids[i], .series = i, row = start:end, stringsAsFactors = FALSE)
      })
      out <- do.call(rbind, out)
      if (is.null(out)) out <- data.frame(USUBJID = character(), .series = integer(), row = integer())
      # Subjects keep the source's shares recorded at their first and last collection visit:
      # among those drawn without, enough have their earliest series start there (or their
      # latest continue to it).
      for (side in names(fit$ends)) {
        bound <- vapply(rows[out$USUBJID], if (side == "first") min else max, 0)
        hit <- tapply(out$row == bound, out$USUBJID, any)
        p <- (fit$ends[[side]] - mean(hit)) / (1 - mean(hit))
        if (!is.finite(p) || p <= 0) next
        move <- names(hit)[!hit][stats::runif(sum(!hit)) < p]
        for (u in move) {
          own_rows <- which(out$USUBJID == u)
          pick <- own_rows[if (side == "first") which.min(out$row[own_rows]) else which.max(out$row[own_rows])]
          series <- out$.series[pick]
          span <- if (side == "first") min(rows[[u]]):(out$row[pick] - 1) else (out$row[pick] + 1):max(rows[[u]])
          out <- rbind(out, data.frame(USUBJID = u, .series = series, row = span, stringsAsFactors = FALSE))
        }
      }
      out <- out[order(out$.series, out$row), , drop = FALSE]
      records <- data.frame(USUBJID = out$USUBJID, .series = out$.series, stringsAsFactors = FALSE)
      records[[s$keys]] <- if (s$keys == "VISITNUM") as.numeric(own$visit[out$row]) else own$date[out$row]
      if (endsWith(s$series, "SPID")) records[[s$series]] <- as.character(stats::ave(out$.series, out$USUBJID, FUN = function(v) match(v, unique(v))))
      records
    }))

# Each subject's collection visits for a series dataset, in date order and numbered k = 1, 2,
# …: SV visits at the dataset's visit numbers. Each is in the SDTMIG reference period of DM
# (RFSTDTC to RFENDTC) or before or after it; `cell` is the visit with its period.
collection_visits <- function(visits, subjects, s) {
  own <- visits[visits$visit %in% format_value(s$schedule) & visits$USUBJID %in% subjects$USUBJID, , drop = FALSE]
  own <- own[order(own$USUBJID, own$date, as.numeric(own$visit)), , drop = FALSE]
  own <- own[!duplicated(own[c("USUBJID", "visit")]), , drop = FALSE]
  own$k <- stats::ave(seq_len(nrow(own)), own$USUBJID, FUN = seq_along)
  own$period <- reference_period(subjects, own$USUBJID, own$date)
  own$cell <- paste(own$visit, own$period)
  rownames(own) <- NULL
  own
}

# Each attended visit's SDTMIG reference period, whether it is the subject's last visit in
# that period (END) and whether it is their last visit (LAST).
visit_position <- function(visits, subjects) {
  period <- reference_period(subjects, visits$USUBJID, visits$date)
  latest <- function(group) stats::ave(as.numeric(visits$date), group, FUN = function(d) if (all(is.na(d))) NA else d == max(d, na.rm = TRUE))
  visits$position <- paste(period, ifelse(latest(paste(visits$USUBJID, period)) %in% 1, "END", ""), ifelse(latest(visits$USUBJID) %in% 1, "LAST", ""))
  visits
}

# A sampled visit's position among the subject's visits, by the record keys and the visit's
# predictors: a disposition at the end of treatment is at that visit whichever it is.
fit_position <- function(ds, frame, subjects, visits) {
  visits <- visit_position(visits, subjects)
  position <- visits$position[match(visit_keys(frame), paste(visits$USUBJID, visits$visit, sep = "\r"))]
  records <- frame[!is.na(position), , drop = FALSE]
  info <- design_info(records, intersect(c(ds$structure$keys, ds$nodes$VISITNUM$predictors), names(records)))
  c(fit_part(position[!is.na(position)], design_matrix(info, records), "categories"), list(info = info))
}

# Each record moves to the subject's visit at its drawn position nearest its drawn visit or,
# without one there, at the nearest position: the same period and END, else the same period.
placed_visit <- function(x, data, ds, fit, subjects) {
  if (is.null(fit) || !nrow(x)) return(x$VISITNUM)
  visits <- visit_position(allowed_visits(data, ds), subjects)
  X <- design_matrix(fit$info, x)[, fit$columns, drop = FALSE]
  want <- fit$levels[softmax_draw(X %*% t(fit$coef))]
  have <- visits$position[match(visit_keys(x), paste(visits$USUBJID, visits$visit, sep = "\r"))]
  visit <- x$VISITNUM
  near <- list(function(p) p, function(p) sub(" [A-Z]*$", "", p), function(p) sub(" .*$", "", p))
  for (i in which(!is.na(visit) & (is.na(have) | have != want))) {
    own <- visits[visits$USUBJID == x$USUBJID[i], , drop = FALSE]
    for (f in near) {
      at <- as.numeric(own$visit[f(own$position) == f(want[i])])
      if (f(have[i]) %in% f(want[i])) break
      if (length(at)) {
        visit[i] <- at[which.min(abs(at - as.numeric(visit[i])))]
        break
      }
    }
  }
  visit
}

# SDTMIG's reference period for --STRF and --ENRF: BEFORE RFSTDTC, DURING up to RFENDTC, or AFTER.
reference_period <- function(subjects, ids, dates) {
  at <- match(ids, subjects$USUBJID)
  start <- as.numeric(subjects[["DM.RFSTDTC"]] %||% rep(NA, nrow(subjects)))[at]
  end <- as.numeric(subjects[["DM.RFENDTC"]] %||% rep(NA, nrow(subjects)))[at]
  d <- as.numeric(dates)
  ifelse(is.na(d) | is.na(start), "UNKNOWN", ifelse(d < start, "BEFORE", ifelse(is.na(end) | d <= end, "DURING", "AFTER")))
}

fit_count <- function(counts, subjects, s) {
  info <- design_info(subjects, intersect(s$predictors, names(subjects)))
  X <- design_matrix(info, subjects)
  poisson <- suppressWarnings(stats::glm.fit(X, counts, family = stats::poisson()))
  mu <- poisson$fitted.values
  dispersion <- sum((counts - mu)^2 / mu) / max(1, length(counts) - poisson$rank)
  nb <- if (dispersion > 1.5) tryCatch(suppressWarnings(MASS::glm.nb(counts ~ X - 1)), error = function(e) NULL)
  coef <- unname(if (is.null(nb)) poisson$coefficients else stats::coef(nb))
  list(info = info, coef = ifelse(is.na(coef), 0, coef), theta = nb$theta, max = max(counts),
    description = sprintf("%s count, mean %.1f per subject", if (is.null(nb)) "Poisson" else "negative binomial", mean(counts)))
}

draw_count <- function(fit, subjects) {
  mu <- exp(drop(design_matrix(fit$info, subjects) %*% fit$coef))
  n <- if (is.null(fit$theta)) stats::rpois(length(mu), mu) else stats::rnbinom(length(mu), size = fit$theta, mu = mu)
  pmin(n, fit$max)
}

# Whether each subject's window, to the end date of the visit, lasts until its early day.
visit_reached <- function(reach, subjects, s, ids, visits) {
  at <- match(ids, subjects$USUBJID)
  visits <- rep_len(visits, length(ids))
  end <- unname(reach$bound[visits])
  end[is.na(end)] <- s$reach[["end"]]
  span <- vapply(seq_along(ids), function(i) as.numeric(subjects[[end[i]]][at[i]]), 0) - as.numeric(subjects[[s$reach[["start"]]]])[at]
  gap <- span - reach$day[visits]
  is.na(gap) | gap >= 0
}


# ---- Study fits -----------------------------------------------------------------------

# A node is refitted only when its specification changed since the last fit. The arm is
# fitted like any categorical node: without predictors that is the allocation ratio.
fit_dataset <- function(study, name, previous = NULL) {
  ds <- study$datasets[[name]]
  data <- study$source$data
  arm <- arm_variable(study$datasets$DM)
  fits <- list(nodes = list(), specs = list())
  if (name != "DM") fits$structure <- RECORD_STRUCTURES[[ds$structure$type]]$fit(data[[name]],
    subject_inputs(study, data, name), paste0("DM.", treatment_variable(study$datasets$DM)), ds$structure, subject_visits(data, ds))
  frame <- dataset_frame(study, name)
  if (isTRUE(ds$structure$sv) && identical(node_type(ds, "VISITNUM"), "sampled"))
    fits$position <- fit_position(ds, frame, subject_inputs(study, data, name), allowed_visits(data, ds))
  for (v in variables_of_type(ds, "sampled")) {
    spec <- list(node = ds$nodes[[v]], level = ds$levels[[v]], structure = ds$structure)
    fits$nodes[[v]] <- if (identical(previous$specs[[v]], spec)) previous$nodes[[v]] else fit_node(ds, v, frame, arm)
    fits$specs[[v]] <- spec
  }
  fits
}

# Every included dataset, one after another (the app's build fits them in parallel workers).
fit_study <- function(study, progress = function(name) NULL) {
  included <- names(study$datasets)[vapply(study$datasets, `[[`, TRUE, "include")]
  for (name in included) {
    progress(name)
    study$fits[[name]] <- fit_dataset(study, name, study$fits[[name]])
  }
  study$fits <- study$fits[included]
  study
}
