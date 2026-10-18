# Models for sampled nodes. Records and dates are never modelled: they are the spine (see
# spine.R).
#
# A fitted node is a set of parts, one per test (findings) and stage (the first record of a
# series, or a later one given the previous value). Each part models one value:
#   categories          multinomial over the observed values, missing being a value
#   number              whether a value is recorded, then a linear model of its normal score
#                       (its percentile in the source, on the normal scale); a drawn score is
#                       read back through the source's percentiles, so the value keeps the
#                       source's shape and repeated values (a dose of 1 mg); the ends leave
#                       out the most extreme subjects
#   tree                a classification or regression tree (CART, as in synthpop): a value
#                       is drawn from the counts of its leaf, or a number from its quantiles
# Fits keep coefficients and summaries only, never source rows; implications keep the
# values they tie together. No fit, lookup table or implication keeps a value held by fewer
# than min_subjects() subjects.

MIN_SUBJECTS <- 5
MAX_MODEL_LEVELS <- 30
MAX_DISCRETE_VALUES <- 10
NUMBER_QUANTILES <- seq(0, 1, 0.01)
NODE_MODELS <- c(auto = "Linear (automatic)", number = "Linear (number)", categories = "Categories", tree = "Tree (CART)")
# The model of a new node: "auto" for the linear models (the default), or "tree" (option
# synth.model, or SYNTH_MODEL in the environment, which the workers share).
default_node_model <- function() getOption("synth.model", Sys.getenv("SYNTH_MODEL", "auto"))

# The fewest subjects a value must be held by to be kept (k): a category value, a predictor
# level, a lookup row or an implication held by fewer is dropped, and a number's ends leave
# out the k subjects with the lowest and the k with the highest values (option
# synth.min_subjects, or SYNTH_MIN_SUBJECTS in the environment, which the workers share).
min_subjects <- function() as.integer(getOption("synth.min_subjects", Sys.getenv("SYNTH_MIN_SUBJECTS", MIN_SUBJECTS)))

# For each record, the number of subjects holding its value (missing being a value).
subjects_holding <- function(value, subject) {
  code <- match(value, unique(value))
  first <- !duplicated(data.frame(code, subject))
  tabulate(code[first], max(code, 0L))[code]
}

# Spine features that say the subject died: T.DIED, a date equal to DTHDTC. The spine copies
# the death date, so a value these force (DEATH, FATAL) may be held by fewer than
# min_subjects() subjects (see `fit_implications()` and the lookup's `learn`).
DEATH_KEY <- "^T[.](DIED|.*=DTHDTC)$"

# Records dated on the subject's death date (in DM, those of subjects who died where the
# key is a death feature): a lookup keeps a key of fewer than min_subjects() subjects found
# only there (DEATH).
death_records <- function(frame, inputs) {
  died <- frame[["DM.DTHDTC"]]
  if (is.null(died)) return(all(grepl(DEATH_KEY, inputs)) & frame[["T.DIED"]] %in% "Y")
  dates <- date_columns(frame)
  same <- lapply(frame[dates[!grepl("^(DM|LINK)[.]", dates)]], function(v) as.numeric(v) == as.numeric(died))
  Reduce(`|`, lapply(same, `%in%`, TRUE), rep(FALSE, nrow(frame)))
}

# ---- Design matrices ------------------------------------------------------------------

# Numeric predictors are standardised (missing values sit at the mean); categorical ones
# use the most common level as reference. Levels of fewer than min_subjects() subjects and
# unseen levels share the reference.
design_info <- function(frame, predictors, categorical = character(), subject = frame$USUBJID) {
  lapply(setNames(predictors, predictors), function(p) {
    v <- frame[[p]]
    if (inherits(v, "Date")) v <- as.numeric(v)
    if (is.numeric(v) && !p %in% categorical) {
      center <- mean(v, na.rm = TRUE)
      scale <- stats::sd(v, na.rm = TRUE)
      list(type = "numeric", center = if (is.finite(center)) center else 0,
        scale = if (is.finite(scale) && scale > 0) scale else 1)
    } else {
      s <- level_string(v)
      counts <- sort(table(s), decreasing = TRUE)
      levels <- intersect(names(counts), s[subjects_holding(s, subject) >= min_subjects()])
      list(type = "categorical", levels = if (length(levels)) levels else "")
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

# Coefficients of codes 2..k against code 1, one row per code, and with `se` standard errors
# of the same shape (only a node drawn with its uncertainty needs them; the Hessian costs a
# product per distinct design row, most of a large dataset's fit). Without predictors, or
# with too many values to model, the estimates are the observed log odds; identical design
# rows are otherwise fitted once, weighted.
fit_multinomial <- function(X, codes, k, se = FALSE) {
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
  fit <- nnet::multinom(.y ~ ., data = data, weights = tabulate(match(key, key[rows])), Hess = se,
    trace = FALSE, MaxNWts = 1e6, maxit = 200)
  coef <- matrix(stats::coef(fit), k - 1, dimnames = dimnames(zero))
  coef[is.na(coef)] <- 0
  list(coef = coef, se = if (se) tryCatch(matrix(sqrt(pmax(diag(solve(fit$Hessian)), 0)), k - 1, byrow = TRUE, dimnames = dimnames(zero)),
    error = function(e) zero) else zero)
}

# The design column of each predictor ("" for the intercept).
design_groups <- function(info) c("", rep(names(info), vapply(info, function(i) if (i$type == "numeric") 1L else length(i$levels) - 1L, 0L)))

# AIC of a model part with the columns of X (identical rows fitted once, weighted).
part_aic <- function(y, X, type) {
  if (type == "categories") {
    values <- level_string(y)
    key <- do.call(paste, c(as.data.frame(X), list(values), sep = "\r"))
    rows <- which(!duplicated(key))
    data <- data.frame(.y = factor(values[rows]), X[rows, -1, drop = FALSE])
    names(data)[-1] <- paste0("x", seq_len(ncol(X) - 1))
    formula <- if (ncol(X) > 1) .y ~ . else .y ~ 1
    return(nnet::multinom(formula, data = data, weights = tabulate(match(key, key[rows])), trace = FALSE, MaxNWts = 1e6, maxit = 200)$AIC)
  }
  observed <- !is.na(y)
  z <- y[observed]
  fit <- stats::lm.fit(X[observed, , drop = FALSE], z)
  length(z) * log(max(sum(fit$residuals^2), 1e-12) / length(z)) + 2 * fit$rank
}

# One model part. A category with too many values to model and a coarser category derived
# from it (`coarse`, given by `fit_node()`) models the coarse value; its values are drawn
# within the coarse one (see `fit_within`). Predictors are used only with at least 10 values per coefficient; for
# categories, 10 values outside the most common one (a rare value is not learnt per level).
# When not all of them fit, whole predictors are added one at a time, the one that lowers
# the AIC most first, while they fit and lower it (36 retrieval terms keep the visit).
fit_part <- function(y, X, type, groups = colnames(X), coarse = NULL, se = FALSE, strata = character(), subject = seq_along(y)) {
  if (type == "categories" && !is.null(coarse)) {
    fit <- fit_part(coarse, X, type, groups, se = se)
    fit$within <- fit_within(level_string(y), level_string(coarse), X, groups, strata, subject)
    fit$numeric <- is.numeric(y)
    return(fit)
  }
  observed <- !is.na(y)
  if (!any(observed)) type <- "categories"
  values <- table(level_string(y))
  n <- if (type == "categories") length(y) - max(values) else sum(observed)
  columns <- if (n >= 10 * ncol(X)) seq_len(ncol(X)) else 1L
  if (length(columns) == 1 && n >= 20 && (type != "categories" || length(values) <= MAX_MODEL_LEVELS)) {
    candidates <- unique(groups[-1])
    best <- part_aic(y, X[, 1, drop = FALSE], type)
    repeat {
      candidates <- Filter(function(g) length(columns) + sum(groups == g) <= n / 10, candidates)
      if (!length(candidates)) break
      aic <- vapply(candidates, function(g) part_aic(y, X[, c(columns, which(groups == g)), drop = FALSE], type), 0)
      if (min(aic) >= best) break
      best <- min(aic)
      columns <- sort(c(columns, which(groups == candidates[which.min(aic)])))
      candidates <- candidates[-which.min(aic)]
    }
  }
  X <- X[, columns, drop = FALSE]
  if (type == "categories") {
    values <- level_string(y)
    levels <- names(sort(table(values), decreasing = TRUE))
    return(c(fit_multinomial(X, match(values, levels), length(levels), se),
      list(type = type, columns = columns, levels = levels, numeric = is.numeric(y))))
  }
  z <- y[observed]
  X_obs <- X[observed, , drop = FALSE]
  fit <- stats::lm.fit(X_obs, z)
  sigma <- sqrt(sum(fit$residuals^2) / max(1, length(z) - fit$rank))
  variance <- tryCatch(diag(solve(crossprod(X_obs))), error = function(e) rep(0, ncol(X)))
  list(type = type, columns = columns, sigma = sigma,
    coef = matrix(ifelse(is.na(fit$coefficients), 0, fit$coefficients), 1, dimnames = list(NULL, colnames(X))),
    se = matrix(sigma * sqrt(pmax(variance, 0)), 1, dimnames = list(NULL, colnames(X))),
    recorded = if (!all(observed)) fit_multinomial(X, 2L - observed, 2L))
}

# Fine values within a coarse one (`fit_part`) are also drawn within the levels of the
# categorical predictors that tell them apart (an application-site reaction only after the
# first dose): predictors are added one at a time while they lower the AIC. Subjects count,
# not records (a subject's events share their last visit), so a record weighs one over its
# subject's records. A coarse value whose predictor levels have fewer than min_subjects()
# subjects uses its table over all levels. Rare values (RARE, see `fit_node()`) are left out.
fit_within <- function(fine, coarse, X, groups, strata, subject) {
  drawn <- fine != RARE
  fine <- fine[drawn]
  coarse <- coarse[drawn]
  X <- X[drawn, , drop = FALSE]
  subject <- subject[drawn]
  weight <- 1 / as.vector(table(subject)[subject])
  big <- function(key) subjects_holding(key, subject) >= min_subjects()
  share <- function(key) { cell <- paste(key, fine, sep = "\t"); as.vector(table(cell)[cell] / table(key)[key]) }
  overall <- share(coarse)
  cells <- function(by) paste(coarse, if (length(by)) do.call(paste, c(as.data.frame(X[, groups %in% by, drop = FALSE]), sep = " ")), sep = "\r")
  aic <- function(by) {
    key <- cells(by)
    inside <- big(key)
    p <- ifelse(inside, share(key), overall)
    used <- c(key[inside], coarse[!inside])
    -2 * sum(weight * log(p)) + 2 * sum(tapply(c(fine[inside], fine[!inside]), used, function(v) length(unique(v)) - 1))
  }
  by <- character()
  best <- aic(by)
  repeat {
    candidates <- setdiff(strata, by)
    if (!length(candidates)) break
    scores <- vapply(candidates, function(g) aic(c(by, g)), 0)
    if (min(scores) >= best) break
    by <- c(by, candidates[which.min(scores)])
    best <- min(scores)
  }
  key <- cells(by)
  inside <- big(key)
  list(by = by, coarse = lapply(split(fine, coarse), table), cells = if (length(by)) lapply(split(fine[inside], key[inside]), table))
}

draw_within <- function(w, coarse, X, groups) {
  key <- paste(coarse, if (length(w$by)) do.call(paste, c(as.data.frame(X[, groups %in% w$by, drop = FALSE]), sep = " ")), sep = "\r")
  out <- character(length(coarse))
  for (k in unique(key)) {
    i <- which(key == k)
    t <- w$cells[[k]] %||% w$coarse[[coarse[i[1]]]]
    out[i] <- names(t)[sample.int(length(t), length(i), replace = TRUE, prob = t)]
  }
  out
}

# Values on a grid coarser than their decimals (whole mg/dL converted to mmol/L: glucose
# 0.05551 apart) as origin and step, so drawn values stay conversions of whole numbers. The
# origin is the grid point nearest `at`, not the source minimum.
value_grid <- function(v, at = min(v)) {
  u <- sort(unique(v))
  tolerance <- 10^-decimals(v)
  if (length(u) < 10 || min(diff(u)) <= 1.5 * tolerance) return(NULL)
  k <- round((u - u[1]) / min(diff(u)))
  fit <- stats::lm.fit(cbind(1, k), u)
  if (max(abs(fit$residuals)) > tolerance) return(NULL)
  step <- unname(fit$coefficients[2])
  c(unname(fit$coefficients[1]) + round((at - fit$coefficients[1]) / step) * step, step)
}

# A number's source percentiles, with ends that leave out the k subjects with the lowest and
# the k with the highest values, so the real minimum and maximum are not kept: percentiles
# beyond are set to the ends. With too few subjects every percentile is the median.
number_margin <- function(v, subject, k = min_subjects()) {
  observed <- !is.na(v)
  v <- v[observed]
  subject <- subject[observed]
  q <- stats::quantile(v, NUMBER_QUANTILES, names = FALSE)
  ends <- number_ends(v, subject, k)
  pmin(pmax(q, ends[1]), ends[2])
}

number_ends <- function(v, subject, k = min_subjects()) {
  low <- sort(tapply(v, subject, min))
  high <- sort(tapply(v, subject, max), decreasing = TRUE)
  ends <- if (length(low) > k) unname(c(low[k + 1], high[k + 1])) else c(Inf, -Inf)
  if (ends[1] > ends[2]) rep(stats::median(v), 2) else ends
}

# Normal score of x given a number's source percentiles q: its percentile (a repeated value
# the middle of its percentiles), on the normal scale.
normal_score <- function(q, x) {
  u <- stats::approx(q, NUMBER_QUANTILES, x, ties = mean, rule = 2)$y
  stats::qnorm(pmin(pmax(u, 0.005), 0.995))
}

# The model part of each record: its test, and whether it follows an earlier value of its
# series.
node_parts <- function(ds, variable, frame, previous) {
  s <- ds$structure
  test <- if (!is.null(s$test) && ds$levels[[variable]] == "record") format_value(frame[[s$test]]) else rep("", nrow(frame))
  list(key = paste(test, ifelse(is.na(previous), "first", "follow"), sep = "|"), scale = test)
}

# A node is fitted on one row per subject, per subject and visit, or per record. The model
# type is chosen once per test, so first and later records share a scale: categories for
# text and numbers with few values, otherwise numbers (on their normal scores, with the
# source's percentiles of the test). A trajectory's first record is fitted without the arm (it is measured before
# treatment); later records add the previous recorded value, and a number also the mean of
# the earlier ones (so it returns to the subject's level instead of drifting like a random
# walk). A category also keeps its implications (see implications.R), and one value per subject in a
# test where each subject has one, however many records of it they have.
fit_node <- function(ds, variable, frame, arm) {
  node <- ds$nodes[[variable]]
  s <- ds$structure
  level <- if (ds$name == "DM") "record" else ds$levels[[variable]]
  frame <- frame[level_rows(frame, level, s), , drop = FALSE]
  y <- frame[[variable]]
  previous <- earlier <- rep(NA_real_, nrow(frame))
  if (isTRUE(node$prev)) {
    series <- group_index(frame, c("USUBJID", setdiff(s$keys, s$visit)))
    o <- order(series, as.numeric(frame[[s$visit]]))
    last <- c(0, cummax(ifelse(is.na(y[o]), 0, seq_along(o))))[seq_along(o)]
    follows <- last > 0 & series[o][pmax(last, 1)] == series[o]
    previous[o[follows]] <- y[o][last[follows]]
    x <- parse_number(y[o])
    seen <- !is.na(x)
    sums <- stats::ave(ifelse(seen, x, 0), series[o], FUN = cumsum) - ifelse(seen, x, 0)
    counts <- stats::ave(as.numeric(seen), series[o], FUN = cumsum) - seen
    earlier[o] <- ifelse(counts > 0, sums / counts, NA)
  }
  parts <- node_parts(ds, variable, frame, previous)
  # Text that reads as a number in every record of a test (VSORRES "58.0") is modelled as
  # that number and written back in the source's format.
  numbers <- parse_number(y)
  by_test <- split(seq_along(y), parts$scale)
  as_number <- vapply(by_test, function(i) is.numeric(y) || all(!is.na(numbers[i][!is.na(y[i])])), TRUE)
  value <- function(i) if (as_number[match(parts$scale[i[1]], names(by_test))]) numbers[i] else y[i]
  arm <- c(arm, paste0("DM.", arm))
  predictors <- intersect(node$predictors, names(frame))
  tree <- node$model == "tree"
  model <- if (tree) "auto" else node$model
  coarse <- coarse_values(ds, variable, y, frame)
  types <- vapply(by_test, function(i) {
    v <- value(i)
    v <- v[!is.na(v)]
    if (!is.numeric(v) || model == "categories" || (model == "auto" && length(unique(v)) <= MAX_DISCRETE_VALUES) || length(unique(v)) < 2)
      "categories" else "number"
  }, "")
  margins <- lapply(by_test, function(i) { v <- value(i); if (is.numeric(v)) number_margin(v, frame$USUBJID[i]) })
  # A category value held by fewer than min_subjects() subjects of its test is never drawn:
  # its records are left out of the fit (a test with none left stays missing), and the
  # implications pool it. Where a lookup derives a coarse value from the node (DSDECOD from
  # DSTERM), a rare value whose coarse value is common but has no common value of its own is
  # drawn as that coarse value (WITHDRAWAL BY SUBJECT, whose 22 verbatims are each rare). A
  # test with too many values to model, drawn within its coarse value, keeps the records of
  # every common coarse value in that model; its rare values are not drawn within it.
  kept <- rep(TRUE, length(y))
  within <- rep(FALSE, length(y))
  fine <- y
  for (i in by_test[types == "categories"]) {
    held <- is.na(y[i]) | subjects_holding(level_string(y[i]), frame$USUBJID[i]) >= min_subjects()
    if (is.null(coarse) || !is.character(y)) {
      kept[i] <- held
      next
    }
    common <- subjects_holding(coarse[i], frame$USUBJID[i]) >= min_subjects()
    fine[i[!held]] <- ifelse(common[!held] & !coarse[i[!held]] %in% coarse[i[held]], coarse[i[!held]], RARE)
    within[i] <- !tree && length(unique(level_string(y[i]))) > MAX_MODEL_LEVELS
    kept[i] <- if (within[i[1]]) common else !fine[i] %in% RARE
  }
  fits <- lapply(split(which(kept), parts$key[kept]), function(i) {
    follow <- grepl("|follow", parts$key[i[1]], fixed = TRUE)
    type <- unname(types[match(parts$scale[i[1]], names(types))])
    margin <- if (type == "number" && !tree) margins[[match(parts$scale[i[1]], names(margins))]]
    records <- frame[i, , drop = FALSE]
    records$.previous <- if (!is.null(margin)) normal_score(margin, parse_number(previous[i])) else if (type == "number") parse_number(previous[i]) else previous[i]
    records$.earlier <- if (!is.null(margin)) normal_score(margin, earlier[i]) else earlier[i]
    used <- if (follow) c(predictors, ".previous", if (type == "number") ".earlier") else if (isTRUE(node$prev)) setdiff(predictors, arm) else predictors
    text <- if (type == "number" && is.character(y)) number_format(y[i])
    if (tree) return(c(fit_tree(if (type == "number") value(i) else fine[i], records, used, type == "categories"), list(text = text)))
    info <- design_info(records, used, c("VISITNUM", s$test))
    if (type == "categories") return(c(fit_part(fine[i], design_matrix(info, records), type, design_groups(info), if (within[i[1]]) coarse[i], isTRUE(node$vary),
      names(Filter(function(p) p$type == "categorical", info)), records$USUBJID), list(info = info)))
    v <- value(i)
    v <- v[!is.na(v)]
    c(fit_part(normal_score(margin, value(i)), design_matrix(info, records), type, design_groups(info)),
      list(info = info, margin = margin, digits = decimals(v), grid = value_grid(v, margin[1]), text = text))
  })
  # Implications are found within a test: the last visit gives PROTOCOL COMPLETED to the
  # disposition event, not to the subject's other DS records. A number keeps those of its
  # missing value (no result where the test was NOT DONE).
  keys <- implication_keys(ds, frame)
  if (ds$name != "DM" && level == "subject") keys <- keys[!record_feature(keys)]
  implications <- do.call(rbind, lapply(by_test, function(i) {
    out <- if (types[match(parts$scale[i[1]], names(types))] == "categories")
      rbind(fit_implications(fine[i], frame[i, , drop = FALSE], keys, !kept[i] | fine[i] %in% RARE, y[i]),
        derived_implications(ds, variable, y[i], frame[i, , drop = FALSE], keys, fine[i], !kept[i] | fine[i] %in% RARE)) else {
      out <- fit_implications(ifelse(is.na(y[i]), NA, "recorded"), frame[i, , drop = FALSE], keys)
      if (NROW(out)) out <- out[vapply(out$value, identical, TRUE, MISSING), , drop = FALSE]
      # A key level whose records hold no amount, only zero or missing (a placebo arm's drug
      # concentrations), keeps none: an amount drawn there becomes the commoner of the two.
      none <- fit_implications(ifelse(no_amount(y[i]), NONE, "amount"), frame[i, , drop = FALSE], keys)
      if (NROW(none)) none <- none[none$kind == "forced" & vapply(none$value, identical, TRUE, NONE) &
        !paste(none$key, none$level) %in% paste(out$key, out$level), , drop = FALSE]
      if (NROW(none)) none$typed <- I(lapply(seq_len(nrow(none)), function(j) {
        at <- level_string(frame[[none$key[j]]][i]) == none$level[[j]]
        if (mean(is.na(y[i][at])) >= 0.5) NA else y[i][at & !is.na(y[i])][1]
      }))
      rbind(out, none)
    }
    if (!NROW(out)) return(NULL)
    within <- length(unique(parts$scale)) > 1
    out$within <- if (within) s$test else NA_character_
    out$part <- if (within) parts$scale[i[1]] else NA_character_
    out
  }))
  reduced <- sum(vapply(fits, function(p) p$type != "tree" && length(p$columns) < length(design_groups(p$info)), TRUE))
  description <- sprintf("%s%s%s, %d %s%s%s", if (tree) sprintf("trees (%s leaves)", paste(range(vapply(fits, `[[`, 0L, "leaves")), collapse = "-"))
    else paste(sort(unique(types)), collapse = "/"),
    if (length(unique(parts$scale)) > 1) paste(" per", s$test) else "",
    if (isTRUE(node$prev)) " with previous values" else "", nrow(frame), if (level == "record") "records" else paste(level, "rows"),
    if (reduced) sprintf(", fewer predictors in %d of %d parts (few values)", reduced, length(fits)) else "",
    if (NROW(implications)) sprintf(", %d implication%s", nrow(implications), if (nrow(implications) > 1) "s" else "") else "")
  # The text a missing --STRESN has in its test (PCSTRESC <BLQ), where every such record has
  # the same one and at least min_subjects() subjects hold it.
  result <- paste0(ds$prefix, "STRESC")
  missing_text <- if (variable == paste0(ds$prefix, "STRESN") && result %in% names(frame)) unlist(lapply(by_test, function(i) {
    i <- i[is.na(y[i])]
    w <- frame[[result]][i]
    if (length(w) && !anyNA(w) && length(unique(w)) == 1 && subjects_holding(w, frame$USUBJID[i])[1] >= min_subjects()) w[1]
  }))
  list(parts = fits, arm = arm, description = description, implications = implications,
    constant = if (level == "record" && !is.null(s$test)) intersect(constant_tests(frame, s$test, variable), names(types)[types == "categories"]),
    missing_text = missing_text)
}

# How a number was written: the decimals every value has (VSORRES "58.0"), or NA when they
# vary, for the shortest form.
number_format <- function(v) {
  v <- v[!is.na(v)]
  written <- nchar(sub("^[^.]*\\.?", "", v))
  if (length(unique(written)) == 1) written[1] else NA_integer_
}

number_text <- function(x, fixed, digits) {
  x <- round(x, if (is.na(fixed)) digits else fixed)
  x[!is.na(x) & x == 0] <- 0
  out <- if (is.na(fixed)) formatC(x, format = "f", digits = digits, drop0trailing = TRUE) else formatC(x, format = "f", digits = fixed)
  out[is.na(x)] <- NA
  out
}

# The values of the coarsest useful category derived from a node by an accepted lookup on
# the node alone (DSDECOD from 36 verbatim DSTERM terms, a body system from AELLT): the one
# with most values, at most MAX_MODEL_LEVELS. NULL when there is none. The source's values
# are used where it has the column and the table was not edited, as the table leaves out
# rare verbatims.
coarse_values <- function(ds, variable, y, frame) {
  rules <- Filter(function(r) r$op == "lookup" && identical(r$inputs, variable), accepted_rules(ds))
  if (!length(rules)) return(NULL)
  values <- level_string(y)
  first <- frame[!duplicated(values), , drop = FALSE]
  derived <- lapply(rules, function(r) if (!isTRUE(r$params$edited) && r$target %in% names(frame)) level_string(frame[[r$target]]) else
    level_string(RULE_OPS$lookup$apply(r, first))[match(values, level_string(first[[variable]]))])
  n <- vapply(derived, function(d) length(unique(d)), 0L)
  ok <- which(n > 1 & n <= MAX_MODEL_LEVELS)
  if (length(ok)) derived[[ok[which.max(n[ok])]]]
}

# A node's rows: one per subject, per subject and visit, per series, or every record.
level_rows <- function(frame, level, s) {
  key <- level_key(level, s)
  if (is.null(key)) seq_len(nrow(frame)) else which(!duplicated(frame[key]))
}

level_key <- function(level, s) switch(level, subject = "USUBJID", visit = c("USUBJID", s$visit), series = c("USUBJID", ".series"), record = NULL)

# ---- Trees ----------------------------------------------------------------------------

# synthpop's CART defaults: leaves of at least 5 records, grown until no split helps.
TREE_MIN_LEAF <- 5
TREE_QUANTILES <- seq(0, 1, 0.05)

# Predictors as rpart reads them: numbers (dates as days, missing below every value, so a
# split can set it apart) or factors over the fitted levels (an unseen level is the most
# common one). Every record then reaches a leaf.
TREE_MISSING <- -1e15
tree_data <- function(records, predictors, levels = NULL, common = NULL) {
  out <- lapply(setNames(predictors, predictors), function(p) {
    v <- records[[p]]
    if (inherits(v, "Date")) v <- as.numeric(v)
    if (is.numeric(v) && is.null(levels[[p]])) return(ifelse(is.na(v), TREE_MISSING, v))
    v <- level_string(v)
    if (!is.null(levels[[p]])) v[!v %in% levels[[p]]] <- common[[p]]
    factor(v, levels[[p]] %||% sort(unique(v)))
  })
  as.data.frame(out, col.names = paste0("x", seq_along(predictors)))
}

# A category is fitted as one class (missing being a class); a number as whether it is
# recorded and, when recorded, its value. The tree keeps its splits and, per leaf, the class
# counts or 21 quantiles of the value (within the number's ends, see `number_margin()`);
# leaves are found again by numbering the tree's nodes. A predictor level of fewer than
# min_subjects() subjects is read as the most common level.
fit_tree <- function(y, records, predictors, categorical) {
  x <- tree_data(records, predictors)
  for (p in names(x)) if (is.factor(x[[p]])) {
    v <- as.character(x[[p]])
    rare <- subjects_holding(v, records$USUBJID) < min_subjects()
    v[rare] <- names(which.max(table(v)))
    x[[p]] <- factor(v)
  }
  levels <- setNames(lapply(x, levels), predictors)
  common <- lapply(x, function(v) if (is.factor(v)) names(which.max(table(v))))
  control <- rpart::rpart.control(minbucket = TREE_MIN_LEAF, minsplit = 2 * TREE_MIN_LEAF, cp = 1e-8, xval = 0,
    maxcompete = 0, maxsurrogate = 0)
  grow <- function(target, rows, method) {
    data <- cbind(.y = target, x[rows, , drop = FALSE])
    tree <- if (length(predictors) && length(unique(target)) > 1) rpart::rpart(.y ~ ., data, method = method, control = control, model = FALSE, x = FALSE, y = FALSE)
    leaf <- if (is.null(tree)) rep(1L, length(rows)) else tree$where
    if (!is.null(tree)) {
      tree$frame$yval <- seq_len(nrow(tree$frame))
      tree$where <- NULL
      tree$call <- NULL
    }
    list(tree = tree, leaf = leaf)
  }
  out <- list(type = "tree", predictors = predictors, levels = levels, common = setNames(common, predictors), categorical = categorical, numeric = is.numeric(y))
  if (categorical) {
    values <- level_string(y)
    g <- grow(factor(values), seq_along(y), "class")
    out$classes <- sort(unique(values))
    out$counts <- table(factor(g$leaf, unique(g$leaf)), factor(values, out$classes))
    out$leaves <- nrow(out$counts)
    out$tree <- g$tree
    return(out)
  }
  observed <- !is.na(y)
  if (!all(observed)) {
    g <- grow(factor(observed), seq_along(y), "class")
    out$recorded <- list(tree = g$tree, counts = table(factor(g$leaf, unique(g$leaf)), factor(observed, c(FALSE, TRUE))))
  }
  g <- grow(y[observed], which(observed), "anova")
  out$tree <- g$tree
  ends <- number_ends(y[observed], records$USUBJID[observed])
  out$quantiles <- pmin(pmax(do.call(rbind, tapply(y[observed], factor(g$leaf, unique(g$leaf)), stats::quantile, TREE_QUANTILES, names = FALSE)), ends[1]), ends[2])
  out$leaves <- nrow(out$quantiles)
  out$digits <- decimals(y[observed])
  out$grid <- value_grid(y[observed], ends[1])
  out
}

# The leaf (row name of the counts or quantiles) of each record. A record that stops at an
# inner node (a level no record at that node had) gets one of the leaves below it, in
# proportion to their records, which draws from the node's pooled distribution.
tree_leaf <- function(tree, x) {
  if (is.null(tree)) return(rep("1", nrow(x)))
  row <- stats::predict(tree, x, type = "vector")
  frame <- tree$frame
  id <- as.integer(rownames(frame))
  leaves <- which(frame$var == "<leaf>")
  for (r in unique(row[!row %in% leaves])) {
    below <- leaves[vapply(id[leaves], function(l) {
      while (l > id[r]) l <- l %/% 2
      l == id[r]
    }, TRUE)]
    i <- which(row == r)
    row[i] <- below[sample.int(length(below), length(i), TRUE, frame$n[below])]
  }
  as.character(row)
}

draw_from_counts <- function(counts, leaf) {
  p <- counts[leaf, , drop = FALSE]
  colnames(counts)[pmin(1L + rowSums(p %*% upper.tri(diag(ncol(p)), diag = TRUE) < stats::runif(nrow(p)) * rowSums(p)), ncol(p))]
}

draw_tree <- function(p, records) {
  x <- tree_data(records, p$predictors, p$levels, p$common)
  if (p$categorical) {
    drawn <- draw_from_counts(p$counts, tree_leaf(p$tree, x))
    drawn[drawn == MISSING] <- NA
    return(if (p$numeric) as.numeric(drawn) else drawn)
  }
  drawn <- quantile_draw(p$quantiles[tree_leaf(p$tree, x), , drop = FALSE])
  if (!is.null(p$grid)) drawn <- p$grid[1] + round((drawn - p$grid[1]) / p$grid[2]) * p$grid[2]
  drawn <- round(drawn, p$digits)
  if (!is.null(p$recorded)) drawn[draw_from_counts(p$recorded$counts, tree_leaf(p$recorded$tree, x)) == "FALSE"] <- NA
  drawn
}

# ---- Drawing --------------------------------------------------------------------------

# The value at probability u of each row of q (equally spaced quantiles of a distribution),
# linear between quantiles; repeated quantiles give a repeated value.
quantile_draw <- function(q, u = stats::runif(nrow(q))) {
  u <- u * (ncol(q) - 1)
  k <- pmin(floor(u), ncol(q) - 2)
  rows <- seq_len(nrow(q))
  q[cbind(rows, k + 1)] + (u - k) * (q[cbind(rows, k + 2)] - q[cbind(rows, k + 1)])
}

# Codes 1..k drawn from the multinomial probabilities of each row of linear predictors.
softmax_draw <- function(eta) {
  eta <- cbind(0, eta)
  p <- exp(eta - apply(eta, 1, max))
  cumulative <- p %*% upper.tri(diag(ncol(p)), diag = TRUE)
  pmin(1L + rowSums(cumulative < stats::runif(nrow(p)) * rowSums(p)), ncol(p))
}

draw_records <- function(ds, variable, fit, frame, previous, earlier = rep(NA_real_, length(previous))) {
  parts <- node_parts(ds, variable, frame, previous)
  values <- rep(NA, nrow(frame))
  for (key in intersect(unique(parts$key), names(fit$parts))) {
    p <- fit$parts[[key]]
    i <- which(parts$key == key)
    records <- frame[i, , drop = FALSE]
    records$.previous <- if (!is.null(p$margin)) normal_score(p$margin, parse_number(previous[i])) else if (!is.null(p$text)) parse_number(previous[i]) else previous[i]
    records$.earlier <- if (!is.null(p$margin)) normal_score(p$margin, earlier[i]) else earlier[i]
    if (p$type == "tree") {
      drawn <- draw_tree(p, records)
      values[i] <- if (is.null(p$text)) drawn else number_text(drawn, p$text, p$digits)
      next
    }
    X <- design_matrix(p$info, records)[, p$columns, drop = FALSE]
    eta <- X %*% t(p$coef)
    if (p$type == "categories") {
      drawn <- p$levels[softmax_draw(eta)]
      if (!is.null(p$within)) drawn <- draw_within(p$within, drawn, design_matrix(p$info, records), design_groups(p$info))
      drawn[drawn == MISSING] <- NA
      if (p$numeric) drawn <- as.numeric(drawn)
    } else {
      u <- stats::pnorm(stats::rnorm(length(i), drop(eta), p$sigma))
      drawn <- quantile_draw(matrix(p$margin, length(i), length(p$margin), byrow = TRUE), u)
      if (!is.null(p$grid)) drawn <- p$grid[1] + round((drawn - p$grid[1]) / p$grid[2]) * p$grid[2]
      drawn <- round(drawn, p$digits)
      if (!is.null(p$recorded)) drawn[softmax_draw(X %*% t(p$recorded$coef)) == 2] <- NA
      if (!is.null(p$text)) drawn <- number_text(drawn, p$text, p$digits)
    }
    values[i] <- drawn
  }
  values
}

# Draws a node at its level, visit by visit when it depends on the previous value, keeps its
# implications, and copies each value to every record of that level. A varying arm effect
# draws the arm coefficients once from N(estimate, SE).
draw_node <- function(ds, variable, fit, frame) {
  node <- ds$nodes[[variable]]
  s <- ds$structure
  level <- if (ds$name == "DM") "record" else ds$levels[[variable]]
  rows <- level_rows(frame, level, s)
  part <- frame[rows, , drop = FALSE]
  if (isTRUE(node$vary)) fit$parts <- lapply(fit$parts, function(p) {
    if (p$type == "tree") return(p)
    arm <- sub("=.*$", "", colnames(p$coef)) %in% fit$arm
    p$coef[, arm] <- stats::rnorm(length(p$coef[, arm]), p$coef[, arm], p$se[, arm])
    p
  })
  series <- if (isTRUE(node$prev)) group_index(part, c("USUBJID", setdiff(s$keys, s$visit))) else rep(1L, nrow(part))
  rank <- if (isTRUE(node$prev)) stats::ave(as.numeric(part[[s$visit]]), series, FUN = function(v) rank(v, ties.method = "first")) else
    rep(1, nrow(part))
  values <- rep(NA, nrow(part))
  last <- rep(NA_real_, max(0, series))
  sums <- counts <- rep(0, max(0, series))
  for (r in sort(unique(rank))) {
    i <- which(rank == r)
    values[i] <- draw_records(ds, variable, fit, part[i, , drop = FALSE], if (isTRUE(node$prev)) last[series[i]] else rep(NA, length(i)),
      ifelse(counts[series[i]] > 0, sums[series[i]] / counts[series[i]], NA))
    if (isTRUE(node$prev)) {
      last[series[i][!is.na(values[i])]] <- values[i][!is.na(values[i])]
      x <- parse_number(values[i])
      sums[series[i]] <- sums[series[i]] + ifelse(is.na(x), 0, x)
      counts[series[i]] <- counts[series[i]] + !is.na(x)
    }
  }
  values <- apply_implications(fit$implications, values, part,
    function(i) draw_records(ds, variable, fit, part[i, , drop = FALSE], rep(NA, length(i))))
  if (length(fit$constant)) {
    key <- paste(part$USUBJID, part[[s$test]], sep = "\r")
    once <- format_value(part[[s$test]]) %in% fit$constant
    values[once] <- values[match(key, key)][once]
  }
  if (level == "record") return(values)
  key <- level_key(level, s)
  values[match(key_strings(frame[key]), key_strings(part[key]))]
}

# ---- Study fits -----------------------------------------------------------------------

# A node is refitted only when its specification changed since the last fit. The arm is
# fitted like any categorical node.
fit_dataset <- function(study, name, previous = NULL) {
  ds <- study$datasets[[name]]
  arm <- arm_variable(study$datasets$DM)
  fits <- list(nodes = list(), specs = list())
  frame <- dataset_frame(study, name)
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
