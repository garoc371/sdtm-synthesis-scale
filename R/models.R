# Models for sampled nodes. Records and dates are never modelled: they are the spine (see
# spine.R).
#
# A fitted node is a set of parts, one per test (findings) and stage (the first record of a
# series, or a later one given the previous value). Each part models one value:
#   categories          multinomial over the observed values, missing being a value
#   normal / lognormal  whether a value is recorded, then a linear model of the value
#   tree                a classification or regression tree (CART, as in synthpop): a value
#                       is drawn from the counts of its leaf, or a number from its quantiles
# Fits keep coefficients and summaries only, never source rows; implications keep the
# values they tie together.

MIN_LEVEL_COUNT <- 5
MAX_MODEL_LEVELS <- 30
MAX_DISCRETE_VALUES <- 10
NODE_MODELS <- c(auto = "Linear (automatic)", normal = "Normal", lognormal = "Log-normal", categories = "Categories", tree = "Tree (CART)")
# The model of a new node: "auto" for the linear models (the default), or "tree" (option
# synth.model, or SYNTH_MODEL in the environment, which the workers share).
default_node_model <- function() getOption("synth.model", Sys.getenv("SYNTH_MODEL", "auto"))

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
  z <- if (type == "lognormal") log(y[observed]) else y[observed]
  fit <- stats::lm.fit(X[observed, , drop = FALSE], z)
  length(z) * log(max(sum(fit$residuals^2), 1e-12) / length(z)) + 2 * fit$rank
}

# One model part. Predictors are used only with at least 10 values per coefficient; for
# categories, 10 values outside the most common one (a rare value is not learnt per level).
# When not all of them fit, whole predictors are added one at a time, the one that lowers
# the AIC most first, while they fit and lower it (36 retrieval terms keep the visit).
fit_part <- function(y, X, type, groups = colnames(X)) {
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
    return(c(fit_multinomial(X, match(values, levels), length(levels)),
      list(type = type, columns = columns, levels = levels, numeric = is.numeric(y))))
  }
  z <- if (type == "lognormal") log(y[observed]) else y[observed]
  X_obs <- X[observed, , drop = FALSE]
  fit <- stats::lm.fit(X_obs, z)
  sigma <- sqrt(sum(fit$residuals^2) / max(1, length(z) - fit$rank))
  variance <- tryCatch(diag(solve(crossprod(X_obs))), error = function(e) rep(0, ncol(X)))
  list(type = type, columns = columns, sigma = sigma,
    coef = matrix(ifelse(is.na(fit$coefficients), 0, fit$coefficients), 1, dimnames = list(NULL, colnames(X))),
    se = matrix(sigma * sqrt(pmax(variance, 0)), 1, dimnames = list(NULL, colnames(X))),
    recorded = if (!all(observed)) fit_multinomial(X, 2L - observed, 2L),
    digits = decimals(y[observed]), range = range(y[observed]), grid = value_grid(y[observed]))
}

# Values on a grid coarser than their decimals (whole mg/dL converted to mmol/L: glucose
# 0.05551 apart) as origin and step, so drawn values stay conversions of whole numbers.
value_grid <- function(v) {
  u <- sort(unique(v))
  tolerance <- 10^-decimals(v)
  if (length(u) < 10 || min(diff(u)) <= 1.5 * tolerance) return(NULL)
  k <- round((u - u[1]) / min(diff(u)))
  fit <- stats::lm.fit(cbind(1, k), u)
  if (max(abs(fit$residuals)) > tolerance) return(NULL)
  unname(fit$coefficients)
}

model_scale <- function(type, x) if (identical(type, "lognormal")) log(pmax(x, 1e-8)) else x

# The model part of each record: its test, and whether it follows an earlier value of its
# series.
node_parts <- function(ds, variable, frame, previous) {
  s <- ds$structure
  test <- if (!is.null(s$test) && ds$levels[[variable]] == "record") format_value(frame[[s$test]]) else rep("", nrow(frame))
  list(key = paste(test, ifelse(is.na(previous), "first", "follow"), sep = "|"), scale = test)
}

# A node is fitted on one row per subject, per subject and visit, or per record. The model
# type is chosen once per test, so first and later records share a scale: categories for
# text and numbers with few values, otherwise normal, or log-normal when positive and
# skewed. A trajectory's first record is fitted without the arm (it is measured before
# treatment); later records add the previous recorded value. A category also keeps its
# implications (see spine.R).
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
  parts <- node_parts(ds, variable, frame, previous)
  arm <- c(arm, paste0("DM.", arm))
  predictors <- intersect(node$predictors, names(frame))
  tree <- node$model == "tree"
  model <- if (tree) "auto" else node$model
  types <- vapply(split(y, parts$scale), function(v) {
    v <- v[!is.na(v)]
    if (!is.numeric(v) || model == "categories" || (model == "auto" && length(unique(v)) <= MAX_DISCRETE_VALUES))
      return("categories")
    skew <- if (length(v) > 2 && stats::sd(v) > 0) mean((v - mean(v))^3) / stats::sd(v)^3 else 0
    if (all(v > 0) && (model == "lognormal" || (model == "auto" && skew > 1))) "lognormal" else "normal"
  }, "")
  fits <- lapply(split(seq_along(y), parts$key), function(i) {
    follow <- grepl("|follow", parts$key[i[1]], fixed = TRUE)
    type <- unname(types[match(parts$scale[i[1]], names(types))])
    records <- frame[i, , drop = FALSE]
    records$.previous <- model_scale(if (tree) "tree" else type, previous[i])
    used <- if (follow) c(predictors, ".previous") else if (isTRUE(node$prev)) setdiff(predictors, arm) else predictors
    if (tree) return(fit_tree(y[i], records, used, type == "categories"))
    info <- design_info(records, used, c("VISITNUM", s$test))
    c(fit_part(y[i], design_matrix(info, records), type, design_groups(info)), list(info = info))
  })
  implications <- if (all(types == "categories")) {
    keys <- implication_keys(ds, frame)
    if (ds$name != "DM" && level == "subject") keys <- keys[!record_feature(keys)]
    rbind(fit_implications(y, frame, keys), derived_implications(ds, variable, y, frame, keys))
  }
  reduced <- sum(vapply(fits, function(p) p$type != "tree" && length(p$columns) < length(design_groups(p$info)), TRUE))
  description <- sprintf("%s%s%s, %d %s%s%s", if (tree) sprintf("trees (%s leaves)", paste(range(vapply(fits, `[[`, 0L, "leaves")), collapse = "-"))
    else paste(sort(unique(types)), collapse = "/"),
    if (length(unique(parts$scale)) > 1) paste(" per", s$test) else "",
    if (isTRUE(node$prev)) " with previous value" else "", nrow(frame), if (level == "record") "records" else paste(level, "rows"),
    if (reduced) sprintf(", fewer predictors in %d of %d parts (few values)", reduced, length(fits)) else "",
    if (NROW(implications)) sprintf(", %d implication%s", nrow(implications), if (nrow(implications) > 1) "s" else "") else "")
  list(parts = fits, arm = arm, description = description, implications = implications)
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
# counts or 21 quantiles of the value; leaves are found again by numbering the tree's nodes.
fit_tree <- function(y, records, predictors, categorical) {
  x <- tree_data(records, predictors)
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
  out$quantiles <- do.call(rbind, tapply(y[observed], factor(g$leaf, unique(g$leaf)), stats::quantile, TREE_QUANTILES, names = FALSE))
  out$leaves <- nrow(out$quantiles)
  out$digits <- decimals(y[observed])
  out$grid <- value_grid(y[observed])
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
  q <- p$quantiles[tree_leaf(p$tree, x), , drop = FALSE]
  # Inverse of the leaf's distribution function, linear between its quantiles.
  u <- stats::runif(nrow(q)) * (length(TREE_QUANTILES) - 1)
  k <- pmin(floor(u), length(TREE_QUANTILES) - 2)
  drawn <- q[cbind(seq_len(nrow(q)), k + 1)] + (u - k) * (q[cbind(seq_len(nrow(q)), k + 2)] - q[cbind(seq_len(nrow(q)), k + 1)])
  if (!is.null(p$grid)) drawn <- p$grid[1] + round((drawn - p$grid[1]) / p$grid[2]) * p$grid[2]
  drawn <- round(drawn, p$digits)
  if (!is.null(p$recorded)) drawn[draw_from_counts(p$recorded$counts, tree_leaf(p$recorded$tree, x)) == "FALSE"] <- NA
  drawn
}

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
    if (p$type == "tree") {
      values[i] <- draw_tree(p, records)
      next
    }
    X <- design_matrix(p$info, records)[, p$columns, drop = FALSE]
    eta <- X %*% t(p$coef)
    if (p$type == "categories") {
      drawn <- p$levels[softmax_draw(eta)]
      drawn[drawn == MISSING] <- NA
      if (p$numeric) drawn <- as.numeric(drawn)
    } else {
      drawn <- stats::rnorm(length(i), drop(eta), p$sigma)
      if (p$type == "lognormal") drawn <- exp(drawn)
      drawn <- pmin(pmax(drawn, p$range[1]), p$range[2])
      if (!is.null(p$grid)) drawn <- p$grid[1] + round((drawn - p$grid[1]) / p$grid[2]) * p$grid[2]
      drawn <- round(drawn, p$digits)
      if (!is.null(p$recorded)) drawn[softmax_draw(X %*% t(p$recorded$coef)) == 2] <- NA
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
  for (r in sort(unique(rank))) {
    i <- which(rank == r)
    values[i] <- draw_records(ds, variable, fit, part[i, , drop = FALSE], if (isTRUE(node$prev)) last[series[i]] else rep(NA, length(i)))
    if (isTRUE(node$prev)) last[series[i][!is.na(values[i])]] <- values[i][!is.na(values[i])]
  }
  values <- apply_implications(fit$implications, values, part,
    function(i) draw_records(ds, variable, fit, part[i, , drop = FALSE], rep(NA, length(i))))
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
