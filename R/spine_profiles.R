# Each source subject's profile: arm (DM's arm columns), last visit that marks progress, and
# death and treatment status; and the study's last visit that marks progress.
spine_profiles <- function(spine, visits) {
  dm <- spine$DM
  N <- nrow(dm)
  date <- function(columns) {
    v <- intersect(columns, date_columns(dm))[1]
    if (is.na(v)) rep(NA_real_, N) else as.numeric(dm[[v]])
  }
  start <- date(c("RFXSTDTC", "RFSTDTC"))
  end <- date(c("RFXENDTC", "RFENDTC"))
  last <- rep(NA_real_, N)
  for (k in which(visits$number %in% visits$progress)) last[visits$attended[, k]] <- visits$number[k]
  arm <- intersect(SPINE_ARM, names(dm))
  has <- vapply(setdiff(names(spine), "DM"), function(d) ifelse(tabulate(spine[[d]]$.subject, N) > 0, "Y", "N"), character(N))
  list(arm = if (length(arm)) key_strings(dm[arm]) else rep("", N), last = last,
    status = paste(ifelse(is.na(date("DTHDTC")), "N", "Y"), ifelse(is.na(start), "N", ifelse(is.na(end), "STARTED", "Y"))),
    end = max(c(visits$progress, NA), na.rm = TRUE), flags = cbind(matrix(has, N, dimnames = list(NULL, colnames(has))),
      as.matrix(dm[grep("^T\\.", names(dm))])))
}

# Keys that pool source subjects, finest first: arm, last visit and status; arm, whether the
# subject reached the study's last visit that marks progress, and status; that and status;
# status; none.
profile_levels <- function(p) {
  last <- level_string(p$last)
  done <- as.character(!is.na(p$last) & p$last %in% p$end)
  list(paste(p$arm, last, p$status), paste(p$arm, done, p$status), paste(done, p$status), p$status, rep("", length(p$status)))
}

# Synthetic profiles: the arm, then the last visit given the arm, then the status given both;
# then, dataset by dataset (and category by category, T.<DATASET>.<CATEGORY>), whether the
# subject has records, given the profile and the one or two flags drawn before that tell most
# about it in the source (`flag_parents()`: RS's subjects all have the CIBIC+ questionnaire),
# else those flags alone, else the profile alone; as often as among those source subjects
# (`pooled_share()`), or fixed where the profile or an earlier flag fixes it (`flag_forced()`).
draw_profiles <- function(src, n) {
  N <- length(src$arm)
  none <- rep("", n)
  all <- rep("", N)
  arm <- pooled_draw(src$arm, seq_len(N), list(none), list(all))
  last_src <- level_string(src$last)
  last <- pooled_draw(last_src, seq_len(N), list(arm, none), list(src$arm, all))
  status <- pooled_draw(src$status, seq_len(N), list(paste(arm, last), arm, none), list(paste(src$arm, last_src), src$arm, all))
  syn <- list(arm = arm, last = src$last[match(last, last_src)], status = status, end = src$end)
  want <- profile_levels(syn)
  have <- profile_levels(src)
  F <- src$flags
  profile_src <- cbind(src$arm, last_src, src$status)
  profile_syn <- cbind(arm, last, status)
  syn$flags <- matrix("N", n, ncol(F), dimnames = list(NULL, colnames(F)))
  for (i in seq_len(ncol(F))) {
    before <- seq_len(i - 1)
    parents <- flag_parents(F[, i], F[, before, drop = FALSE])
    forced <- flag_forced(F[, i], cbind(profile_src, F[, before, drop = FALSE]))
    keys <- function(flags, levels, profile) flag_keys(flags, levels, parents, forced(cbind(profile, flags[, before, drop = FALSE])))
    yes <- pooled_share(F[, i] == "Y", seq_len(N), keys(syn$flags, want, profile_syn), keys(F, have, profile_src))
    syn$flags[, i] <- ifelse(yes, "Y", "N")
  }
  syn
}

# Where the profile or an earlier flag (`before`, columns) fixes `flag` in the source (every
# subject with RS has the CIBIC+ questionnaire, only placebo subjects the placebo epoch; at
# least min_subjects() of them), a function giving each subject the value their columns fix,
# or "" where none does.
flag_forced <- function(flag, before) {
  # For each column, the value of `flag` each of its values fixes.
  fixes <- lapply(seq_len(ncol(before)), function(h) {
    groups <- split(flag, before[, h])
    fixed <- vapply(groups, function(f) length(f) >= min_subjects() && length(unique(f)) == 1, TRUE)
    vapply(groups[fixed], `[`, "", 1)
  })
  function(flags) {
    out <- rep("", nrow(flags))
    # The first column that fixes it wins.
    for (h in rev(seq_along(fixes))) {
      v <- fixes[[h]][match(flags[, h], names(fixes[[h]]))]
      out[!is.na(v)] <- v[!is.na(v)]
    }
    out
  }
}

# Keys that pool subjects for a flag, finest first: each of the profile keys `levels` with the
# subject's `parents` flags (all of them, then fewer), those flags alone, then the profile keys
# alone; each with the value the profile or an earlier flag fixes (`fixed`).
flag_keys <- function(flags, levels, parents, fixed) {
  held <- lapply(rev(seq_along(parents)), function(p) do.call(paste0, c(list(""), unname(as.data.frame(flags[, parents[seq_len(p)], drop = FALSE])))))
  keys <- unlist(lapply(held, function(k) c(lapply(levels, paste, k), list(k))), recursive = FALSE)
  lapply(c(keys, levels), paste, fixed)
}

# The flags `before` (columns) that tell most about `flag` in the source, at most two, each
# chosen to add the most mutual information given those chosen.
flag_parents <- function(flag, before) {
  info <- function(key) {
    p <- table(flag, key) / length(flag)
    e <- outer(rowSums(p), colSums(p))
    sum(ifelse(p > 0, p * log(p / e), 0))
  }
  chosen <- integer()
  key <- rep("", length(flag))
  for (r in 1:2) {
    rest <- setdiff(seq_len(ncol(before)), chosen)
    if (!length(rest)) break
    gain <- vapply(rest, function(h) info(paste0(key, before[, h])), 0)
    if (max(gain) <= info(key) + 1e-9) break
    chosen <- c(chosen, rest[which.max(gain)])
    key <- paste0(key, before[, rest[which.max(gain)]])
  }
  chosen
}

# Whether each source subject has a record with each value of a dataset's content columns of
# at most VALUE_FLAG_LEVELS values (TUORRES NEW, TULOC BONE), for values at least
# min_subjects() subjects have: flags V.<DATASET>.<COLUMN>=<VALUE> (`spec` says which).
# Series are left out.
VALUE_FLAG_LEVELS <- 10

value_flags <- function(study, N) {
  spec <- unlist(lapply(setdiff(names(study$spine), "DM"), function(d) dataset_value_flags(study, d)), recursive = FALSE)
  flags <- vapply(spec, function(f) ifelse(seq_len(N) %in% f$holders, "Y", "N"), character(N))
  list(flags = matrix(flags, N, length(spec), dimnames = list(NULL, names(spec))), spec = lapply(spec, `[`, c("column", "value")))
}

# Dataset `d`'s value flags (see `value_flags()`), each with its column, value and holders.
dataset_value_flags <- function(study, d) {
  x <- study$spine[[d]]
  ds <- study$datasets[[d]]
  if (identical(ds$structure$type, "series")) return(list())
  columns <- setdiff(content_columns(x, ds, date_columns(x)), ".category")
  unlist(lapply(columns, function(v) {
    s <- ifelse(is.na(x[[v]]), NA, level_string(x[[v]]))
    u <- unique(s[!is.na(s)])
    if (length(u) < 2 || length(u) > VALUE_FLAG_LEVELS) return(list())
    holders <- lapply(setNames(nm = u), function(w) unique(x$.subject[s %in% w]))
    holders <- holders[lengths(holders) >= min_subjects()]
    flags <- Map(function(w, h) list(column = v, value = w, holders = h), names(holders), holders)
    setNames(flags, paste0("V.", d, ".", v, "=", names(holders)))
  }), recursive = FALSE)
}

# Each subject's flags for dataset `d`: whether they have records, in which categories and,
# unless `values` is FALSE, with which values.
presence <- function(flags, d, values = TRUE) {
  f <- colnames(flags)
  cols <- f[f == d | startsWith(f, paste0("T.", d, ".")) | values & startsWith(f, paste0("V.", d, "."))]
  if (!length(cols)) return(rep("", nrow(flags)))
  do.call(paste0, unname(as.data.frame(flags[, cols, drop = FALSE])))
}

