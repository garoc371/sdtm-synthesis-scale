# Privacy on gen.R's split: what the synthetic data give away about the members (the half the
# generator was built on), measured against the held-out half. Real data as the app reads it.
#   Rscript privacy.R <label> <seed files...>
# Timeline: a subject's dates as (dataset, variable, days from their first SV date); the shift
# to 2020-01-01 keeps these. Correct link: synthetic subjects whose timeline matches exactly
# one member, and that member is their donor. Membership: a real subject is called a member
# when some synthetic subject has their timeline; balanced accuracy over members and held out.
# Singleton values: text values held by one member and no held-out subject, and the share of
# them the synthetic data reproduce in the same variable. Exact records: synthetic records
# equal to a member's record, and to a held-out subject's, without identifiers and --SEQ.
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
args <- commandArgs(TRUE)
label <- args[1]
split <- readRDS(file.path(OUT, paste0(label, "_split.rds")))
real <- readRDS(file.path(OUT, paste0("real_", label, "_seed1.rds")))$data
IDS <- c("STUDYID", "DOMAIN", "USUBJID", "SUBJID", "IDVARVAL")
subject_data <- function(data) Filter(function(x) "USUBJID" %in% names(x) && nrow(x) > 0, lapply(data, as.data.frame))

# Days from each subject's first SV date (their earliest date when they have no visits).
anchors <- function(data) {
  sv <- data$SV
  first <- tapply(as.numeric(as_date(sv$SVSTDTC)), sv$USUBJID, min, na.rm = TRUE)
  all <- do.call(rbind, lapply(data, function(x) {
    v <- grep("DTC$", names(x), value = TRUE)
    if (length(v)) data.frame(id = rep(x$USUBJID, length(v)), d = unlist(lapply(x[v], function(z) as.numeric(as_date(z)))))
  }))
  early <- tapply(all$d, all$id, min, na.rm = TRUE)
  out <- early
  out[names(first)] <- ifelse(is.finite(first), first, early[names(first)])
  out
}
timelines <- function(data) {
  a <- anchors(data)
  t <- do.call(rbind, lapply(names(data), function(d) {
    x <- data[[d]]
    do.call(rbind, lapply(grep("DTC$", names(x), value = TRUE), function(v) {
      day <- as.numeric(as_date(x[[v]])) - a[x$USUBJID]
      keep <- !is.na(day)
      if (any(keep)) data.frame(id = x$USUBJID[keep], e = paste0(d, ".", v, ":", day[keep]))
    }))
  }))
  vapply(split(t$e, t$id), function(e) paste(sort(e), collapse = " "), "")
}
# Each record without identifiers and --SEQ, dates as days from the subject's anchor.
record_keys <- function(x, cols, a) {
  z <- lapply(cols, function(v) if (grepl("DTC$", v)) as.character(as.numeric(as_date(x[[v]])) - a[x$USUBJID]) else as.character(x[[v]]))
  do.call(paste, c(z, sep = "\r"))
}

real <- subject_data(real)
members <- lapply(real, function(x) x[x$USUBJID %in% split$members, , drop = FALSE])
held <- lapply(real, function(x) x[x$USUBJID %in% split$held_out, , drop = FALSE])
tm <- timelines(members)
th <- timelines(held)
a_m <- anchors(members); a_h <- anchors(held)

# Text values one member holds and no held-out subject does, per dataset and variable.
singletons <- do.call(rbind, lapply(names(members), function(d) {
  x <- members[[d]]
  cols <- names(x)[vapply(names(x), function(v) is.character(x[[v]]) && !v %in% IDS && !grepl("DTC$|SEQ$", v), TRUE)]
  do.call(rbind, lapply(cols, function(v) {
    holders <- tapply(x$USUBJID, x[[v]], function(u) length(unique(u)))
    one <- setdiff(names(holders)[holders == 1], held[[d]][[v]])
    if (length(one)) data.frame(d = d, v = v, value = one)
  }))
}))

rows <- list()
for (f in args[-1]) {
  g <- readRDS(f)
  syn <- subject_data(g$data)
  ts <- timelines(syn)
  donors <- g$donors
  # Correct link and membership.
  matches <- lapply(ts, function(k) names(tm)[tm == k])
  linked <- mean(vapply(names(ts), function(s) length(matches[[s]]) == 1 && matches[[s]] == donors[[s]], TRUE))
  ids_m <- split$members; ids_h <- split$held_out
  found <- mean(!is.na(tm[ids_m]) & tm[ids_m] %in% ts)
  false <- mean(!is.na(th[ids_h]) & th[ids_h] %in% ts)
  rows[[f]] <- list(seed = basename(f), n = length(ts), unique = mean(!duplicated(tm) & !duplicated(tm, fromLast = TRUE)),
    linked = linked, found = found, false = false, accuracy = (found + 1 - false) / 2,
    singletons = {
      kept <- mapply(function(d, v, value) !is.null(syn[[d]][[v]]) && value %in% syn[[d]][[v]], singletons$d, singletons$v, singletons$value)
      s <- data.frame(dv = paste0(singletons$d, ".", singletons$v), kept)
      list(n = nrow(s), kept = sum(kept), top = utils::head(sort(tapply(s$kept, s$dv, sum), decreasing = TRUE), 8),
        of = tapply(s$kept, s$dv, length))
    },
    records = do.call(rbind, lapply(intersect(names(syn), names(members)), function(d) {
      cols <- setdiff(intersect(names(syn[[d]]), names(members[[d]])), IDS)
      cols <- cols[!grepl("^[A-Z]{2}SEQ$", cols)]
      if (!length(cols)) return(NULL)
      k <- record_keys(syn[[d]], cols, anchors(syn))
      data.frame(dataset = d, records = length(k), member = mean(k %in% record_keys(members[[d]], cols, a_m)),
        held_out = mean(k %in% record_keys(held[[d]], cols, a_h)))
    })))
}
saveRDS(list(rows = rows, singletons = singletons), file.path(OUT, paste0("priv_", label, ".rds")))

num <- function(name) vapply(rows, function(r) r[[name]], 0)
show <- function(name) sprintf("%.3f (%s)", mean(num(name)), paste(sprintf("%.3f", num(name)), collapse = " "))
cat(sprintf("%s: %d members, %d held out, %d synthetic per seed\n", label, length(split$members), length(split$held_out), rows[[1]]$n))
cat("  members unique on timeline ", show("unique"), "\n")
cat("  correct link                ", show("linked"), "\n")
cat("  members found               ", show("found"), "\n")
cat("  held out matched            ", show("false"), "\n")
cat("  membership accuracy         ", show("accuracy"), "\n")
s <- vapply(rows, function(r) r$singletons$kept / r$singletons$n, 0)
cat(sprintf("  singleton text values reproduced %.3f (%s) of %d\n", mean(s), paste(sprintf("%.3f", s), collapse = " "), rows[[1]]$singletons$n))
top <- rows[[1]]$singletons
cat("   seed 1 top:", paste(sprintf("%s %d/%d", names(top$top), top$top, top$of[names(top$top)]), collapse = ", "), "\n")
rec <- do.call(rbind, lapply(rows, `[[`, "records"))
rec <- aggregate(cbind(member, held_out) ~ dataset, rec, mean)
rec <- rec[order(-rec$member), ]
cat("  exact records, share equal to a member / held-out record (mean of seeds):\n")
writeLines(sprintf("    %-8s %.3f  %.3f", rec$dataset, rec$member, rec$held_out))
