# SDTMIG conformance, read from the datasets alone (and the trial design among them):
# nothing here reads the fitted spec, accepted rules or implications. Each check counts the
# records it tests and those that violate it. The real data (DATA, read as filed) is the
# reference: a synthetic violation rate up to twice the real one is inherited, a higher one
# (any, where the real data has none) fails.
#   Rscript coherence.R <label> <seed files...>   (rds files from gen.R)
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))

tidy <- function(data) lapply(data, function(x) {
  x <- as.data.frame(x)
  for (v in names(x)) {
    if (grepl("DTC$", v)) x[[v]] <- as_date(x[[v]])
    else if (is.character(x[[v]])) x[[v]][!nzchar(trimws(x[[v]]))] <- NA
  }
  x
})

# SUPP and RELREC rows name their parent record by USUBJID and IDVAR = IDVARVAL; with no
# IDVAR, the subject (SUPP) or, with no USUBJID either, the variable (RELREC) must exist.
resolves <- function(s, p) {
  if (is.null(p)) return(rep(FALSE, nrow(s)))
  ok <- ifelse(is.na(s$USUBJID), s$IDVAR %in% names(p), s$USUBJID %in% p$USUBJID)
  for (v in unique(stats::na.omit(s$IDVAR[!is.na(s$USUBJID)]))) {
    i <- which(s$IDVAR %in% v & !is.na(s$USUBJID))
    val <- trimws(s$IDVARVAL[i])
    ok[i] <- if (v %in% names(p))
      paste(s$USUBJID[i], if (is.numeric(p[[v]])) as.numeric(val) else val) %in% paste(p$USUBJID, p[[v]]) else FALSE
  }
  ok
}

# A record's EPOCH is the epoch of an SE element covering its date (SE's own EPOCH, or TA's
# for the subject's actual arm and the element), ignoring case; NA where no element with an
# epoch covers it.
epoch_ok <- function(x, date, data) {
  se <- data$SE; dm <- data$DM
  arm <- dm[[intersect(c("ACTARMCD", "ARMCD"), names(dm))[1]]][match(se$USUBJID, dm$USUBJID)]
  se$ELEPOCH <- if ("EPOCH" %in% names(se)) se$EPOCH else
    data$TA$EPOCH[match(paste(arm, se$ETCD), paste(data$TA$ARMCD, data$TA$ETCD))]
  r <- data.frame(i = seq_len(nrow(x)), USUBJID = x$USUBJID, date = date, EPOCH = x$EPOCH)
  m <- merge(r[!is.na(r$EPOCH) & !is.na(r$date), ], se[c("USUBJID", "SESTDTC", "SEENDTC", "ELEPOCH")], by = "USUBJID")
  m <- m[which(m$SESTDTC <= m$date & (is.na(m$SEENDTC) | m$date <= m$SEENDTC) & !is.na(m$ELEPOCH)), ]
  ok <- rep(NA, nrow(x))
  hit <- tapply(toupper(m$EPOCH) == toupper(m$ELEPOCH), m$i, any)
  ok[as.integer(names(hit))] <- hit
  ok
}

checks <- function(data) {
  out <- list()
  add <- function(check, dataset, ok) out[[length(out) + 1]] <<- data.frame(check, dataset, n = sum(!is.na(ok)), bad = sum(!ok, na.rm = TRUE))
  dm <- data$DM
  at <- function(x, v) dm[[v]][match(x$USUBJID, dm$USUBJID)]
  subject <- names(data)[vapply(data, function(x) "USUBJID" %in% names(x), TRUE) & !grepl("^SUPP|^RELREC$", names(data))]
  for (d in subject) {
    x <- data[[d]]
    for (v in grep("^..SEQ$", names(x), value = TRUE))
      add("--SEQ unique in subject", paste(d, v), !is.na(x[[v]]) & !duplicated(x[c("USUBJID", v)]))
    for (v in grep("^..(ST|EN)?DY$", names(x), value = TRUE)) {
      dtc <- sub("DY$", "DTC", v)
      if (!dtc %in% names(x)) next
      days <- as.numeric(x[[dtc]] - at(x, "RFSTDTC"))
      add("--DY from RFSTDTC, no day 0", paste(d, v), x[[v]] == days + (days >= 0))
    }
    for (v in grep("STDTC$", names(x), value = TRUE)) {
      en <- sub("STDTC$", "ENDTC", v)
      if (en %in% names(x)) add("--STDTC <= --ENDTC", paste(d, v), x[[v]] <= x[[en]])
    }
    dates <- grep("DTC$", names(x), value = TRUE)
    if (d != "DS" && "DTHDTC" %in% names(dm) && length(dates))
      add("not dated after DTHDTC", d, do.call(pmax, c(x[dates], na.rm = TRUE)) <= at(x, "DTHDTC"))
    if ("VISITNUM" %in% names(x) && d != "SV" && !is.null(data$SV))
      add("VISITNUM in subject's SV", d, ifelse(is.na(x$VISITNUM), NA, paste(x$USUBJID, x$VISITNUM) %in% paste(data$SV$USUBJID, data$SV$VISITNUM)))
    date <- c(grep("^..DTC$", names(x), value = TRUE), grep("^..STDTC$", names(x), value = TRUE))[1]
    if ("EPOCH" %in% names(x) && d != "SE" && !is.na(date) && !is.null(data$SE) && !is.null(data$TA))
      add("EPOCH of covering SE element", d, epoch_ok(x, x[[date]], data))
  }
  for (d in grep("^SUPP", names(data), value = TRUE)) {
    s <- data[[d]]
    add("SUPP resolves to parent", d, resolves(s, data[[if (sub("^SUPP", "", d) %in% names(data)) sub("^SUPP", "", d) else s$RDOMAIN[1]]]))
  }
  if (!is.null(data$RELREC)) for (p in unique(data$RELREC$RDOMAIN))
    add("RELREC resolves to parent", p, resolves(data$RELREC[data$RELREC$RDOMAIN %in% p, ], data[[p]]))
  if (!is.null(data$TR) && !is.null(data$TU))
    add("TRLNKID in subject's TU", "TR", ifelse(is.na(data$TR$TRLNKID), NA, paste(data$TR$USUBJID, data$TR$TRLNKID) %in% paste(data$TU$USUBJID, data$TU$TULNKID)))
  if (!is.null(data$SV) && !is.null(data$TV)) {
    sv <- data$SV
    planned <- if ("SVUPDES" %in% names(sv)) is.na(sv$SVUPDES) else rep(TRUE, nrow(sv))
    add("planned VISITNUM in TV", "SV", ifelse(planned, sv$VISITNUM %in% data$TV$VISITNUM, NA))
  }
  if (!is.null(data$TA)) for (v in intersect(c("ARMCD", "ACTARMCD"), names(dm)))
    add("ARMCD/ACTARMCD in TA", paste("DM", v), ifelse(is.na(dm[[v]]), NA, dm[[v]] %in% data$TA$ARMCD))
  do.call(rbind, out)
}

args <- commandArgs(TRUE)
label <- args[1]
real <- checks(tidy(read_real(DATA)))
synth <- do.call(rbind, lapply(args[-1], function(f) checks(tidy(readRDS(f)$data))))
synth <- aggregate(cbind(n, bad) ~ check + dataset, synth, sum)
res <- merge(real, synth, by = c("check", "dataset"), all = TRUE, suffixes = c(".real", ".synth"))
res[is.na(res)] <- 0
res$real <- ifelse(res$n.real > 0, res$bad.real / res$n.real, 0)
res$synth <- ifelse(res$n.synth > 0, res$bad.synth / res$n.synth, 0)
res$status <- ifelse(res$bad.synth == 0, "pass", ifelse(res$synth > 2 * res$real, "fails", "inherited"))
res <- res[order(match(res$status, c("fails", "inherited", "pass")), res$check, res$dataset), ]
saveRDS(res, file.path(OUT, paste0("coherence_", label, ".rds")))

pct <- function(b, n) sprintf("%d/%d (%.2f%%)", b, n, 100 * b / pmax(n, 1))
show <- res[res$status != "pass", ]
cat(label, "coherence: failing", sum(res$status == "fails"), "inherited", sum(res$status == "inherited"), "pass", sum(res$status == "pass"), "of", nrow(res), "\n")
if (nrow(show)) print(data.frame(status = show$status, check = show$check, dataset = show$dataset,
  real = pct(show$bad.real, show$n.real), synthetic = pct(show$bad.synth, show$n.synth)), row.names = FALSE, right = FALSE)
by <- aggregate(cbind(n.real, bad.real, n.synth, bad.synth) ~ check, res, sum)
cat("\nper check, all datasets:\n")
print(data.frame(check = by$check, real = pct(by$bad.real, by$n.real), synthetic = pct(by$bad.synth, by$n.synth),
  failing = as.vector(table(factor(res$check[res$status == "fails"], by$check)))), row.names = FALSE, right = FALSE)
