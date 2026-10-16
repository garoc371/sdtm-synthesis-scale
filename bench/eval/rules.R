# SDTM rules (R/review.R's sdtm_rules()) on the members' real data (gen.R's split) and each
# seed: records checked and failing per rule. Synthetic data must keep every rule; the
# members' rate shows which failures the source has too. Partial dates stay as given.
#   Rscript rules.R <label> <seed files...>
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
`%||%` <- function(a, b) if (is.null(a)) b else a
sys.source(file.path(REPO, "R", "review.R"), environment())
args <- commandArgs(TRUE)
define <- list.files(DATA, "\\.xml$", full.names = TRUE)[1]
define <- if (!is.na(define)) read_define(define)
key <- function(r) paste(r$Dataset, r$Check, sep = "\r")
members <- readRDS(file.path(OUT, paste0(args[1], "_split.rds")))$members
real <- lapply(read_real(DATA, dates = FALSE), function(x) if ("USUBJID" %in% names(x)) x[x$USUBJID %in% members, , drop = FALSE] else x)
real <- sdtm_rules(real, define)
seeds <- lapply(args[-1], function(f) sdtm_rules(readRDS(f)$data, define))
rules <- unique(c(key(real), unlist(lapply(seeds, key))))
count <- function(r, what) setNames(r[[what]], key(r))[rules]
out <- data.frame(Dataset = sub("\r.*", "", rules), Check = sub(".*\r", "", rules),
  real_failed = count(real, "Failed"), real_checked = count(real, "Checked"))
for (i in seq_along(seeds)) {
  out[[paste0("failed", i)]] <- count(seeds[[i]], "Failed")
  out[[paste0("checked", i)]] <- count(seeds[[i]], "Checked")
}
out$example <- count(seeds[[1]], "Problems")
saveRDS(out, file.path(OUT, paste0("rules_", args[1], ".rds")))
failed <- as.matrix(out[grep("^failed", names(out))])
broken <- rowSums(failed > 0, na.rm = TRUE) > 0
# A failure is the source's own when the members' data break the rule about as often.
rate <- failed / as.matrix(out[grep("^checked", names(out))])
out$above_real <- broken & (is.na(out$real_failed) | rowSums(rate > out$real_failed / out$real_checked + 0.01, na.rm = TRUE) > 0)
cat(sprintf("%s: %d rules; synthetic data break %d, %d of them more often than the members' real data (by over 1 point)\n",
  args[1], length(rules), sum(broken), sum(out$above_real)))
f <- out[broken, ]
f <- f[order(!f$above_real, f$Dataset), ]
seed_text <- apply(f[grep("^(failed|checked)[0-9]", names(f))], 1, function(z) {
  n <- length(z) / 2
  paste(sprintf("%s/%s", z[2 * seq_len(n) - 1], z[2 * seq_len(n)]), collapse = " ")
})
writeLines(sprintf("  %-7s %-42s members %s/%s  seeds %s\n          e.g. %s", f$Dataset, f$Check,
  ifelse(is.na(f$real_failed), "-", f$real_failed), ifelse(is.na(f$real_checked), "-", f$real_checked), seed_text,
  substr(ifelse(is.na(f$example), "", f$example), 1, 200)))
