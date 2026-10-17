# SDTM rules (R/review.R's sdtm_rules(): SDTMIG's general rules and the trial design), run on
# the members' real data (gen.R's split) and each seed. A rule is run on the synthetic data
# only where the members' data keep it in every record they have for it; the synthetic data
# must then keep it too. Partial dates stay as given.
#   Rscript rules.R <label> <seed files...>   (SHOW=1 also lists the rules the real data break)
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
rules <- key(real)
count <- function(r, what) unname(setNames(r[[what]], key(r))[rules])
out <- data.frame(Dataset = real$Dataset, Category = real$Category, Check = real$Check, real_failed = real$Failed, real_checked = real$Checked)
for (i in seq_along(seeds)) {
  out[[paste0("failed", i)]] <- count(seeds[[i]], "Failed")
  out[[paste0("checked", i)]] <- count(seeds[[i]], "Checked")
}
out$example <- count(seeds[[1]], "Problems")
out$run <- out$real_failed == 0
saveRDS(out, file.path(OUT, paste0("rules_", args[1], ".rds")))
failed <- as.matrix(out[grep("^failed", names(out))])
broken <- out$run & rowSums(failed > 0, na.rm = TRUE) > 0
cat(sprintf("%s: %d rules the members' real data keep (%d not run: the real data break them); synthetic data break %d\n",
  args[1], sum(out$run), sum(!out$run), sum(broken)))
show <- function(f) {
  seed_text <- apply(f[grep("^(failed|checked)[0-9]", names(f))], 1, function(z) {
    n <- length(z) / 2
    paste(sprintf("%s/%s", ifelse(is.na(z[2 * seq_len(n) - 1]), "-", z[2 * seq_len(n) - 1]), ifelse(is.na(z[2 * seq_len(n)]), "-", z[2 * seq_len(n)])), collapse = " ")
  })
  writeLines(sprintf("  %-7s %-50s members %s/%s  seeds %s\n          e.g. %s", f$Dataset, f$Check, f$real_failed, f$real_checked, seed_text,
    substr(ifelse(is.na(f$example), "", f$example), 1, 200)))
}
show(out[broken, ])
if (identical(Sys.getenv("SHOW"), "1")) {
  cat("\nNot run (the members' real data break them):\n")
  f <- out[!out$run, ]
  writeLines(sprintf("  %-7s %-50s %d/%d", f$Dataset, f$Check, f$real_failed, f$real_checked))
}
