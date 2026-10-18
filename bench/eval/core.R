# An independent check with the CDISC Open Rules Engine (CORE, github.com/cdisc-org/cdisc-rules-engine),
# whose rules we did not write: CORE is run on the members' real files (gen.R's split, written
# as gen.R gives them to the app, Latin-1 text recoded to UTF-8) and on each seed's files as the
# app writes them. Lists the CORE rules the members' files pass and a seed fails, with the first
# failing records. Not part of run.sh; run it after run.sh on the same label and folder.
#   Rscript core.R <label> [data folder]     (seeds: OUT/<label>_seed*.rds; SHOW=1 also lists the rules the real files fail)
#   CORE    the executable of CORE release v0.17.1 (core-mac-apple-silicon.zip or the Linux or
#           Windows asset from github.com/cdisc-org/cdisc-rules-engine/releases, unzipped outside
#           the repo), else `core` on PATH; without it the check is skipped
#   SDTMIG  rule set (default 3-2, the oldest CORE has), CT  terminology package (default sdtmct-2026-03-27)
core <- Sys.getenv("CORE", Sys.which("core"))
if (!nzchar(core) || !file.exists(core)) {
  cat("CORE check skipped: set CORE to, or put on PATH, the core executable of CDISC CORE v0.17.1 (github.com/cdisc-org/cdisc-rules-engine/releases)\n")
  quit(status = 0)
}
args <- commandArgs(TRUE)
if (!is.na(args[2])) Sys.setenv(DATA = args[2])
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
`%||%` <- function(a, b) if (is.null(a)) b else a
for (f in c("source.R", "generate.R")) sys.source(file.path(REPO, "R", f), environment())
core <- normalizePath(core)
version <- system2(core, "version", stdout = TRUE)[1]
seed_files <- list.files(OUT, paste0("^", args[1], "_seed\\d+\\.rds$"), full.names = TRUE)
members <- readRDS(file.path(OUT, paste0(args[1], "_split.rds")))$members
work <- file.path(tempdir(), "core")
real <- file.path(work, "real")
dir.create(real, recursive = TRUE)
for (f in list.files(DATA, "\\.xpt$", full.names = TRUE, ignore.case = TRUE)) {
  x <- haven::read_xpt(f)
  if ("USUBJID" %in% names(x)) x <- x[x$USUBJID %in% members, ]
  for (v in names(x)) if (is.character(x[[v]])) { bad <- !validUTF8(x[[v]]); x[[v]][bad] <- iconv(x[[v]][bad], "latin1", "UTF-8") }
  haven::write_xpt(x, file.path(real, tolower(basename(f))), version = 5, name = toupper(tools::file_path_sans_ext(basename(f))))
}
run <- function(dir) {
  out <- file.path(work, basename(dir))
  owd <- setwd(dirname(core)); on.exit(setwd(owd))
  log <- system2(core, c("validate", "-s", "sdtmig", "-v", Sys.getenv("SDTMIG", "3-2"), "-ct", Sys.getenv("CT", "sdtmct-2026-03-27"),
    "-d", dir, "-of", "JSON", "-o", out, "-p", "disabled", "-l", "error"), stdout = TRUE, stderr = TRUE)
  if (!file.exists(paste0(out, ".json"))) stop("CORE wrote no report for ", dir, ":\n", paste(log, collapse = "\n"))
  r <- jsonlite::fromJSON(paste0(out, ".json"))
  status <- setNames(r$Rules_Report$status, r$Rules_Report$core_id)
  issues <- tapply(r$Issue_Summary$issues, r$Issue_Summary$core_id, sum)
  list(status = status, message = setNames(r$Rules_Report$message, r$Rules_Report$core_id), issues = issues, details = r$Issue_Details)
}
ref <- run(real)
seeds <- lapply(seq_along(seed_files), function(i) {
  dir <- file.path(work, paste0("seed", i))
  write_datasets(readRDS(seed_files[i])$data, dir, "xpt")
  run(dir)
})
failing <- c("ISSUE REPORTED", "EXECUTION ERROR")
ids <- names(ref$status)
out <- data.frame(core_id = ids, message = ref$message[ids], real = ref$status[ids], real_issues = as.vector(ref$issues[ids] %||% NA))
for (i in seq_along(seeds)) {
  out[[paste0("seed", i)]] <- unname(seeds[[i]]$status[ids])
  out[[paste0("issues", i)]] <- as.vector(seeds[[i]]$issues[ids])
}
status <- as.matrix(out[grep("^seed", names(out))])
out$broken <- out$real == "SUCCESS" & rowSums(matrix(status %in% failing, nrow(status))) > 0
saveRDS(list(rules = out, real = ref$details, seeds = lapply(seeds, `[[`, "details")), file.path(OUT, paste0("core_", args[1], ".rds")))
cat(sprintf("%s: CORE %s on SDTMIG %s; %d rules the members' files pass, of which the synthetic files fail %d\n",
  args[1], version, Sys.getenv("SDTMIG", "3-2"), sum(out$real == "SUCCESS"), sum(out$broken)))
for (k in which(out$broken)) {
  id <- out$core_id[k]
  cat(sprintf("  %s  %s\n    seeds %s\n", id, out$message[k],
    paste(sprintf("%s %s", status[k, ], ifelse(is.na(out[k, grep("^issues", names(out))]), "", out[k, grep("^issues", names(out))])), collapse = " | ")))
  i <- which(status[k, ] %in% failing)[1]
  d <- seeds[[i]]$details
  d <- head(d[d$core_id == id, , drop = FALSE], 3)
  if (nrow(d)) writeLines(sprintf("    seed%d %s %s row %s: %s = %s", i, d$dataset, d$USUBJID, d$row,
    vapply(d$variables, paste, "", collapse = ","), substr(vapply(d$values, paste, "", collapse = ","), 1, 160)))
}
if (identical(Sys.getenv("SHOW"), "1")) {
  cat("\nNot gated in (the members' files fail them):\n")
  f <- out[out$real %in% failing, ]
  writeLines(sprintf("  %s  %-15s %5s  %s", f$core_id, f$real, ifelse(is.na(f$real_issues), "", f$real_issues), substr(f$message, 1, 100)))
}
