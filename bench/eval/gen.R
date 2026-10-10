# Build the study from DATA with the app's code and generate n subjects for several seeds;
# with n = 0, write the real data as the app reads it (partial dates imputed) instead, the
# reference for failures the real data already has.
#   Rscript gen.R <label> <n> <seeds...>     (SYNTH_MODEL=tree for decision trees)
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
args <- commandArgs(TRUE)
label <- args[1]; n <- as.integer(args[2]); seeds <- as.integer(args[-(1:2)])
setwd(REPO)
for (f in c("source.R", "domains.R", "supp.R", "rules.R", "spine.R", "study.R", "models.R", "generate.R", "review.R", "pipeline.R"))
  sys.source(file.path("R", f), globalenv())
files <- list.files(DATA, full.names = TRUE)
if (n == 0) {
  d <- lapply(read_source(files)$data, function(x) { for (v in names(x)) if (inherits(x[[v]], "Date")) x[[v]] <- format(x[[v]]); x })
  saveRDS(list(data = d), file.path(OUT, paste0(label, "_seed1.rds")))
  quit(save = "no")
}
start_workers(8)
b <- new_build(8, file.path(tempdir(), paste0("cache-", label)), n = n, seed = seeds[1])
start_load(b, files)
run_build(b)
print(build_status(b)[c("dataset", "stage", "message")])
bundle <- build_bundle(b)
for (s in seeds) {
  g <- generate_study(bundle, n, s)
  checks <- coherence_checks(bundle, g)
  cat(label, "seed", s, "app checks failed:", sum(checks$Status == "failed"), "of", nrow(checks), "\n")
  saveRDS(list(data = output_datasets(bundle, g), checks = checks), file.path(OUT, sprintf("%s_seed%d.rds", label, s)))
}
