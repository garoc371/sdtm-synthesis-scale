# Build the study from a random half of the subjects in DATA (the members) with the app's
# code and generate n subjects (default: as many as there are members) for several seeds;
# the other half (held out) is the reference for relations and privacy. With n = 0, write
# all of the real data as the app reads it (partial dates imputed) instead.
#   Rscript gen.R <label> <n | members> <seeds...>     (SPLIT=1 picks the half; SYNTH_MODEL=tree for trees)
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
args <- commandArgs(TRUE)
label <- args[1]; seeds <- as.integer(args[-(1:2)])
setwd(REPO)
for (f in c("source.R", "domains.R", "supp.R", "rules.R", "spine.R", "study.R", "models.R", "generate.R", "review.R", "pipeline.R"))
  sys.source(file.path("R", f), globalenv())
files <- list.files(DATA, full.names = TRUE)
if (args[2] == "0") {
  d <- lapply(read_source(files)$data, function(x) { for (v in names(x)) if (inherits(x[[v]], "Date")) x[[v]] <- format(x[[v]]); x })
  saveRDS(list(data = d), file.path(OUT, paste0(label, "_seed1.rds")))
  quit(save = "no")
}
# The members' records, written as the source files are, so the app reads them unchanged.
subjects <- haven::read_xpt(grep("/dm\\.xpt$", files, value = TRUE, ignore.case = TRUE))$USUBJID
set.seed(as.integer(Sys.getenv("SPLIT", "1")))
members <- sample(subjects, length(subjects) %/% 2)
saveRDS(list(members = members, held_out = setdiff(subjects, members)), file.path(OUT, paste0(label, "_split.rds")))
dir <- file.path(tempdir(), "members")
dir.create(dir)
for (f in files) {
  to <- file.path(dir, basename(f))
  if (!grepl("\\.xpt$", f, ignore.case = TRUE)) { file.copy(f, to); next }
  x <- haven::read_xpt(f)
  if ("USUBJID" %in% names(x)) x <- x[x$USUBJID %in% members, ]
  haven::write_xpt(x, to, version = 5, name = toupper(tools::file_path_sans_ext(basename(f))))
}
n <- if (args[2] == "members") length(members) else as.integer(args[2])
start_workers(8)
b <- new_build(8, file.path(tempdir(), paste0("cache-", label)), n = n, seed = seeds[1])
start_load(b, list.files(dir, full.names = TRUE))
run_build(b)
print(build_status(b)[c("dataset", "stage", "message")])
bundle <- build_bundle(b)
for (s in seeds) {
  g <- generate_study(bundle, n, s)
  saveRDS(list(data = output_datasets(bundle, g)), file.path(OUT, sprintf("%s_seed%d.rds", label, s)))
  cat(label, "seed", s, "written\n")
}
