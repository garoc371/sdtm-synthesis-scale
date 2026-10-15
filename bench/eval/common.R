# Shared by the evaluation scripts: paths, and a reader for the real data that is kept
# apart from the app's own (R/source.R), so the checks do not share its date handling.
#   DATA  folder of real SDTM files (default: data/pilot)
#   OUT   where generated studies and results go (default: bench/out/eval)
arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
EVAL <- dirname(normalizePath(sub("^--file=", "", arg[1])))
REPO <- normalizePath(file.path(EVAL, "..", ".."))
DATA <- Sys.getenv("DATA", file.path(REPO, "data", "pilot"))
OUT <- Sys.getenv("OUT", file.path(REPO, "bench", "out", "eval"))
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# Complete ISO dates become Dates; partial ones ("2013-04") are missing, or all dates stay
# text with dates = FALSE. Blanks are NA.
as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- substr(as.character(x), 1, 10)
  x[!grepl("^\\d{4}-\\d{2}-\\d{2}$", x)] <- NA
  as.Date(x)
}
read_real <- function(dir, dates = TRUE) {
  files <- list.files(dir, "\\.xpt$", full.names = TRUE, ignore.case = TRUE)
  data <- lapply(files, function(f) {
    x <- as.data.frame(haven::read_xpt(f))
    names(x) <- toupper(names(x))
    x[] <- lapply(names(x), function(n) {
      v <- x[[n]]; attributes(v) <- NULL
      if (is.character(v)) v[!validUTF8(v)] <- iconv(v[!validUTF8(v)], "latin1", "UTF-8")   # SAS transport is often Latin-1
      if (is.character(v)) v[!nzchar(trimws(v))] <- NA
      if (dates && grepl("DTC$", n)) as_date(v) else v
    })
    x
  })
  setNames(data, toupper(tools::file_path_sans_ext(basename(files))))
}
