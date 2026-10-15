# Synthetic SDTM. Shiny sources this file first, then every file in R/, then ui.R and
# server.R. Run with shiny::runApp().

# Every package the app and its workers use. Missing ones are reported together.
PACKAGES <- c(
  "shiny",
  "bslib",
  "DT",
  "ggplot2",
  "haven",
  "xml2",
  "nnet",
  "MASS",
  "rpart",
  "zip",
  "mirai",
  "digest",
  "jsonlite",
  "htmltools"
)
missing <- PACKAGES[
  !vapply(PACKAGES, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop(
    "Install the missing packages first:\n  install.packages(c(",
    paste0('"', missing, '"', collapse = ", "),
    "))",
    call. = FALSE
  )
}
suppressPackageStartupMessages(
  for (package in PACKAGES) {
    library(package, character.only = TRUE)
  }
)
rm(missing, package)

options(shiny.maxRequestSize = 500 * 1024^2)

# Background workers (option synth.cores) and the build cache (option synth.cache).
WORKERS <- getOption(
  "synth.cores",
  max(2L, min(8L, parallel::detectCores() - 1L))
)
CACHE <- getOption("synth.cache", file.path(getwd(), "cache"))

# Example studies offered on the start page.
EXAMPLES <- list(
  pilot = list(
    title = "CDISC pilot",
    folder = "data/pilot",
    about = "Study CDISCPILOT01: demographics, exposure, labs, vitals, questionnaires and events."
  ),
  bench28 = list(
    title = "bench28",
    folder = "data/bench28",
    about = paste(
      "The pilot with pharmaversesdtm disposition, ECG, PK, ophthalmology and oncology data,",
      "supplemental qualifiers and trial design."
    )
  ),
  bench72 = list(
    title = "bench72",
    folder = "data/bench72",
    about = "bench28 with QS, LB and OE split by category, and 34 custom domains."
  )
)
