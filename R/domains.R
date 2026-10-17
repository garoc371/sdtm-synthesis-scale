# Datasets by SDTM class: the class decides the dataset order and how a dataset is handled.
#
#   subject        DM
#   special        SE, SV, CO, SM: subject-level special-purpose datasets
#   interventions  topic variable --TRT (EX, EC, CM, …)
#   events         topic variable --TERM or --DECOD (AE, DS, MH, …)
#   findings       topic variable --TESTCD (VS, LB, QS, …; split datasets such as LBCH too)
#   design         no USUBJID (TA, TE, TV, TI, TS, …): copied to the output, not generated
#   relationship   RELREC: not generated
# SUPP-- datasets never reach this point: they are folded into their parent (R/supp.R).

CLASSES <- c(subject = "Subject", special = "Special purpose", interventions = "Interventions", events = "Events",
  findings = "Findings", other = "Other", design = "Trial design", relationship = "Relationship")

# The variable prefix: the DOMAIN value, so the split dataset LBCH uses LB (LBTESTCD).
domain_prefix <- function(name, x) {
  domain <- if ("DOMAIN" %in% names(x)) unique(stats::na.omit(as.character(x$DOMAIN)))
  if (length(domain) == 1 && nchar(domain) == 2) domain else substr(name, 1, 2)
}

domain_class <- function(name, x) {
  p <- domain_prefix(name, x)
  if (!"USUBJID" %in% names(x)) return("design")
  if (name == "RELREC") return("relationship")
  if (p == "DM") return("subject")
  if (p %in% c("SE", "SV", "CO", "SM")) return("special")
  has <- function(suffix) any(paste0(p, suffix) %in% names(x))
  if (has("TESTCD")) "findings" else if (has("TRT")) "interventions" else if (has(c("TERM", "DECOD"))) "events" else "other"
}

# Datasets in class order (CLASSES), then by name: DM first, as every other dataset's subjects
# and inputs come from it. The order decides which record counts (N.*) a dataset may use; a
# dataset also comes after the one its link id points to, whose records it reads.
order_datasets <- function(data) {
  class <- mapply(domain_class, names(data), data)
  ordered <- names(data)[order(match(class, names(CLASSES)), names(data))]
  out <- character()
  for (name in ordered) {
    chain <- name
    while (!is.null(to <- dataset_link(data, chain[1])$dataset) && !to %in% c(out, chain)) chain <- c(to, chain)
    out <- union(out, chain)
  }
  out
}

# SDTMIG: --LNKID links a record to a record of another dataset (TRLNKID to the lesion TU
# identifies). The other dataset is the one whose own --LNKID names one record of the subject
# and holds most of the dataset's link ids, more than half. Two datasets that each name one
# record per link id and hold each other's (a domain and its copy) do not link.
dataset_link <- function(data, name) {
  x <- data[[name]]
  key <- paste0(domain_prefix(name, x), "LNKID")
  if (is.null(x[[key]])) return(NULL)
  ids <- paste(x$USUBJID, x[[key]])[!is.na(x[[key]])]
  best <- NULL
  share <- 0.5
  for (d in setdiff(names(data), name)) {
    y <- data[[d]]
    by <- paste0(domain_prefix(d, y), "LNKID")
    if (is.null(y[[by]]) || is.null(y$USUBJID)) next
    own <- paste(y$USUBJID, y[[by]])[!is.na(y[[by]])]
    if (anyDuplicated(own) || !anyDuplicated(ids) && mean(own %in% ids) > 0.5) next
    if (mean(ids %in% own) > share) {
      best <- list(dataset = d, key = key, by = by)
      share <- mean(ids %in% own)
    }
  }
  best
}

# The source as the study uses it: SUPP-- datasets folded into their parents, datasets
# without subjects set aside to be copied, and the rest in class order.
organise_source <- function(source) {
  source <- fold_supp(source)
  aside <- names(source$data)[vapply(names(source$data), function(n) domain_class(n, source$data[[n]]) %in%
    c("design", "relationship"), TRUE)]
  source$design <- source$data[aside]
  source$data <- source$data[order_datasets(source$data[setdiff(names(source$data), aside)])]
  source$classes <- mapply(domain_class, names(source$data), source$data)
  source$prefixes <- mapply(domain_prefix, names(source$data), source$data)
  source
}
