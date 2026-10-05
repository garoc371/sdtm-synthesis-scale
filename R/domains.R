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

# Known domains keep the fixed order of DATASET_ORDER (split datasets take their domain's
# place); other datasets follow the last known domain of their class, then by name.
order_datasets <- function(data) {
  prefix <- mapply(domain_prefix, names(data), data)
  class <- mapply(domain_class, names(data), data)
  slot <- c(subject = "DM", special = "SV", interventions = "CM", events = "CE", findings = "QS", other = "MH")
  rank <- match(prefix, DATASET_ORDER)
  rank[is.na(rank)] <- match(slot[class[is.na(rank)]], DATASET_ORDER) + 0.5
  rank[is.na(rank)] <- length(DATASET_ORDER) + 1
  names(data)[order(rank, names(data))]
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
