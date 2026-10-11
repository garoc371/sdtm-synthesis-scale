# Failing real relations per label: those broken in most seeds (2 of 3; in any split), split into ones the imputed real data also fail ("inherited"), ones between a
# benchmark replica and a copy of the same domain ("copies": XA.XACLAS = CM.CMCLAS holds
# only because XA copies CM's records; from the folder's manifest, when there is one) and
# the rest. A relation is broken in a seed when more of its subjects (or records) break it
# than chance allows at 1% (one-sided binomial test at 5%): one subject of 20 is not counted.
#   Rscript summary.R <labels...>
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
ids <- function(label) {
  r <- readRDS(file.path(OUT, paste0("check_", label, ".rds")))
  r <- r[r$real, ]
  syn <- grep("^syn", names(r), value = TRUE)
  broken <- function(s) {
    share <- r[[s]]; n <- r[[sub("^syn", "n", s)]]
    !is.na(share) & n > 0 & stats::pbinom(round((1 - share) * n) - 1, n, 0.01, lower.tail = FALSE) < 0.05
  }
  nf <- rowSums(sapply(syn, broken), dims = 1)
  r <- r[nf > length(syn) / 2, ]
  r[!duplicated(paste(r$kind, r$a, r$rel, r$b)), c("kind", "a", "rel", "b")]
}
manifest <- file.path(dirname(DATA), paste0(basename(DATA), "_manifest.csv"))
m <- if (file.exists(manifest)) utils::read.csv(manifest) else data.frame(dataset = character(), kind = character(), source = character())
replicas <- m$dataset[m$kind == "replica"]
origin <- stats::setNames(m$source, m$dataset)[replicas]
dataset_of <- function(x) sub("[. ].*$", "", x)
# The domain a dataset copies (SUPPXB -> SUPPAE -> AE).
domain_of <- function(d) sub("^SUPP", "", ifelse(d %in% names(origin), origin[d], d))
copies <- function(f) {
  a <- dataset_of(f$a); b <- dataset_of(f$b)
  a != b & domain_of(a) == domain_of(b) & (a %in% replicas | b %in% replicas)
}
key <- function(f) paste(f$kind, f$a, f$rel, f$b)
inherited <- key(ids(Sys.getenv("REF", "realimp")))
for (l in commandArgs(TRUE)) {
  f <- ids(l)
  inh <- key(f) %in% inherited
  cp <- !inh & copies(f)
  cat(sprintf("%-8s failing %4d  inherited %3d  copies %4d  other %4d\n", l, nrow(f), sum(inh), sum(cp), sum(!inh & !cp)))
  if (identical(Sys.getenv("SHOW"), "1")) { cat("\n", l, "\n"); writeLines(paste(" ", sort(key(f)[!inh & !cp]))) }
}
