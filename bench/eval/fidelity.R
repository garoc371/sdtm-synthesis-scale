# Marginal fidelity of generated (non-spine) columns: total variation distance for
# categories (numbers with <= 20 values count as categories), KS statistic otherwise.
#   Rscript fidelity.R <label> <seed files...>
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
real <- read_real(DATA)
args <- commandArgs(TRUE)
timing <- function(d, v, x) inherits(x, "Date") || grepl("DTC$|DY$|^VISIT|SEQ$|^USUBJID$|^STUDYID$|^DOMAIN$|^SUBJID$|TPT|ELTM|EPOCH", v)
tvd <- function(a, b) { a <- ifelse(is.na(a), "<NA>", as.character(a)); b <- ifelse(is.na(b), "<NA>", as.character(b))
  l <- union(a, b); 0.5 * sum(abs(table(factor(a, l)) / length(a) - table(factor(b, l)) / length(b))) }
ks <- function(a, b) { a <- a[!is.na(a)]; b <- b[!is.na(b)]; if (!length(a) || !length(b)) return(NA); suppressWarnings(stats::ks.test(a, b)$statistic[[1]]) }
rows <- list()
for (f in args[-1]) {
  g <- readRDS(f)$data
  for (d in intersect(names(real), names(g))) for (v in intersect(names(real[[d]]), names(g[[d]]))) {
    x <- real[[d]][[v]]; y <- g[[d]][[v]]
    if (timing(d, v, x)) next
    if (is.numeric(x) && length(unique(x)) > 20) { y <- suppressWarnings(as.numeric(y)); rows[[length(rows) + 1]] <- data.frame(seed = f, d, v, type = "ks", value = ks(x, y)) }
    else rows[[length(rows) + 1]] <- data.frame(seed = f, d, v, type = "tvd", value = tvd(x, y))
  }
}
r <- do.call(rbind, rows)
saveRDS(r, file.path(OUT, paste0("fid_", args[1], ".rds")))
a <- aggregate(value ~ d + v + type, r, mean)
cat(args[1], ": mean TVD", round(mean(a$value[a$type == "tvd"]), 4), " mean KS", round(mean(a$value[a$type == "ks"], na.rm = TRUE), 4), "\n")
print(aggregate(value ~ d + type, a, function(z) round(mean(z), 3)))
