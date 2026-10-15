# build_bench.R -- reproducible SDTM benchmark folders (bench28, bench72)
# Run: Rscript bench/build_bench.R   (writes data/bench28 and data/bench72)
# Needs: haven, pharmaversesdtm.
t0 <- Sys.time()
suppressPackageStartupMessages({ library(haven); library(pharmaversesdtm) })

# --- paths ---------------------------------------------------------------
arg  <- grep("^--file=", commandArgs(FALSE), value = TRUE)
here <- if (length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else getwd()
out  <- file.path(here, "..", "data")
pilot <- file.path(here, "..", "data", "pilot")      # CDISC pilot (in the repo)
td    <- file.path(here, "..", "data", "pilot_td")   # CDISC pilot trial design + define.xml

# --- helpers -------------------------------------------------------------
rd <- function(f) as.data.frame(read_xpt(f))
pv <- function(n) as.data.frame(getExportedValue("pharmaversesdtm", n))
blank <- function(d) {                         # character NA -> "" (SDTM style)
  for (v in names(d)) if (is.character(d[[v]])) d[[v]][is.na(d[[v]])] <- ""
  d
}
# set one column, keeping its attributes (labels)
setv <- function(d, v, x) {
  a <- attributes(d[[v]]); a$names <- NULL
  d[[v]] <- x
  for (k in names(a)) attr(d[[v]], k) <- a[[k]]
  d
}
# row subset, keeping column and dataset labels (base [ drops them)
take <- function(d, i) {
  o <- d[i, , drop = FALSE]
  for (v in names(d)) {
    a <- attributes(d[[v]]); a$names <- NULL
    for (k in names(a)) attr(o[[v]], k) <- a[[k]]
  }
  attr(o, "label") <- attr(d, "label")
  rownames(o) <- NULL
  o
}
# registry: named list of list(d, kind, source)
put <- function(reg, name, d, kind, source) {
  reg[[name]] <- list(d = blank(d), kind = kind, source = source)
  reg
}
# swap the source domain prefix of a name vector (blank stays blank)
ren <- function(x, from, to) {
  hit <- startsWith(x, from) & nchar(x) > nchar(from)
  x[hit] <- paste0(to, substring(x[hit], nchar(from) + 1))
  x
}
# text form of a key column (numeric 1 -> "1")
txt <- function(x) if (is.numeric(x)) trimws(formatC(x, format = "fg", digits = 15)) else as.character(x)
dom_of <- function(d) {
  if ("DOMAIN" %in% names(d)) unique(d$DOMAIN) else unique(d$RDOMAIN)
}

# SUPP rows with no parent record (USUBJID + parent[[IDVAR]] == IDVARVAL as text;
# blank IDVAR = subject level); dl = named list of data frames
supp_orphans <- function(s, dl) {
  rdom <- unique(s$RDOMAIN)
  par <- dl[vapply(dl, function(d) "DOMAIN" %in% names(d) && any(d$DOMAIN == rdom), NA)]
  psub <- unique(unlist(lapply(par, function(d) d$USUBJID)))
  idv <- ifelse(is.na(s$IDVAR), "", s$IDVAR)
  ok <- rep(FALSE, nrow(s))
  for (v in unique(idv)) {
    w <- which(idv == v)
    if (v == "") { ok[w] <- s$USUBJID[w] %in% psub; next }
    keys <- unlist(lapply(par, function(d)
      if (v %in% names(d)) paste(d$USUBJID, txt(d[[v]]), sep = "|")))
    ok[w] <- paste(s$USUBJID[w], s$IDVARVAL[w], sep = "|") %in% keys
  }
  sum(!ok)
}

write_folder <- function(reg, dir) {
  unlink(dir, recursive = TRUE); dir.create(dir, recursive = TRUE)
  for (n in names(reg)) {
    d <- reg[[n]]$d
    write_xpt(d, file.path(dir, paste0(tolower(n), ".xpt")), version = 5,
              name = toupper(n), label = attr(d, "label"))
  }
  m <- do.call(rbind, lapply(names(reg), function(n) {
    d <- reg[[n]]$d
    data.frame(dataset = n, kind = reg[[n]]$kind, source = reg[[n]]$source,
               domain = paste(dom_of(d), collapse = ";"), records = nrow(d),
               subjects = length(unique(d$USUBJID)))
  }))
  write.csv(m, file.path(dirname(dir), paste0(basename(dir), "_manifest.csv")), row.names = FALSE)
  file.copy(file.path(td, "define.xml"), file.path(dir, "define.xml"), overwrite = TRUE)
  m
}

# --- bench28: real datasets ------------------------------------------------
b28 <- list()
for (n in c("dm", "ex", "lb", "mh", "qs", "sv", "vs", "ae", "cm"))
  b28 <- put(b28, toupper(n), rd(file.path(pilot, paste0(n, ".xpt"))), "real", "pilot")
pvmap <- c(ds = "DS", eg = "EG", pc = "PC", pp = "PP", oe_ophtha = "OE", rs_onco = "RS",
           sc_ophtha = "SC", tr_onco = "TR", tu_onco = "TU")
for (n in names(pvmap))
  b28 <- put(b28, pvmap[[n]], pv(n), "real", paste0("pharmaversesdtm::", n))
supmap <- c(suppae = "SUPPAE", suppdm = "SUPPDM", suppds = "SUPPDS", supptr_onco = "SUPPTR")
for (n in names(supmap))
  b28 <- put(b28, supmap[[n]], pv(n), "supp", paste0("pharmaversesdtm::", n))
for (n in c("se", "ta", "te", "ti", "ts", "tv"))
  b28 <- put(b28, toupper(n), rd(file.path(td, paste0(n, ".xpt"))), "design", "sdtm_pilot_td")

# app ae vs pharmaversesdtm ae: SUPPAE must link to the AE used
dl <- lapply(b28, function(x) x$d)
o <- supp_orphans(dl$SUPPAE, dl)
cat(sprintf("SUPPAE orphans against app ae: %d\n", o))
if (o > 0) {
  b28 <- put(b28, "AE", pv("ae"), "real", "pharmaversesdtm::ae")
  cat("  -> using pharmaversesdtm::ae instead\n")
}

# --- bench72: splits ---------------------------------------------------------
# SDTMIG 4.1.7: DOMAIN and variables unchanged, name = domain + 2-letter suffix
splits <- list(
  QS = list(col = "QSCAT", map = c("ALZHEIMER'S DISEASE ASSESSMENT SCALE" = "AD",
      "CLINICIAN'S INTERVIEW-BASED IMPRESSION OF CHANGE (CIBIC+)" = "CB",
      "DISABILITY ASSESSMENT FOR DEMENTIA (DAD)" = "DA", "MINI-MENTAL STATE" = "MM",
      "MODIFIED HACHINSKI ISCHEMIC SCORE" = "HI",
      "NEUROPSYCHIATRIC INVENTORY - REVISED (NPI-X)" = "NP")),
  LB = list(col = "LBCAT", map = c(CHEMISTRY = "CH", HEMATOLOGY = "HE",
      URINALYSIS = "UR", OTHER = "OT")),     # blank LBCAT (8 HBA1C rows) -> OT
  OE = list(col = "OECAT", map = c("BEST CORRECTED VISUAL ACUITY" = "VA",
      "INTRAOCULAR PRESSURE" = "IO", "OPHTHALMIC ASSESSMENTS" = "OA")))
# no SUPPQS/SUPPLB/SUPPOE exist, so nothing to split on the SUPP side
stopifnot(!any(paste0("SUPP", names(splits)) %in% names(b28)))

b72 <- b28[!(names(b28) %in% names(splits))]
split_map <- NULL
for (p in names(splits)) {
  d <- b28[[p]]$d; sp <- splits[[p]]
  cat_ <- d[[sp$col]]; cat_[cat_ == ""] <- "OTHER"
  sfx <- unname(sp$map[cat_])
  stopifnot(!anyNA(sfx))
  for (s in unique(sp$map)) {
    nm <- paste0(p, s)
    b72 <- put(b72, nm, take(d, sfx == s), "split", p)
    split_map <- rbind(split_map, data.frame(parent = p, dataset = nm,
      category = names(sp$map)[sp$map == s], records = sum(sfx == s)))
  }
}

# --- bench72: custom-domain replicas ------------------------------------------
# classes cycled in turn; sources inside a class cycled in order
cls <- list(
  interv = c("CM", "EX"),
  events = c("AE", "MH", "DS"),
  finds  = c("VS", "EG", "PC", "SC", "TR", "TU", "RS", "LBCH", "LBHE", "LBUR", "LBOT",
             "QSAD", "QSCB", "QSDA", "QSMM", "QSHI", "QSNP", "OEVA", "OEIO", "OEOA"))
codes <- paste0(rep(c("X", "Y", "Z"), each = 26), LETTERS)   # custom domain codes
target <- 72
k <- 0; rep_map <- NULL
while (length(b72) < target) {
  k <- k + 1
  ci <- (k - 1) %% length(cls) + 1
  src <- cls[[ci]][((k - 1) %/% length(cls)) %% length(cls[[ci]]) + 1]
  code <- codes[k]
  d <- b72[[src]]$d
  dom <- unique(d$DOMAIN)
  seed <- 1000 + k                       # seed per replica
  set.seed(seed)
  subj <- unique(d$USUBJID)
  keep <- sample(subj, round(0.7 * length(subj)))   # random 70% of subjects
  r <- take(d, d$USUBJID %in% keep)
  names(r) <- ren(names(r), dom, code)
  r <- setv(r, "DOMAIN", rep(code, nrow(r)))
  b72 <- put(b72, code, r, "replica", src)
  ns <- 0
  sn <- paste0("SUPP", src)
  if (sn %in% names(b72)) {              # SUPP replica
    s <- take(b72[[sn]]$d, b72[[sn]]$d$USUBJID %in% keep)
    s <- setv(s, "RDOMAIN", rep(code, nrow(s)))
    s <- setv(s, "IDVAR", ren(s$IDVAR, dom, code))
    s <- setv(s, "QNAM", ren(s$QNAM, dom, code))
    attr(s, "label") <- sub(paste0(dom, "$"), code, attr(s, "label"))
    b72 <- put(b72, paste0("SUPP", code), s, "replica", sn)
    ns <- nrow(s)
  }
  rep_map <- rbind(rep_map, data.frame(code = code, source = src, seed = seed,
    subjects = length(keep), records = nrow(r), supp_records = ns))
}

# --- write -------------------------------------------------------------------
m28 <- write_folder(b28, file.path(out, "bench28"))
m72 <- write_folder(b72, file.path(out, "bench72"))

# --- checks (on the files as written) ----------------------------------------
check <- function(dir) {
  cat(sprintf("\n== %s ==\n", basename(dir)))
  fs <- list.files(dir, pattern = "\\.xpt$", full.names = TRUE)
  stem <- sub("\\.xpt$", "", basename(fs))
  dl <- lapply(fs, rd); names(dl) <- toupper(stem)
  man <- read.csv(file.path(dirname(dir), paste0(basename(dir), "_manifest.csv")), stringsAsFactors = FALSE)
  cat(sprintf("datasets: %d   records: %s\n", length(dl),
              format(sum(vapply(dl, nrow, 1L)), big.mark = ",")))
  print(table(man$kind))
  cat(sprintf("manifest matches files (names, records): %s\n",
              identical(sort(man$dataset), sort(names(dl))) &&
                all(man$records == vapply(dl[man$dataset], nrow, 1L))))
  cat(sprintf("duplicate dataset names: %d\n", sum(duplicated(names(dl)))))
  # xpt v5 limits
  bad <- names(dl)[nchar(names(dl)) > 8]
  cat(sprintf("dataset names > 8 chars: %d %s\n", length(bad), paste(bad, collapse = " ")))
  long <- unlist(lapply(names(dl), function(n) {
    v <- names(dl[[n]])[nchar(names(dl[[n]])) > 8]
    if (length(v)) paste0(n, ":", v) }))
  cat(sprintf("variable names > 8 chars: %d %s\n", length(long), paste(long, collapse = " ")))
  # character NA should not survive (written as "")
  na_chr <- sum(vapply(dl, function(d) sum(vapply(d, function(x) is.character(x) && anyNA(x), NA)), 1))
  cat(sprintf("character columns with NA: %d\n", na_chr))
  # subjects in DM
  dm <- dl$DM$USUBJID
  miss <- vapply(dl, function(d) if ("USUBJID" %in% names(d)) sum(!(unique(d$USUBJID) %in% dm)) else 0L, 1L)
  nsub <- sum(vapply(dl, function(d) "USUBJID" %in% names(d), NA))
  cat(sprintf("datasets with USUBJID: %d; USUBJIDs not in DM: %d %s\n", nsub, sum(miss),
              paste(names(miss)[miss > 0], collapse = " ")))
  # SUPP linkage
  sp <- names(dl)[startsWith(names(dl), "SUPP")]
  orph <- vapply(dl[sp], supp_orphans, 1L, dl = dl)
  cat(sprintf("SUPP datasets: %d; orphan rows: %d %s\n", length(sp), sum(orph),
              paste(names(orph)[orph > 0], collapse = " ")))
  # DOMAIN vs file name (non-SUPP)
  dd <- names(dl)[!startsWith(names(dl), "SUPP")]
  dif <- vapply(dd, function(n) !identical(unique(dl[[n]]$DOMAIN), n), NA)
  cat(sprintf("DOMAIN differs from file name: %d\n", sum(dif)))
  if (any(dif)) print(data.frame(dataset = dd[dif],
    DOMAIN = vapply(dd[dif], function(n) paste(unique(dl[[n]]$DOMAIN), collapse = ";"), "")),
    row.names = FALSE)
  # SUPP RDOMAIN vs name
  rdif <- sp[vapply(sp, function(n) !identical(unique(dl[[n]]$RDOMAIN), sub("^SUPP", "", n)), NA)]
  cat(sprintf("RDOMAIN differs from SUPP name: %d %s\n", length(rdif), paste(rdif, collapse = " ")))
  invisible(dl)
}
check(file.path(out, "bench28"))
check(file.path(out, "bench72"))

cat("\nsplit map\n"); print(split_map, row.names = FALSE)
cat("\nreplica map (seed = 1000 + replica index; 70% of source subjects)\n")
print(rep_map, row.names = FALSE)
cat(sprintf("\nruntime: %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
