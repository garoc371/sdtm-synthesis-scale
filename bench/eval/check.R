# Independent coherence check. Relations are mined on a random half of the real subjects
# (holding for 100% of them), kept when they hold for >= 99% of the other half, and tested
# on synthetic data (pass when they hold for >= 99%). Nothing here reads the pipeline's
# rules or edges.
#   Rscript check.R <label> <seed files...>   (rds files from gen.R)
source(file.path(dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))), "common.R"))
real <- read_real(DATA)
DATASETS <- names(real)[vapply(real, function(x) "USUBJID" %in% names(x) && nrow(x) > 0, TRUE)]
HOLD <- 0.99

TIMING_SUFFIX <- c("DY", "STDY", "ENDY", "DUR", "TPT", "TPTNUM", "ELTM", "TPTREF", "STRF", "ENRF", "EVLINT", "STRTPT",
  "STTPT", "ENRTPT", "ENTPT")
is_timing <- function(name, col, x) inherits(x, "Date") || col %in% c("VISITNUM", "VISIT", "VISITDY", "EPOCH", "TAETORD") ||
  col %in% paste0(if (name == "DM") "DM" else substr(name, 1, 2), TIMING_SUFFIX)

normalise <- function(data) {
  data <- data[intersect(DATASETS, names(data))]
  lapply(data, function(x) {
    x <- as.data.frame(x)
    for (v in names(x)) {
      if (grepl("DTC$", v)) x[[v]] <- as_date(x[[v]])
      else if (is.character(x[[v]])) x[[v]][!nzchar(trimws(x[[v]]))] <- NA
    }
    x
  })
}
subset_subjects <- function(data, ids) lapply(data, function(x) x[x$USUBJID %in% ids, , drop = FALSE])

main_date <- function(name, x) {
  p <- substr(name, 1, 2)
  intersect(paste0(p, c("STDTC", "DTC")), names(x))[1]
}

# Categorical content columns used for category nodes and indicators: 2..15 values.
category_columns <- function(name, x) {
  Filter(function(v) {
    if (v %in% c("USUBJID", "STUDYID", "DOMAIN") || is_timing(name, v, x[[v]]) || grepl("SEQ$|SPID$", v)) return(FALSE)
    k <- length(unique(stats::na.omit(x[[v]])))
    k >= 2 && k <= 15
  }, names(x))
}

# ---- Subject-level date nodes ----------------------------------------------------------
date_nodes <- function(data, ids, categories) {
  out <- list()
  dm <- data$DM[match(ids, data$DM$USUBJID), ]
  for (v in names(dm)) if (inherits(dm[[v]], "Date")) out[[paste0("DM.", v)]] <- as.numeric(dm[[v]])
  pick <- function(x, v, f, rows = TRUE) {
    keep <- rows & !is.na(x[[v]])
    z <- tapply(as.numeric(x[[v]][keep]), x$USUBJID[keep], f)
    unname(z[ids])
  }
  for (d in setdiff(names(data), "DM")) {
    x <- data[[d]]
    for (v in names(x)) if (inherits(x[[v]], "Date")) {
      out[[paste0(d, ".", v, ".FIRST")]] <- pick(x, v, min)
      out[[paste0(d, ".", v, ".LAST")]] <- pick(x, v, max)
    }
    m <- main_date(d, x)
    if (!is.na(m)) for (cv in names(categories[[d]])) {
      for (val in names(categories[[d]][[cv]])) {
        rows <- !is.na(x[[cv]]) & as.character(x[[cv]]) == val
        out[[sprintf("%s.%s=%s.FIRST", d, cv, val)]] <- pick(x, m, min, rows)
        out[[sprintf("%s.%s=%s.LAST", d, cv, val)]] <- pick(x, m, max, rows)
      }
    }
  }
  out
}
node_dataset <- function(n) sub("\\..*$", "", n)
is_category_node <- function(n) grepl("=", n, fixed = TRUE)

# ---- Subject-level indicators ----------------------------------------------------------
indicators <- function(data, ids, categories, dm_categories) {
  out <- list()
  dm <- data$DM[match(ids, data$DM$USUBJID), ]
  for (v in names(dm)) if (inherits(dm[[v]], "Date")) out[[paste0("DM.", v, " known")]] <- !is.na(dm[[v]])
  for (v in names(dm_categories)) for (val in dm_categories[[v]]) out[[sprintf("DM.%s=%s", v, val)]] <- !is.na(dm[[v]]) & as.character(dm[[v]]) == val
  for (d in setdiff(names(data), "DM")) {
    x <- data[[d]]
    out[[paste0(d, " has records")]] <- ids %in% x$USUBJID
    for (cv in names(categories[[d]])) for (val in names(categories[[d]][[cv]]))
      out[[sprintf("%s.%s=%s", d, cv, val)]] <- ids %in% x$USUBJID[!is.na(x[[cv]]) & as.character(x[[cv]]) == val]
  }
  do.call(cbind, out)
}
indicator_dataset <- function(n) sub("[. ].*$", "", n)
indicator_is_timing <- function(n) grepl(" known$| has records$", n)

# ---- Mining on half A -----------------------------------------------------------------
mine <- function(A) {
  ids <- A$DM$USUBJID
  rel <- list()
  add <- function(...) rel[[length(rel) + 1]] <<- data.frame(..., stringsAsFactors = FALSE)
  # Categories: values held by >= 5 subjects (dates) / >= 3 subjects (indicators).
  categories <- lapply(setNames(names(A), names(A)), function(d) {
    if (d == "DM") return(NULL)
    x <- A[[d]]
    cols <- category_columns(d, x)
    res <- lapply(setNames(cols, cols), function(cv) {
      n <- tapply(x$USUBJID, as.character(x[[cv]]), function(u) length(unique(u)))
      n[n >= 3]
    })
    Filter(length, res)
  })
  dm_categories <- lapply(setNames(nm = category_columns("DM", A$DM)), function(v) {
    n <- table(as.character(A$DM[[v]]))
    names(n)[n >= 3]
  })
  # 1. Date relations between subject nodes of different datasets (or DM); category nodes only against DM dates.
  nodes <- date_nodes(A, ids, categories)
  nn <- names(nodes)
  for (i in seq_along(nn)) for (j in seq_along(nn)) {
    if (i >= j) next
    a <- nn[i]; b <- nn[j]
    if (node_dataset(a) == node_dataset(b)) next
    if ((is_category_node(a) || is_category_node(b)) && !(startsWith(a, "DM.") || startsWith(b, "DM."))) next
    if (is_category_node(a) && is_category_node(b)) next
    both <- !is.na(nodes[[a]]) & !is.na(nodes[[b]])
    if (sum(both) < 10) next
    g <- nodes[[b]][both] - nodes[[a]][both]
    if (all(g == 0)) add(kind = "date", a = a, rel = "=", b = b)
    else if (all(g >= 0)) add(kind = "date", a = a, rel = "<=", b = b)
    else if (all(g <= 0)) add(kind = "date", a = b, rel = "<=", b = a)
  }
  # 2. Within-record date order.
  for (d in names(A)) {
    x <- A[[d]]
    dates <- names(x)[vapply(x, inherits, TRUE, "Date")]
    for (a in dates) for (b in dates) if (a < b) {
      both <- !is.na(x[[a]]) & !is.na(x[[b]])
      if (sum(both) < 10) next
      g <- as.numeric(x[[b]][both] - x[[a]][both])
      if (all(g == 0)) add(kind = "record date", a = paste0(d, ".", a), rel = "=", b = paste0(d, ".", b))
      else if (all(g >= 0)) add(kind = "record date", a = paste0(d, ".", a), rel = "<=", b = paste0(d, ".", b))
      else if (all(g <= 0)) add(kind = "record date", a = paste0(d, ".", b), rel = "<=", b = paste0(d, ".", a))
    }
  }
  # 3. Visit links.
  sv <- A$SV
  svkey <- paste(sv$USUBJID, sv$VISITNUM)
  for (d in setdiff(names(A), c("DM", "SV"))) {
    x <- A[[d]]
    if (!"VISITNUM" %in% names(x) || !nrow(x)) next
    k <- paste(x$USUBJID, x$VISITNUM)
    if (mean(k %in% svkey) == 1) add(kind = "visit", a = paste0(d, ".VISITNUM"), rel = "in", b = "SV.VISITNUM")
    for (v in names(x)[vapply(x, inherits, TRUE, "Date")]) {
      s <- sv$SVSTDTC[match(k, svkey)]
      ok <- !is.na(x[[v]]) & !is.na(s)
      if (sum(ok) >= 10 && all(x[[v]][ok] == s[ok])) add(kind = "visit", a = paste0(d, ".", v), rel = "= visit date", b = "SV.SVSTDTC")
    }
  }
  # 4. Implications between subject indicators of different datasets.
  M <- indicators(A, ids, categories, dm_categories)
  storage.mode(M) <- "double"
  na <- colSums(M)
  co <- crossprod(M)
  nms <- colnames(M)
  ds <- indicator_dataset(nms)
  for (i in which(na >= 3)) for (j in seq_along(nms)) {
    if (ds[i] == ds[j] || na[j] == nrow(M) || na[j] == 0) next
    # Keep only implications that would be unlikely (p < 0.001) if the two were independent.
    pb <- na[j] / nrow(M)
    if (co[i, j] == na[i] && pb^na[i] < 0.001) add(kind = "implies", a = nms[i], rel = "=>", b = nms[j])
    else if (co[i, j] == 0 && (1 - pb)^na[i] < 0.001) add(kind = "implies", a = nms[i], rel = "=> not", b = nms[j])
  }
  # 5. Functional dependencies within a dataset: one own column, or a DM column and an own column, determine another.
  for (d in names(A)) {
    x <- A[[d]]
    if (d != "DM") {
      dmc <- intersect(names(dm_categories), names(A$DM))
      x <- cbind(x, setNames(A$DM[match(x$USUBJID, A$DM$USUBJID), dmc, drop = FALSE], paste0("DM.", dmc)))
    }
    own <- setdiff(names(A[[d]]), c("USUBJID", "STUDYID", "DOMAIN"))
    own <- own[!vapply(A[[d]][own], inherits, TRUE, "Date") & !grepl("SEQ$", own)]
    nval <- vapply(x, function(v) length(unique(v)), 0L)
    keys <- own[nval[own] >= 2 & nval[own] <= 0.5 * nrow(x)]
    targets <- own[nval[own] >= 2]
    dmk <- if (d != "DM") paste0("DM.", intersect(names(dm_categories), names(A$DM)))
    fd <- function(key, y) {
      g <- do.call(paste, c(lapply(x[key], as.character), sep = "\r"))
      size <- table(g)[g]
      chk <- size >= 2
      if (sum(chk) < 20) return(FALSE)
      yy <- as.character(x[[y]]); yy[is.na(yy)] <- "\rNA"
      length(unique(paste(g, yy)[chk])) == length(unique(g[chk]))
    }
    for (y in targets) {
      single <- Filter(function(k) k != y && fd(k, y), keys)
      for (k in single) add(kind = "fd", a = paste0(d, ".", k), rel = "->", b = paste0(d, ".", y))
      if (!length(single) && length(dmk)) for (k1 in dmk) for (k2 in setdiff(keys, y)) {
        if (fd(c(k1, k2), y) && !fd(k1, y)) add(kind = "fd", a = paste0(d, ".", k1, "+", k2), rel = "->", b = paste0(d, ".", y))
      }
    }
  }
  list(relations = do.call(rbind, rel), categories = categories, dm_categories = dm_categories)
}

# ---- Evaluation: share of subjects / records for which a relation holds ------------------
evaluate <- function(m, data) {
  ids <- data$DM$USUBJID
  r <- m$relations
  nodes <- date_nodes(data, ids, m$categories)
  M <- indicators(data, ids, m$categories, m$dm_categories)
  sv <- data$SV
  svkey <- paste(sv$USUBJID, sv$VISITNUM)
  col_of <- function(node) { p <- strsplit(node, ".", fixed = TRUE)[[1]]; list(d = p[1], v = p[2]) }
  # Share of checked units (subjects or records) keeping the relation, and how many were checked.
  kept <- function(ok) c(mean(ok), length(ok))
  none <- c(NA_real_, 0)
  res <- vapply(seq_len(nrow(r)), function(i) {
    a <- r$a[i]; b <- r$b[i]
    switch(r$kind[i],
      date = {
        x <- nodes[[a]]; y <- nodes[[b]]
        if (is.null(x) || is.null(y)) return(none)
        ok <- !is.na(x) & !is.na(y)
        if (!any(ok)) return(none)
        kept(if (r$rel[i] == "=") x[ok] == y[ok] else x[ok] <= y[ok])
      },
      `record date` = {
        ca <- col_of(a); cb <- col_of(b)
        x <- data[[ca$d]]
        ok <- !is.na(x[[ca$v]]) & !is.na(x[[cb$v]])
        if (!any(ok)) return(none)
        kept(if (r$rel[i] == "=") x[[ca$v]][ok] == x[[cb$v]][ok] else x[[ca$v]][ok] <= x[[cb$v]][ok])
      },
      visit = {
        ca <- col_of(a)
        x <- data[[ca$d]]
        k <- paste(x$USUBJID, x$VISITNUM)
        if (r$rel[i] == "in") kept(k %in% svkey) else {
          s <- sv$SVSTDTC[match(k, svkey)]
          ok <- !is.na(x[[ca$v]]) & !is.na(s)
          # Records whose visit is not in SV count as failures here too.
          kept(c(x[[ca$v]][ok] == s[ok], rep(FALSE, sum(!is.na(x[[ca$v]]) & is.na(s)))))
        }
      },
      implies = {
        if (!a %in% colnames(M) || !b %in% colnames(M)) return(none)
        ant <- M[, a]
        if (!any(ant)) return(none)
        kept(if (r$rel[i] == "=>") M[ant, b] else !M[ant, b])
      },
      fd = {
        d <- sub("\\..*$", "", a)
        key <- strsplit(sub("^[^.]+\\.", "", a), "+", fixed = TRUE)[[1]]
        y <- sub("^[^.]+\\.", "", b)
        x <- data[[d]]
        if (any(startsWith(key, "DM."))) {
          dmc <- sub("^DM\\.", "", grep("^DM\\.", key, value = TRUE))
          x <- cbind(x, setNames(data$DM[match(x$USUBJID, data$DM$USUBJID), dmc, drop = FALSE], paste0("DM.", dmc)))
        }
        if (!all(c(key, y) %in% names(x))) return(none)
        g <- do.call(paste, c(lapply(x[key], as.character), sep = "\r"))
        yy <- as.character(x[[y]]); yy[is.na(yy)] <- "\rNA"
        size <- table(g)[g]
        chk <- size >= 2
        if (!any(chk)) return(none)
        # Share of checked records that carry their group's most common value.
        top <- tapply(yy[chk], g[chk], function(z) max(table(z)))
        c(sum(top) / sum(chk), sum(chk))
      })
  }, numeric(2))
  list(share = res[1, ], n = res[2, ])
}

# Is a relation about timing only (every column involved is part of the spine in the new design)?
involves_content <- function(r) {
  side <- function(s, kind) {
    if (kind %in% c("record date", "visit")) return(FALSE)
    if (kind == "date") return(is_category_node(s))
    if (kind == "implies") return(!indicator_is_timing(s))
    # fd: columns
    d <- sub("\\..*$", "", s)
    cols <- strsplit(sub("^[^.]+\\.", "", s), "+", fixed = TRUE)[[1]]
    any(vapply(cols, function(v) {
      if (startsWith(v, "DM.")) { v <- sub("^DM\\.", "", v); d <- "DM" }
      x <- real[[d]][[v]]
      is.null(x) || !is_timing(d, v, x)
    }, TRUE))
  }
  mapply(function(a, b, k) side(a, k) || side(b, k), r$a, r$b, r$kind)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  label <- args[1]
  files <- args[-1]
  synth <- lapply(files, function(f) normalise(readRDS(f)$data))
  real_n <- normalise(real)
  results <- list()
  for (split in 1:3) {
    set.seed(100 + split)
    ids <- real_n$DM$USUBJID
    a_ids <- sample(ids, length(ids) %/% 2)
    A <- subset_subjects(real_n, a_ids)
    B <- subset_subjects(real_n, setdiff(ids, a_ids))
    m <- mine(A)
    r <- m$relations
    e <- evaluate(m, B)
    r$share_B <- e$share; r$n_B <- e$n
    r$real <- !is.na(r$share_B) & r$share_B >= HOLD
    for (s in seq_along(synth)) { e <- evaluate(m, synth[[s]]); r[[paste0("syn", s)]] <- e$share; r[[paste0("n", s)]] <- e$n }
    r$content <- involves_content(r)
    r$split <- split
    results[[split]] <- r
    cat(label, "split", split, ":", nrow(r), "mined,", sum(r$real), "hold on half B\n")
  }
  res <- do.call(rbind, results)
  saveRDS(res, file.path(OUT, paste0("check_", label, ".rds")))
}
