# SE's elements for each synthetic subject, in order: the first k of the arm's path in TA, k
# drawn as among source subjects of the same profile, and the elements off the path (a
# follow-up, an unplanned element, a screen failure's screening), each after the same number
# of planned elements as in the source, drawn given k. An element is the record most of the
# arm's source subjects have for it.
draw_elements <- function(x, code, ta, arm_src, arm_syn, ctx) {
  ta <- ta[order(ta$TAETORD), , drop = FALSE]
  path <- split(level_string(ta$ETCD), level_string(ta$ARMCD))
  etcd <- level_string(x$ETCD)
  arm <- arm_src[x$.subject]
  planned <- vapply(seq_along(etcd), function(i) etcd[i] %in% unlist(path[arm[i]]), TRUE)
  after <- stats::ave(as.numeric(planned), x$.subject, FUN = cumsum)
  subjects <- factor(x$.subject, seq_len(ctx$N))
  k_src <- tabulate(x$.subject[planned], ctx$N)
  off_src <- vapply(split(paste(etcd, after, sep = "@")[!planned], subjects[!planned]), paste, "", collapse = " ")
  k <- as.integer(pooled_draw(as.character(k_src), seq_len(ctx$N), ctx$want, ctx$have))
  own <- lapply(arm_syn, function(a) unlist(path[a]) %||% character())
  k <- pmin(k, lengths(own))
  off <- pooled_draw(off_src, seq_len(ctx$N), c(lapply(ctx$want, paste, k), ctx$want), c(lapply(ctx$have, paste, k_src), ctx$have))
  top <- tapply(code, paste(arm, etcd), function(z) as.integer(names(which.max(table(z)))))
  any_arm <- tapply(code, etcd, function(z) as.integer(names(which.max(table(z)))))
  out <- lapply(seq_len(ctx$n), function(j) {
    e <- own[[j]][seq_len(k[j])]
    at <- seq_along(e)
    if (!is.na(off[j]) && nzchar(off[j])) {
      o <- strsplit(strsplit(off[j], " ", fixed = TRUE)[[1]], "@", fixed = TRUE)
      e <- c(e, vapply(o, `[`, "", 1))
      at <- c(at, pmin(as.numeric(vapply(o, `[`, "", 2)), k[j]) + seq_along(o) / (length(o) + 1))
    }
    e <- e[order(at)]
    cd <- top[paste(arm_syn[j], e)]
    cd[is.na(cd)] <- any_arm[e[is.na(cd)]]
    data.frame(.j = rep(j, length(e)), code = unname(cd))
  })
  do.call(rbind, out) %||% data.frame(.j = integer(), code = integer())
}

# SE's elements in order `o` (rows in their order by default), each starting no earlier than
# the one before and ending where the next starts.
chain_elements <- function(x, prefix, o = seq_len(nrow(x))) {
  start <- paste0(prefix, "STDTC")
  end <- paste0(prefix, "ENDTC")
  if (!nrow(x) || !start %in% names(x)) return(x)
  j <- x$.j[o]
  st <- stats::ave(ifelse(is.na(x[[start]][o]), -Inf, x[[start]][o]), j, FUN = cummax)
  x[[start]][o] <- ifelse(is.finite(st), st, NA)
  if (end %in% names(x)) {
    following <- c(x[[start]][o][-1], NA)
    following[c(j[-1] != j[-length(j)], TRUE)] <- NA
    x[[end]][o] <- ifelse(is.na(following), pmax(x[[end]][o], x[[start]][o]), following)
  }
  x
}

# Each subject's actual arm (ACTARMCD), else the planned one (ARMCD), as in TA.
subject_arm <- function(dm) {
  arm <- if (!is.null(dm$ACTARMCD)) level_string(dm$ACTARMCD) else rep(MISSING, nrow(dm))
  planned <- if (!is.null(dm$ARMCD)) level_string(dm$ARMCD) else rep(MISSING, nrow(dm))
  ifelse(arm == MISSING | !nzchar(arm), planned, arm)
}

# EPOCH of each dataset's records: TA's epoch of the subject's SE element holding the record's
# start (--STDTC, else --DTC), in the source dataset's spelling; the drawn one where no planned
# element holds it.
element_epochs <- function(study, composed) {
  ta <- study_design(study)$TA
  se <- composed$SE
  if (is.null(se) || !all(c("ARMCD", "ETCD", "EPOCH") %in% names(ta)) || !all(c("ETCD", "SESTDTC") %in% names(se))) return(composed)
  epoch <- stats::setNames(level_string(ta$EPOCH), paste(level_string(ta$ARMCD), level_string(ta$ETCD)))
  arm <- subject_arm(composed$DM)[match(se$.j, composed$DM$.j)]
  se <- data.frame(.j = se$.j, start = as.numeric(se$SESTDTC), end = as.numeric(se$SEENDTC %||% se$SESTDTC),
    epoch = unname(epoch[paste(arm, level_string(se$ETCD))]))
  se$end[is.na(se$end)] <- Inf
  se <- se[!is.na(se$epoch) & !is.na(se$start), , drop = FALSE]
  others <- setdiff(names(composed), c("SE", "DM"))
  composed[others] <- lapply(others, function(name) record_epochs(composed[[name]], study$datasets[[name]]$prefix, study$spine[[name]]$EPOCH, se))
  composed
}

# EPOCH of records `x` from their subjects' elements `se` (see `element_epochs()`), spelled as
# the source's epochs `spelled` are.
record_epochs <- function(x, prefix, spelled, se) {
  v <- intersect(paste0(prefix, c("STDTC", "DTC")), names(x))[1]
  if (is.null(x$EPOCH) || !nrow(x) || is.na(v)) return(x)
  m <- merge(data.frame(i = seq_len(nrow(x)), .j = x$.j, d = as.numeric(x[[v]])), se)
  m <- m[!is.na(m$d) & m$d >= m$start & m$d <= m$end, , drop = FALSE]
  m <- m[!duplicated(m$i), , drop = FALSE]
  spelled <- level_string(spelled)
  spelling <- tapply(spelled, toupper(spelled), most_common)
  new <- spelling[toupper(m$epoch)]
  x$EPOCH[m$i] <- ifelse(is.na(new), m$epoch, new)
  x
}
