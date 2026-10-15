# Display helpers shared by the app's modules: theme, colours, badges and tables.
# Layout and colour rules live in www/style.css; these functions only pick classes.

COLORS <- c(
  primary = "#14706b",
  secondary = "#6c7276",
  success = "#2f7d4f",
  info = "#3a6ab0",
  warning = "#b7791f",
  danger = "#b3443a"
)

# Node types and statuses use the subtle variants of the theme colours.
TYPE_COLORS <- c(
  key = "secondary",
  rule = "warning",
  identifier = "info",
  sampled = "primary"
)
STATUS_COLORS <- c(
  ok = "success",
  accepted = "success",
  proposed = "warning",
  error = "danger",
  failed = "danger"
)
TYPE_LABELS <- c(
  key = "Key",
  rule = "Calculated",
  identifier = "Identifier",
  sampled = "Sampled",
  blank = "Not generated"
)
STAGES <- c(
  waiting = "Waiting",
  queued = "Queued",
  running = "Building",
  built = "Built",
  failed = "Check failed",
  error = "Error",
  skipped = "Not generated"
)

# A colour mixed with white, as Bootstrap's tint-color().
tint <- function(color, white) {
  mix <- function(w) {
    grDevices::rgb(
      t(grDevices::col2rgb(color) * (1 - w) + 255 * w),
      maxColorValue = 255
    )
  }
  vapply(white, mix, "")
}

PLOT_COLORS <- c(
  Source = tint(COLORS[["secondary"]], 0.4),
  Synthetic = COLORS[["primary"]]
)

app_theme <- function() {
  bs_theme(
    version = 5,
    primary = COLORS[["primary"]],
    secondary = COLORS[["secondary"]],
    success = COLORS[["success"]],
    info = COLORS[["info"]],
    warning = COLORS[["warning"]],
    danger = COLORS[["danger"]],
    bg = "#ffffff",
    fg = "#1d2327",
    base_font = "Manrope, 'Avenir Next', system-ui, sans-serif",
    code_font = "'SFMono-Regular', Consolas, monospace",
    font_scale = 0.92,
    `border-radius` = "0.5rem",
    `border-color` = "#e4e2dc",
    `headings-font-weight` = 700,
    `btn-font-weight` = 600,
    `form-label-font-weight` = 600,
    `badge-font-weight` = 600,
    `code-color` = "#0e524e",
    `card-cap-bg` = "transparent",
    `card-box-shadow` = "none"
  )
}

# A pill in a theme colour, or outlined without one.
badge <- function(color, text) {
  class <- if (is.na(color)) {
    "badge rounded-pill border text-body-secondary"
  } else {
    sprintf("badge rounded-pill bg-%s-subtle text-%s-emphasis", color, color)
  }
  span(class = class, text)
}

type_badge <- function(type) badge(TYPE_COLORS[type], TYPE_LABELS[[type]])

status_badge <- function(status) {
  label <- switch(
    status,
    ok = "OK",
    error = "Error",
    failed = "Failed",
    proposed = "Proposed",
    accepted = "Accepted",
    rejected = "Rejected",
    status
  )
  badge(STATUS_COLORS[status], label)
}

stage_badge <- function(stage) {
  span(class = "stage-badge", `data-stage` = stage, STAGES[[stage]])
}

# A button that sends `value` to the Shiny input `input` when clicked (see www/app.js).
input_button <- function(input, value, ..., class = NULL) {
  tags$button(
    type = "button",
    class = class,
    `data-input` = input,
    `data-value` = value,
    ...
  )
}

hint <- function(...) div(class = "hint", ...)

muted <- function(...) span(class = "muted", ...)

empty_state <- function(title, text, ...) {
  div(class = "empty-state", h5(title), p(text), ...)
}

share_text <- function(share) {
  ifelse(is.na(share), "–", sprintf("%.1f%%", 100 * share))
}

# A share as a small bar, as HTML for a table cell.
share_bar <- function(share) {
  if (is.na(share)) {
    return("–")
  }
  bar <- div(
    class = "share-bar",
    div(style = sprintf("width: %.1f%%", 100 * share))
  )
  as.character(div(class = "share", bar, span(share_text(share))))
}

plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")

big <- function(n) format(n, big.mark = ",")

# A table without paging, search or sorting.
compact_table <- function(
  data,
  selection = "none",
  escape = TRUE,
  height = NULL,
  empty = "Nothing to show",
  ...
) {
  datatable(
    data,
    rownames = FALSE,
    selection = selection,
    escape = escape,
    class = "compact hover",
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      scrollX = TRUE,
      scrollY = height,
      scrollCollapse = TRUE,
      autoWidth = FALSE,
      language = list(emptyTable = empty)
    ),
    ...
  )
}

# Run an action and report errors as a notification instead of ending the session.
guarded <- function(notify, expr) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "shiny.silent.error")) {
      return(invisible())
    }
    notify(conditionMessage(e), "error")
  })
}

# A reactive value that follows `compute()` but invalidates its readers only when the
# value changes, so that outputs built from it are not redrawn on every poll.
changes <- function(compute) {
  value <- reactiveVal()
  observe(value(compute()), priority = 100)
  value
}
