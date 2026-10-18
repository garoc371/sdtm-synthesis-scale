# Plots of generated data against its source, for the Compare tab (mod_compare.R).

# One variable in source and synthetic data: densities for many numeric values, otherwise
# the shares of the most common values.
marginal_plot <- function(source, synthetic, variable) {
  d <- rbind(
    data.frame(Data = "Source", value = source),
    data.frame(Data = "Synthetic", value = synthetic)
  )
  observed <- d[!is.na(d$value), ]
  continuous <- is.numeric(d$value) || inherits(d$value, "Date")
  if (continuous && length(unique(observed$value)) > MAX_DISCRETE_VALUES) {
    return(
      ggplot(observed, aes(value, fill = Data, colour = Data)) +
        geom_density(alpha = 0.25) +
        scale_fill_manual(values = PLOT_COLORS) +
        scale_colour_manual(values = PLOT_COLORS) +
        labs(x = variable, y = NULL, fill = NULL, colour = NULL) +
        plot_theme()
    )
  }
  counts <- as.data.frame(
    table(Data = d$Data, value = level_string(d$value)),
    stringsAsFactors = FALSE
  )
  counts$share <- counts$Freq / ave(counts$Freq, counts$Data, FUN = sum)
  totals <- sort(tapply(counts$share, counts$value, sum), decreasing = TRUE)
  top <- utils::head(names(totals), 12)
  counts <- counts[counts$value %in% top, ]
  counts$value <- factor(counts$value, rev(top))
  ggplot(counts, aes(share, value, fill = Data)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    scale_fill_manual(values = PLOT_COLORS) +
    scale_x_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(x = NULL, y = NULL, fill = NULL, title = variable) +
    plot_theme()
}

# The mean of a numeric result at each visit, by arm, in source and synthetic data. `test`:
# the test followed in a findings dataset, or NA.
trajectory_plot <- function(study, generated, name, variable, test) {
  arm <- arm_variable(study$datasets$DM)
  trajectory <- function(data) trajectory_data(study, data, name, variable, test, arm)
  source <- trajectory(study$source$data)
  synthetic <- trajectory(generated)
  req(source, synthetic)
  d <- rbind(cbind(source, Data = "Source"), cbind(synthetic, Data = "Synthetic"))
  d$visit <- factor(format(d$visit), levels = format(sort(unique(d$visit))))
  ggplot(d, aes(visit, mean, colour = arm, linetype = Data, group = interaction(arm, Data))) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.6) +
    scale_linetype_manual(values = c(Source = "dashed", Synthetic = "solid")) +
    labs(
      x = "Visit",
      y = paste("Mean", variable),
      colour = NULL,
      linetype = NULL,
      title = if (!is.na(test)) test
    ) +
    plot_theme()
}

# Records per subject by arm, in source and synthetic data.
event_plot <- function(study, generated, name) {
  arm <- arm_variable(study$datasets$DM)
  d <- rbind(
    cbind(event_rate_data(study$source$data, name, arm), Data = "Source"),
    cbind(event_rate_data(generated, name, arm), Data = "Synthetic")
  )
  ggplot(d, aes(arm, rate, fill = Data)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_fill_manual(values = PLOT_COLORS) +
    labs(x = NULL, y = paste(name, "records per subject"), fill = NULL) +
    plot_theme()
}

plot_theme <- function() {
  theme_minimal(base_size = 11) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      text = element_text(colour = "#1d2327")
    )
}
