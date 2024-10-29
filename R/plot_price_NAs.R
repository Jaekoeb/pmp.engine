#' Visualize NAs in price time-series
#'
#' Function to get an idea where NAs are in the data set. Recommended to take a look before running any analysis.
#'
#' @param df Price data frame (usually downloaded via `pmp.engine::download_port_bbg`)
#' @param col.date Name of the date column
#' @param col.id Name of the id column
#' @param col.px Name of the price column
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual labs theme_minimal element_text theme element_line scale_x_date
#' @importFrom dplyr mutate
#'
#' @return Returns a plot
#'
#' @export
#'
plot_price_NAs <- function(df, col.date = date, col.id = id, col.px = px) {

  df |>
    mutate(is_missing = is.na({{col.px}})) |>
    ggplot(aes(x = {{col.date}}, y = as.factor({{col.id}}), fill = is_missing)) +
    geom_tile(linewidth = 0) +  # Optimization for faster rendering
    scale_fill_manual(
      values = c("TRUE" = "tomato", "FALSE" = "#E0E0E0"),
      labels = c("TRUE" = "Missing", "FALSE" = "Available")
    ) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +  # Optimized date breaks
    labs(
      title = "Missing Values in Time-Series Data",
      subtitle = "Highlighting periods with missing data",
      x = "Date",
      y = "Asset ID",
      fill = "Data Status"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
