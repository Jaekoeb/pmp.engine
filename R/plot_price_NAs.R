#' Visualize NAs in price time-series
#'
#' Function to get an idea where NAs are in the data set. Recommened to take a look before running any analysis.
#'
#' @param df Price data frame (usually downloaded via `pmp.engine::download_port_bbg`)
#' @param col.date Name of the date column
#' @param col.id Name of the id column
#' @param col.px Name of the price column
#'
#'
#' @importFrom ggplot2 ggplot geom_tile scale_fill_manual labs aes
#' @importFrom dplyr mutate
#'
#' @return Returns a plot
#'
#'
plot_price_NAs <- function(df, col.date = date, col.id = id, col.px = px){

  df |>
    mutate(is_missing = is.na({{col.px}})) |>
    ggplot(aes(x = {{col.date}}, y = {{col.id}}, fill = is_missing)) +
    geom_tile() +
    scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "green")) +
    labs(title = "Missing Values in Time-Series Data", fill = "Missing")
}
