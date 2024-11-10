#' Fill Missing Values in Price Time-Series
#'
#' This function fills missing values in a price time-series dataset using a chosen method.
#'
#' @param df Data frame containing price data
#' @param col.date Name of the date column
#' @param col.id Name of the id column
#' @param col.px Name of the price column
#' @param method Method to handle missing values. Options: "interpolate", "forward", "backward"
#'
#' @importFrom dplyr arrange group_by mutate ungroup
#' @importFrom zoo na.approx na.locf
#' @importFrom dplyr %>%
#'
#' @return A data frame with missing values filled
#'
#' @export
#'
fill_price_NAs <- function(df, col.date, col.id, col.px, method = "interpolate") {
  library(dplyr)
  library(zoo)

  df <- df %>%
    arrange({{ col.id }}, {{ col.date }}) %>%
    group_by({{ col.id }}) %>%
    mutate(
      {{ col.px }} := case_when(
        method == "interpolate" ~ na.approx(!!ensym(col.px), x = !!ensym(col.date), na.rm = FALSE),
        method == "forward" ~ na.locf(!!ensym(col.px), na.rm = FALSE),
        method == "backward" ~ na.locf(!!ensym(col.px), na.rm = FALSE, fromLast = TRUE),
        TRUE ~ !!ensym(col.px)
      )
    ) %>%
    ungroup()

  return(df)
}
