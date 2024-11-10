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
#' @importFrom tidyr fill
#' @importFrom zoo na.approx
#' @importFrom dplyr %>%
#'
#' @return A data frame with missing values filled
#'
#' @export
#'
fill_price_NAs <- function(df, col.date, col.id, col.px, method = "interpolate") {
  # Load necessary libraries
  library(dplyr)  # For data manipulation
  library(tidyr)  # For forward and backward fill
  library(zoo)    # For interpolation

  # Ensure proper sorting
  df <- df %>%                              # dplyr: Pipe operator to chain operations
    arrange({{ col.id }}, {{ col.date }})   # dplyr::arrange(): Sorts by ID and date

  # Handle missing values based on the method
  df <- df %>%
    group_by({{ col.id }}) %>%              # dplyr::group_by(): Groups by asset ID
    mutate(                                 # dplyr::mutate(): Creates/updates columns
      {{ col.px }} := case_when(            # dplyr::case_when(): Conditional transformations
        method == "interpolate" ~ na.approx(  # zoo::na.approx(): Interpolation
          {{ col.px }},
          x = {{ col.date }},
          na.rm = FALSE
        ),
        method == "forward" ~ fill(          # tidyr::fill(): Forward fill
          {{ col.px }}, .direction = "down"
        ),
        method == "backward" ~ fill(         # tidyr::fill(): Backward fill
          {{ col.px }}, .direction = "up"
        ),
        TRUE ~ {{ col.px }}                  # Default: No transformation
      )
    ) %>%
    ungroup()                               # dplyr::ungroup(): Removes grouping

  return(df)                                # base::return(): Returns the modified data frame
}
