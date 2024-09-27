#' Automated Portfolio Reporting
#'
#' This function should automate large parts of the reporting.
#'
#' @param df_value Xts object containing historic value of the portfolio.
#'
#'
#' @details
#' The dataframe for df_value can be downloaded from llb.at, by clicking
#' Wealth -> Performance -> ... -> Export. Make sure to adjust the timeframe and save
#' as a csv. Convert the dataframe into an xts before inputing it.
#'
#'
#' @return Returns nothing currently
#'
#' @importFrom xts is.xts to.monthly
#' @importFrom PerformanceAnalytics Return.calculate
#' @importFrom lubridate year month
#' @importFrom dplyr mutate select arrange
#' @importFrom tidyr pivot_wider
#'
portfolio_reporting <- function(df_value){


# Checks ------------------------------------------------------------------

  # df_value
  if (!is.xts(df_value)) {stop("Input is not a xts object")}
  if (ncol(df_value) != 1) {stop("Xts does not have exactly two columns")}


  # set column name to zz
  colnames(df_value) <- "zz"


# Functions ---------------------------------------------------------------


  # Compute the monthly performance and output a dataframe
  monthly_performance <- function(df_value){

    df <- df_value
    df <- to.monthly(df, OHLC = F)
    df <- Return.calculate(df)
    df <- as.data.frame(df)

    df <- df |>
      mutate(
        date = as.Date(paste(rownames(df), "01"), format = "%b %Y %d"),
        year = year(date),
        month = month(date, label = T)
      ) |>
      select(-date) |>
      arrange(month)

    rownames(df) <- NULL

    df <- df |>
      pivot_wider(names_from = month, values_from = zz) |>
      arrange(year)

    return(df)
  }


  # Compute current performance
  current_performance <- function(df_value){
    df <- df_value

  }


# Results -----------------------------------------------------------------

  # Monthly Performance
  df_monperf <- monthly_performance(df_value)

  return(df_monperf)

}


