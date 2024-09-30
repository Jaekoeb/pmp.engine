#' Compute Returns
#'
#' This function computes the return for each security on the portfolio.
#'
#' @param df_price Time-series price data frame
#' @param period Frequency of returns, based on `xts::to.period()`
#'
#' @return Data frame with all security returns
#' @export
#'
#' @importFrom xts xts to.period
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr select pull mutate
#' @importFrom PerformanceAnalytics Return.calculate
#'
security_return <- function(df_price, period = "week", col.id = id, col.px = px, col.date = date){


  # Pivot to a long format
  df <- df_price |> pivot_wider(names_from = {{col.id}}, values_from = {{col.px}})

  # Convert to xts
  df <- df |> select(-{{col.date}}) |> xts(order.by = df |> select({{col.date}}) |> pull())

  # change to given frequency
  df <- df |> to.period(period = period, OHLC = F)

  # compute return
  df <- df |> Return.calculate()

  # change back to data frame
  df <- df |> as.data.frame()
  df <- df |> mutate(date = rownames(df))
  rownames(df) <- NULL

  # pivot longer
  df <- df |> pivot_longer(cols = !date, names_to = "id", values_to = "ret")

  return(df)

}
