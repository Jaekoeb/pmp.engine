#' Sample Price data
#'
#' Downloading the portfolio assets using `download_port_bbg()`, returns two data frames. One is `price`,
#' which contains daily prices for each position in our portfolio.
#'
#' This sample data frame can be used to test functions that are based on the `download_port_bbg()` function
#' data. It includes price data for 2023.
#'
#' *It should not be used for current analysis, as it might not be up to date*
#'
#' @format Data frame with three columns
#' \describe{
#' \item{date}{Date}
#' \item{id}{BBG ID of the position}
#' \item{px}{The last price of the day of our position}
#' }
#'
#'
#' @examples
#' data(port_price)
#'
#'
"port_price"
