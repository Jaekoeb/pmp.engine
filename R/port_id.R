#' Sample ID data
#'
#' Downloading the portfolio assets using `download_port_bbg()`, returns two data frames. One is `id`,
#' which contains several characteristics for each position in our portfolio. Most importantly the `weight`
#' within our portfolio.
#'
#'
#' This sample data frame can be used to test functions that are based on the `download_port_bbg()` function
#' data.
#'
#' *It should not be used for current analysis, as it might not be up to date*
#'
#' @format Data frame with six columns
#' \describe{
#' \item{name}{Name of the position}
#' \item{issuer}{Issuer, mainly relevant for bonds}
#' \item{crncy}{Currency the position is denominated in}
#' \item{mkt_sector}{Market Sector (sometimes a bit arbitrary)}
#' \item{id}{The BBG id of the position}
#' \item{weight}{Weight of the position in our portfolio}
#' }
#'
#'
#' @examples
#' data(port_id)
#'
#'
"port_id"
