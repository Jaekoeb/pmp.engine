#' Data to specify replacement assets for portfolio download
#'
#' When downloading the portfolio assets using `download_port_bbg()`, some of the assets
#' might have a lot of missings. In order to perform proper analysis it is recommended to
#' replace those securities with similar securities. This dataframe contains information on which
#' securities to replace.
#'
#' I will save the most common replacement, but the dataframe should be updated with portfolio changes.
#'
#' Current Replacements:
#'
#' Replaced the Eskom Bond with a South African Governement Bond. Both are denominated in ZAR and have similar maturity.
#'
#' @format Data frame with two columns
#' \describe{
#' \item{replace}{BBG ID of security you want to replace}
#' \item{with}{BBG ID of security to use as replacement}
#' }
#'
#'
#' @examples
#' data(replaceDF)
#'
#'
"replaceDF"
