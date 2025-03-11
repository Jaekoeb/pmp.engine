#' Option Volatility
#'
#' This function retrieves and calculates various volatility metrics for the S&P 500 tickers.
#' It uses Bloomberg data to fetch implied volatility, historical volatility, and average bid-ask spread.
#' It then calculates the difference and ratio between historical and implied volatility.
#'
#' @return A data frame containing the following columns:
#' \itemize{
#'   \item \code{Implied Volatility by Moneyness (IVOL_MONEYNESS)}: Implied volatility (IVOL) corresponding to the specified Implied Volatility Maturity (RK447, IVOL_MATURITY) Implied Volatility Moneyness Level (VL363, IVOL_MONEYNESS_LEVEL) overrides. Volatilities are from Listed Implied Volatility Engine (LIVE) calculator. This field displays END OF DAY volatilities only.
#'   \item \code{Volatility 360 Day (VOLATILITY: 360D)}: A measure of the risk of price moves for a security calculated from the standard deviation of day to day logarithmic historical price changes. The 360-day price volatility equals the annualized standard deviation of the relative price change for the 360 most recent trading days closing price, expressed as a percentage. When looking at current value, the last price point will be the most recently traded price of the security.
#'   \item \code{Average Bid Ask-Spread Percentage (AVERAGE_BID_ASK_SPREAD-\%)}: Provides the average of all bid/ask spreads taken as a percentage of the mid price. The bid/ask points used for the computation correspond to the quotes received for the period indicated by Calc Interval (PX393, CALC_INTERVAL) (default value is five days) ending in the complete trading day prior to the date indicated by End Date Override (PX392, END_DATE_OVERRIDE) (default value is latest completed trading day). For a trading day to contribute to the calculation, there should be at least ten valid bid/ask spread points on that day. The field returns values only if more than 50% of trading days in the period are eligible to contribute to the calculation. The CMC Interval (PX393, CALC_INTERVAL) override will only support periods from one day (1D) up to 90 days (90D).
#'   \item \code{Volatility Difference}: The difference between historical and implied volatility.
#'   \item \code{Volatility Difference Ratio}: The ratio of the volatility difference to historical volatility.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   Rblpapi::blpConnect()
#'   tickers <- data(""sp500tickers"")
#'
#'   option_data <- option_vol(tickers)
#'   head(option_data)
#' }
#'
#' @importFrom Rblpapi bdp
#'
option_vol <- function(tickers = data("sp500tickers", package = "pmp.engine")) {
  colnames(tickers) <- c("Ticker", "Comp Name")

  data <- bdp(tickers$Ticker, c("IVOL_MONEYNESS", "VOLATILITY_360D", "AVERAGE_BID_ASK_SPREAD_%"))

  data$VOLATILITY_DIFF <- round(data$VOLATILITY_360D / data$IVOL_MONEYNESS, 4)

  colnames(data) <- c("Implied Volatility", "Historical Volatility", "Average Bid-Ask Spread", "Volatility Difference")

  return(data)
}




