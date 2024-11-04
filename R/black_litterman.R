#' Black Litterman Model
#' This model can be used for asset allocation.
#'
#' @param returns A matrix of returns of the asset classes.
#' @param weights Strategic / Current Weights of the asset classes.
#' @param tau Uncertainty parameter
#' @param views Views data frame should be of specific form given in details.
#' @param col.date Name of the date columns in returns.
#' @param col.symb Name of the symbol column in returns.
#' @param col.return Name of the return column in returns.
#'
#' @importFrom dplyr filter summarize, pull
#'
#' @return
#' @export
#'
black_litterman <- function(returns, weights, tau, views, lambda.symb, col.date, col.symb, col.return){


  # First compute the risk aversion lambda
  lambda <- returns |>
    filter({{col.symb}} == {{lambda.symb}}) |>
    summarize(
      lambda = mean({{col.return}}) / var({{col.return}})
    ) |>
    pull()





}
