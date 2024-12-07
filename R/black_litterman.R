#' Black Litterman Model
#'
#' This model can be used for asset allocation. The main idea is to combine market implied returns with manager views
#' to get a tactical asset allocation.
#'
#' @param returns A matrix of returns of the asset classes in long format.
#' @param weights Strategic / Current Weights of the asset classes.
#' @param tau Uncertainty parameter.
#' @param view Views data frame should be of specific form given in details.
#' @param freq Frequency of returns, given as an integer.
#' @param lambda Level of risk aversion in the market
#' @param col.date Name of the date columns in returns.
#' @param col.symb Name of the symbol column in returns.
#' @param col.return Name of the return column in returns.
#'
#' @details
#' There is a huge number of explanations and extensions of the Black Litterman model. Here we implemented it in a very simple form.
#' An important step of implementing the model is calibrating the parameters, I will go into detail of one possible way:
#'
#' `tau`: Tau corresponds to the uncertainty of the market. A lower tau, means more confidence that the market is right. The original paper suggests 0.25.
#'
#' `lambda`: The risk aversion in the market. There are different ways to estimate this value. One nice approach is to use a baseline
#' say 3, and then scale corresponding to the level of the VIX. Is the VIX historically high, risk aversion is higher in the market thus leading
#' to a higher risk aversion parameter.
#'
#' `view`: The views can be in principle any linear combinations of the asset classes. This implementation only considers absolute views about asset classes.
#' In this way we can give views as confidence intervals. For example we say: "Equities will return 5-10% with 90% confidence".
#' The input `view` should be of a specific form for this functions. It should be a data frame with columns: `asset`, `min`, `max`
#' and `conf`. Here each view corresponds to a column.
#'
#'
#' @importFrom dplyr select filter summarize pull
#' @importFrom MASS cov.rob
#'
#' @return Returns a list containing the implied returns, posterior returns, input weights and new weights.
#' @export
#'
black_litterman <- function(returns, weights, tau, view, freq, lambda, col.date, col.symb, col.return){



# COVARIANCE --------------------------------------------------------------
  # Compute the covariance matrix
  cov <- returns |>
    pivot_wider(names_from = {{col.symb}}, values_from = {{col.return}}) |>
    select(-{{col.date}}) |>
    cov.rob()

  # extract and annualize
  cov <- cov$cov * freq

  # rearrange it alphabetically
  cov <- cov[sort(rownames(cov)), sort(colnames(cov))]




# IMPLIED RETURNS ---------------------------------------------------------
  # compute the implied returns
  implied <- lambda * crossprod(cov, weights)



# VIEWS MATRIX ------------------------------------------------------------
  # sort the views based on the covariance matrix
  # construct the Views matrix
  mu <- view |>
    mutate(view = 0.5 * min + 0.5 * max) |>
    select(view) |>
    pull()

  # add columns names
  names(mu) <- view |> select(asset) |> pull()

  # rearrange alphabetically
  mu <- mu[sort(names(mu))]


# CONFIDENCE MATRIX -------------------------------------------------------

  # compute the implied standard error
  view <- view |>
    mutate(
      sigma = (max - min)/(2 * (-1) * qnorm((1-conf)/2))
    )

  # construct confidence matrix
  omega <- view |> select(sigma) |> pull() |> diag()

  # add row and column names
  rownames(omega) <- view |> select(asset) |> pull()
  colnames(omega) <- view |> select(asset) |> pull()

  # rearrange it alphabetically
  omega <- omega[sort(rownames(omega)), sort(colnames(omega))]


# VIEWS MATRIX ------------------------------------------------------------

  # first construct diagonal matrix for all views
  P <- diag(nrow(cov))
  rownames(P) <- rownames(cov)

  # delete rows if we dont have a view
  P <- P[rownames(omega), ]



# POSTERIOR ---------------------------------------------------------------


  # compute posterior variance covariance update
  upd.cov <- solve(
    solve(tau * cov) + t(P) %*% solve(omega) %*% P
  )

  posterior <- upd.cov %*% ( solve(tau * cov) %*% implied + t(P) %*% solve(omega) %*% mu)



# UPDATED WEIGHTS ---------------------------------------------------------

  # compute updated weights for posterior returns
  upd.weight <- solve(lambda * cov) %*% posterior



# FINAL -------------------------------------------------------------------


  # change results back to vectors
  implied <- implied[, 1]
  posterior <- posterior[, 1]
  upd.weight <- upd.weight[, 1]

  # norm the updated weights to 1
  upd.weight <- upd.weight / sum(upd.weight)

  # sort weights
  weights <- weights[sort(names(weights))]


  return(
    list(
      implied = implied,
      posterior = posterior,
      weights.old = weights,
      weights.new = upd.weight
    )
  )

}
