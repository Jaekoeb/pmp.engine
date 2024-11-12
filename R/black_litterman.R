#' Black Litterman Model
#' This model can be used for asset allocation.
#'
#' @param returns A matrix of returns of the asset classes.
#' @param weights Strategic / Current Weights of the asset classes.
#' @param tau Uncertainty parameter
#' @param views Views data frame should be of specific form given in details.
#' @param freq Frequency of returns, given as an integer.
#' @param lambda.simb Which column to use for computing the risk aversion, given as `char`
#' @param col.date Name of the date columns in returns.
#' @param col.symb Name of the symbol column in returns.
#' @param col.return Name of the return column in returns.
#'
#' @importFrom dplyr filter summarize pull
#' @importFrom MASS cov.rob
#'
#' @return results
#' @export
#'
black_litterman <- function(returns, weights, tau, views, freq, lambda.symb, col.date, col.symb, col.return){



# LAMBDA ------------------------------------------------------------------
  # Compute the risk aversion lambda
  lambda <- returns |>
    filter({{col.symb}} == {{lambda.symb}}) |>
    summarize(
      lambda = mean({{col.return}}) / var({{col.return}})
    ) |>
    pull()



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
  mu <- views |>
    mutate(view = 0.5 * min + 0.5 * max) |>
    select(view) |>
    pull()

  # add columns names
  names(mu) <- views |> select(asset) |> pull()

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
