#' @title Download Portfolio Data from Bloomberg
#'
#' @param id Portfolio ID, as shown in the BBG Terminal under the PRTU Function, default argument is our current portfolio
#' @param start_date Prices for the portfolio are downloaded starting from this date, default argument is 10 years back
#' @param df.replace Data frame containing information which securities to replace
#'
#' @return Returns a list with two dataframes. "id" contains fundamental data for each position. "prices" contains historical prices for each position.
#' @export
#'
#' @details
#' You can load the standard format for df.replace by `load(replaceDF)`.
#' In this dataframe we will update the most practical replacements, but feel free
#' to adjust the dataframe as needed. The dataframe should however remain in that format in
#' order for the function to work properly.
#'
#'
#'
#' @importFrom dplyr rename full_join left_join
#' @importFrom tidyr pivot_longer
#' @importFrom Rblpapi bdh bdp getPortfolio
#' @importFrom lubridate years
#'
download_port_bbg <- function(id = "U31911605-2 Client",
                              start_date = Sys.Date()-years(3),
                              df.replace = NULL){

  # get the current portfolio
  current.port <- getPortfolio(id, "Portfolio_Data")

  # get portfolio weights
  port.weights <- getPortfolio(id, "Portfolio_MWeight")
  port.weights <- port.weights |>
    rename("id" = Security,
           "weight" = Weight)


  # remove all securities with 0 weight
  port.weights <- port.weights |>
    filter(weight != 0)


  # Replace Securities ------------------------------------------------------

  # replace securities with many missings with similar ones
  if (!is.null(df.replace)) {

    port.weights <- port.weights |>
      left_join(df.replace, by = c("id" = "replace")) |>
      mutate(id = ifelse(is.na(with), id, with)) |>
      select(-with)

  }



  # Price Data --------------------------------------------------------------

  # download price data for all securities in the portfolio
  df <- bdh(port.weights$id, "PX_LAST", start.date = start_date)


  # define helper function to rename the columns
  renamer <- function(df, name){
    df |>
      rename(!!name := PX_LAST)
  }

  # rename "PX_LAST" columns to the element name
  df <- lapply(names(df), function(x){renamer(df[[x]], x)})

  # merge list into single data frame
  df <- Reduce(function(x, y){full_join(x, y, by = "date")}, df)

  # remove clutter
  rm(renamer)

  # pivot df longer
  df <- df |> pivot_longer(cols = !date, names_to = "id", values_to = "px" )



  # ID Data -----------------------------------------------------------------

  # download names for all securities
  df.id <- bdp(port.weights$id, c("SECURITY_NAME", "ISSUER", "CRNCY", "MARKET_SECTOR_DES"))

  df.id$id <- rownames(df.id)
  rownames(df.id) <- NULL

  df.id <- df.id |> rename(
    "name" = SECURITY_NAME,
    "issuer" = ISSUER,
    "crncy" = CRNCY,
    "mkt_sector" = MARKET_SECTOR_DES
  )



  # Join Dataframes ---------------------------------------------------------

  # add the weights to the id dataframe
  df.id <- left_join(df.id, port.weights, by = "id")



  return(list("id" = df.id, "prices" = df))
}
