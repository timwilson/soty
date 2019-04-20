#' Reshapes a two-column dataframe of archery scores and Shooter of the Year points.
#'
#' Long dataframes are inconvenient to print in a document or web page. This function converts a two-
#' column dataframe to one with 4, 6, or more columns so it can be printed more compactly.
#' @param results A dataframe with a specific (even) number of columns.
#' @param cols The number of columns in the reshaped dataframe. It must be an even number (default = 4).
#' @return A new dataframe containing the reshaped dataframe.
#' @importFrom stats reshape
#' @export

reshape_points_table <- function(results, cols = 4) {
  # "Pretty print" the data frame in a multi-column table
  # Credit to Onyambu via Stack Overflow
  # https://stackoverflow.com/users/8380272/onyambu
  n <- nrow(results)
  m <- ceiling(n/cols)
  time <- rep(1:cols, each = m, len = n)
  id <- rep(1:m, times = cols, len = n)
  reshape(cbind(id, time, results), idvar = 'id', direction='wide')[-1]
}
