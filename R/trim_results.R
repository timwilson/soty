#' Trim results to remove top and bottom 10\% of results.
#'
#' In order to fit the score data to an exponential or linear model of the CDF, the top and bottom
#' 10\% of scores are removed to fit with the requirements that a score of at least the 10th percentile
#' is required to earn any Shooter of the Year points. By removing the top 10\% of results, we can guarantee
#' that a score at the 90th percentile will earn 100 Shooter of the Year points.
#' @param results A dataframe containing archery scores.
#' @param col The dataframe column corresponding to the scores. Default is "Score".
#' @return A new dataframe containing the middle 80\% of the scores.
#' @importFrom stats quantile
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @export

trim_results <- function(results, col="Score") {
  #p10 <- quantile(results[[col]], c(0.1))
  #p90 <- quantile(results[[col]], c(0.9))
  p10 <- quantile(results$Score, c(0.1))
  p90 <- quantile(results$Score, c(0.9))

  trimmed_results <- results %>%
    filter(col >= p10, col <= p90)

  return (trimmed_results)
}
