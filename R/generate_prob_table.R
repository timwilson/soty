#' Generate a table of CDF probabilities
#'
#' Trim the results and generate a CDF for the score data. Create a new data frame from
#' the CDF probabilities containing each score and its associated probability. This new data frame
#' can be used to fit a linear or exponential model
#'
#' @param results A data frame containing archery scores
#' @param score_col The data fram column containing the scores
#' @return A table of scores and their probabilities according to the
#' @importFrom stats ecdf
#' @importFrom dplyr tibble
#' @export
#'
generate_prob_table <- function(results, score_col) {
  trimmed <- trim_results(results, score_col)
  trimmed_ecdf <- ecdf(trimmed$Score)  # Need to generaliz to accept any column name
  prob_table <- tibble(score=trimmed$Score, prob=trimmed_ecdf(trimmed$Score)*100)

  return (prob_table)
}
