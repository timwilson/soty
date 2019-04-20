#' Create regression model
#'
#' Fit a linear or exponential model to the trimmed CDF of archery scores.
#'
#' @param results A dataframe containing archery scores.
#' @param perfect_score The maximum number of points possible for the specific archery round
#' @param score_col The name of the column in the data frame that contains the scores
#' @param mode The type of function ("exp" or "linear") to fit to the results data.
#' @return An R regression model
#' @importFrom stats lm
#' @importFrom stats nls
#' @export

create_model <- function(results, perfect_score, score_col, mode="exp") {
  prob_table <- generate_prob_table(results, score_col)
  max_score <- max(prob_table$score)
  min_score <- min(prob_table$score)

  if (mode == "linear") {   # Check whether to run an exp or linear fit
    model <- lm(data=prob_table, formula = prob ~ score)
  } else if (mode == "exp") {
    model <- nls(data=prob_table, formula = prob ~ 100*exp(a*(score-max_score)), start=list(a=0.01))
  } else {
    stop("Must set mode to 'exp' or 'linear'")
  }

  # Add min and max score attributes to the regression model
  model$min_score <- min_score
  model$max_score <- max_score

  return (model)
}
