#' Filters a data frame by round and equipment class and applies a score multiplier.
#'
#' All event scores are contained in a single data frame. This function filter the data frame by the
#' 'round' and 'class' columns and applies a multiplier to the score column to adjust for a one-day, two-day, or
#' three-day event.
#' @param results A dataframe containing scores from various archery events.
#' @param round The specific round to filter.
#' @param class The specific equipment class to filter.
#' @param multiplier A multiplier to apply to each score (default = 1).
#' @return A new dataframe containing the filtered results.
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @export
filter_results <- function(results, round, class, multiplier = 1) {
  df <- results %>%
    filter(round == round,
           class == class) %>%
    mutate(score = score * multiplier)

  return (df)
}
