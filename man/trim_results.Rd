% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim_results.R
\name{trim_results}
\alias{trim_results}
\title{Trim results to remove top and bottom 10\% of results.}
\usage{
trim_results(results, col = "Score")
}
\arguments{
\item{results}{A dataframe containing archery scores.}

\item{col}{The dataframe column corresponding to the scores. Default is "Score".}
}
\value{
A new dataframe containing the middle 80\% of the scores.
}
\description{
In order to fit the score data to an exponential or linear model of the CDF, the top and bottom
10\% of scores are removed to fit with the requirements that a score of at least the 10th percentile
is required to earn any Shooter of the Year points. By removing the top 10\% of results, we can guarantee
that a score at the 90th percentile will earn 100 Shooter of the Year points.
}
