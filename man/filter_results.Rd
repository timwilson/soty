% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_results.R
\name{filter_results}
\alias{filter_results}
\title{Filters a data frame by round and equipment class and applies a score multiplier.}
\usage{
filter_results(results, archery_round, equipment_class, multiplier = 1)
}
\arguments{
\item{results}{A dataframe containing scores from various archery events.}

\item{archery_round}{The specific round to filter.}

\item{equipment_class}{The specific equipment class to filter.}

\item{multiplier}{A multiplier to apply to each score (default = 1).}
}
\value{
A new dataframe containing the filtered results.
}
\description{
All event scores are contained in a single data frame. This function filter the data frame by the
'round' and 'class' columns and applies a multiplier to the score column to adjust for a one-day, two-day, or
three-day event.
}
