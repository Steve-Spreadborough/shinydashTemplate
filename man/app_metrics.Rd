% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server_utils.R
\name{app_metrics}
\alias{app_metrics}
\title{Dashboard metrics}
\usage{
app_metrics(metric_ids, r6_data, ...)
}
\arguments{
\item{metric_ids}{ID of the metric to be used}

\item{r6_data}{R6 object that contains the app data (i.e. app_data/dash_data)}

\item{...}{other fields to group data by}
}
\description{
Function gets metric data. Either calculated within function, or calls
another function where more complex.
}
