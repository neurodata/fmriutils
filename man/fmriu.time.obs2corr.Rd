% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obs2corr.R
\name{fmriu.time.obs2corr}
\alias{fmriu.time.obs2corr}
\title{Observations to Correlation}
\usage{
fmriu.time.obs2corr(observations, include_diag = FALSE)
}
\arguments{
\item{observations:}{[[subs]][timesteps, rois] a list of observations for a particular subject.}

\item{include_diag=FALSE:}{whether to include the diagonal in the result.}
}
\value{
corr_data: [[subs]][rois, rois] the locally correlated roi timeseries.
}
\description{
a utility to convert timeseries to correlation matrices.
}
\author{
Eric Bridgeford
}
