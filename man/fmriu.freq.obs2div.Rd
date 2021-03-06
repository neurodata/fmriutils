% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/freq2div.R
\name{fmriu.freq.obs2div}
\alias{fmriu.freq.obs2div}
\title{observations to Divergence}
\usage{
fmriu.freq.obs2div(observations, tr = NaN, lc = 0.01, hc = NaN,
  spectrum = "amp")
}
\arguments{
\item{observations:}{[[n]][nt, nroi] a list of signals for n subjects, each containing nt timesteps and nroi rois.}

\item{tr:}{[1] the repetition time (in sec) of the dataset. corresponds to the time to take a single timestep.}

\item{lc:}{[1] the lower cutoff (in Hz) below which the fourier domain components will be set to zero.}

\item{hc:}{[1] the higher cutoff (in Hz) above which the fourier domain components will be set to zero.}

\item{spectrum='amp':}{the frequency spectrum to use for computations. Can be 'amp' or 'pow'.}
}
\value{
div_mtx [[n]][nroi, nroi] a list of n divergence matrices where each edge is a divergence between the frequency spectrum of two pairs of rois.
}
\description{
a function that converts lists of timeseries signals to divergence matrices, using the fourier domain.
This function serves as a wrapper for the obs2freq and freq2div functions.
}
\keyword{divergence}
\keyword{frequency,}
