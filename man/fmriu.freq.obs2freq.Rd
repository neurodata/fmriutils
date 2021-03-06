% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/obs2freq.R
\name{fmriu.freq.obs2freq}
\alias{fmriu.freq.obs2freq}
\title{Observations to Frequency Spectrum}
\usage{
fmriu.freq.obs2freq(observations, tr = NaN, lc = 0.01, hc = NaN,
  normalize = TRUE, spectrum = "amp")
}
\arguments{
\item{observations:}{[[n]][nt, nroi] a list of signals for n subjects, each containing nt timesteps and nroi rois.}

\item{tr:}{[1] the repetition time (in sec) of the dataset. corresponds to the time to take a single timestep.}

\item{lc:}{[1] the lower cutoff (in Hz) below which the fourier domain components will be set to zero.}

\item{hc:}{[1] the higher cutoff (in Hz) above which the fourier domain components will be set to zero.}

\item{spectrum='amp':}{the frequency option to use for computations. Can be 'amp' (amplitude) or 'pow' (power).}
}
\value{
amp_data: [[n]][nt/2, nroi] the frequency spectrum of the dataset.
}
\description{
A utility to convert timeseries data to frequency spectra.
}
\author{
Eric Bridgeford
}
