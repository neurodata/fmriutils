#' Observations to Amplitude Spectrum
#'
#' A utility to convert timeseries data to amplitude spectra.
#'
#' @author Eric Bridgeford
#' @param observations [[n]][nt, nroi]: a list of signals for n subjects, each containing nt timesteps and nroi rois.
#' @param tr [1]: the repetition time (in sec) of the dataset. corresponds to the time to take a single timestep.
#' @param lc [1]: the lower cutoff (in Hz) below which the fourier domain components will be set to zero.
#' @return amp_data [[n]][nt/2, nroi]: the amplitude spectrum of the dataset.
#' @export
obs2amp <- function(observations, tr=NaN, lc=0.01, normalize=TRUE) {
  amp_data <- sapply(observations,  function(x) {
    amp_sig <- highpass_fft(x, tr=tr, lc=lc)
    amp_sig <- 2*abs(amp_sig)
    # normalized
    if (normalize) {
      amp_sig <- amp_sig %*% diag(1/apply(X=amp_sig, MARGIN=2, FUN=sum))
    }
    return(amp_sig)
  }, simplify=FALSE, USE.NAMES=TRUE)

  return(amp_data)
}

#' Observations to Power Spectrum
#'
#' A utility to convert timeseries data to power spectra.
#'
#' @author Eric Bridgeford
#' @param observations [[n]][nt, nroi]: a list of signals for n subjects, each containing nt timesteps and nroi rois.
#' @param tr [1]: the repetition time (in sec) of the dataset. corresponds to the time to take a single timestep.
#' @param lc [1]: the lower cutoff (in Hz) below which the fourier domain components will be set to zero.
#' @return pow_data [[n]][nt/2, nroi]: the power spectrum of the dataset.
#' @export
obs2pow <- function(observations, tr=NaN, lc=0.01, normalize=TRUE) {
  pow_data <- sapply(observations,  function(x) {
    pow_sig <- highpass_fft(x, tr=tr, lc=lc)
    # one sided
    pow_sig <- pow_sig^2
    # normalized
    if (normalize) {
      pow_sig <- pow_sig %*% diag(1/apply(X=pow_sig, MARGIN=2, FUN=sum))
    }
    return(pow_sig)
  }, simplify=FALSE, USE.NAMES=TRUE)

  return(pow_data)
}

#' Highpass Fast Fourier Transform
#'
#' A utility function for computing the highpass-filtered, single-sided, real fourier transform.
#'
#' @author Eric Bridgeford
#' @param signal [nbin, nroi]: an input signal.
#' @param tr [1]: the repetition time of the data (in seconds).
#' @param lc [1]: the lower cutoff (in Hz) for the fourier transform. fourier components below this will be set to 0.
#' @return f [nbin/2, nroi]: the single-sided frequency domain of the signal per roi.
#' @export
highpass_fft <- function(signal, tr=NaN, lc=NaN) {
  nt <- dim(signal)[1]
  x <- apply(X=signal, MARGIN=c(2), FUN=fft)/nt
  if (!is.nan(tr) && !is.nan(lc)) {
    fs <- 1/tr  # the sampling frequency
    freq <- fs*seq(from=0, to=ceiling(nt/2)-1)/nt
    x[freq < lc] <- 0
  }
  x <- Re(abs(x))
  f <- x[1:ceiling(nt/2),,drop=FALSE]
  return(f)
}
