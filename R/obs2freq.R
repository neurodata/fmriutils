#' Observations to Frequency Spectrum
#'
#' A utility to convert timeseries data to frequency spectra.
#'
#' @author Eric Bridgeford
#' @param observations: [[n]][nt, nroi] a list of signals for n subjects, each containing nt timesteps and nroi rois.
#' @param tr: [1] the repetition time (in sec) of the dataset. corresponds to the time to take a single timestep.
#' @param lc: [1] the lower cutoff (in Hz) below which the fourier domain components will be set to zero.
#' @param hc: [1] the higher cutoff (in Hz) above which the fourier domain components will be set to zero.
#' @param spectrum='amp': the frequency option to use for computations. Can be 'amp' (amplitude) or 'pow' (power).
#' @return amp_data: [[n]][nt/2, nroi] the frequency spectrum of the dataset.
#' @export
fmriu.freq.obs2freq <- function(observations, tr=NaN, lc=0.01, hc=NaN, normalize=TRUE, spectrum='amp') {
  if (!(spectrum %in% c('amp', 'pow'))) {
    stop('You have passed an invalid frequency option. Options are: [\'amp\', \'pow\'].')
  }
  freq_data <- sapply(observations,  function(x) {
    # bandpass filter if necessary; the bandpass function will take care of checking whether desired
    x <- fmriu.freq.bandpass_fft(x, tr=tr, lc=lc, hc=hc)
    if (spectrum == 'amp') {  # amp spectrum is 2*|x|
      x <- 2*abs(x)
    } else if (spectrum == 'pow') {  # power spectrum is x.^2
      x <- x^2
    }
    # normalized
    if (normalize) {  # normalize so that all entries sum to one (ie, makes it a pdf)
      x <- x %*% diag(1/apply(X=x, MARGIN=2, FUN=sum))
    }
    return(x)
  }, simplify=FALSE, USE.NAMES=TRUE)
  return(freq_data)
}

#' Bandpass Fourier Transform
#'
#' A utility function for bandpass-fourier-transforming timeseries data.
#'
#' @author Eric Bridgeford
#' @param signal [nbin, nroi]: an input signal.
#' @param tr [1]: the repetition time of the data (in seconds).
#' @param lc [1]: the lower cutoff (in Hz) for the fourier transform. fourier components below this will be set to 0.
#' @param hc [1]: the high cutoff (in Hz) for the fourier transform. fourier components above this will be set to 0.
#' @return f [nbin/2, nroi]: the single-sided frequency domain of the signal per roi.
#' @export
fmriu.freq.bandpass_fft <- function(signal, tr=NaN, lc=NaN, hc=NaN) {
  # we are given a tr, and at least one of a lower or upper cutoff
  if (! is.nan(tr) && (!is.nan(lc) || !is.nan(hc))) {
    nt <- dim(signal)[1]
    signal <- apply(X=signal, MARGIN=c(2), FUN=fft)/nt
    fs <- 1/tr  # the sampling frequency
    freq <- fmriu.freq.freq_seq(fs, nt)  # compute frequency per bin
    # remove appropriate fourier components depending on lc and hc desired
    if (!is.nan(lc)) {
      signal[freq < lc] <- 0
    }
    if (!is.nan(hc)) {
      signal[freq > hc] <- 0
    }
    signal <- Re(abs(signal))
    signal <- signal[1:ceiling(nt/2),,drop=FALSE]
  }
  return(signal)
}

#' Frequency per Bin
#'
#' A utility to compute the frequency per bin of a frequency-domain signal.
#'
#' @param fs [1]: the sampling frequency of the signal.
#' @param nt [1]: the number of bins.
#' @return freq_bin [nt/2]: the frequency per bin of the frequency-domain signal.
#' @export
fmriu.freq.freq_seq <- function(fs, nt) {
  return(fs*seq(from=0, to=ceiling(nt/2)-1)/nt)
}
