#' Frequency to Divergence
#'
#' a function that converts lists of frequency domain signals to divergence matrices.
#'
#' @param observations [[n]][t, nroi] a list of n timeseries with t timesteps and nroi rois.
#' @param spectrum='amp': the frequency spectrum to use for computations.
#' @return div_mtx [[n]][nroi, nroi] a list of n divergence matrices where each edge is a divergence between two pairs of rois.
#' @keywords frequency, divergence
#' @export
fmriu.freq.freq2div <- function(observations) {
  sapply(freq, function(x) {
    nroi <- dim(x)[2]
    div_mtx <- array(NaN, dim=c(nroi, nroi))
    for (roi1 in 1:nroi) {
      for (roi2 in 1:nroi) {
        div_mtx[roi1, roi2] <- fmriu.kl_div(x[,roi1, drop=FALSE], x[,roi2, drop=FALSE])
      }
    }
    return(div_mtx)
  }, USE.NAMES=TRUE, simplify=FALSE)
}

#' observations to Divergence
#'
#' a function that converts lists of timeseries signals to divergence matrices, using the fourier domain.
#' This function serves as a wrapper for the obs2freq and freq2div functions.
#'
#' @param observations: [[n]][nt, nroi] a list of signals for n subjects, each containing nt timesteps and nroi rois.
#' @param tr: [1] the repetition time (in sec) of the dataset. corresponds to the time to take a single timestep.
#' @param lc: [1] the lower cutoff (in Hz) below which the fourier domain components will be set to zero.
#' @param hc: [1] the higher cutoff (in Hz) above which the fourier domain components will be set to zero.
#' @param spectrum='amp': the frequency spectrum to use for computations. Can be 'amp' or 'pow'.
#' @return div_mtx [[n]][nroi, nroi] a list of n divergence matrices where each edge is a divergence between two pairs of rois.
#' @keywords frequency, divergence
#' @export
fmriu.freq.obs2div <- function(observations, tr=NaN, lc=0.01, hc=NaN, spectrum='amp', rtype='list') {
  if (! (rtype %in% c('list', 'array'))) {
    stop('You have passed an invalid return type. Options are: [\'list\', \'array\'].')
  }
  freq <- fmriu.freq.obs2freq(observations, tr=tr, lc=lc, hc=hc, normalize=TRUE, spectrum=spectrum)
  div_mtx <- fmriu.freq.freq2div(freq)
  if (rtype == 'array') {
    div_mtx <- fmriu.list2array(div_mtx)
  }
  return(div_mtx)
}
