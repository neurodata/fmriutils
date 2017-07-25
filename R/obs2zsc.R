#' signal to Z-scores
#'
#' Scales observations by their z-scores for a given signal.
#'
#' @param signal: [[n]][nt, nroi] computes the z-scores of each of the nt observations with respect to the scaling of each roi.
#' @param rtype='list': the type of output to return. Options are 'list' and 'array'.
#' @return zscore_data: [[n]][nt, nroi] the z-scored timeseries for each roi.
#' @author Eric Bridgeford
#' @export
fmriu.obs2zsc <- function(signal, rtype='list') {
  zscore_data <- sapply(names(signal),  function(x) scale(signal[[x]], center=TRUE, scale=TRUE),
                        simplify=FALSE, USE.NAMES=TRUE)
  if (rtype != 'list') {
    zscore_data <- fmriu.list2array(zscore_data)
  }
  return(zscore_data)
}
