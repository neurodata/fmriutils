#' signal to Z-scores
#'
#' Scales observations by their z-scores for a given signal.
#'
#' @param signal: [[n]][nt, nroi] or [[n]][nroi, nroi] computes the z-scores of our data.
#' @param scale='roi': whether to scale by 'roi' columns (ROIs; our features, this gives a z-score per ROI) or 'global' by the overall mean (this gives a global z-score).
#' @param rtype='list': the type of output to return. Options are 'list' and 'array'.
#' @return zscore_data: [[n]][nt, nroi] or [[n]][nroi, nroi] the z-scored data.
#' @author Eric Bridgeford
#' @export
fmriu.obs2zsc <- function(signal, scale='roi', rtype='list') {
  if (! (rtype %in% c('list', 'array'))) {
    stop('You have passed an invalid return type. Options are: [\'list\', \'array\'].')
  }
  if (scale == 'roi') {
    zscore_data <- sapply(signal,  function(x) scale(x, center=TRUE, scale=TRUE),
                          simplify=FALSE, USE.NAMES=TRUE)
  } else if (scale == 'global') {
    zscore_data <- sapply(signal, function(x) (x - mean(x))/sd(x), simplify=FALSE, USE.NAMES=TRUE)
  } else {
    stop('You have passed an invalid scaling option. Options are: [\'roi\', \'global\'].')
  }
  if (rtype == 'array') {
    zscore_data <- fmriu.list2array(zscore_data)
  }
  return(zscore_data)
}
