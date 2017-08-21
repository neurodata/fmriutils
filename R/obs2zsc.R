#' signal to Z-scores
#'
#' Scales observations by their z-scores for a given signal.
#'
#' @param signal: [[n]][nt, nroi] timeseries or [[n]][nroi, nroi] graphs to z-score.
#' @param scale='roi': how to handle scaling.
#' + 'roi' columns of each element of our input (ROIs; our features, this gives a z-score per ROI; suggested if inputs are timeseries).
#' + 'global' by the overall mean (this gives a global z-scorel; suggested for graphs).
#' @return zscore_data: [[n]][nt, nroi] timeseries or [[n]][nroi, nroi] graphs the z-scored data.
#' @author Eric Bridgeford
#' @export
fmriu.obs2zsc <- function(signal, scale='roi') {
  if (scale == 'roi') {
    zscore_data <- sapply(signal,  function(x) scale(x, center=TRUE, scale=TRUE),
                          simplify=FALSE, USE.NAMES=TRUE)
  } else if (scale == 'global') {
    zscore_data <- sapply(signal, function(x) (x - mean(x))/sd(x), simplify=FALSE, USE.NAMES=TRUE)
  } else {
    stop('You have passed an invalid scaling option. Options are: [\'roi\', \'global\'].')
  }
  return(zscore_data)
}
