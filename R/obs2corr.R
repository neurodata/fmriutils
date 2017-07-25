#' Observations to Correlation
#' a utility to convert timeseries to correlation matrices.
#' @param observations: [[subs]][timesteps, rois] a list of observations for a particular subject.
#' @param rtype='list': the type of output to return. Options are 'list' and 'array'.
#' @return corr_data: [[subs]][rois, rois] the locally correlated roi timeseries.
#' @author Eric Bridgeford
#' @export
fmriu.time.obs2corr <- function(observations, rtype='list') {
  corr_data <- sapply(observations,  function(x) abs(cor(x)),
                      simplify=FALSE, USE.NAMES=TRUE)
  if (rtype != 'list') {
    corr_data <- fmriu.list2array(corr_data)
  }
  return(corr_data)
}
