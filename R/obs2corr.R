#' Observations to Correlation
#'
#' a utility to convert timeseries to correlation matrices.
#'
#' @param observations: [[subs]][timesteps, rois] a list of observations for a particular subject.
#' @param include_diag=FALSE: whether to include the diagonal in the result.
#' @return corr_data: [[subs]][rois, rois] the locally correlated roi timeseries.
#' @author Eric Bridgeford
#' @export
fmriu.time.obs2corr <- function(observations, include_diag=FALSE) {
  corr_data <- sapply(observations,  function(x) {
      c <- abs(cor(x))
      diag(c) <- include_diag*1
      return(c)
    }, simplify=FALSE, USE.NAMES=TRUE)
  return(corr_data)
}
