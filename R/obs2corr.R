#' Observations to Correlation
#' a utility to convert timeseries to correlation matrices.
#' @param observations [[subs]][timesteps, rois]: a list of observations for a particular subject.
#' @return corr_data [[subs]][rois, rois]: the locally correlated roi timeseries.
#' @author Eric Bridgeford
#' @export
obs2corr <- function(observations) {

  corr_data <- sapply(observations,  function(x) abs(cor(x)),
                      simplify=FALSE, USE.NAMES=TRUE)

  return(corr_data)
}
