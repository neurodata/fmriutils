#' Graph Thresholding
#'
#' A utility function for easily applying various methods of thresholding to graphs.
#'
#' @param graphs: [[n]][nroi, nroi] graphs to threshold.
#' @param method=NaN: the method to use for thresholding the graphs.
#' \describe{
#'   \item{'cutoff'}{use a cutoff value. Sets all elements in the graph below this value to zero.}
#'   \item{'ptile'}{use a percentile. Calculates the cutoff for each list element given the appropriate percentile, and sets elements under this percentile to zero.}
#'    \item{NaN}{do not apply any thresholding to the graphs.}
#' }
#' @param t=0.5: the threshold to use for the thresholding method selected. Behavior depends on the method above chosen:
#' \describe{
#'   \item{method == 'cutoff'}{$t$ corresponds to the cutoff value. All values in the graphs less than $t$ will be set to zero.}
#'   \item{method == 'ptile'}{$t$ corresponds to the normalized  percentile between 0 and 1. The value of $t$ corresponds to the $100*t$ percentile.}
#'   \item{method == NaN}{$t$ does nothing.}
#' }
#' @param binarize=`FALSE`: whether to binarize the graphs after the thresholding has been applied.
#' \describe{
#'  \item{FALSE}{do not binarize the graphs.}
#'  \item{TRUE}{binarize the graphs by setting all nonzero values to 1, and leaving all zero values as 0.}
#' }
#' @return tgraphs: [[n]][nroi, nroi] the thresholded graphs.
#' @examples
#' graphs <- list()
#' graphs[[1]] <- matrix(runif(25), nrow=5, ncol=5)
#' graphs[[2]] <- matrix(runif(25), nrow=5, ncol=5)
#' graphs[[3]] <- matrix(runif(25), nrow=5, ncol=5)
#' # I want to binarize my graphs without applying any thresholding
#' # setting all edges nonzero to connected (1) and zero edges to disconnected (0).
#' fmriu.thresh_graphs(graphs, binarize=TRUE)
#'
#' # For each graph, I want to threshold the botton 70% of edges
#' # and then binarize such that the bottom 70% of edges are disconnected (0)
#' # and the top 30% of edges are connected (1).
#' fmriu.thresh_graphs(graphs, method='ptile', t=0.7, binarize=TRUE)
#'
#' # For each graph, remove all edges less than 0.5.
#' fmriu.thresh_graphs(graphs, method='cutoff', t=0.5)
#'
#' @author Eric Bridgeford
#' @export
fmriu.thresh_graphs <- function(graphs, method=NaN, t=0.5, binarize=FALSE) {
  if(method == 'cutoff') {
    graphs <- sapply(graphs, function(x) {
      x <- ifelse(x < t, 0, x)
    }, simplify=FALSE)
  } else if (method == 'ptile') {
    graphs <- sapply(graphs, function(x) {
      q <- quantile(x, t)
      x <- ifelse(x < q, 0, x)
    }, simplify=FALSE)
  }
  if (binarize) {
    graphs <- sapply(graphs, function(x) {
      x <- ifelse(x > 0, 1, 0)
    }, simplify=FALSE)
  }
  return(graphs)
}
