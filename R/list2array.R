#' Reorganize a list of graphs to an array of graphs
#'
#' \code{fmriu.list2array} uses abind to reshape a list of same-dimension graphs to
#' an array. Takes from list[[listels]] to array[,,length(listels)].
#'
#' @import abind
#' @param list_in a list with p elements of dimensions [n x m].
#' @param flatten=FALSE whether to flatten the elements. The output will be of dimensions:
#' \describe{
#'    \item{TRUE}{output array will be [p x (n*m)]}
#'    \item{FALSE}{output array will be [p x n x m]}
#' }
#' @return array_out an array of dimensions depending on the flatten parameter.
#' @examples
#' test <- list()
#' test[[1]] <- matrix(1:35, nrow=5, ncol=7)
#' test[[2]] <- matrix(36:70, nrow=5, ncol=7)
#' test[[3]] <- matrix(71:105, nrow=5, ncol=7)
#' fmriu.list2array(test)  # has dimensions [3, 5, 7]
#' @export
#' @seealso \code{\link{abind}} \code{\link{array2list}} \code{\link{aperm}}
#'
fmriu.list2array <- function(list_in, flatten=FALSE) {
  array_out <- do.call(abind, c(list_in, list(along=3)))
  array_out <- aperm(array_out, perm = c(3, 1, 2))
  if (flatten) {
    dimar <- dim(array_out)
    dim(array_out) <- c(dimar[1], dimar[2]*dimar[3])
  }
  return(array_out)
}
