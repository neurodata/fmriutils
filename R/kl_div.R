#' Kullback Leibler Divergence
#'
#' A function that computes the kullback leibler divergence for two distributions.
#'
#' @param a the first distribution
#' @param b the second distribution
#' @keywords kullback, leibler, divergence
#' @author Eric Bridgeford
#' @export
kl_div <- function(a, b) {
  disc_div <- a*log(a/b)  # KL divergence at each point
  t <- dim(disc_div)[1]
  disc_div[a == 0 & b == 0] <- 0  # replace divide by zeros, or log(0)s, with 0
  disc_div[(b == 0 & a != 0) | (a == 0 & b != 0)] <- 1/t  # if we get inf value, set as the maximum divergence for each element
  return(abs(sum(disc_div)))  # KL divergence is the sum of the divergence at each point
}
