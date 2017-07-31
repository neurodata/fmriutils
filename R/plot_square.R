#' Square plot
#'
#' A function that plots a square matrix. Good for things like Correlation matrices.
#'
#' @param in [nroi, nroi] or [nt, nroi]: the input. Can either be a square matrix of featuresxfeatures, or an array of observationsxfeatures (see itype parameter).
#' @param title="": the title for the square plot.
#' @param xlabel="ROIs": the x label for the square plot.
#' @param ylabel="ROIs": the y label for the square plot.
#' @param legend="": the legend title for the square plot.
#' @param itype="sq": the shape of the input. If "sq", the plot will be generated as is for the input signal. If "ts", we will assume the input is observationsxfeatures, and will correlate the features first.
#' @param fsize=12: the default font size for the plot text.
#' @param rem_diag=FALSE: whether to remove the diagonal from the plot.
#' @param limits=c(0, 1): the limits for the fill color in the heatmap of our matrix.
#' @return sqplot : a plot of the square.
#' @author Eric Bridgeford
#' @export
fmriu.plot.plot_square <- function(mtx, title="",xlabel="ROI", ylabel="ROI", legend="metric", itype="sq", fsize=12, rem_diag=FALSE, limits=c(0, 1)) {
  if (itype == "ts") {
    mtx <- abs(cor(mtx))  # if a timeseries is passed in, correlate the features first
  }
  if (rem_diag) {
    diag(mtx) <- 0
  }
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  sqplot <- ggplot(melt(mtx), aes(x=Var1, y=Var2, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(colours=jet.colors(7), name=legend, limits=limits) +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title) +
    theme(text=element_text(size=fsize))
  return(sqplot)
}
