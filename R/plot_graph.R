#' Graph plot
#'
#' A function that plots a graph.
#'
#' @import ggplot2
#' @import reshape2
#' @param mtx [nroi, nroi] or [nt, nroi]: the input. Can either be a square matrix of featuresxfeatures, or a timeseries of observationsxfeatures (see itype parameter).
#' @param title="": the title for the square plot.
#' @param xlabel="ROIs": the x label for the square plot.
#' @param ylabel="ROIs": the y label for the square plot.
#' @param legend.name="": the legend title for the square plot.
#' @param legend.show=TRUE: whether to show the legend on the plot.
#' @param itype="sq": the shape of the input. If "sq", the plot will be generated as is for the input signal. If "ts", we will assume the input is observationsxfeatures, and will correlate the features first.
#' @param font.size=12: the default font size for the plot text.
#' @param include_diag=FALSE: whether to include the diagonal from the plot.
#' @param limits=c(0,1): the limits for the fill color in the heatmap of our matrix.
#' @return sqplot : a plot of the graph.
#' @author Eric Bridgeford
#' @export
fmriu.plot.plot_graph <- function(mtx, title="",xlabel="ROI", ylabel="ROI", legend.name="metric", legend.show=TRUE, itype="sq",
                                  font.size=12, rinclude_diag=FALSE, limits=c(0, 1)) {
  if (itype == "ts") {
    mtx <- abs(cor(mtx))  # if a timeseries is passed in, correlate the features first
  }
  if (!include_diag) {
    diag(mtx) <- 0
  }
  dm <- melt(mtx)
  colnames(dm) <- c("x", "y", "value")
  jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  sqplot <- ggplot(dm, aes(x=x, y=y, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(colours=jet.colors(7), name=legend.name, limits=limits) +
    xlab(xlabel) +
    ylab(ylabel) +
    ggtitle(title)
  if (legend.show) {
    sqplot <- sqplot +
      theme(text=element_text(size=font.size))
  } else {
    sqplot <- sqplot +
      theme(text=element_text(size=font.size, legend.position="none"))
  }
  return(sqplot)
}
