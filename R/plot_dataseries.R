#' Dataseries plot
#'
#' A function that plots a dataseries, such as a time series or frequency series.
#'
#' @import ggplot2
#' @import reshape2
#' @param ds: [nt,nroi] the signal, containing an array of nt observations for nroi rois.
#' @param title="": the title for the dataseries plot.
#' @param xlabel="TRs": the x label for the dataseries plot.
#' @param ylabel="intensity": the y label for the dataseries plot.
#' @param legend.title="ROIs": the legend title for the dataseries plot.
#' @param legend.show=FALSE: whether to show the legend for the dataseries plot.
#' @param font.size=10: the default font size for the plot text.
#' @param line.alpha=.2: the alpha for the lines.
#' @param line.size=1: the width of the line.
#' @return dsplot : a plot of the dataseries.
#' @author Eric Bridgeford
#' @export
fmriu.plot.plot_dataseries <- function(ds, xax=NaN, title="",xlabel="TRs", ylabel="intensity", legend.title="ROI",
                                       legend.show=FALSE, font.size=12, line.alpha=.2, line.size=1) {

  if (all(is.nan(xax))) {
    dsm <- melt(ds)
  } else {
    dsm <- melt(data.frame(ds, x=xax), "x")
  }
  colnames(dsm) <- c("x", "value", "y")
  dsm$value <- factor(dsm$value)  # make the ROIs a factor
  dsplot <- ggplot(data=dsm, aes_string(x="x", y="y", group="value", color="value")) +
    geom_line(alpha=line.alpha, size=line.size) +
    scale_y_continuous() +
    ylab(ylabel) +
    xlab(xlabel) +
    ggtitle(title)
  dsplot$labels$colour <- legend.title
  if (legend.show) {
    dsplot <- dsplot +
      theme(text=element_text(size=font.size))
  } else {
    dsplot <- dsplot +
      theme(text=element_text(size=font.size), legend.position="none")
  }

  return(dsplot)
}
