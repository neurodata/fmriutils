#' Dataseries plot
#'
#' A function that plots a dataseries, such as a time series or frequency series.
#'
#' @param ds: [nt,nroi] the signal, containing an array of nt observations for nroi rois.
#' @param title="": the title for the dataseries plot.
#' @param xlabel="TRs": the x label for the dataseries plot.
#' @param ylabel="intensity": the y label for the dataseries plot.
#' @param legend="ROIs": the legend title for the dataseries plot.
#' @param fsize=10: the default font size for the plot text.
#' @return dsplot : a plot of the dataseries.
#' @author Eric Bridgeford
#' @export
fmriu.plot.plot_dataseries <- function(ds, xax=NaN, title="",xlabel="TRs", ylabel="intensity", legend="ROI", fsize=12) {
  dsm = melt(ds)
  colnames(dsm) <- c(xlabel, legend, ylabel)

  dsplot <- ggplot(data=dsm, aes_string(x=xlabel, y=ylabel, group=legend, color=legend)) +
    geom_line(alpha=.2) +
    scale_y_continuous() +
    ylab(ylabel) +
    xlab(xlabel) +
    ggtitle(title) +
    theme(text=element_text(size=fsize))
  if (!is.nan(xax)) {
    dsplot <- dsplot +
      scale_x_continuous(labels=xax)
  }
  return(dsplot)
}
