#' Open Graphs
#' A utility to open graph information from a dataset.
#' @param fnames [n]: a vector of filenames, with separation by underscores IE, dataset_subject_run_(other information).rds
#' @param dataset [1]: the 1-indexed position of the dataset id in the filenames, assuming - or _ delimiting in the filename
#' @param scan_pos [1]: the 1-indexed position of the subject id in the filenames, assuming - or _ delimiting in the filename
#' @param run_pos [1]: the position of the run information in the filenames
#' @param exclude=FALSE : a parameter idicating whether to ignore timeseries that contain missing data.
#' @return gr [[subs]][nt, nroi]: the gr loaded from the specified file names. list of n subjects, each with nt timesteps and nroi rois
#' @return dataset [n]: a vector of the dataset ids for each subject.
#' @return subjects [n]: the subject ids
#' @return runs [n]: the run ids
#' @export
open_graphs <- function(fnames, dataset_pos=1, sub_pos=2, run_pos=3, exclude=FALSE) {
  print("opening graphs...")
  require(igraph)
  subjects <- vector("character", length(fnames))
  dataset <- vector("character", length(fnames))
  runs <- vector("character", length(fnames))
  numscans<-length(fnames)
  gr <- list()
  counter <- 0
  for (i in 1:length(fnames)) {
    print(i)
    tgr <- read.graph(fnames[i], format='graphml') # read the graph from the filename
    tgr <- get.adjacency(tgr, type="both", attr="weight", sparse=FALSE) # convert graph to adjacency matrix
    basename <- basename(fnames[i])# the base name of the file
    base_split <- strsplit(basename, "\\.|-|_") # parse out the subject, which will be after the study name
    name <- unlist(base_split)
    dataset[i] <- name[dataset_pos]

    tgr[is.nan(tgr)] <- 0
    if (exclude == FALSE) {
      # if (!any(apply(tgr, MARGIN=1, function(x) sum(abs(x))) == 0)) {
      counter <- counter + 1
      gr[[counter]] <-tgr
      subjects[counter] <- name[sub_pos] # subject name must be a string, so do not convert to numeric
      runs[counter] <- name[run_pos]
    } else {
      if (!any(apply(tgr, MARGIN=1, function(x) sum(abs(x))) == 0)) {
        counter <- counter + 1
        gr[[counter]] <-t(tgr)
        subjects[counter] <- name[sub_pos] # subject name must be a string, so do not convert to numeric
        runs[counter] <- name[run_pos]
      }
    }
  }
  pack <- list(gr=gr, dataset=dataset, subjects=subjects[1:counter], runs=runs[1:counter])# pack up the dataset, subject, and run ids witht the timeseries
  return(pack)
}
