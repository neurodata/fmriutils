#' Open Graphs
#' A utility to open graph information from a dataset.
#' @param fnames: [n] a vector of filenames, with separation by underscores IE, dataset_subject_run_(other information).rds
#' @param dataset_id: [1] the dataset id in the filenames.
#' @param atlas_id: [1] the atlas id in the filenames.
#' @param scan_pos: [1] the 1-indexed position of the subject id in the filenames, assuming - or _ delimiting in the filename
#' @param run_pos: [1] the position of the run information in the filenames
#' @param exclude=FALSE : a parameter idicating whether to ignore timeseries that contain missing data.
#' @return gr: [[subs]][nt, nroi] the gr loaded from the specified file names. list of n subjects, each with nt timesteps and nroi rois
#' @return dataset: [n] a vector of the dataset ids for each subject.
#' @return subjects: [n] the subject ids
#' @return sessions: [n] the run ids
#' @export
open_graphs <- function(fnames, dataset_id="", atlas_id="", sub_pos=1, run_pos=2, exclude=FALSE) {
  print("opening graphs...")
  subjects <- vector("character", length(fnames))
  dataset <- rep(dataset_id, length(fnames))
  atlas <- rep(atlas_id, length(fnames))
  sessions <- vector("character", length(fnames))
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

    tgr[is.nan(tgr)] <- 0
    if (exclude == FALSE) {
      # if (!any(apply(tgr, MARGIN=1, function(x) sum(abs(x))) == 0)) {
      counter <- counter + 1
      gr[[basename]] <-tgr
      subjects[counter] <- name[sub_pos] # subject name must be a string, so do not convert to numeric
      sessions[counter] <- name[run_pos]
    } else {
      if (!any(apply(tgr, MARGIN=1, function(x) sum(abs(x))) == 0)) {
        counter <- counter + 1
        gr[[counter]] <-t(tgr)
        subjects[counter] <- name[sub_pos] # subject name must be a string, so do not convert to numeric
        sessions[counter] <- name[run_pos]
      }
    }
  }
  pack <- list(graphs=gr, dataset=dataset, atlas=atlas, subjects=subjects[1:counter], sessions=sessions[1:counter])# pack up the dataset, subject, and run ids witht the timeseries
  return(pack)
}
