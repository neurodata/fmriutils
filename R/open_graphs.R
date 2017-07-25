#' Open Graphs
#'
#' A utility to open graph information from a dataset.Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[#####]_task-[abcde]_ses-[#####]_(generic info).file
#'
#' @param fnames: [n] a vector of filenames, with separation by underscores IE, dataset_subject_run_(other information).rds
#' @param dataset_id="": [1] the dataset id in the filenames.
#' @param atlas_id="": [1] the atlas id in the filenames.
#' @param fmt="graphml" : a parameter idicating the format for graphs to be read in as.
#' @param verbose=FALSE : whether to print the iteration being loaded.
#' @return gr: [[subs]][nt, nroi] the gr loaded from the specified file names. list of n subjects, each with nt timesteps and nroi rois
#' @return dataset: [n] a vector of the dataset ids for each subject.
#' @return subjects: [n] the subject ids
#' @return sessions: [n] the run ids
#' @export
open_graphs <- function(fnames, dataset_id="", atlas_id="", fmt='graphml', verbose=FALSE) {
  print("opening graphs...")
  subjects <- vector("character", length(fnames))
  dataset <- rep(dataset_id, length(fnames))
  atlas <- rep(atlas_id, length(fnames))
  sessions <- vector("character", length(fnames))
  tasks <- vector("character", length(fnames))
  gr <- list()

  for (i in 1:length(fnames)) {
    basename <- basename(fnames[i])     # the base name of the file
    if (verbose) {
      print(paste('Loading', basename, '...'))
    }
    tgr <- read.graph(fnames[i], format=fmt) # read the graph from the filename
    tgr <- get.adjacency(tgr, type="both", attr="weight", sparse=FALSE) # convert graph to adjacency matrix
    tgr[is.nan(tgr)] <- 0  # missing entries substituted with 0s
    gr[[basename]] <-t(tgr)
    subjects[i] <- str_extract(basename, 'sub(.?)+?(?=_)')
    sessions[i] <- str_extract(basename, 'ses(.?)+?(?=_)')
    tasks[i] <- str_extract(basename, 'task(.?)+?(?=_)')
  }
  return(list(graphs=gr, dataset=dataset, atlas=atlas, subjects=subjects,
              sessions=sessions, tasks=tasks))
}
