#' Open Timeseries
#'
#' A utility to open timeseries information from a dataset. Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[#####]_task-[abcde]_ses-[#####]_(generic info).file
#'
#' @param fnames: [n] a vector of filenames, with separation by underscores IE, dataset_subject_run_(other information).rds
#' @param dataset_id: [1] the dataset id in the filenames.
#' @param atlas_id: [1] the atlas id in the filenames.
#' @param rtype='list': the type of output to return. Options are 'list' and 'array'.
#' @return ts: [[subs]][nt, nroi] the ts loaded from the specified file names. list of n subjects, each with nt timesteps and nroi rois
#' @return dataset: [n] a vector of the dataset ids for each subject.
#' @return subjects: [n] the subject ids
#' @return sessions: [n] the run ids
#' @export
fmriu.io.open_timeseries <- function(fnames, dataset_id="", atlas_id="", verbose=FALSE, rtype='list') {
  print("opening timeseries...")
  subjects <- vector("character", length(fnames))
  dataset <- rep(dataset_id, length(fnames))
  atlas <- rep(atlas_id, length(fnames))
  sessions <- vector("character", length(fnames))
  tasks <- vector("character", length(fnames))
  ts <- list()
  for (i in 1:length(fnames)) {
    basename <- basename(fnames[i])     # the base name of the file
    if (verbose) {
      print(paste('Loading', basename, '...'))
    }
    tts <- readRDS(fnames[i]) # read the timeseries from the filename
    tts[is.nan(tts)] <- 0  # missing entries substituted with 0s
    ts[[basename]] <-t(tts)
    subjects[i] <- str_extract(basename, 'sub(.?)+?(?=_)')
    sessions[i] <- str_extract(basename, 'ses(.?)+?(?=_)')
    tasks[i] <- str_extract(basename, 'task(.?)+?(?=_)')
  }
  if (rtype != 'list') {
    ts = fmriu.list2array(ts)
  }
  return(list(ts=ts, dataset=dataset, atlas=atlas, subjects=subjects,
              sessions=sessions, tasks=tasks))
}
