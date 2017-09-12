#' Open Timeseries
#'
#' A utility to open timeseries information from a dataset. Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[###]_task-[abcde]_ses-[###]_run-[###]_(generic info).file
#'
#' @import stringr
#' @param fnames: [n] a vector of filenames, with separation by underscores IE, dataset_subject_run_(other information).rds. Alternatively, a path to a directory containing rds files of an appropriate naming convention. See `fmriutils` vignette for details.
#' @param dataset_id: [1] the dataset id in the filenames.
#' @param atlas_id: [1] the atlas id in the filenames.
#' @param verbose=FALSE: whether to print the id of the scan being loaded.
#' @param rtype='list': the type of output to return. Options are 'list' and 'array'.
#' @return ts: [[n]][nt, nroi] the ts loaded from the specified file names. list of n subjects, each with nt timesteps and nroi rois
#' @return dataset: [n] a vector of the dataset ids for each subject.
#' @return subjects: [n] the subject ids
#' @return sessions: [n] the session ids
#' @return tasks: [n] the task ids
#' @return runs: [n] the run ids
#' @export
fmriu.io.open_timeseries <- function(fnames, dataset_id="", atlas_id="", verbose=FALSE, rtype='list') {
  if (is.character(fnames)) {
    fnames <- list.files(fnames, pattern=paste('\\.rds', sep=""), full.names=TRUE)
  }
  if (! (rtype %in% c('list', 'array'))) {
    stop('You have passed an invalid return type. Options are: [\'list\', \'array\'].')
  }
  if (length(fnames) == 0) {
    stop(sprintf('You do not have any files.', fnames))
  }

  print(sprintf("opening timeseries for %s dataset and %s parcellation atlas...", dataset_id, atlas_id))
  subjects <- vector("character", length(fnames))
  dataset <- rep(dataset_id, length(fnames))
  atlas <- rep(atlas_id, length(fnames))
  sessions <- vector("character", length(fnames))
  tasks <- vector("character", length(fnames))
  runs <- vector("character", length(fnames))
  ts <- list()
  for (i in 1:length(fnames)) {
    basename <- basename(fnames[i])     # the base name of the file
    if (verbose) {
      print(paste('Loading', basename, '...'))
    }
    tts <- readRDS(fnames[i]) # read the timeseries from the filename
    tts[is.nan(tts)] <- 0  # missing entries substituted with 0s
    ts[[basename]] <-t(tts)
    subjects[i] <- str_extract(basename, '(?<=sub-)(.*?)(?=[._])')
    sessions[i] <- str_extract(basename, '(?<=ses-)(.*?)(?=[._])')
    tasks[i] <- str_extract(basename, '(?<=task-)(.*?)(?=[._])')
    runs[i] <- str_extract(basename, '(?<=run-)(.*?)(?=[._])')
  }
  if (rtype == 'array') {
    ts = fmriu.list2array(ts)
  }
  return(list(ts=ts, dataset=dataset, atlas=atlas, subjects=subjects,
              sessions=sessions, tasks=tasks, runs=runs))
}

#' Open Timeseries Collection
#'
#' A utility to open timeseries information from multiple datasets. Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[###]_task-[abcde]_ses-[###]_run-[###]_(generic info).file
#'
#' @param basepath: [1] a path to a properly spec'd directory. See the example in `fmriutils.html` for details.
#' @param datasets: [d] the datasets as a vector. Note that we expect a folder `<basepath>/<dataset>` for `<dataset> in <datasets>`.
#' @param atlases: [a] the atlases as a vector. Note that we expect a folder `<basepath>/<dataset>/timeseries/roi/<atlas>` for `<atlas> in <atlases>`.
#' @param verbose=FALSE: whether to print the id of the scan being loaded.
#' @param rtype='list': the type of output to return. Options are 'list' and 'array'.
#' @return ts: [[n]][nt, nroi] the ts loaded from the specified directory. list of n subjects, each with nt timesteps and nroi rois
#' @return dataset: [n] a vector of the dataset ids for each subject.
#' @return atlas: [n] a vector of the atlas ids for each subject.
#' @return subjects: [n] the subject ids
#' @return sessions: [n] the session ids
#' @return tasks: [n] the task ids
#' @return runs: [n] the run ids
#' @export
fmriu.io.collection.open_timeseries <- function(basepath, datasets="", atlases="", verbose=FALSE, rtype='list') {
  if (! (rtype %in% c('list', 'array'))) {
    stop('You have passed an invalid return type. Options are: [\'list\', \'array\'].')
  }

  ts <-list()
  subjects <- c()
  sessions <- c()
  tasks <- c()
  runs <- c()
  dataset <- c()
  atlas <- c()

  for (ds in datasets) {
    for (at in atlases) {
      path <- file.path(basepath, ds, 'timeseries', 'roi', at, fsep = "/")
      signalobj <- fmriu.io.open_timeseries(path, dataset_id = ds, atlas_id = at,
                                            verbose = verbose, rtype = 'list')

      ts <- append(ts, signalobj$ts)
      subjects <- c(subjects, signalobj$subjects)
      sessions <- c(sessions, signalobj$sessions)
      tasks <- c(tasks, signalobj$tasks)
      runs <- c(runs, signalobj$runs)
      dataset <- c(dataset, signalobj$dataset)
      atlas <- c(atlas, signalobj$atlas)
    }
  }
  if (rtype == 'array') {
    ts = fmriu.list2array(ts)
  }

  return(list(ts=ts, dataset=dataset, atlas=atlas, subjects=subjects,
              sessions=sessions, tasks=tasks, runs=runs))
}
