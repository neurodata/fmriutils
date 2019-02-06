#' Open Graphs
#'
#' A utility to open graph information from a dataset.Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[#####]_task-[abcde]_ses-[#####]_(generic info).file
#'
#' @import stringr
#' @import igraph
#' @param fnames either:
#' \describe{
#'     \item{\code{vector} or 1-d \code{array}}{a vector of filenames in the BIDs specification, with separation by underscores IE, dataset_subject_run_(other information).ext.}
#'     \item{\code{character}}{a path to a directory containing a collection of appropriately named files with extension as specified below}
#' }
#' @param dataset_id the dataset id in the filenames. Defaults to \code{""}.
#' @param atlas_id the atlas id in the filenames. Defaults to \code{""}.
#' @param fmt a parameter indicating the format for graphs to be read in as. Options are \code{'adj'} or \code{'elist'}. Defaults to \code{'elist'}:
#' \describe{
#'     \item{\code{'adj'}}{Graphs are stored as an adjacency matrix, with a single header line indicating the parcel identifier (ROI), and successive lines the adjacency matrix as a csv. Note that if all graphs are not of the same dimensionality and the \code{'array'} return-type argument is specified, an error will be thrown.}
#'     \item{\code{'elist'}}{Graphs are stored in comma-separated value format. Note that isolated ROIs within individual subjects are included.}
#' }
#' @param verbose whether to print the iteration being loaded. Defaults to \code{TRUE}.
#' @param rtyp the type of output to return. Options are \code{'list'} and \code{'array'}. Defaults to \code{'list'}.
#' @param flatten a parameter to flatten the array if the rtype is set to \code{'array'}:
#' \describe{
#'   \item{TRUE}{returns an \code{[n x r^2]} array}
#'   \item{FALSE}{returns an \code{[n x r x r]} array}
#' }
#' @param rem.diag whether to remove diagonal (self-connetions) from the graph. Defaults to \code{TRUE}.
#' @return a list containing the following:
#' \item{\code{graphs} the graphs of the subjects. Size depends on the parameters specified.}
#' \item{dataset} a \code{[n]} vector of the dataset ids for each subject.}
#' \item{\code{subjects} a code{[n]} vector of the subject ids}
#' \item{\code{runs} a code{[n]} vector of the run ids}
#' \item{\code{tasks} a code{[n]} vector of the task ids}
#' \item{\code{runs} a code{[n]} vector of the run ids}
#' @export
fmriu.io.open_graphs <- function(fnames, dataset_id="", atlas_id="",
                                 fmt='elist', verbose=FALSE, rtype='list', flatten=FALSE,
                                 rem.diag=TRUE) {
  if (! (fmt %in% c('adj', 'elist'))) {
    stop('You have passed an invalid format type. Options are: [\'adj\', \'elist\', and \'graphml\'].')
  }

  if (fmt == 'elist') {
    fmt = 'ncol'; ext = "csv"
  } else if (fmt == "graphml") {
    fmt = "graphml"; ext = "graphml"
  } else if (fmt == "adj") {
    fmt = "adj"; ext="adj"
  }

  if (is.character(fnames)) {
    fnames <- list.files(fnames, pattern=paste('\\.', ext, sep=""), full.names=TRUE)
  }

  if (! (rtype %in% c('list', 'array'))) {
    stop('You have passed an invalid return type. Options are: [\'list\', \'array\'].')
  }

  print(sprintf("opening graphs for %s dataset and %s parcellation atlas...", dataset_id, atlas_id))
  subjects <- vector("character", length(fnames))
  sessions <- vector("character", length(fnames))
  tasks <- vector("character", length(fnames))
  gr <- list()

  vertices <- c()

  # so that we don't get any annoying errors if particular vertices are empty
  if (fmt != "adj") {
    for (i in 1:length(fnames)) {
      tgr <- igraph::read_graph(fnames[i], format=fmt) # read the graph from the filename
      vertices <- union(vertices, V(tgr))
    }
  } else {

  }

  vertices <- order(vertices)
  counter <- 1
  for (i in 1:length(fnames)) {
    basename <- basename(fnames[i])     # the base name of the file
    if (verbose) {
      print(paste('Loading', basename, '...'))
    }
    tgr <- tryCatch({
      igraph::read_graph(fnames[i], format=fmt, predef=vertices) # read the graph from the filename, ordering by the vertices we found previously
    }, error = function(e) {
      return(NaN)
    })

    if (is.igraph(tgr)) {
      tgr <- get.adjacency(tgr, type="both", attr="weight", sparse=FALSE) # convert graph to adjacency matrix
      tgr[is.nan(tgr)] <- 0  # missing entries substituted with 0s
      if (rem.diag) {
        diag(tgr) <- 0
      }
      gr[[basename]] <-t(tgr)
      subjects[counter] <- str_extract(basename, 'sub(.?)+?(?=_)')
      sessions[counter] <- str_extract(basename, 'ses(.?)+?(?=_)')
      tasks[counter] <- str_extract(basename, 'task(.?)+?(?=_)')
      counter <- counter + 1
    }
  }

  dataset <- rep(dataset_id, counter - 1)
  atlas <- rep(atlas_id, counter - 1)
  subjects <- subjects[1:counter - 1]
  sessions <- sessions[1:counter - 1]
  tasks <- tasks[1:counter - 1]

  if (rtype == 'array') {
    aro <- fmriu.list2array(gr, flatten=flatten)
    gr <- aro$array
    dataset <- dataset[aro$incl_ar]
    atlas <- atlas[aro$incl_ar]
    subjects <- subjects[aro$incl_ar]
    sessions <- sessions[aro$incl_ar]
    tasks <- tasks[aro$incl_ar]
  }
  return(list(graphs=gr, dataset=dataset, atlas=atlas, subjects=subjects,
              sessions=sessions, tasks=tasks))
}

#' Open Graphs Collection
#'
#' A utility to open graphs information from multiple datasets. Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[###]_task-[abcde]_ses-[###]_run-[###]_(generic info).file
#'
#' @param basepath a path to a properly spec'd directory. See the example in `fmriutils.html` for details.
#' @param datasets the datasets as a vector. Note that we expect a folder `<basepath>/<dataset>` for `dataset in datasets`.
#' @param modality the modality identifier. Note that we expect a folder `<basepath>/<dataset>/<modality>` for `dataset` in `datasets`.
#' @param atlases the atlases as a vector. Note that we expect a folder `<basepath>/<dataset>/<modality>/<gname>/<atlas>` for `atlas in atlases`, `dataset` in `datasets`.
#' @param gname the folder name in which graphs are stored. Defaults to \code{'roi-connectomes'}.
#' @param fmt a parameter indicating the format for graphs to be read in as. Options are \code{'adj'} or \code{'elist'}. Defaults to \code{'elist'}.
#' @param verbose=FALSE: whether to print the id of the scan being loaded.
#' @param rtyp the type of output to return. Options are \code{'list'} and \code{'array'}. Defaults to \code{'list'}.
#' @param flatten a parameter to flatten the array if the rtype is set to \code{'array'}:
#' \describe{
#'   \item{TRUE}{returns an \code{[n x r^2]} array}
#'   \item{FALSE}{returns an \code{[n x r x r]} array}
#' }
#' @param rem.diag a parameter for whether to remove the diagonal (self-connections) from the graph. Defaults to \code{TRUE}.
#' @return a list containing the following:
#' \item{\code{graphs} the graphs of the subjects. Size depends on the parameters specified.}
#' \item{dataset} a \code{[n]} vector of the dataset ids for each subject.}
#' \item{\code{subjects} a code{[n]} vector of the subject ids}
#' \item{\code{runs} a code{[n]} vector of the run ids}
#' \item{\code{tasks} a code{[n]} vector of the task ids}
#' \item{\code{runs} a code{[n]} vector of the run ids}
#' @export
fmriu.io.collection.open_graphs <- function(basepath, datasets, modality, atlases, gname='roi-connectomes',
                                            fmt='elist', verbose=FALSE, rtype='list', flatten=FALSE,
                                            rem.diag=TRUE) {
  if (! (rtype %in% c('list', 'array'))) {
    stop('You have passed an invalid return type. Options are: [\'list\', \'array\'].')
  }

  gr <-list()
  subjects <- c()
  sessions <- c()
  tasks <- c()
  runs <- c()
  dataset <- c()
  atlas <- c()

  for (ds in datasets) {
    for (at in atlases) {
      path <- file.path(basepath, ds, modality, gname, at, fsep = "/")
      print(path)
      signalobj <- fmriu.io.open_graphs(path, dataset_id = ds, atlas_id = at,
                                        verbose = verbose, rtype = 'list',
                                        fmt=fmt, rem.diag=rem.diag)

      gr <- append(gr, signalobj$graphs)
      subjects <- c(subjects, signalobj$subjects)
      sessions <- c(sessions, signalobj$sessions)
      tasks <- c(tasks, signalobj$tasks)
      runs <- c(runs, signalobj$runs)
      dataset <- c(dataset, signalobj$dataset)
      atlas <- c(atlas, signalobj$atlas)
    }
  }
  if (rtype == 'array') {
    aro <- fmriu.list2array(gr, flatten=flatten)
    gr <- aro$array
    dataset <- dataset[aro$incl_ar]
    atlas <- atlas[aro$incl_ar]
    subjects <- subjects[aro$incl_ar]
    sessions <- sessions[aro$incl_ar]
    tasks <- tasks[aro$incl_ar]
  }
  return(list(graphs=gr, dataset=dataset, atlas=atlas, subjects=subjects,
              sessions=sessions, tasks=tasks, runs=runs))
}
