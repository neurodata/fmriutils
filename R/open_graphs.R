#' Open Graphs
#'
#' A utility to open graph information from a dataset.Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[#####]_task-[abcde]_ses-[#####]_(generic info).file
#'
#' @import stringr
#' @import igraph
#' @param fnames [n] a vector of filenames, with separation by underscores IE, dataset_subject_run_(other information).ext. Alternatively, a path to a directory containing a collection of appropriately named files with extension as specified below.
#' @param dataset_id="" [1] the dataset id in the filenames.
#' @param atlas_id="" [1] the atlas id in the filenames.
#' @param fmt="graphml" a parameter indicating the format for graphs to be read in as. Options are ['graphml', 'edgelist'].
#' @param verbose=FALSE whether to print the iteration being loaded.
#' @param rtype='list' the type of output to return. Options are 'list' and 'array'.
#' @param flatten=FALSE a parameter to flatten the array if the rtype is set to 'array'.
#' \describe{
#'   \item{TRUE}{If rtype == 'array', then returns an [n x r^2] array}
#'   \item{FALSE}{If rtype == 'array', then returns an [n x r x r] array}
#' }
#' @return gr the graphs of the subjects. Size depends on the parameters specified.
#' @return dataset [n] a vector of the dataset ids for each subject.
#' @return subjects [n] the subject ids
#' @return sessions [n] the run ids
#' @return tasks [n] the task ids
#' @return runs [n] the run ids
#' @export
fmriu.io.open_graphs <- function(fnames, dataset_id="", atlas_id="", fmt='graphml', verbose=FALSE, rtype='list', flatten=FALSE) {
  if (! (fmt %in% c('graphml', 'edgelist'))) {
    stop('You have passed an invalid format type. Options are: [\'graphml\', \'edgelist\'].')
  }

  if (is.character(fnames)) {
    fnames <- list.files(fnames, pattern=paste('\\.', fmt, sep=""), full.names=TRUE)
  }

  if (fmt == 'edgelist') {
    fmt = 'ncol'
  }

  if (! (rtype %in% c('list', 'array'))) {
    stop('You have passed an invalid return type. Options are: [\'list\', \'array\'].')
  }

  print(sprintf("opening graphs for %s dataset and %s parcellation atlas...", dataset_id, atlas_id))
  subjects <- vector("character", length(fnames))
  dataset <- rep(dataset_id, length(fnames))
  atlas <- rep(atlas_id, length(fnames))
  sessions <- vector("character", length(fnames))
  tasks <- vector("character", length(fnames))
  gr <- list()

  vertices <- c()

  # so that we don't get any annoying errors if particular vertices are empty
  for (i in 1:length(fnames)) {
    tgr <- igraph::read_graph(fnames[i], format=fmt) # read the graph from the filename
    vertices <- union(vertices, V(tgr))
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
      gr[[basename]] <-t(tgr)
      subjects[counter] <- str_extract(basename, 'sub(.?)+?(?=_)')
      sessions[counter] <- str_extract(basename, 'ses(.?)+?(?=_)')
      tasks[counter] <- str_extract(basename, 'task(.?)+?(?=_)')
      counter <- counter + 1
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
              sessions=sessions, tasks=tasks))
}

#' Open Graphs Collection
#'
#' A utility to open graphs information from multiple datasets. Assumes data is formatted by the BIDs spec.
#' This means that derivatives are named as follows: sub-[###]_task-[abcde]_ses-[###]_run-[###]_(generic info).file
#'
#' @param basepath: [1] a path to a properly spec'd directory. See the example in `fmriutils.html` for details.
#' @param datasets: [d] the datasets as a vector. Note that we expect a folder `<basepath>/<dataset>` for `dataset in datasets`.
#' @param atlases: [a] the atlases as a vector. Note that we expect a folder `<basepath>/<dataset>/<gname>/<atlas>` for `atlas in atlases`.
#' @param gname='connectomes': the folder name in which graphs are stored. Suggested options are ['connectomes', 'graphs'].
#' @param fmt="graphml" a parameter indicating the format for graphs to be read in as. Options are ['graphml', 'edgelist'].
#' @param verbose=FALSE: whether to print the id of the scan being loaded.
#' @param rtype='list': the type of output to return. Options are 'list' and 'array'. Note that if rtype is 'array', all of the graphs must be derived from parcellations with the same number of vertices.
#' @param flatten=FALSE a parameter to flatten the array if the rtype is set to 'array'.
#' \describe{
#'   \item{TRUE}{If rtype == 'array', then returns an [n x r^2] array}
#'   \item{FALSE}{If rtype == 'array', then returns an [n x r x r] array}
#' }
#' @return graphs: [[n]][nroi, nroi] the graphs loaded from the specified directory. Shape depends on the parameters specified.
#' @return dataset: [n] a vector of the dataset ids for each subject.
#' @return atlas: [n] a vector of the atlas ids for each subject.
#' @return subjects: [n] the subject ids
#' @return sessions: [n] the session ids
#' @return tasks: [n] the task ids
#' @return runs: [n] the run ids
#' @export
fmriu.io.collection.open_graphs <- function(basepath, datasets="", atlases="", gname='connectomes', fmt='graphml', verbose=FALSE, rtype='list', flatten=FALSE) {
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
      path <- file.path(basepath, ds, gname, at, fsep = "/")
      signalobj <- fmriu.io.open_graphs(path, dataset_id = ds, atlas_id = at,
                                        verbose = verbose, rtype = 'list',
                                        fmt=fmt)

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
