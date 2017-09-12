# fmriutils
The fmriutils package provides functions for loading, visualizing, and interacting with basic fMRI derivatives. 

## Installation

The following installation has been tested on Ubuntu 16.04 with R version 3.4.1. Prior to starting, we need to install several packages in a bash terminal session that are required for proper configuration of igraph:

```
sudo apt-get -y install libcurl4-openssl-dev libssl-dev libxml2-dev
```

Next, we can install the R package dependencies directly. From an R terminal (or a GUI such as Rstudio), type:

```
install.packages(c('devtools', 'Rcpp', 'RSpectra', 'stringr', 'ggplot2', 'reshape2', 'abind', 'plyr'))
require(devtools)
install_github('igraph/rigraph')  # installs latest version of igraph
install_github('neurodata/fmriutils')
require('fmriutils')
```

For tutorials on usage, see our vignettes:

[fmriutils](http://docs.neurodata.io/fmriutils/vignettes/fmriutils.html)
