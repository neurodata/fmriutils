# fmriutils
The fmriutils package provides functions for loading, visualizing, and interacting with basic fMRI derivatives. 

## Installation

The following installation has been tested on Ubuntu 16.04 with R version 3.4.1. Prior to starting, we need to install several packages in a terminal session that are required for proper configuration of igraph:

```
sudo apt-get -y install libcurl4-openssl-dev libssl-dev libxml2-dev
```

Next, we can install the R dependencies directly. From an R terminal, type:

```
install.packages(c('devtools', 'Rcpp', 'RSpectra', 'stringr', 'ggplot2', 'reshape2'))
require(devtools)
install_github('igraph/rigraph')  # installs latest version of igraph
install_github('neurodata/fmriutils')
require('fmriutils')
```

For tutorials on usage, see our html documents:

[fmriutils](http://docs.neurodata.io/fmriutils/vignettes/fmriutils.html)
