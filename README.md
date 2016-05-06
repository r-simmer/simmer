simmer
======

[![Build Status](https://travis-ci.org/r-simmer/simmer.svg?branch=master)](https://travis-ci.org/r-simmer/simmer)
[![Coverage Status](https://coveralls.io/repos/github/r-simmer/simmer/badge.svg?branch=master)](https://coveralls.io/github/r-simmer/simmer?branch=master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/simmer)](http://cran.r-project.org/package=simmer)
[![Downloads](http://cranlogs.r-pkg.org/badges/simmer)](http://cran.rstudio.com/package=simmer)

*by Bart Smeets, Iñaki Ucar*

**simmer** is a process-oriented and trajectory-based Discrete-Event Simulation (DES) package for R. Designed to be a generic framework like [SimPy](https://simpy.readthedocs.org) or [SimJulia](http://simjuliajl.readthedocs.org), it leverages the use of [Rcpp](http://www.rcpp.org/) to boost the performance and turning DES in R feasible. As a noteworthy characteristic, simmer exploits the concept of _trajectory_: a common path in the simulation model for entities of the same type. It is pretty flexible and simple to use, and leverages the chaining/piping workflow introduced by the [magrittr](https://github.com/smbache/magrittr) package.

## Documentation

Documentation is available at [r-simmer.org/docs](http://r-simmer.org/docs).

## Mailing list

For bugs and/or issues, create a new issue on GitHub. For other questions or comments, please subscribe to the [simmer-devel mailing list](https://groups.google.com/forum/#!forum/simmer-devel). You must be a member to post messages, but anyone can read the archived discussions.

## Installation

Install the release version from CRAN:

``` r
install.packages("simmer")
```

The installation from GitHub requires the [devtools](https://github.com/hadley/devtools) package.

``` r
devtools::install_github("r-simmer/simmer")
```

Please note that the package contains some C++ code and thus you need a development environment to build the package (e.g. [Rtools](http://cran.r-project.org/bin/windows/Rtools/) for Windows).

## Learning simmer

To get started, please explore the vignettes: 

``` r
vignette(package = "simmer")
```

These are currently available:

* [Introduction to simmer](https://cran.r-project.org/web/packages/simmer/vignettes/A-introduction.html)
* [Terminology](https://cran.r-project.org/web/packages/simmer/vignettes/B-terminology.html)
* [Advanced trajectory usage](https://cran.r-project.org/web/packages/simmer/vignettes/C-trajectories.html)
* [Queueing systems](https://cran.r-project.org/web/packages/simmer/vignettes/D-queueing-systems.html)
* [Continuous-Time Markov Chains](https://cran.r-project.org/web/packages/simmer/vignettes/E-ctmc.html)
