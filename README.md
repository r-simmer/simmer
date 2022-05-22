# <img src="https://raw.githubusercontent.com/r-simmer/r-simmer.github.io/master/images/simmer-logo.png" alt="simmer" width="200" />

[![build](https://github.com/r-simmer/simmer/actions/workflows/build.yml/badge.svg)](https://github.com/r-simmer/simmer/actions/workflows/build.yml)
[![Coverage Status](https://codecov.io/gh/r-simmer/simmer/branch/master/graph/badge.svg)](https://app.codecov.io/gh/r-simmer/simmer)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/simmer)](https://cran.r-project.org/package=simmer)
[![Downloads](https://cranlogs.r-pkg.org/badges/simmer)](https://cran.r-project.org/package=simmer)
[![DOI](https://img.shields.io/badge/doi-10.18637/jss.v090.i02-informational.svg)](https://doi.org/10.18637/jss.v090.i02)

**simmer** is a process-oriented and trajectory-based Discrete-Event Simulation (DES) package for R. Designed to be a generic framework like [SimPy](https://simpy.readthedocs.io/) or [SimJulia](https://simjuliajl.readthedocs.io/), it leverages the power of [Rcpp](https://www.rcpp.org/) to boost the performance and turning DES in R feasible. As a noteworthy characteristic, simmer exploits the concept of _trajectory_: a common path in the simulation model for entities of the same type. It is pretty flexible and simple to use, and leverages the chaining/piping workflow introduced by the [magrittr](https://github.com/tidyverse/magrittr) package.

## Extensions

Package          | Description                                     | Status
---------------- | ----------------------------------------------- | ------------------
[**simmer.plot**](https://r-simmer.org/extensions/plot)  |  Plotting Methods for `simmer` | [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/simmer.plot)](https://cran.r-project.org/package=simmer.plot)
[**simmer.bricks**](https://r-simmer.org/extensions/bricks)  |  Helper Methods for `simmer` Trajectories | [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/simmer.bricks)](https://cran.r-project.org/package=simmer.bricks)
[**simmer.optim**](https://github.com/r-simmer/simmer.optim) | Parameter Optimization Functions for `simmer` | [![Status\_Badge](https://img.shields.io/badge/lifecycle-dormant-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[**simmer.json**](https://github.com/r-simmer/simmer.json)  | Read / Load `simmer` Definitions in JSON Format | [![Status\_Badge](https://img.shields.io/badge/lifecycle-dormant-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[**simmer.mon**](https://github.com/r-simmer/simmer.mon)  | Monitoring Backends for `simmer` | [![Status\_Badge](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

## Mailing list

For bugs and/or issues, create a new issue on GitHub. For other questions or comments, please subscribe to the [simmer-devel mailing list](https://groups.google.com/forum/#!forum/simmer-devel). You must be a member to post messages, but anyone can read the archived discussions.

## Documentation

Documentation is available at [r-simmer.org/reference](https://r-simmer.org/reference). To get started, please explore our [vignettes online](https://r-simmer.org/articles/), or in R:

``` r
vignette(package = "simmer")
```

## Installation

Install the release version from CRAN:

``` r
install.packages("simmer")
```

The installation from GitHub requires the [remotes](https://cran.r-project.org/package=remotes) package.

``` r
remotes::install_github("r-simmer/simmer")
```

Please note that the package contains some C++ code and thus you need a development environment to build the package (e.g., [Rtools](https://cran.r-project.org/bin/windows/Rtools/) for Windows).

## Hexagon stickers!

You can purchase `simmer` hex stickers on Redbubble ([sticker 1](https://www.redbubble.com/i/sticker/simmer-DES-for-R-by-puratura/32157547.EJUG5), [sticker 2](https://www.redbubble.com/i/sticker/simmer-DES-for-R-by-puratura/32155608.EJUG5)). Browse there for more stuff such as T-shirts and mugs!

<img src="https://r-simmer.org/images/simmer-hex-01.svg" alt="design1" width="150" /><span style="margin:10px"></span><img src="https://r-simmer.org/images/simmer-hex-02.svg" alt="design2" width="150" />
