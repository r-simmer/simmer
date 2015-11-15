simmer
======

[![Build Status](https://travis-ci.org/Bart6114/simmer.svg?branch=master)](https://travis-ci.org/Bart6114/simmer) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/simmer)](http://cran.r-project.org/package=simmer)

*by Bart Smeets (<bartsmeets86@gmail.com>), Iñaki Ucar (<i.ucar86@gmail.com>)*

**simmer** is a Discrete-Event Simulation (DES) package for the R language designed to be a generic framework like [SimPy](https://simpy.readthedocs.org) or [SimJulia](http://simjuliajl.readthedocs.org). Although R alone is definitely not made for DES, we use [Rcpp](http://www.rcpp.org/) to boost the performance of simmer. This faces us with an important trade-off between flexibility and performance, depending on how much code remains in R or goes to C++ respectively.

Our implementation solves this problem by introducing the concept of *trajectory*: a common path in the simulation model for arrivals of the same type. As we will see, it is pretty flexible and simple to use, and leverages the chaining/piping workflow introduced by the [magrittr](https://github.com/smbache/magrittr) package. In terms of performance, simmer is faster than SimPy when it comes to simulating queue networks.

Installation
------------

Install the release version from CRAN:

``` r
install.packages("simmer")
```

The installation from GitHub requires the [devtools](https://github.com/hadley/devtools) package.

``` r
devtools::install_github("Bart6114/simmer")
```

Please note that the package contains some C++ code and thus you need a development environment to build the package (e.g. [Rtools](http://cran.r-project.org/bin/windows/Rtools/) for Windows).

Learning simmer
---------------

To get started, please read the [intro vignette](https://cran.r-project.org/web/packages/simmer/vignettes/introduction.html):

``` r
vignette("introduction", package = "simmer")
```

Roadmap
-------

-   Refine queue discipline (add priorities).
-   Time-specific resource availability.

Contact
-------

For bugs and/or issues, create a new issue on GitHub. For other questions or comments, do not hesitate to contact us by email.
