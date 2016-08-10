## Release summary

This early patch release solves memtest notes on CRAN (gcc6-ASAN, gcc7-ASAN: heap-use-after-free).

## Test environments

* Fedora 24 + clang (local), R 3.3.1
* Rocker image, GCC + SAN + R devel
* Ubuntu 12.04 + GCC (on travis-ci), R 3.3.1
* win-builder, R devel

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Iñaki Ucar <i.ucar86@gmail.com>’

License components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2014-2016
  COPYRIGHT HOLDERS: BART SMEETS, IÑAKI UCAR

Possibly mis-spelled words in DESCRIPTION:
  DES (6:6, 8:5)
  Rcpp (7:39)
  SimJulia (7:5)
  SimPy (6:66)

* checking installed package size ... NOTE
  installed size is  8.9Mb
  sub-directories of 1Mb or more:
    libs   7.9Mb

## Downstream dependencies

There are currently no downstream dependencies for this package.
