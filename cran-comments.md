## New minor release

Some new features and improvements for dealing with trajectories. Functions have been converted to S3 methods and some of them have been deprecated. In particular, we plan to remove all the plotting functionalities in the next release, which are non-essential to DES, and in the meantime, release a new package on CRAN covering these features.

## Test environments

* Fedora 25 + GCC + clang (local), R 3.3.2
* Rocker image, GCC + SAN + R devel
* Rocker image, clang + UBSAN + R devel
* Docker image, clang + R patched
* Ubuntu 12.04 + GCC (on travis-ci), R 3.2.5, 3.3.1, devel
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
  DES (9:6, 10:55)

* checking installed package size ... NOTE
  installed size is  10.1Mb
  sub-directories of 1Mb or more:
    libs   9.0Mb

## Downstream dependencies

There are currently no downstream dependencies for this package.
