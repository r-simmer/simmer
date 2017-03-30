## Resubmission

The incoming pretest failed. Fixed compilation error on Windows x64: changed size_t to unsigned long in conversion to SEXP.

## New patch release

Some minor new features and fixes. Plotting functionalities (deprecated in the previous version) covered by the simmer.plot package have been removed.

## Test environments

* Fedora 25 + GCC + clang (local), R 3.3.3
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
  YEAR: 2014-2017
  COPYRIGHT HOLDER: Bart Smeets, Iñaki Ucar

Possibly mis-spelled words in DESCRIPTION:
  DES (9:6)

* checking installed package size ... NOTE
  installed size is  11.0Mb
  sub-directories of 1Mb or more:
    libs   10.0Mb

## Downstream dependencies

There is one downstream dependency, simmer.plot, for which I'm the maintainer too.
