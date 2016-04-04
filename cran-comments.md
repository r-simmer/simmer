## Release summary

The GitHub repository has been transferred from one author to the other (Bart -> Iñaki), and so the corresponding URLs (URL and BugReports) have changed in the DESCRIPTION file.

This version implements two major features, several minor fixes and improvements (including compatibility issues with the new version of `testthat`), and removes two functions which were deprecated in previous releases.

## Test environments

* Fedora 23 + clang (local), R 3.2.3 and devel
* Ubuntu 12.04 + GCC (on travis-ci), R 3.2.4
* win-builder, R release and devel

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

* checking installed package size ... NOTE
  installed size is  5.1Mb
  sub-directories of 1Mb or more:
    libs   4.6Mb

## Downstream dependencies

There are currently no downstream dependencies for this package.
