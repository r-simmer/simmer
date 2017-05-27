## Maintenance release

Some minor changes and fixes.

## Test environments

* Fedora 25 + GCC + clang (local), R 3.3.3
* Rocker image, GCC + SAN + R devel
* Rocker image, clang + UBSAN + R devel
* Ubuntu 12.04 + GCC (on travis-ci), R 3.3.3, 3.4.0, devel
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
