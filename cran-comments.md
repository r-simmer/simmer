## Release summary

This release adds an important new feature requested by our users. Also the ERRORs reported on some platforms (platforms using clang and Sparc Solaris) should be fixed because, although we were unable to reproduce them, they seemed to be nothing more than weak test designs.

## Test environments

* Fedora 23 + clang (local), R 3.2.3 and devel
* Ubuntu 12.04 + GCC (on travis-ci), R 3.2.3
* win-builder, R release and devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are currently no downstream dependencies for this package.
