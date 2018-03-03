## Minor release

- Several API improvements and bug fixes.
- Deprecations from the v3.6.x series have been removed.
- A paper accepted for publication in the Journal of Statistical Software has been included as a vignette.
- Supplementary materials for a paper accepted for publication in the IEEE Communications Magazine have been included as a vignette.
- We haven't included any reference to those papers in the DESCRIPTION because there is no DOI assigned yet. We do have included a CITATION file with provisional references.

Regarding the package title, Uwe asked us in our last submission to remove "for R" because it is redundant. If it is not an issue, we would like to keep it in our case. The reason is that simmer is the only DES framework for R, and "Discrete-Event Simulation for R" has become a distinctive tag as compared to our most direct competitors, which are "SimPy: Discrete event simulation for Python" and "SimJulia: A discrete event process oriented simulation framework written in Julia".

## Test environments

* Fedora 27 + GCC + clang (local), R 3.4.3
* Ubuntu 14.04 + GCC (on Travis-CI), R 3.3.3, 3.4.2, devel
* linux-x86_64-rocker-gcc-san (on r-hub)
* ubuntu-rchk (on r-hub)
* win-builder, R devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are 2 downstream dependencies, simmer.plot and simmer.bricks, for which I'm the maintainer too.
