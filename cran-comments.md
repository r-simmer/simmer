## Re-submission of major release

Trivial change to fix "empty macro argument" warning in pre-tests. Thus, version number unchanged unless otherwise requested.

* The license has been changed to GPL >= 2.
* The C++ core has been refactorised and exposed under `inst/include`.
* Multiple enhancements and bug fixes.

We haven't included any reference to the papers in the DESCRIPTION yet because they are still waiting to be published and there is no DOI assigned. We will add them in a future release.

Regarding the package title, Uwe asked us in our last submission to remove "for R" because it is redundant. If it is not an issue, we would like to keep it in our case. The reason is that simmer is the only DES framework for R, and "Discrete-Event Simulation for R" has become a distinctive tag as compared to our most direct competitors, which are "SimPy: Discrete event simulation for Python" and "SimJulia: A discrete event process oriented simulation framework written in Julia".

## Test environments

* Fedora 28 + GCC + clang (local), R 3.5.0
* Ubuntu 14.04 + GCC (on Travis-CI), R 3.4.4, 3.5.0, devel
* linux-x86_64-rocker-gcc-san (on r-hub)
* ubuntu-rchk (on r-hub)
* win-builder, R devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are 2 downstream dependencies, simmer.plot and simmer.bricks, for which I'm the maintainer too.
