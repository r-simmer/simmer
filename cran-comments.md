## Minor release

Several new features, improvements and bug fixes.

Regarding the package title, Uwe asked us in a past submission to remove
"for R" because it is redundant. If it is not an issue, we would like to keep
it in our case. The reason is that simmer is the only DES framework for R, and
"Discrete-Event Simulation for R" has become a distinctive tag as compared to
our most direct competitors, which are "SimPy: Discrete event simulation for
Python" and "SimJulia: A discrete event process oriented simulation framework
written in Julia".

## Test environments

- Fedora 29 + GCC + clang (local), R 3.5.1
- Ubuntu 14.04 + GCC (on Travis-CI), R 3.4.4, 3.5.1, devel
- linux-x86_64-rocker-gcc-san (on r-hub)
- ubuntu-rchk (on r-hub)
- win-builder, R devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are 2 downstream dependencies, simmer.plot and simmer.bricks, for which
I'm the maintainer too.
