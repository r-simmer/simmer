## Patch release (resubmission)

URLs fixed. Maintenance release, several bug fixes and minor enhancements.

Regarding the package title, Uwe asked us in a past submission to remove
"for R" because it is redundant. If it is not an issue, we would like to keep
it in our case. The reason is that simmer is the only DES framework for R, and
"Discrete-Event Simulation for R" has become a distinctive tag as compared to
our most direct competitors, which are "SimPy: Discrete event simulation for
Python" and "SimJulia: A discrete event process oriented simulation framework
written in Julia".

## Test environments

- Fedora 34 + GCC (local), R 4.0.5
- macOS-latest, windows-latest, ubuntu-latest (on GA), R devel, release, oldrel
- win-builder, R devel

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.

## Downstream dependencies

There are 2 downstream dependencies, simmer.plot and simmer.bricks, for which
I'm the maintainer too. No issues found.
