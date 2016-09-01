library(covr)

options(covr.flags = c(CXXFLAGS = "-O0 -g -coverage -fno-inline", LDFLAGS = "-coverage"))
package_coverage(quiet = FALSE, clean = FALSE)
