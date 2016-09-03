library(covr)

options(covr.flags = c(CXXFLAGS = "-O0 -g --coverage", LDFLAGS = "--coverage"))
package_coverage(quiet = FALSE, clean = FALSE)
