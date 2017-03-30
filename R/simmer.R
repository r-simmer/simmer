#' \pkg{simmer}: Discrete-Event Simulation for \R
#'
#' A process-oriented and trajectory-based Discrete-Event Simulation (DES)
#' package for \R. Designed to be a generic framework like \pkg{SimPy} or
#' \pkg{SimJulia}, it leverages the power of \pkg{Rcpp} to boost the performance
#' and turning DES in \R feasible. As a noteworthy characteristic, \pkg{simmer}
#' exploits the concept of trajectory: a common path in the simulation model for
#' entities of the same type. It is pretty flexible and simple to use, and
#' leverages the chaining/piping workflow introduced by the \pkg{magrittr} package.
#'
#' @author Iñaki Ucar and Bart Smeets
#'
#' @seealso \pkg{simmer}'s homepage \url{http://r-simmer.org} and
#' GitHub repository \url{https://github.com/r-simmer/simmer}.
#'
#' @examples
#' \dontrun{
#' # introduction to simmer
#' vignette("A-introduction")
#'
#' # more vignettes
#' vignette(package = "simmer")
#' }
#'
#' @docType package
#' @name simmer
#'
#' @useDynLib simmer, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom R6 R6Class
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
