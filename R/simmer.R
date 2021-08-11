# Copyright (C) 2014 Bart Smeets
# Copyright (C) 2017-2019 Iñaki Ucar
#
# This file is part of simmer.
#
# simmer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# simmer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with simmer. If not, see <http://www.gnu.org/licenses/>.

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
#' @references Ucar I., Smeets B., Azcorra A. (2019).
#' "\pkg{simmer}: Discrete-Event Simulation for \R."
#' \emph{Journal of Statistical Software}, \strong{90}(2), 1-30.
#' \doi{10.18637/jss.v090.i02}.
#'
#' Ucar I., Hernández J.A., Serrano P., Azcorra A. (2018).
#' "Design and Analysis of 5G Scenarios with \pkg{simmer}: An \R Package for
#' Fast DES Prototyping."
#' \emph{IEEE Communications Magazine}, \strong{56}(11), 145-151.
#' \doi{10.1109/MCOM.2018.1700960}.
#'
#' @seealso \pkg{simmer}'s homepage \url{https://r-simmer.org} and
#' GitHub repository \url{https://github.com/r-simmer/simmer}.
#'
#' @examples
#' \dontrun{
#' # introduction to simmer
#' vignette("simmer-01-introduction")
#'
#' # JSS paper available as vignette
#' vignette("simmer-02-jss")
#'
#' # more vignettes
#' vignette(package = "simmer")
#' }
#'
#' @docType package
#' @name simmer-package
#'
#' @useDynLib simmer, .registration=TRUE
#' @importFrom Rcpp evalCpp
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`
