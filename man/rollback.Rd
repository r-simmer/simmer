% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectory-activities.R
\name{rollback}
\alias{rollback}
\title{Rollback to a Previous Activity}
\usage{
rollback(.trj, target, times = Inf, check = NULL, ..., tag)
}
\arguments{
\item{.trj}{the trajectory object.}

\item{target}{tag name (previously set with the \code{tag} argument in any
activity) or amount of activities (of the same or parent trajectories) to
roll back (see examples).}

\item{times}{the number of repetitions until an arrival may continue.}

\item{check}{a callable object (a function) which must return a boolean. If
present, the \code{times} parameter is ignored, and the activity uses this
function to check whether the rollback must be done or not.}

\item{...}{unused}

\item{tag}{activity tag name to perform named rollbacks (see
\code{\link{rollback}}) or just to better identify your activities.}
}
\value{
Returns the trajectory object.
}
\description{
Activity for going backwards to a previous point in the trajectory. Useful to
implement loops.
}
\examples{
## rollback a specific number of times
traj <- trajectory() \%>\%
  log_("hello!") \%>\%
  timeout(1) \%>\%
  rollback(2, 3)

simmer() \%>\%
  add_generator("hello_sayer", traj, at(0)) \%>\%
  run() \%>\% invisible

## same but with a tag as target
traj <- trajectory() \%>\%
  log_("hello!", tag="msg") \%>\%
  timeout(1) \%>\%
  rollback("msg", 3)

simmer() \%>\%
  add_generator("hello_sayer", traj, at(0)) \%>\%
  run() \%>\% invisible

## custom check
env <- simmer()

traj <- trajectory() \%>\%
  set_attribute("var", 0) \%>\%
  log_(tag="msg", function()
    paste("attribute level is at:", get_attribute(env, "var"))) \%>\%
  set_attribute("var", 25, mod="+") \%>\%
  rollback("msg", check=function() get_attribute(env, "var") < 100) \%>\%
  log_("done")

env \%>\%
  add_generator("dummy", traj, at(0)) \%>\%
  run() \%>\% invisible

}
