% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectory-activities.R
\name{seize}
\alias{seize}
\alias{seize_selected}
\alias{release}
\alias{release_selected}
\alias{release_all}
\alias{release_selected_all}
\title{Seize/Release Resources}
\usage{
seize(.trj, resource, amount = 1, continue = NULL, post.seize = NULL,
  reject = NULL, ..., tag)

seize_selected(.trj, amount = 1, id = 0, continue = NULL,
  post.seize = NULL, reject = NULL, ..., tag)

release(.trj, resource, amount = 1, ..., tag)

release_selected(.trj, amount = 1, id = 0, ..., tag)

release_all(.trj, resource, ..., tag)

release_selected_all(.trj, id = 0, ..., tag)
}
\arguments{
\item{.trj}{the trajectory object.}

\item{resource}{the name of the resource.}

\item{amount}{the amount to seize/release, accepts either a numeric or a
callable object (a function) which must return a numeric.}

\item{continue}{a boolean (if \code{post.seize} OR \code{reject} is defined)
or a pair of booleans (if \code{post.seize} AND \code{reject} are defined; if
only one value is provided, it will be recycled) to indicate whether these
subtrajectories should continue to the next activity in the main trajectory.}

\item{post.seize}{an optional trajectory object which will be followed after
a successful seize.}

\item{reject}{an optional trajectory object which will be followed if the
arrival is rejected (dropped).}

\item{...}{unused.}

\item{tag}{activity tag name to perform named rollbacks (see
\code{\link{rollback}}) or just to better identify your activities.}

\item{id}{selection identifier for nested usage.}
}
\value{
Returns the trajectory object.
}
\description{
Activities for seizing/releasing a resource, by name or a previously selected
one. Resources must be defined in the simulation environment (see
\code{\link{add_resource}}).
}
\details{
Rejection happens when a resource is at full capacity and there is
no room in the queue (either because there is a finite \code{queue_size} and
it is full, or because \code{queue_size=0} and thus it is disabled). In those
cases, the \code{reject} parameter defines a fallback trajectory. Note,
however, that, if the arrival is accepted (either in the queue or in the
server) and then it is dropped afterwards due to preemption or resource
shrinkage, then this trajectory will not be executed. Instead, see
\code{\link{handle_unfinished}} for another, more general, method for
handling all kinds of unfinished arrivals.
}
\examples{
## simple seize, delay, then release
traj <- trajectory() \%>\%
  seize("doctor", 1) \%>\%
  timeout(3) \%>\%
  release("doctor", 1)

simmer() \%>\%
  add_resource("doctor", capacity=1) \%>\%
  add_generator("patient", traj, at(0, 1)) \%>\%
  run() \%>\%
  get_mon_resources()

## arrival rejection (no space left in the queue)
traj <- trajectory() \%>\%
  log_("arriving...") \%>\%
  seize("doctor", 1) \%>\%
  # the second patient won't reach this point
  log_("doctor seized") \%>\%
  timeout(5) \%>\%
  release("doctor", 1)

simmer() \%>\%
  add_resource("doctor", capacity=1, queue_size=0) \%>\%
  add_generator("patient", traj, at(0, 1)) \%>\%
  run() \%>\% invisible

## capturing rejection to retry
traj <- trajectory() \%>\%
  log_("arriving...") \%>\%
  seize(
    "doctor", 1, continue = FALSE,
    reject = trajectory() \%>\%
      log_("rejected!") \%>\%
      # go for a walk and try again
      timeout(2) \%>\%
      log_("retrying...") \%>\%
      rollback(amount = 4, times = Inf)) \%>\%
  # the second patient will reach this point after a couple of walks
  log_("doctor seized") \%>\%
  timeout(5) \%>\%
  release("doctor", 1) \%>\%
  log_("leaving")

simmer() \%>\%
  add_resource("doctor", capacity=1, queue_size=0) \%>\%
  add_generator("patient", traj, at(0, 1)) \%>\%
  run() \%>\% invisible

## combining post.seize and reject
traj <- trajectory() \%>\%
  log_("arriving...") \%>\%
  seize(
    "doctor", 1, continue = c(TRUE, TRUE),
    post.seize = trajectory("admitted patient") \%>\%
      log_("admitted") \%>\%
      timeout(5) \%>\%
      release("doctor", 1),
    reject = trajectory("rejected patient") \%>\%
      log_("rejected!") \%>\%
      seize("nurse", 1) \%>\%
      timeout(2) \%>\%
      release("nurse", 1)) \%>\%
  # both patients will reach this point, as continue = c(TRUE, TRUE)
  timeout(10) \%>\%
  log_("leaving...")

simmer() \%>\%
  add_resource("doctor", capacity=1, queue_size=0) \%>\%
  add_resource("nurse", capacity=10, queue_size=0) \%>\%
  add_generator("patient", traj, at(0, 1)) \%>\%
  run() \%>\% invisible

}
\seealso{
\code{\link{select}}, \code{\link{set_capacity}}, \code{\link{set_queue_size}},
\code{\link{set_capacity_selected}}, \code{\link{set_queue_size_selected}}
}
