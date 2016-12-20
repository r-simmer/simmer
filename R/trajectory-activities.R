#' Add a seize/release activity
#'
#' Activities for seizing/releasing a resource, by name or a previously selected one.
#'
#' @param .trj the trajectory object.
#' @inheritParams select
#' @param resource the name of the resource.
#' @param amount the amount to seize/release, accepts either a numeric or a callable object
#' (a function) which must return a numeric.
#' @param continue a boolean (if \code{post.seize} OR \code{reject} is defined) or a pair of booleans
#' (if \code{post.seize} AND \code{reject} are defined) to indicate whether these subtrajectories
#' should continue to the next activity in the main trajectory.
#' @param post.seize an optional trajectory object which will be followed after a successful seize.
#' @param reject an optional trajectory object which will be followed if the arrival is rejected.
#'
#' @return Returns the trajectory object.
#' @seealso \code{\link{select}}, \code{\link{set_capacity}}, \code{\link{set_queue_size}},
#' \code{\link{set_capacity_selected}}, \code{\link{set_queue_size_selected}}.
#' @export
seize <- function(.trj, resource, amount=1, continue=NULL, post.seize=NULL, reject=NULL)
  .trj$seize(resource, amount, 0, continue, post.seize, reject)

#' @rdname seize
#' @export
seize_selected <- function(.trj, amount=1, id=0, continue=NULL, post.seize=NULL, reject=NULL)
  .trj$seize(NA, amount, id, continue, post.seize, reject)

#' @rdname seize
#' @export
release <- function(.trj, resource, amount=1) .trj$release(resource, amount)

#' @rdname seize
#' @export
release_selected <- function(.trj, amount=1, id=0) .trj$release(NA, amount, id)

#' Add a set capacity/queue size activity
#'
#' Modify a resource's server capacity or queue size, by name or a previously selected one.
#'
#' @inheritParams seize
#' @inheritParams select
#' @param resource the name of the resource.
#' @param value new value to set.
#'
#' @return Returns the trajectory object.
#' @seealso \code{\link{select}}, \code{\link{seize}}, \code{\link{release}},
#' \code{\link{seize_selected}}, \code{\link{release_selected}}.
#' @export
set_capacity <- function(.trj, resource, value) .trj$set_capacity(resource, value)

#' @rdname set_capacity
#' @export
set_capacity_selected <- function(.trj, value, id=0) .trj$set_capacity(NA, value, id)

#' @rdname set_capacity
#' @export
set_queue_size <- function(.trj, resource, value) .trj$set_queue_size(resource, value)

#' @rdname set_capacity
#' @export
set_queue_size_selected <- function(.trj, value, id=0) .trj$set_queue_size(NA, value, id)

#' Select a resource
#'
#' Resource selector for a subsequent seize/release.
#'
#' @inheritParams seize
#' @param resources one or more resource names, or a callable object (a function) which
#' must return a resource name to select.
#' @param policy if \code{resources} is a vector of names, this parameter determines
#' the criteria for selecting a resource among the set of policies available; otherwise,
#' it is ignored.
#' @param id selection identifier for nested usage.
#'
#' @return Returns the trajectory object.
#' @seealso \code{\link{seize_selected}}, \code{\link{release_selected}},
#' \code{\link{set_capacity_selected}}, \code{\link{set_queue_size_selected}}.
#' @export
select <- function(.trj, resources, policy=c("shortest-queue", "round-robin",
                                             "first-available", "random"), id=0)
  .trj$select(resources, policy, id)

#' Add a timeout activity
#'
#' Insert delays and execute user-defined tasks.
#'
#' @inheritParams seize
#' @param task the timeout duration supplied by either passing a numeric or a
#' callable object (a function) which must return a numeric (negative values are
#' automatically coerced to positive).
#'
#' @return Returns the trajectory object.
#' @export
timeout <- function(.trj, task) .trj$timeout(task)

#' Add a set attribute activity
#'
#' Modify an attribute in the form of a key/value pair.
#'
#' @inheritParams seize
#' @param key the attribute key (coerced to a string).
#' @param value the value to set, accepts either a numeric or a callable object
#' (a function) which must return a numeric.
#' @param global if \code{TRUE}, the attribute will be global instead of per-arrival.
#'
#' @return Returns the trajectory object.
#' @export
set_attribute <- function(.trj, key, value, global=FALSE) .trj$set_attribute(key, value, global)

#' Add a activate/deactivate activity
#'
#' Activate or deactivate the generation of arrivals by name.
#'
#' @inheritParams seize
#' @param generator the name of the generator or a function returning a name.
#'
#' @return Returns the trajectory object.
#' @seealso \code{\link{set_trajectory}}, \code{\link{set_distribution}}.
#' @export
activate <- function(.trj, generator) .trj$activate(generator)

#' @rdname activate
#' @export
deactivate <- function(.trj, generator) .trj$deactivate(generator)

#' Add a set trajectory/distribution activity
#'
#' Modify a generator's trajectory or distribution by name.
#'
#' @inheritParams seize
#' @inheritParams activate
#' @param trajectory the trajectory that the generated arrivals will follow.
#'
#' @return Returns the trajectory object.
#' @seealso \code{\link{activate}}, \code{\link{deactivate}}.
#' @export
set_trajectory <- function(.trj, generator, trajectory) .trj$set_trajectory(generator, trajectory)

#' @rdname set_trajectory
#' @param distribution a function modelling the interarrival times (returning a
#' negative value stops the generator).
#' @export
set_distribution <- function(.trj, generator, distribution) .trj$set_distribution(generator, distribution)

#' Add a set prioritization activity
#'
#' Modify the arrival's prioritization values.
#'
#' @inheritParams seize
#' @param values expects either a vector/list or a callable object (a function)
#' returning a vector/list of three values \code{c(priority, preemptible, restart)}.
#' A negative value leaves the corresponding parameter unchanged.
#' See \code{\link{add_generator}} for more information about these parameters.
#'
#' @return Returns the trajectory object.
#' @export
set_prioritization <- function(.trj, values) .trj$set_prioritization(values)

#' Add a branch activity
#'
#' Define a fork with \code{N} alternative sub-trajectories.
#'
#' @inheritParams seize
#' @param option a callable object (a function) which must return an integer between
#' \code{0} and \code{N}. A return value equal to \code{0} skips the branch and
#' continues to the next activity. A returning value between \code{1} to \code{N}
#' makes the arrival to follow the corresponding sub-trajectory.
#' @param continue a vector of \code{N} booleans that indicate whether the arrival must
#' continue to the main trajectory after each sub-trajectory or not.
#' @param ... \code{N} trajectory objects describing each sub-trajectory.
#'
#' @return Returns the trajectory object.
#' @export
branch <- function(.trj, option, continue, ...) .trj$branch(option, continue, ...)

#' Add a rollback activity
#'
#' Go backwards to a previous point in the trajectory. Useful to implement loops.
#'
#' @inheritParams seize
#' @param amount the amount of activities (of the same or parent trajectories) to roll back.
#' @param times the number of repetitions until an arrival may continue.
#' @param check a callable object (a function) which must return a boolean. If
#' present, the \code{times} parameter is ignored, and the activity uses this
#' function to check whether the rollback must be done or not.
#'
#' @return Returns the trajectory object.
#' @export
rollback <- function(.trj, amount, times=1, check) .trj$rollback(amount, times, check)

#' Add a leave activity
#'
#' Leave the trajectory with some probability.
#'
#' @inheritParams seize
#' @param prob a probability or a function returning a probability.
#'
#' @return Returns the trajectory object.
#' @export
leave <- function(.trj, prob) .trj$leave(prob)

#' Add a renege activity
#'
#' Set or unset a timer or a signal after which the arrival will abandon.
#'
#' @inheritParams seize
#' @param t timeout to trigger reneging, accepts either a numeric or a callable object
#' (a function) which must return a numeric.
#' @param out optional sub-trajectory in case of reneging.
#'
#' @return Returns the trajectory object.
#' @export
renege_in <- function(.trj, t, out=NULL) .trj$renege_in(t, out)

#' @param signal signal to trigger reneging, accepts either a string or a callable object
#' (a function) which must return a string.
#'
#' @rdname renege_in
#' @seealso \code{\link{send}}
#' @export
renege_if <- function(.trj, signal, out=NULL) .trj$renege_if(signal, out)

#' @rdname renege_in
#' @export
renege_abort <- function(.trj) .trj$renege_abort()

#' Add a clone/synchronize activity
#'
#' A \code{clone} activity replicates an arrival \code{n} times (the original
#' one + \code{n-1} copies). A \code{synchronize} activity removes all but one clone.
#'
#' @inheritParams seize
#' @param n number of clones, accepts either a numeric or a callable object
#' (a function) which must return a numeric.
#' @param ... optional parallel sub-trajectories. Each clone will follow
#' a different sub-trajectory if available.
#'
#' @return Returns the trajectory object.
#' @export
clone <- function(.trj, n, ...) .trj$replicate(n, ...)

#' @inheritParams seize
#' @param wait if \code{FALSE}, all clones but the first to arrive are removed.
#' if \code{TRUE} (default), all clones but the last to arrive are removed.
#' @param mon_all if \code{TRUE}, \code{get_mon_arrivals} will show one
#' line per clone.
#'
#' @rdname clone
#' @export
synchronize <- function(.trj, wait=TRUE, mon_all=FALSE) .trj$synchronize(wait, mon_all)

#' Add a batch/separate activity
#'
#' Collect a number of arrivals before they can continue processing
#' or split a previously established batch.
#'
#' @inheritParams seize
#' @param n batch size, accepts a numeric.
#' @param timeout set an optional timer which triggers batches every \code{timeout} time
#' units even if the batch size has not been fulfilled, accepts a numeric (0 = disabled).
#' @param permanent if \code{TRUE}, batches cannot be split.
#' @param name optional string. Unnamed batches from different \code{batch} activities are
#' independent. However, if you want to feed arrivals from different trajectories into a
#' same batch, you need to specify a common name across all your \code{batch} activities.
#' @param rule an optional callable object (a function) which will be applied to
#' every arrival to determine whether it should be included into the batch, thus
#  it must return a boolean.
#'
#' @return Returns the trajectory object.
#' @export
batch <- function(.trj, n, timeout=0, permanent=FALSE, name="", rule=NULL)
  .trj$batch(n, timeout, permanent, name, rule)

#' @inheritParams seize
#'
#' @rdname batch
#' @export
separate <- function(.trj) .trj$separate()

#' Add an inter-arrival communication activity
#'
#' These activities enable asynchronous programming. \code{send()} broadcasts a signal or a list
#' of signals. Arrivals can subscribe to signals and (optionally) assign a handler with
#' \code{trap()}. Note that, while inside a batch, all the signals subscribed before entering
#' the batch are ignored. Upon a signal reception, the arrival stops the current activity and
#' executes the handler (if provided). Then, the execution returns to the activity following the
#' point of the interruption. \code{untrap()} can be used to unsubscribe from signals.
#' \code{wait()} blocks until a signal is received.
#'
#' @inheritParams seize
#' @param signals signal or list of signals, accepts either a string, a list of strings or a
#' callable object (a function) which must return a string or a list of strings.
#' @param delay optional timeout to trigger the signals, accepts either a numeric or a callable
#' object (a function) which must return a numeric.
#'
#' @return Returns the trajectory object.
#' @seealso \code{\link{renege_if}}
#' @export
send <- function(.trj, signals, delay=0) .trj$send(signals, delay)

#' @param handler optional trajectory object to handle a signal received.
#' @param interruptible whether the handler can be interrupted by signals.
#'
#' @rdname send
#' @export
trap <- function(.trj, signals, handler=NULL, interruptible=TRUE)
  .trj$trap(signals, handler, interruptible)

#' @rdname send
#' @export
untrap <- function(.trj, signals) .trj$untrap(signals)

#' @rdname send
#' @export
wait <- function(.trj) .trj$wait()

#' Add a logging activity
#'
#' Display a message preceded by the simulation time and the name of the arrival.
#'
#' @inheritParams seize
#' @param message the message to display, accepts either a string or a callable object
#' (a function) which must return a string.
#'
#' @return Returns the trajectory object.
#' @export
log_ <- function(.trj, message) .trj$log(message)
