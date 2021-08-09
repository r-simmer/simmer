# Copyright (C) 2014-2015 Bart Smeets
# Copyright (C) 2015-2016 Bart Smeets and Iñaki Ucar
# Copyright (C) 2016-2019,2021 Iñaki Ucar
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

#' Seize/Release Resources
#'
#' Activities for seizing/releasing a resource, by name or a previously selected
#' one. Resources must be defined in the simulation environment (see
#' \code{\link{add_resource}}).
#'
#' @param .trj the trajectory object.
#' @inheritParams select
#' @param resource the name of the resource.
#' @param amount the amount to seize/release, accepts either a numeric or a
#' callable object (a function) which must return a numeric.
#' @param continue a boolean (if \code{post.seize} OR \code{reject} is defined)
#' or a pair of booleans (if \code{post.seize} AND \code{reject} are defined; if
#' only one value is provided, it will be recycled) to indicate whether these
#' subtrajectories should continue to the next activity in the main trajectory.
#' @param post.seize an optional trajectory object which will be followed after
#' a successful seize.
#' @param reject an optional trajectory object which will be followed if the
#' arrival is rejected (dropped).
#'
#' @details Rejection happens when a resource is at full capacity and there is
#' no room in the queue (either because there is a finite \code{queue_size} and
#' it is full, or because \code{queue_size=0} and thus it is disabled). In those
#' cases, the \code{reject} parameter defines a fallback trajectory. Note,
#' however, that, if the arrival is accepted (either in the queue or in the
#' server) and then it is dropped afterwards due to preemption or resource
#' shrinkage, then this trajectory will not be executed. Instead, see
#' \code{\link{handle_unfinished}} for another, more general, method for
#' handling all kinds of unfinished arrivals.
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{select}}, \code{\link{set_capacity}}, \code{\link{set_queue_size}},
#' \code{\link{set_capacity_selected}}, \code{\link{set_queue_size_selected}}
#'
#' @examples
#' ## simple seize, delay, then release
#' traj <- trajectory() %>%
#'   seize("doctor", 1) %>%
#'   timeout(3) %>%
#'   release("doctor", 1)
#'
#' simmer() %>%
#'   add_resource("doctor", capacity=1) %>%
#'   add_generator("patient", traj, at(0, 1)) %>%
#'   run() %>%
#'   get_mon_resources()
#'
#' ## arrival rejection (no space left in the queue)
#' traj <- trajectory() %>%
#'   log_("arriving...") %>%
#'   seize("doctor", 1) %>%
#'   # the second patient won't reach this point
#'   log_("doctor seized") %>%
#'   timeout(5) %>%
#'   release("doctor", 1)
#'
#' simmer() %>%
#'   add_resource("doctor", capacity=1, queue_size=0) %>%
#'   add_generator("patient", traj, at(0, 1)) %>%
#'   run() %>% invisible
#'
#' ## capturing rejection to retry
#' traj <- trajectory() %>%
#'   log_("arriving...") %>%
#'   seize(
#'     "doctor", 1, continue = FALSE,
#'     reject = trajectory() %>%
#'       log_("rejected!") %>%
#'       # go for a walk and try again
#'       timeout(2) %>%
#'       log_("retrying...") %>%
#'       rollback(amount = 4, times = Inf)) %>%
#'   # the second patient will reach this point after a couple of walks
#'   log_("doctor seized") %>%
#'   timeout(5) %>%
#'   release("doctor", 1) %>%
#'   log_("leaving")
#'
#' simmer() %>%
#'   add_resource("doctor", capacity=1, queue_size=0) %>%
#'   add_generator("patient", traj, at(0, 1)) %>%
#'   run() %>% invisible
#'
#' ## combining post.seize and reject
#' traj <- trajectory() %>%
#'   log_("arriving...") %>%
#'   seize(
#'     "doctor", 1, continue = c(TRUE, TRUE),
#'     post.seize = trajectory("admitted patient") %>%
#'       log_("admitted") %>%
#'       timeout(5) %>%
#'       release("doctor", 1),
#'     reject = trajectory("rejected patient") %>%
#'       log_("rejected!") %>%
#'       seize("nurse", 1) %>%
#'       timeout(2) %>%
#'       release("nurse", 1)) %>%
#'   # both patients will reach this point, as continue = c(TRUE, TRUE)
#'   timeout(10) %>%
#'   log_("leaving...")
#'
#' simmer() %>%
#'   add_resource("doctor", capacity=1, queue_size=0) %>%
#'   add_resource("nurse", capacity=10, queue_size=0) %>%
#'   add_generator("patient", traj, at(0, 1)) %>%
#'   run() %>% invisible
#'
#' @export
seize <- function(.trj, resource, amount=1, continue=NULL, post.seize=NULL, reject=NULL)
  UseMethod("seize")

#' @export
seize.trajectory <- function(.trj, resource, amount=1,
                             continue=NULL, post.seize=NULL, reject=NULL)
{
  continue <- recycle(continue, length(c(post.seize, reject)))
  stopifnot(length(continue) == length(c(post.seize, reject)))
  if (!length(continue)) continue <- TRUE
  check_args(resource="character", amount=c("numeric", "function"),
             continue="flag", post.seize=c("trajectory", "NULL"),
             reject=c("trajectory", "NULL"))

  trj <- as.list(c(post.seize[], reject[]))
  mask <- sum(c(1, 2) * !sapply(list(post.seize, reject), is.null))
  switch(
    binarise(is.function(amount)),
    add_activity(.trj, Seize__new(resource, positive(amount), continue, trj, mask)),
    add_activity(.trj, Seize__new_func(resource, amount, continue, trj, mask))
  )
}

#' @rdname seize
#' @export
seize_selected <- function(.trj, amount=1, id=0,continue=NULL, post.seize=NULL, reject=NULL)
  UseMethod("seize_selected")

#' @export
seize_selected.trajectory <- function(.trj, amount=1, id=0,
                                      continue=NULL, post.seize=NULL, reject=NULL)
{
  continue <- recycle(continue, length(c(post.seize, reject)))
  stopifnot(length(continue) == length(c(post.seize, reject)))
  if (!length(continue)) continue <- TRUE
  check_args(amount=c("numeric", "function"), id="numeric", continue="flag",
             post.seize=c("trajectory", "NULL"), reject=c("trajectory", "NULL"))

  trj <- as.list(c(post.seize[], reject[]))
  mask <- sum(c(1, 2) * !sapply(list(post.seize, reject), is.null))
  switch(
    binarise(is.function(amount)),
    add_activity(.trj, SeizeSelected__new(positive(id), positive(amount), continue, trj, mask)),
    add_activity(.trj, SeizeSelected__new_func(positive(id), amount, continue, trj, mask))
  )
}

#' @rdname seize
#' @export
release <- function(.trj, resource, amount=1) UseMethod("release")

#' @export
release.trajectory <- function(.trj, resource, amount=1) {
  check_args(resource="character", amount=c("numeric", "function"))

  switch(
    binarise(is.function(amount)),
    add_activity(.trj, Release__new(resource, positive(amount))),
    add_activity(.trj, Release__new_func(resource, amount))
  )
}

#' @rdname seize
#' @export
release_selected <- function(.trj, amount=1, id=0) UseMethod("release_selected")

#' @export
release_selected.trajectory <- function(.trj, amount=1, id=0) {
  check_args(amount=c("numeric", "function"), id="numeric")

  switch(
    binarise(is.function(amount)),
    add_activity(.trj, ReleaseSelected__new(positive(id), positive(amount))),
    add_activity(.trj, ReleaseSelected__new_func(positive(id), amount))
  )
}

#' @rdname seize
#' @export
release_all <- function(.trj, resource) UseMethod("release_all")

#' @export
release_all.trajectory <- function(.trj, resource) {
  if (missing(resource))
    return(add_activity(.trj, ReleaseAll__new_void()))
  check_args(resource="character")

  add_activity(.trj, ReleaseAll__new(resource))
}

#' @rdname seize
#' @export
release_selected_all <- function(.trj, id=0) UseMethod("release_selected_all")

#' @export
release_selected_all.trajectory <- function(.trj, id=0) {
  check_args(id="numeric")

  add_activity(.trj, ReleaseSelectedAll__new(positive(id)))
}

#' Set Resource Parameters
#'
#' Activities for dynamically modifying a resource's server capacity or queue
#' size, by name or a previously selected one. Resources must be defined in the
#' simulation environment (see \code{\link{add_resource}}).
#'
#' @inheritParams seize
#' @inheritParams select
#' @inheritParams set_attribute
#' @param resource the name of the resource.
#' @param value numeric value to set, or a callable object (a function) which must return a numeric value.
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{select}}, \code{\link{seize}}, \code{\link{release}},
#' \code{\link{seize_selected}}, \code{\link{release_selected}},
#' \code{\link{get_capacity}}, \code{\link{get_queue_size}}
#'
#' @examples
#' ## a resource with a queue size equal to the number of arrivals waiting
#' traj <- trajectory() %>%
#'   set_queue_size("res", 1, mod="+") %>%
#'   seize("res") %>%
#'   set_queue_size("res", -1, mod="+") %>%
#'   timeout(10) %>%
#'   release("res")
#'
#' simmer() %>%
#'   add_resource("res", 1, 0) %>%
#'   add_generator("dummy", traj, at(0:2)) %>%
#'   run() %>%
#'   get_mon_resources()
#'
#' @export
set_capacity <- function(.trj, resource, value, mod=c(NA, "+", "*"))
  UseMethod("set_capacity")

#' @export
set_capacity.trajectory <- function(.trj, resource, value, mod=c(NA, "+", "*")) {
  check_args(resource="character", value=c("numeric", "function"))
  mod <- match.arg(mod)

  switch(
    binarise(is.function(value)),
    add_activity(.trj, SetCapacity__new(resource, value, mod)),
    add_activity(.trj, SetCapacity__new_func(resource, value, mod))
  )
}

#' @rdname set_capacity
#' @export
set_capacity_selected <- function(.trj, value, id=0, mod=c(NA, "+", "*"))
  UseMethod("set_capacity_selected")

#' @export
set_capacity_selected.trajectory <- function(.trj, value, id=0, mod=c(NA, "+", "*")) {
  check_args(value=c("numeric", "function"), id="numeric")
  mod <- match.arg(mod)

  switch(
    binarise(is.function(value)),
    add_activity(.trj, SetCapacitySelected__new(positive(id), value, mod)),
    add_activity(.trj, SetCapacitySelected__new_func(positive(id), value, mod))
  )
}

#' @rdname set_capacity
#' @export
set_queue_size <- function(.trj, resource, value, mod=c(NA, "+", "*"))
  UseMethod("set_queue_size")

#' @export
set_queue_size.trajectory <- function(.trj, resource, value, mod=c(NA, "+", "*")) {
  check_args(resource="character", value=c("numeric", "function"))
  mod <- match.arg(mod)

  switch(
    binarise(is.function(value)),
    add_activity(.trj, SetQueue__new(resource, value, mod)),
    add_activity(.trj, SetQueue__new_func(resource, value, mod))
  )
}

#' @rdname set_capacity
#' @export
set_queue_size_selected <- function(.trj, value, id=0, mod=c(NA, "+", "*"))
  UseMethod("set_queue_size_selected")

#' @export
set_queue_size_selected.trajectory <- function(.trj, value, id=0, mod=c(NA, "+", "*")) {
  check_args(value=c("numeric", "function"), id="numeric")
  mod <- match.arg(mod)

  switch(
    binarise(is.function(value)),
    add_activity(.trj, SetQueueSelected__new(positive(id), value, mod)),
    add_activity(.trj, SetQueueSelected__new_func(positive(id), value, mod))
  )
}

#' Select Resources
#'
#' Activity for selecting a resource for a subsequent seize/release or setting
#' its parameters (capacity or queue size). Resources must be defined in the
#' simulation environment (see \code{\link{add_resource}}).
#'
#' @inheritParams seize
#' @param resources one or more resource names, or a callable object (a function)
#' which must return one or more resource names.
#' @param policy if \code{resources} is a character vector, this parameter
#' determines the criteria for selecting a resource among the set of policies
#' available (see details).
#' @param id selection identifier for nested usage.
#'
#' @return Returns the trajectory object.
#'
#' @details The 'shortest-queue' policy selects the least busy resource;
#' 'round-robin' selects resources in cyclical order; 'first-available' selects
#' the first resource available, and 'random' selects a resource randomly.
#'
#' All the 'available'-ending policies ('first-available', but also
#' 'shortest-queue-available', 'round-robin-available' and 'random-available')
#' check for resource availability (i.e., whether the capacity is non-zero),
#' and exclude from the selection procedure those resources with capacity set
#' to zero. This means that, for these policies, an error will be raised if all
#' resources are unavailable.
#'
#' @seealso \code{\link{seize_selected}}, \code{\link{release_selected}},
#' \code{\link{set_capacity_selected}}, \code{\link{set_queue_size_selected}}
#'
#' @examples
#' ## predefined policy
#' traj <- trajectory() %>%
#'   select(paste0("doctor", 1:3), "round-robin") %>%
#'   seize_selected(1) %>%
#'   timeout(5) %>%
#'   release_selected(1)
#'
#' simmer() %>%
#'   add_resource("doctor1") %>%
#'   add_resource("doctor2") %>%
#'   add_resource("doctor3") %>%
#'   add_generator("patient", traj, at(0, 1, 2)) %>%
#'   run() %>%
#'   get_mon_resources()
#'
#' ## custom policy
#' env <- simmer()
#' res <- paste0("doctor", 1:3)
#'
#' traj <- trajectory() %>%
#'   select(function() {
#'     occ <- get_server_count(env, res) + get_queue_count(env, res)
#'     res[which.min(occ)[1]]
#'   }) %>%
#'   seize_selected(1) %>%
#'   timeout(5) %>%
#'   release_selected(1)
#'
#' for (i in res) env %>%
#'   add_resource(i)
#' env %>%
#'   add_generator("patient", traj, at(0, 1, 2)) %>%
#'   run() %>%
#'   get_mon_resources()
#'
#' @export
select <- function(
  .trj, resources,
  policy=c("shortest-queue", "shortest-queue-available",
          "round-robin", "round-robin-available",
          "first-available", "random", "random-available"),
  id=0) UseMethod("select")

#' @export
select.trajectory <- function(
  .trj, resources,
  policy=c("shortest-queue", "shortest-queue-available",
           "round-robin", "round-robin-available",
           "first-available", "random", "random-available"),
  id=0)
{
  check_args(resources=c("character", "function"), id="numeric")
  policy <- match.arg(policy)

  switch(
    binarise(is.function(resources)),
    add_activity(.trj, Select__new(resources, policy, positive(id))),
    add_activity(.trj, Select__new_func(resources, policy, positive(id)))
  )
}

#' Delay
#'
#' Activity for inserting delays and execute user-defined tasks.
#'
#' @inheritParams seize
#' @param task the timeout duration supplied by either passing a numeric or a
#' callable object (a function) which must return a numeric (negative values are
#' automatically coerced to positive).
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{set_attribute}}, \code{\link{set_global}}
#'
#' @examples
#' env <- simmer()
#'
#' traj <- trajectory() %>%
#'
#'   # static delay
#'   timeout(3) %>%
#'
#'   # dynamic, exponential delay
#'   timeout(function() rexp(1, 10)) %>%
#'
#'   # dependent on an attribute
#'   set_attribute("delay", 2) %>%
#'   set_global("other", function() rexp(1, 2)) %>%
#'   timeout_from_attribute("delay") %>%
#'   timeout_from_global("other")
#'
#' env %>%
#'   add_generator("dummy", traj, at(0)) %>%
#'   run() %>%
#'   get_mon_arrivals()
#'
#' @export
timeout <- function(.trj, task) UseMethod("timeout")

#' @export
timeout.trajectory <- function(.trj, task) {
  check_args(task=c("numeric", "function"))

  switch(
    binarise(is.function(task)),
    add_activity(.trj, Timeout__new(task)),
    add_activity(.trj, Timeout__new_func(task))
  )
}

#' @rdname timeout
#' @inheritParams set_attribute
#' @param key the attribute name, or a callable object (a function) which
#' must return the attribute name.
#' @seealso \code{\link{set_attribute}}, \code{\link{set_global}}
#' @export
timeout_from_attribute <- function(.trj, key) UseMethod("timeout_from_attribute")

#' @export
timeout_from_attribute.trajectory <- function(.trj, key) {
  check_args(key="character")

  add_activity(.trj, Timeout__new_attr(key, FALSE))
}

#' @rdname timeout
#' @export
timeout_from_global <- function(.trj, key) UseMethod("timeout_from_global")

#' @export
timeout_from_global <- function(.trj, key) {
  check_args(key="character")

  add_activity(.trj, Timeout__new_attr(key, TRUE))
}

#' Set Attributes
#'
#' Activity for modifying attributes. Attributes defined with
#' \code{set_attribute} are \emph{per arrival}, meaning that each arrival has
#' its own set of attributes, not visible by any other one. On the other hand,
#' attributes defined with \code{set_global} are shared by all the arrivals in
#' the simulation.
#'
#' @inheritParams seize
#' @param keys the attribute name(s), or a callable object (a function) which
#' must return attribute name(s).
#' @param values numeric value(s) to set, or a callable object (a function) which
#' must return numeric value(s).
#' @param mod if set, \code{values} modify the attributes rather than substituting them.
#' @param init initial value, applied if \code{mod} is set and the attribute was
#' not previously initialised. Useful for counters or indexes.
#'
#' @return Returns the trajectory object.
#'
#' @details Attribute monitoring is disabled by default. To enable it, set
#' \code{mon=2} in the corresponding source (see, e.g., \code{\link{add_generator}}).
#' Then, the evolution of the attributes during the simulation can be retrieved
#' with \code{\link{get_mon_attributes}}. Global attributes are reported as
#' unnamed key/value pairs.
#'
#' @seealso \code{\link{get_attribute}}, \code{\link{get_global}},
#' \code{\link{timeout_from_attribute}}, \code{\link{timeout_from_global}}
#'
#' @examples
#' env <- simmer()
#'
#' traj <- trajectory() %>%
#'
#'   # simple assignment
#'   set_attribute("my_key", 123) %>%
#'   set_global("global_key", 321) %>%
#'
#'   # more than one assignment at once
#'   set_attribute(c("my_key", "other_key"), c(5, 64)) %>%
#'
#'   # increment
#'   set_attribute("my_key", 1, mod="+") %>%
#'
#'   # assignment using a function
#'   set_attribute("independent_key", function() runif(1)) %>%
#'
#'   # assignment dependent on another attribute
#'   set_attribute("dependent_key", function()
#'     ifelse(get_attribute(env, "my_key") <= 0.5, 1, 0))
#'
#' env %>%
#'   add_generator("dummy", traj, at(3), mon=2) %>%
#'   run() %>%
#'   get_mon_attributes()
#'
#' @export
set_attribute <- function(.trj, keys, values, mod=c(NA, "+", "*"), init=0)
  UseMethod("set_attribute")

#' @export
set_attribute.trajectory <- function(.trj, keys, values, mod=c(NA, "+", "*"), init=0) {
  check_args(keys=c("character", "function"), values=c("numeric", "function"),
             init="numeric")
  mod <- match.arg(mod)

  switch(
    binarise(is.function(keys), is.function(values)),
    add_activity(.trj, SetAttribute__new(keys, values, FALSE, mod, init)),
    add_activity(.trj, SetAttribute__new_func1(keys, values, FALSE, mod, init)),
    add_activity(.trj, SetAttribute__new_func2(keys, values, FALSE, mod, init)),
    add_activity(.trj, SetAttribute__new_func3(keys, values, FALSE, mod, init))
  )
}

#' @rdname set_attribute
#' @export
set_global <- function(.trj, keys, values, mod=c(NA, "+", "*"), init=0)
  UseMethod("set_global")

#' @export
set_global.trajectory <- function(.trj, keys, values, mod=c(NA, "+", "*"), init=0) {
  check_args(keys=c("character", "function"), values=c("numeric", "function"),
             init="numeric")
  mod <- match.arg(mod)

  switch(
    binarise(is.function(keys), is.function(values)),
    add_activity(.trj, SetAttribute__new(keys, values, TRUE, mod, init)),
    add_activity(.trj, SetAttribute__new_func1(keys, values, TRUE, mod, init)),
    add_activity(.trj, SetAttribute__new_func2(keys, values, TRUE, mod, init)),
    add_activity(.trj, SetAttribute__new_func3(keys, values, TRUE, mod, init))
  )
}

#' Activate/Deactivate Sources
#'
#' Activities for activating or deactivating the generation of arrivals by name.
#' Sources must be defined in the simulation environment (see
#' \code{\link{add_generator}}, \code{\link{add_dataframe}}).
#'
#' @inheritParams seize
#' @param sources the name(s) of the source(s) or a function returning the name(s).
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{set_trajectory}}, \code{\link{set_source}}
#'
#' @examples
#' traj <- trajectory() %>%
#'   deactivate("dummy") %>%
#'   timeout(1) %>%
#'   activate("dummy")
#'
#' simmer() %>%
#'   add_generator("dummy", traj, function() 1) %>%
#'   run(10) %>%
#'   get_mon_arrivals()
#'
#' @export
activate <- function(.trj, sources) UseMethod("activate")

#' @export
activate.trajectory <- function(.trj, sources) {
  check_args(sources=c("character", "function"))

  switch(
    binarise(is.function(sources)),
    add_activity(.trj, Activate__new(sources)),
    add_activity(.trj, Activate__new_func(sources))
  )
}

#' @rdname activate
#' @export
deactivate <- function(.trj, sources) UseMethod("deactivate")

#' @export
deactivate.trajectory <- function(.trj, sources) {
  check_args(sources=c("character", "function"))

  switch(
    binarise(is.function(sources)),
    add_activity(.trj, Deactivate__new(sources)),
    add_activity(.trj, Deactivate__new_func(sources))
  )
}

#' Set Source Parameters
#'
#' Activities for modifying a source's trajectory or source object by name.
#' Sources must be defined in the simulation environment (see
#' \code{\link{add_generator}}, \code{\link{add_dataframe}}).
#'
#' @inheritParams seize
#' @inheritParams activate
#' @param trajectory the trajectory that the generated arrivals will follow.
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{activate}}, \code{\link{deactivate}}
#'
#' @examples
#' traj1 <- trajectory() %>%
#'   timeout(1)
#'
#' traj2 <- trajectory() %>%
#'   set_source("dummy", function() 1) %>%
#'   set_trajectory("dummy", traj1) %>%
#'   timeout(2)
#'
#' simmer() %>%
#'   add_generator("dummy", traj2, function() 2) %>%
#'   run(6) %>%
#'   get_mon_arrivals()
#'
#' @export
set_trajectory <- function(.trj, sources, trajectory) UseMethod("set_trajectory")

#' @export
set_trajectory.trajectory <- function(.trj, sources, trajectory) {
  check_args(sources=c("character", "function"), trajectory="trajectory")

  switch(
    binarise(is.function(sources)),
    add_activity(.trj, SetTraj__new(sources, trajectory[])),
    add_activity(.trj, SetTraj__new_func(sources, trajectory[]))
  )
}

#' @rdname set_trajectory
#' @param object a function modelling the interarrival times (if the source type
#' is a generator; returning a negative value or a missing value stops the
#' generator) or a data frame (if the source type is a data source).
#' @export
set_source <- function(.trj, sources, object) UseMethod("set_source")

#' @export
set_source.trajectory <- function(.trj, sources, object) {
  check_args(sources=c("character", "function"), object=c("function", "data.frame"))

  switch(
    binarise(is.function(sources), is.function(object)),
    add_activity(.trj, SetSourceDF__new(sources, object)),
    add_activity(.trj, SetSourceDF__new_func(sources, object)),
    add_activity(.trj, SetSourceFn__new(sources, make_resetable(object))),
    add_activity(.trj, SetSourceFn__new_func(sources, make_resetable(object)))
  )
}

#' Set Prioritization Values
#'
#' Activity for dynamically modifying an arrival's prioritization values.
#' Default prioritization values are defined by the source (see
#' \code{\link{add_generator}}, \code{\link{add_dataframe}}).
#'
#' @inheritParams seize
#' @inheritParams set_attribute
#' @param values expects either a vector/list or a callable object (a function)
#' returning a vector/list of three values \code{c(priority, preemptible, restart)}.
#' A negative value leaves the corresponding parameter unchanged.
#' See \code{\link{add_generator}} for more information about these parameters.
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{get_prioritization}}
#'
#' @examples
#' traj <- trajectory() %>%
#'
#'   # static values
#'   set_prioritization(c(3, 7, TRUE)) %>%
#'
#'   # increment
#'   set_prioritization(c(2, 1, 0), mod="+") %>%
#'
#'   # dynamic, custom
#'   set_attribute("priority", 3) %>%
#'   set_prioritization(function() {
#'     prio <- get_prioritization(env)
#'     attr <- get_attribute(env, "priority")
#'     c(attr, prio[[2]]+1, FALSE)
#'   })
#'
#' @export
set_prioritization <- function(.trj, values, mod=c(NA, "+", "*"))
  UseMethod("set_prioritization")

#' @export
set_prioritization.trajectory <- function(.trj, values, mod=c(NA, "+", "*")) {
  check_args(values=c("numeric", "function"))
  mod <- match.arg(mod)

  switch(
    binarise(is.function(values)),
    add_activity(.trj, SetPrior__new(values, mod)),
    add_activity(.trj, SetPrior__new_func(values, mod))
  )
}

#' Fork the Trajectory Path
#'
#' Activity for defining a fork with \code{N} alternative sub-trajectories.
#'
#' @inheritParams seize
#' @param option a callable object (a function) which must return an integer between
#' \code{0} and \code{N}. A return value equal to \code{0} skips the branch and
#' continues to the next activity. A returning value between \code{1} to \code{N}
#' makes the arrival to follow the corresponding sub-trajectory.
#' @param continue a vector of \code{N} booleans that indicate whether the arrival
#' must continue to the main trajectory after each sub-trajectory or not (if only
#' one value is provided, it will be recycled to match the number of sub-trajectories).
#' @param ... \code{N} trajectory objects (or a list of \code{N} trajectory objects)
#' describing each sub-trajectory.
#'
#' @return Returns the trajectory object.
#'
#' @examples
#' env <- simmer()
#'
#' traj <- trajectory() %>%
#'   set_global("path", 1, mod="+", init=-1) %>%
#'   log_(function() paste("Path", get_global(env, "path"), "selected")) %>%
#'   branch(
#'     function() get_global(env, "path"), continue=c(TRUE, FALSE),
#'     trajectory() %>%
#'       log_("following path 1"),
#'     trajectory() %>%
#'       log_("following path 2")) %>%
#'   log_("continuing after the branch (path 0)")
#'
#' env %>%
#'   add_generator("dummy", traj, at(0:2)) %>%
#'   run() %>% invisible
#'
#' @export
branch <- function(.trj, option, continue, ...) UseMethod("branch")

#' @export
branch.trajectory <- function(.trj, option, continue, ...) {
  dots. <- c(...)
  check_args(option="function", continue="flag", dots.="trajectory")
  continue <- recycle(continue, length(dots.))
  stopifnot(length(continue) == length(dots.))

  add_activity(.trj, Branch__new(option, continue, sapply(dots., `[`)))
}

#' Rollback a Number of Activities
#'
#' Activity for going backwards to a previous point in the trajectory. Useful to
#' implement loops.
#'
#' @inheritParams seize
#' @param amount the amount of activities (of the same or parent trajectories)
#' to roll back.
#' @param times the number of repetitions until an arrival may continue.
#' @param check a callable object (a function) which must return a boolean. If
#' present, the \code{times} parameter is ignored, and the activity uses this
#' function to check whether the rollback must be done or not.
#'
#' @return Returns the trajectory object.
#'
#' @examples
#' ## rollback a specific number of times
#' traj <- trajectory() %>%
#'   log_("hello!") %>%
#'   timeout(1) %>%
#'   rollback(2, 3)
#'
#' simmer() %>%
#'   add_generator("hello_sayer", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' ## custom check
#' env <- simmer()
#'
#' traj <- trajectory() %>%
#'   set_attribute("var", 0) %>%
#'   log_(function()
#'     paste("attribute level is at:", get_attribute(env, "var"))) %>%
#'   set_attribute("var", 25, mod="+") %>%
#'   rollback(2, check=function() get_attribute(env, "var") < 100) %>%
#'   log_("done")
#'
#' env %>%
#'   add_generator("dummy", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' @export
rollback <- function(.trj, amount, times=Inf, check=NULL) UseMethod("rollback")

#' @export
rollback.trajectory <- function(.trj, amount, times=Inf, check=NULL) {
  check_args(amount="numeric", times="numeric", check=c("function", "NULL"))

  switch(
    binarise(is.function(check)),
    add_activity(.trj, Rollback__new(positive(amount), positive(times))),
    add_activity(.trj, Rollback__new_func(amount, check))
  )
}

#' Renege on some Condition
#'
#' Activities for leaving with some probability, or for setting or unsetting a
#' timer or a signal after which the arrival will abandon.
#'
#' @inheritParams seize
#' @param prob a probability or a function returning a probability.
#' @param out optional sub-trajectory in case of reneging.
#' @param keep_seized whether to keep already seized resources. By default, all
#' resources are released.
#'
#' @return Returns the trajectory object.
#'
#' @details Arrivals that leave the trajectory will set the \code{finished} flag
#' to \code{FALSE} in the output of \code{\link{get_mon_arrivals}}. Unfinished
#' arrivals can be handled with a drop-out trajectory that can be set using the
#' optional argument \code{out} or the \code{\link{handle_unfinished}} activity.
#'
#' Note that, for historical reasons, \code{leave} has \code{keep_seized=TRUE}
#' by default, while \code{renege_*} does not.
#'
#' @seealso \code{\link{handle_unfinished}}
#' @examples
#' ## leave with some probability
#' set.seed(1234)
#'
#' traj <- trajectory() %>%
#'   log_("leave with some probability") %>%
#'   leave(function() runif(1) < 0.5) %>%
#'   log_("didn't leave")
#'
#' simmer() %>%
#'   add_generator("dummy", traj, at(0, 1)) %>%
#'   run() %>% invisible
#'
#' @name renege
#' @export
leave <- function(.trj, prob, out=NULL, keep_seized=TRUE) UseMethod("leave")

#' @export
leave.trajectory <- function(.trj, prob, out=NULL, keep_seized=TRUE) {
  check_args(prob=c("numeric", "function"), out=c("trajectory", "NULL"),
             keep_seized="flag")

  traj <- as.list(c(out[]))
  switch(
    binarise(is.function(prob)),
    add_activity(.trj, Leave__new(positive(prob), traj, keep_seized)),
    add_activity(.trj, Leave__new_func(prob, traj, keep_seized))
  )
}


#' @param t timeout to trigger reneging, accepts either a numeric or a callable
#' object (a function) which must return a numeric.
#'
#' @details Note that \code{renege_if} works similarly to \code{\link{trap}},
#' but in contrast to that, reneging is triggered even if the arrival is waiting
#' in a queue or is part of a non-permanent \code{\link{batch}}.
#'
#' @examples
#' ## reneging after some time
#' bank <- trajectory() %>%
#'   log_("here I am") %>%
#'   # renege in 5 minutes
#'   renege_in(
#'     5,
#'     out = trajectory() %>%
#'       log_("lost my patience. Reneging...")) %>%
#'   seize("clerk") %>%
#'   # stay if I'm being attended within 5 minutes
#'   renege_abort() %>%
#'   log_("I'm being attended") %>%
#'   timeout(10) %>%
#'   release("clerk") %>%
#'   log_("finished")
#'
#' simmer() %>%
#'   add_resource("clerk", 1) %>%
#'   add_generator("customer", bank, at(0, 1)) %>%
#'   run() %>% invisible
#'
#' @rdname renege
#' @export
renege_in <- function(.trj, t, out=NULL, keep_seized=FALSE)
  UseMethod("renege_in")

#' @export
renege_in.trajectory <- function(.trj, t, out=NULL, keep_seized=FALSE) {
  check_args(t=c("numeric", "function"), out=c("trajectory", "NULL"),
             keep_seized="flag")

  traj <- as.list(c(out[]))
  switch(
    binarise(is.function(t)),
    add_activity(.trj, RenegeIn__new(positive(t), traj, keep_seized)),
    add_activity(.trj, RenegeIn__new_func(t, traj, keep_seized))
  )
}

#' @param signal signal to trigger reneging, accepts either a string or a
#' callable object (a function) which must return a string.
#'
#' @rdname renege
#' @seealso \code{\link{send}}
#' @export
renege_if <- function(.trj, signal, out=NULL, keep_seized=FALSE)
  UseMethod("renege_if")

#' @export
renege_if.trajectory <- function(.trj, signal, out=NULL, keep_seized=FALSE) {
  check_args(signal=c("character", "function"), out=c("trajectory", "NULL"),
             keep_seized="flag")

  traj <- as.list(c(out[]))
  switch(
    binarise(is.function(signal)),
    add_activity(.trj, RenegeIf__new(signal, traj, keep_seized)),
    add_activity(.trj, RenegeIf__new_func(signal, traj, keep_seized))
  )
}

#' @rdname renege
#' @export
renege_abort <- function(.trj) UseMethod("renege_abort")

#' @export
renege_abort.trajectory <- function(.trj) add_activity(.trj, RenegeAbort__new())

#' Handle Unfinished Arrivals
#'
#' Activity for setting a drop-out trajectory for unfinished arrivals, i.e.,
#' those dropped from a resource (due to preemption, resource shrinkage or a
#' rejected \code{\link{seize}}) or those that \code{\link{leave}} a trajectory.
#'
#' @inheritParams seize
#' @param handler trajectory object to handle unfinished arrivals. A \code{NULL}
#' value will unset the drop-out trajectory.
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{leave}}, \code{\link{set_capacity}}
#'
#' @examples
#' traj <- trajectory() %>%
#'   log_("arrived") %>%
#'   handle_unfinished(
#'     trajectory() %>%
#'       log_("preempted!")) %>%
#'   seize("res") %>%
#'   log_("resource seized") %>%
#'   timeout(10) %>%
#'   release("res") %>%
#'   log_("leaving")
#'
#' simmer() %>%
#'   add_resource("res", 1, 0, preemptive=TRUE, queue_size_strict=TRUE) %>%
#'   add_generator("dummy", traj, at(0)) %>%
#'   add_generator("priority_dummy", traj, at(5), priority=1) %>%
#'   run() %>% invisible
#'
#' @export
handle_unfinished <- function(.trj, handler) UseMethod("handle_unfinished")

#' @export
handle_unfinished.trajectory <- function(.trj, handler) {
  check_args(handler=c("trajectory", "NULL"))

  traj <- as.list(c(handler[]))
  add_activity(.trj, HandleUnfinished__new(traj))
}

#' Clone/Synchronize Arrivals
#'
#' Activities for defining a parallel fork and removing the copies. \code{clone}
#' replicates an arrival \code{n} times (the original one + \code{n-1} copies).
#' \code{synchronize} removes all but one clone for each set of clones.
#'
#' @inheritParams seize
#' @param n number of clones, accepts either a numeric or a callable object
#' (a function) which must return a numeric.
#' @param ... a number of optional parallel sub-trajectories (or a list of
#' sub-trajectories). Each clone will follow a different sub-trajectory if available.
#'
#' @return Returns the trajectory object.
#'
#' @examples
#' ## clone and wait for the others
#' traj <- trajectory() %>%
#'   clone(
#'     n = 3,
#'     trajectory() %>%
#'       log_("clone 0 (original)") %>%
#'       timeout(1),
#'     trajectory() %>%
#'       log_("clone 1") %>%
#'       timeout(2),
#'     trajectory() %>%
#'       log_("clone 2") %>%
#'       timeout(3)) %>%
#'   log_("sync reached") %>%
#'   synchronize(wait = TRUE) %>%
#'   log_("leaving")
#'
#' simmer() %>%
#'   add_generator("arrival", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' ## more clones that trajectories available
#' traj <- trajectory() %>%
#'   clone(
#'     n = 5,
#'     trajectory() %>%
#'       log_("clone 0 (original)") %>%
#'       timeout(1)) %>%
#'   log_("sync reached") %>%
#'   synchronize(wait = TRUE) %>%
#'   log_("leaving")
#'
#' simmer() %>%
#'   add_generator("arrival", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' ## clone and continue without waiting
#' traj <- trajectory() %>%
#'   clone(
#'     n = 3,
#'     trajectory() %>%
#'       log_("clone 0 (original)") %>%
#'       timeout(1),
#'     trajectory() %>%
#'       log_("clone 1") %>%
#'       timeout(2),
#'     trajectory() %>%
#'       log_("clone 2") %>%
#'       timeout(3)) %>%
#'   log_("sync reached") %>%
#'   synchronize(wait = FALSE) %>%
#'   log_("leaving")
#'
#' simmer() %>%
#'   add_generator("arrival", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' @export
clone <- function(.trj, n, ...) UseMethod("clone")

#' @export
clone.trajectory <- function(.trj, n, ...) {
  dots. <- c(...)
  check_args(n=c("numeric", "function"), dots.="trajectory")

  trj <- sapply(dots., `[`)
  switch(
    binarise(is.function(n)),
    add_activity(.trj, Clone__new(positive(n), trj)),
    add_activity(.trj, Clone__new_func(n, trj))
  )
}

#' @inheritParams seize
#' @param wait if \code{FALSE}, all clones but the first to arrive are removed.
#' if \code{TRUE} (default), all clones but the last to arrive are removed.
#' @param mon_all if \code{TRUE}, \code{get_mon_arrivals} will show one
#' line per clone.
#'
#' @rdname clone
#' @export
synchronize <- function(.trj, wait=TRUE, mon_all=FALSE) UseMethod("synchronize")

#' @export
synchronize.trajectory <- function(.trj, wait=TRUE, mon_all=FALSE) {
  check_args(wait="flag", mon_all="flag")

  add_activity(.trj, Synchronize__new(wait, mon_all))
}

#' Batch/Separate Arrivals
#'
#' Activities for collecting a number of arrivals before they can continue processing
#' and splitting a previously established batch.
#'
#' @inheritParams seize
#' @param n batch size, accepts a numeric.
#' @param timeout set an optional timer which triggers batches every
#' \code{timeout} time units even if the batch size has not been fulfilled,
#' accepts a numeric or a callable object (a function) which must return a
#' numeric (0 = disabled).
#' @param permanent if \code{TRUE}, batches cannot be split.
#' @param name optional string. Unnamed batches from different \code{batch}
#' activities are independent. However, if you want to feed arrivals from
#' different trajectories into a same batch, you need to specify a common name
#' across all your \code{batch} activities.
#' @param rule an optional callable object (a function) which will be applied to
#' every arrival to determine whether it should be included into the batch, thus
#' it must return a boolean.
#'
#' @return Returns the trajectory object.
#'
#' @examples
#' ## unnamed batch with a timeout
#' traj <- trajectory() %>%
#'   log_("arrived") %>%
#'   batch(2, timeout=5) %>%
#'   log_("in a batch") %>%
#'   timeout(5) %>%
#'   separate() %>%
#'   log_("leaving")
#'
#' simmer() %>%
#'   add_generator("dummy", traj, at(0:2)) %>%
#'   run() %>% invisible
#'
#' ## batching based on some dynamic rule
#' traj <- trajectory() %>%
#'   log_("arrived") %>%
#'   # always FALSE -> no batches
#'   batch(2, rule=function() FALSE) %>%
#'   log_("not in a batch") %>%
#'   timeout(5) %>%
#'   separate() %>%
#'   log_("leaving")
#'
#' simmer() %>%
#'   add_generator("dummy", traj, at(0:2)) %>%
#'   run() %>% invisible
#'
#' ## named batch, shared across trajectories
#' traj0 <- trajectory() %>%
#'   log_("arrived traj0") %>%
#'   batch(2, name = "mybatch")
#'
#' traj1 <- trajectory() %>%
#'   log_("arrived traj1") %>%
#'   timeout(1) %>%
#'   batch(2, name = "mybatch") %>%
#'   log_("in a batch") %>%
#'   timeout(2) %>%
#'   separate() %>%
#'   log_("leaving traj1")
#'
#' simmer() %>%
#'   add_generator("dummy0", traj0, at(0)) %>%
#'   add_generator("dummy1", traj1, at(0)) %>%
#'   run() %>% invisible
#'
#' @export
batch <- function(.trj, n, timeout=0, permanent=FALSE, name="", rule=NULL)
  UseMethod("batch")

#' @export
batch.trajectory <- function(.trj, n, timeout=0, permanent=FALSE, name="", rule=NULL) {
  check_args(n="numeric", timeout=c("numeric", "function"), permanent="flag",
             name="character", rule=c("function", "NULL"))

  switch(
    binarise(is.function(timeout), is.function(rule)),
    add_activity(.trj, Batch__new(positive(n), timeout, permanent, name)),
    add_activity(.trj, Batch__new_func1(positive(n), timeout, permanent, name)),
    add_activity(.trj, Batch__new_func2(positive(n), timeout, permanent, name, rule)),
    add_activity(.trj, Batch__new_func3(positive(n), timeout, permanent, name, rule))
  )
}

#' @inheritParams seize
#'
#' @rdname batch
#' @export
separate <- function(.trj) UseMethod("separate")

#' @export
separate.trajectory <- function(.trj) add_activity(.trj, Separate__new())

#' Inter-arrival Communication
#'
#' These activities enable asynchronous programming. \code{send()} broadcasts a
#' signal or a list of signals. Arrivals can subscribe to signals and (optionally)
#' assign a handler with \code{trap()}. Note that, while inside a batch, all the
#' signals subscribed before entering the batch are ignored. Upon a signal
#' reception, the arrival stops the current activity and executes the handler
#' (if provided). Then, the execution returns to the activity following the
#' point of the interruption. \code{untrap()} can be used to unsubscribe from
#' signals. \code{wait()} blocks until a signal is received.
#'
#' @inheritParams seize
#' @param signals signal or list of signals, accepts either a string, a list of
#' strings or a callable object (a function) which must return a string or a
#' list of strings.
#' @param delay optional timeout to trigger the signals, accepts either a
#' numeric or a callable object (a function) which must return a numeric.
#'
#' @return Returns the trajectory object.
#'
#' @seealso \code{\link{renege_if}}
#'
#' @examples
#' ## block, signal and continue with a handler
#' signal <- "you shall pass"
#'
#' t_blocked <- trajectory() %>%
#'   trap(
#'     signal,
#'     trajectory() %>%
#'       log_("executing the handler")) %>%
#'   log_("waiting...") %>%
#'   wait() %>%
#'   log_("continuing!")
#'
#' t_signaler <- trajectory() %>%
#'   log_(signal) %>%
#'   send(signal)
#'
#' simmer() %>%
#'   add_generator("blocked", t_blocked, at(0)) %>%
#'   add_generator("signaler", t_signaler, at(5)) %>%
#'   run() %>% invisible
#'
#' ## handlers can be interrupted, unless interruptible=FALSE
#' t_worker <- trajectory() %>%
#'   trap(
#'   signal,
#'   handler = trajectory() %>%
#'     log_("ok, I'm packing...") %>%
#'     timeout(1)) %>%
#'   log_("performing a looong task...") %>%
#'   timeout(100) %>%
#'   log_("and I'm leaving!")
#'
#' simmer() %>%
#'   add_generator("worker", t_worker, at(0)) %>%
#'   add_generator("signaler", t_signaler, at(5, 5.5)) %>%
#'   run() %>% invisible
#'
#' @export
send <- function(.trj, signals, delay=0) UseMethod("send")

#' @export
send.trajectory <- function(.trj, signals, delay=0) {
  check_args(signals=c("character", "function"), delay=c("numeric", "function"))

  switch(
    binarise(is.function(signals), is.function(delay)),
    add_activity(.trj, Send__new(signals, positive(delay))),
    add_activity(.trj, Send__new_func1(signals, positive(delay))),
    add_activity(.trj, Send__new_func2(signals, delay)),
    add_activity(.trj, Send__new_func3(signals, delay))
  )
}

#' @param handler optional trajectory object to handle a signal received.
#' @param interruptible whether the handler can be interrupted by signals.
#'
#' @rdname send
#' @export
trap <- function(.trj, signals, handler=NULL, interruptible=TRUE) UseMethod("trap")

#' @export
trap.trajectory <- function(.trj, signals, handler=NULL, interruptible=TRUE) {
  check_args(signals=c("character", "function"), handler=c("trajectory", "NULL"),
             interruptible="flag")

  traj <- as.list(c(handler[]))
  switch(
    binarise(is.function(signals)),
    add_activity(.trj, Trap__new(signals, traj, interruptible)),
    add_activity(.trj, Trap__new_func(signals, traj, interruptible))
  )
}

#' @rdname send
#' @export
untrap <- function(.trj, signals) UseMethod("untrap")

#' @export
untrap.trajectory <- function(.trj, signals) {
  check_args(signals=c("character", "function"))

  switch(
    binarise(is.function(signals)),
    add_activity(.trj, UnTrap__new(signals)),
    add_activity(.trj, UnTrap__new_func(signals))
  )
}

#' @rdname send
#' @export
wait <- function(.trj) UseMethod("wait")

#' @export
wait.trajectory <- function(.trj) add_activity(.trj, Wait__new())

#' Debugging
#'
#' Activities for displaying messages preceded by the simulation time and the
#' name of the arrival, and for setting conditional breakpoints.
#'
#' @inheritParams seize
#' @param message the message to display, accepts either a string or a callable object
#' (a function) which must return a string.
#' @param level debugging level. The \code{message} will be printed if, and only if,
#' the \code{level} provided is less or equal to the \code{log_level} defined in the
#' simulation environment (see \code{\link{simmer}}).
#' @param condition a boolean or a function returning a boolean.
#'
#' @return Returns the trajectory object.
#'
#' @examples
#' ## log levels
#' traj <- trajectory() %>%
#'   log_("this is always printed") %>% # level = 0 by default
#'   log_("this is printed if `log_level>=1`", level = 1) %>%
#'   log_("this is printed if `log_level>=2`", level = 2)
#'
#' simmer() %>%
#'   add_generator("dummy", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' simmer(log_level = 1) %>%
#'   add_generator("dummy", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' simmer(log_level = Inf) %>%
#'   add_generator("dummy", traj, at(0)) %>%
#'   run() %>% invisible
#'
#' @export
log_ <- function(.trj, message, level=0) UseMethod("log_")

#' @export
log_.trajectory <- function(.trj, message, level=0) {
  check_args(message=c("character", "function"), level="numeric")

  switch(
    binarise(is.function(message)),
    add_activity(.trj, Log__new(message, positive(level))),
    add_activity(.trj, Log__new_func(message, positive(level)))
  )
}

#' @rdname log_
#' @export
stop_if <- function(.trj, condition) UseMethod("stop_if")

#' @export
stop_if.trajectory <- function(.trj, condition) {
  check_args(condition=c("logical", "function"))

  switch(
    binarise(is.function(condition)),
    add_activity(.trj, StopIf__new(condition)),
    add_activity(.trj, StopIf__new_func(condition))
  )
}
