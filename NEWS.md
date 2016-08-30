# Changes in version 3.4.1.9000

## Minor changes and fixes

* Simplify Rcpp glue (remove unnecessary `as<>()` calls).

# Changes in version 3.4.1

## Minor changes and fixes

* Fix memtest notes on CRAN (heap-use-after-free).
* Fix memory leaks.

# Changes in version 3.4.0

## New features:

* Prioritization (`priority`, `preemptible`, `restart`) has been moved from `seize` to `add_generator` (#69). This leads to a more natural interpretation of prioritization values as attributes of arrivals from the same generator, rather than attributes of a `seize`. Still, prioritization values can be redefined dynamically from inside a trajectory with the new activity `set_prioritization`.
* New optional `post.seize` and `reject` subtrajectories in `seize` and `seize_selected` (#49). This feature allows us to fine-tune what happens to an arrival if it cannot seize a resource: instead of getting dropped, it may execute a given subtrajectory.
* New `clone` and `synchronize` activities (#71). `clone` implements the workflow pattern in which an entity is processed in multiple parallel threads. The user can define a different sub-trajectory for each clone. With `synchronize`, multiple parallel clones converge and are synchronized: only one continues (the first or the last to arrive), and the others are removed.
* New `batch` and `separate` activities (#45). They can be used to implement a rollercoaster process: `batch` collects a number of arrivals before they can continue processing as a block, and `separate` splits a previously established batch.
* New `renege_in` and `renege_abort` activities (#58). They can be used to set or unset a timer after which the arrival will abandon.

## Minor changes and fixes

* If a `branch`'s `option` returns `0`, the arrival skips the `branch` and continues to the next activity instead of throwing an `index out of range` error (#70).
* Throw errors on incorrect releases (#72).
* Remove deprecated convenience function `every` (#65) and `branch`'s deprecated argument `merge` (#57).
* New "The Bank Tutorial: Part I" vignette, by Duncan Garmonsway @nacnudus (#68).
* Update "Advanced Trajectory Usage" vignette.

# Changes in version 3.3.0

## New features:

* New `join` activity to concatenate trajectories (#50).
* Batched generation: the generation function can return more than one interarrival value at a time (#65).
* Add the option `queue_size_strict` to `add_resource` to guarantee the queue size limit with preemption (#59).
* New `select`, `seize_selected` and `release_selected` activities (#52).
* Modify resources (capacity, queue size) from inside a trajectory (#66).
* New `leave` activity (#63).

## Major fixes:

* Fix per-resource activity time monitoring (#67). The problem emerged when an arrival revisited a resource and it was enqueued. An uninitialised variable could lead to an activity time greater than `end_time - start_time`. All versions 3.2.x are affected by this bug.

## Minor changes and fixes:

* Fix the description of `preemptible` in the documentation of `seize` and force `preemptible` to be equal or greater than `priority` (#53).
* Reset finite generators (#51).
* Fix the handling of a capacity change when the new value is infinite (#60).
* Various doc fixes (#61).
* Change branch's `merge` parameter name to `continue`. The old name is deprecated (#57).
* Use `match.arg()` in multiple-choice arguments (#55).
* Fix `branch` backwards linking and count (#56).
* Split `release` in two steps to deal properly with capacity changes at the same point in time (#64).
* The convenience function `every` is deprecated due to #65.
* Update and extend previous vignettes.

# Changes in version 3.2.1

## New features:

* Add time-specific resource availability support (#21). Both resources' `capacity` and `queue_size` can change over time following a user-defined scheduling, which can be generated with the new function `schedule`.
* Advanced peek: inspect any number of future events in the event queue (8147820). For more details, see `?peek`.

## Minor changes and fixes:

* Fix steps grouping in `plot_resource_usage` (8da9b97).
* Fix incorrect trajectory behaviour when a rejection occurs inside a branch with `merge=TRUE` (#46).
* Fix a couple of segmentation faults in preemptive resources (f64f6b2).
* Improve verbose output (9013db0).
* New multiset-based event queue with unscheduling capabilities (a615fea and d6a9d67).
* A simulation may run forever (until the user interrupts it), that is, `until=Inf` is allowed now (f47baa9).
* New vignette on queueing systems.
* New vignette on Continuous-Time Markov Chains.
* Update and extend previous vignettes.

# Changes in version 3.2.0

## Major fix:

* In previous versions, resources were monitored __before__ performing the corresponding seize/release activity, before changing the status of the system. Thus, `t=3, queue=2` meant that, until `t=3`, the queue had 2 customers, and at `t=3` the system changed (because of a new arrival or a new departure). The idea was to keep the values and time vectors aligned (see #28). But from this moment on, the resources are monitored __after_ changing the status of the system. This is more consistent with what a user would expect, and more consistent with the behaviour of other related R functions (e.g., see `stepfun`, from the `stats` package). Wrapping up and from now on, `t=3, queue=2` means that some event happened at `t=3` whose immediate and subsequent result was a queue with 2 customers.

## New features:

* Add preemption functionality (#34). Preemption comes into play when a resource is specified as `preemptive=TRUE`. Arrivals in the server can be preempted on a `preempt_order="fifo"` or `preempt_order="lifo"` basis. Each `seize` has three basic properties:
    * `priority`: already present in previous versions.
    * `preemptible`: another `seize` with a `priority` value greater than this may preempt the present `seize`.
    * `restart`: whether the current task (a `timeout` activity, for instance) should be restarted if the arrival is preempted.

## Minor changes and fixes:

* Remove deprecated functions `show_activity` and `show_trajectory`.
* Add `every`, `to` and `from_to` convenience functions (8e524cd).
* Fix colour scale in `plot_resource_usage` (6b034a7).
* Fix compatibility with the upcoming version of `testthat` (#41).
* The `branch` activity now provides attributes to its `option` function, as the other activities (#42).
* Implement error handling in `plot_*` functions (#44).

# Changes in version 3.1.2

## New features:

* Monitor arrivals' start/activity/end times on a per-resource basis (#38). So far, the function `get_mon_arrivals()` returned the start/activity/end times per arrival for the whole trajectory. This behaviour remains, but additionally, `get_mon_arrivals(per_resource = TRUE)` returns these times per resource, so that it is possible to retrieve queueing/system times per resource.

## Minor changes and fixes:

* Fix testing ERRORs reported on platforms using clang and Sparc Solaris.
* `get_mon_*` functions accept a single simulation environment as well as a list of environments representing several replications (5ee2725). A new column (`replication`) in the resulting data frame indicates the corresponding replication number.
* Monitoring subsystem refactored (as a consequence of #38).

# Changes in version 3.1.1

## New features:

* Add attributes to arrivals and new `set_attribute` activity (#16).
* New `rollback` activity (#17 and #22).
* Add priorities to resources' queue (#20).

## Minor changes and fixes:

* Performance improvements with Boost (1b654fd).
* Fix arrivals' timing issues (#24).
* Nicer object printing (#26).
* Return `self` visibly, instead of invisibly (#35).
* Add `at` and `from` convenience functions (29cccd2 and 7cfdd90).
* Some work on vignettes (#29).
* Fix ggplot2 2.0.0 compatibility issues (#37).

# Changes in version 3.0.1

## Minor changes and fixes:

* Complete test coverage (#12).
* Minor fix in the documentation (42363d2).
* Set finalizer in the simulator object (#14).
* Fix test errors under Windows r-oldrelease (#15).

# Changes in version 3.0.0

## New features:

* First major release submitted to CRAN. The philosophy and workflow of the pre-release remain with a more robust event-based C++ backend and a more flexible frontend.
* __Enhanced programmability__. The timeout activity is more than just a delay. It admits a user-defined function, which can be as complex as needed in order to interact with the simulation model. The old v2.0 was no more than a queueing network simulator. This feature makes simmer a flexible and generic DES framework. Moreover, we have finally got rid of the infamous `add_skip_event` function to implement a more flexible and user-friendly branching method.
* __Robustness__. The event-based core design is rigorous and simple, which makes simmer faster and less error-prone, at the same level of other state-of-the-art DES frameworks.
* __Much better performance__. Instead of creating `n` arrivals beforehand, this release leverages the concept of _generator_ of arrivals, which is faster and more flexible. At the same time, the concept of _trajectory_ as a chain of activities is implemented entirely in C++ internally. Our tests show that simmer is even faster than SimPy when it comes to simulate queueing networks.
* __Replication__. In the pre-release, replication was implemented inside simmer. This no longer makes sense since, with the current design, it is more than straightforward to replicate and even parallelize the execution of replicas using standard R tools.
