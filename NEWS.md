## simmer 3.2.0.9000

New features:

* Add time-specific resource availability support (#21). Both resources' `capacity` and `queue_size` can change over time following a user-defined scheduling, which can be generated with the new function `schedule`.

Minor changes and fixes:

* Fix steps grouping in `plot_resource_usage` (8da9b97).
* Fix incorrect trajectory behaviour when a rejection occurs inside a branch with `merge=TRUE` (#46).
* Fix a couple of segmentation faults in preemptive resources (f64f6b2).
* New vignette on queueing systems.
* New vignette on Continuous-Time Markov Chains.

## simmer 3.2.0

**Important fix:** In previous versions, resources were monitored **before** performing the corresponding seize/release activity, before changing the status of the system. Thus, `t=3, queue=2` meant that, until `t=3`, the queue had 2 customers, and at `t=3` the system changed (because of a new arrival or a new departure). The idea was to keep the values and time vectors aligned (see #28).

But from this moment on, the resources are monitored **after** changing the status of the system. This is more consistent with what a user would expect, and more consistent with the behaviour of other related R functions (e.g., see `stepfun`, from the `stats` package). Wrapping up and from now on, `t=3, queue=2` means that some event happened at `t=3` whose immediate and subsequent result was a queue with 2 customers.

New features:

* Add preemption functionality (#34). Preemption comes into play when a resource is specified as `preemptive=TRUE`. Arrivals in the server can be preempted on a `preempt_order="fifo"` or `preempt_order="lifo"` basis. Each `seize` has three basic properties:
    * `priority`: already present in previous versions.
    * `preemptible`: another `seize` with a `priority` value greater than this may preempt the present `seize`.
    * `restart`: whether the current task (a `timeout` activity, for instance) should be restarted if the arrival is preempted.

Minor changes and fixes:

* Remove deprecated functions `show_activity` and `show_trajectory`.
* Add `every`, `to` and `from_to` convenience functions (8e524cd).
* Fix colour scale in `plot_resource_usage` (6b034a7).
* Fix compatibility with the upcoming version of `testthat` (#41).
* The `branch` activity now provides attributes to its `option` function, as the other activities (#42).
* Implement error handling in `plot_*` functions (#44).

## simmer 3.1.2

New features:

* Monitor arrivals' start/activity/end times on a per-resource basis (#38). So far, the function `get_mon_arrivals()` returned the start/activity/end times per arrival for the whole trajectory. This behaviour remains, but additionally, `get_mon_arrivals(per_resource = TRUE)` returns these times per resource, so that it is possible to retrieve queueing/system times per resource.

Minor changes and fixes:

* Fix testing ERRORs reported on platforms using clang and Sparc Solaris.
* `get_mon_*` functions accept a single simulation environment as well as a list of environments representing several replications (5ee2725). A new column (`replication`) in the resulting data frame indicates the corresponding replication number.
* Monitoring subsystem refactored (as a consequence of #38).

## simmer 3.1.1

New features:

* Add attributes to arrivals and new `set_attribute` activity (#16).
* New `rollback` activity (#17 and #22).
* Add priorities to resources' queue (#20).

Minor changes and fixes:

* Performance improvements with Boost (1b654fd).
* Fix arrivals' timing issues (#24).
* Nicer object printing (#26).
* Return `self` visibly, instead of invisibly (#35).
* Add `at` and `from` convenience functions (29cccd2 and 7cfdd90).
* Some work on vignettes (#29).
* Fix ggplot2 2.0.0 compatibility issues (#37).

## simmer 3.0.1

Minor fixes:

* Complete test coverage (#12).
* Minor fix in the documentation (42363d2).
* Set finalizer in the simulator object (#14).
* Fix test errors under Windows r-oldrelease (#15).

## simmer 3.0.0

First major release submitted to CRAN. The philosophy and workflow of the pre-release remain with a more robust event-based C++ backend and a more flexible frontend. These are the main improvements:

* **Enhanced programmability**. The timeout activity is more than just a delay. It admits a user-defined function, which can be as complex as needed in order to interact with the simulation model. The old v2.0 was no more than a queueing network simulator. This feature makes simmer a flexible and generic DES framework. Moreover, we have finally got rid of the infamous `add_skip_event` function to implement a more flexible and user-friendly branching method.
* **Robustness**. The event-based core design is rigorous and simple, which makes simmer faster and less error-prone, at the same level of other state-of-the-art DES frameworks.
* **Much better performance**. Instead of creating `n` arrivals beforehand, this release leverages the concept of *generator* of arrivals, which is faster and more flexible. At the same time, the concept of *trajectory* as a chain of activities is implemented entirely in C++ internally. Our tests show that simmer is even faster than SimPy when it comes to simulate queueing networks.
* **Replication**. In the pre-release, replication was implemented inside simmer. This no longer makes sense since, with the current design, it is more than straightforward to replicate and even parallelize the execution of replicas using standard R tools.
