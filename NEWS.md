## simmer 3.1.1.9000

Minor changes and fixes:

* `get_mon_*` functions accept a single simulation environment as well as a list of environments representing several replications (5ee2725). A new column (`replication`) in the resulting data frame indicates the corresponding replication number.

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
