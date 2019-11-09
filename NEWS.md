# simmer 4.3.0.9000

## New features

- Add `out` and `keep_seized` parameters to `leave()` with the same behaviour as
  in `renege_in()` and `renege_if()`. Code and documentation of these functions
  are now integrated under `help(renege)` (#208).

## Minor changes and fixes:

- Fix `get_mon_*()` dispatch for named lists (#210).
- Get/put the RNG state when random numbers are required in the backend (#218).

# simmer 4.3.0

## New features

- Add ability to `keep_seized` resources after reneging (#204 addressing #200).
- Add ability to define a range of arrival priorities that are allowed to access
  a resource's queue if there is no room in the server (#205 addressing #202).

## Minor changes and fixes:

- Drop R6 as a dependency (#193 addressing #190).
- Small fix in `from` and `from_to` + documentation update (75a9569).
- Move activity usage examples to help pages (#194).
- Fix shortest-queue selection policies (#196).
- Fix batch triggering (#203).
- Update JSS paper, CITATION, references and DOI.

# simmer 4.2.2

## Minor changes and fixes:

- Fix batches with an infinite timeout (#184).
- Fix preemption for arrivals previously stopped by a signal (#187).
- Fix handler loop for two consecutive signals (#188).
- Fix incorrect handler linking (#189).

# simmer 4.2.1

## Minor changes and fixes:

- Fix memtest notes on CRAN (e741686).

# simmer 4.2.0

## New features:

- New `handle_unfinished()` activity sets a drop-out trajectory for unfinished
  arrivals, i.e., those dropped from a resource (due to preemption, resource
  shrinkage or a rejected `seize`) or those that `leave` a trajectory (#178
  addressing #177).
- New `release_all()` and `release_selected_all()` activities automatically
  retrieve the amount of resources seized and release it (#180 addressing #25).
- New `get_seized()` and `get_seized_selected()` getters allow an arrival to
  retrieve the amount of resources seized (#180 addressing #179).
- New `stop_if()` activity sets a conditional breakpoint (#181 addressing #100).

## Minor changes and fixes:

- Fix performance issues in data sources (#176).
- Update CITATION.
- Fix monitored activity for preempted arrivals (as part of #178).
- Fix seizes/releases with a null amount (as part of #180).
- Rename internal status codes (as part of #181).
- Provide more context on error or warning (as part of #181).
- Extend the `Queueing Systems` vignette with a section about state-dependent
  service rates.
- Fix performance issues in getters (#183).

# simmer 4.1.0

## New features:

- New getter `get_selected()` retrieves names of selected resources via the
  `select()` activity (#172 addressing #171).
- Source and resource getters have been vectorised to retrieve parameters from
  multiple entities (as part of #172).
- Simplify C++ `Simulator` interface for adding processes and resources (#162).
  The responsibility of building the objects has been moved to the caller.
- New `add_global()` method to attach global attributes to a simulation
  environment (#174 addressing #158).

## Minor changes and fixes:

- Remove 3.8.0 and 4.0.1 deprecations (#170 addressing #165).
- Fix `get_global()` to work outside trajectories (#170 addressing #165).
- Fix `rollback()` with an infinite amount (#173).
- Fix and improve schedules and managers (as part of #174).
- Fix `reset()` to avoid overwriting the simulation environment (#175).

# simmer 4.0.1

## New features:

- New getters (#159):
    - `get_sources()` and `get_resources()` retrieve a character vector of
      source/resource names defined in a simulation environment.
    - `get_trajectory()` retrieves a trajectory to which a given source is
      attached.
- New resource selection policies: `shortest-queue-available`,
  `round-robin-available`, `random-available` (#156). These are the same as the
  existing non-`available` ones, but they exclude unavailable resources
  (capacity set to zero). Thus, if all resources are unavailable, an error is
  raised.

## Minor changes and fixes:

- Rename `-DRCPP_PROTECTED_EVAL` (Rcpp >= 0.12.17.4) as
  `-DRCPP_USE_UNWIND_PROTECT` (6d27671).
- Keep compilation quieter with `-DBOOST_NO_AUTO_PTR` (70328b6).
- Improve `log_` print (7c2e3b1).
- Add `when_activated()` convenience function to easily generate arrivals on
  demand from trajectories (#161 closing #160).
- Enhance `schedule` printing (9c66285).
- Fix generator-manager name clashing (#163).
- Deprecate `set_attribute(global=TRUE)`, `get_attribute(global=TRUE)` and
  `timeout_from_attribute(global=TRUE)` (#164), the `*_global` versions should
  be used instead.

# simmer 4.0.0

- The `simmer` license has been changed to GPL >= 2.

## New features:

- The C++ core has been refactorised into a header-only library under
  `inst/include` (#147 closing #145). Therefore, from now on it is possible to
  extend the C++ API from another package by listing `simmer` under the
  `LinkingTo` field in the DESCRIPTION file.
- New generic `monitor` constructor enables the development of new monitoring
  backends in other packages (179f656, as part of #147).
- New simulation-scoped logging levels. The `log_` activity has a new argument
  `level` which determines whether the message is printed depending on a global
  `log_level` defined in the `simmer` constructor (#152).
- `set_attribute` and `set_global` gain a new argument to automatically
  initialise new attributes (#157). Useful to update counters and indexes in a
  single line, without initialisation boilerplate.

## Minor changes and fixes:

- Enhanced exception handling, with more informative error messages (#148).
- Refactorisation of the printing methods and associated code (#149).
- Allow empty trajectories in sources and activities with sub-trajectories
  (#151 closing #150).
- Enable `-DRCPP_PROTECTED_EVAL` (Rcpp >= 0.12.17.3), which provides fast
  evaluation of R expressions by leveraging the new stack unwinding protection
  API (R >= 3.5.0).
- Replace backspace usage in vector's `ostream` method (2b2f43e).
- Fix namespace clashes with `rlang` and `purrr` (#154).

# simmer 3.8.0

## New features:

- New data source `add_dataframe` enables the attachment of precomputed data,
  in the form of a data frame, to a trajectory. It can be used instead of (or
  along with) `add_generator`. The most notable advantage over the latter is
  that `add_dataframe` is able to automatically set attributes and
  prioritisation values per arrival based on columns of the provided data frame
  (#140 closing #123).
- New `set_source` activity deprecates `set_distribution()`. It works both for
  generators and data sources (275a09c, as part of #140).
- New monitoring interface allows for disk offloading. The `simmer()`
  constructor gains a new argument `mon` to provide different types of monitors.
  By default, monitoring is performed in-memory, as usual. Additionally,
  monitoring can be offloaded to disk through `monitor_delim` and `monitor_csv`,
  which produce flat delimited files. But more importantly, the C++ interface
  has been refactorised to enable the development of new monitoring backends
  (#146 closing #119).

## Minor changes and fixes:

- Some documentation improvements (1e14ed7, 194ed05).
- New default `until=Inf` for the `run` method (3e6aae9, as part of #140).
- `branch` and `clone` now accept lists of trajectories, in the same way as
  `join`, so that there is no need to use `do.call` (#142).
- The argument `continue` (present in `seize` and `branch`) is recycled if only
  one value is provided but several sub-trajectories are defined (#143).
- Fix process reset: sources are reset in strict order of creation (e7d909b).
- Fix infinite timeouts (#144).

# simmer 3.7.0

## New features:

- New `timeout_from_attribute()` activity makes it easier to set a timeout based
  on an attribute (#129).
- The activities `set_attribute()`, `set_prioritization()`, `set_capacity()` and
  `set_queue_size()` get a new argument `mod` which, if set to `"+"` or `"*"`,
  modifies the corresponding value instead of substituting it. This makes it
  easier to increment, decrement or scale one of these values (#130).
- New `*_selected()` versions for the already available resource getters:
  `get_capacity()`, `get_queue seize()`, `get_server_count()` and
  `get_queue_count()` (#134).

## Minor changes and fixes:

- Broadcast signals with higher priority to prevent an arrival to catch its own
  signal with a `trap()` after a `send()` (#135).
- Generate new arrivals with minimum priority to avoid wrong interactions with
  simultaneous activities (#136).
- Remove v3.6.x deprecations: the old attribute retrieval system (see notes for
  v3.6.3), as well as methods `create_trajectory()` and `onestep()` (#117).
- Remove `get_mon_resources()`'s `data` argument. It was there for historical
  reasons and probably nobody was using it (851d34b).
- New vignette, "simmer: Discrete-Event Simuation for R", paper accepted for
  publication in the Journal of Statistical Software. Remove "Terminology"
  vignette (#127).
- New vignette, "Design and Analysis of 5G Scenarios", supplementary materials
  for a paper accepted for publication in the IEEE Communications Magazine (#137).

# simmer 3.6.5

## New features:

- `set_attribute()` (and `set_global()` by extension) can set multiple
  attributes at once by providing vectors of `keys` and `values` (or functions
  returning such `keys` and/or `values`). `get_attribute()` (and `get_global()`
  by extension) can retrieve multiple `keys` (#122).
- New `stepn()` method deprecates `onestep()` (e452975).

## Minor changes and fixes:

- Restore `ostream` after formatting (9ff11f8).
- Fix arrival cloning to copy attributes over to the clone (#118).
- Fix self-induced preemption through `set_capacity()` (#125).
- Update "Queueing Systems" vignette (a0409a0, 8f03f4f).
- Update "Advanced Trajectory Usage" vignette (4501927).
- Fix print methods to return the object invisibly (#128).
- New "Dining Philosophers Problem" vignette (ff6137e).

# simmer 3.6.4

## Minor changes and fixes:

- Fix preemption in non-saturated multi-server resources when seizing
  amounts > 1 (#114).
- Fix queue priority in non-saturated finite-queue resources when seizing
  amounts > 1 (#115).
- Fix resource seizing: avoid jumping the queue when there is room in the
  server but other arrivals are waiting (#116).

# simmer 3.6.3

## New features:

- Show simulation progress via an optional `progress` callback in `run()` (#103).
- New "The Bank Tutorial: Part II" vignette, by Duncan Garmonsway @nacnudus (#106).
- New getters for running arrivals (#109), meant to be used inside trajectories:
    - `get_name()` retrieves the arrival name.
    - `get_attribute()` retrieves an attribute by name. The old method of
      retrieving them by providing a function with one argument is deprecated in
      favour of `get_attribute()`, and will be removed in version 3.7.x.
    - `get_prioritization()` retrieves the three prioritization values
      (`priority`, `preemptible`, `restart`) of the active arrival.
- New shortcuts for global attributes (#110): `set_global()` and `get_global()`,
  equivalent to `set_attribute(global=TRUE)` and `get_attribute(global=TRUE)`
  respectively.

## Minor changes and fixes:

- Some code refactoring and performance improvements (2f4b484, ffafe1e, f16912a,
  fb7941b, 2783cd8).
- Use `Rcpp::DataFrame` instead of `Rcpp::List` (#104).
- Improve argument parsing and error messages (#107).
- Improve internal function `make_resetable()` (c596f73).

# simmer 3.6.2

## Minor changes and fixes:

- Update "The Bank Tutorial: Part I" vignette (@nacnudus in #90).
- Fix `trap()`'s handler cloning and associated test (#91).
- Apply `select()`'s `policy` also when `resources` is a function (#92).
- Accept dynamic timeouts in batches (#93).
- Change `rollback()`'s default behaviour to `times=Inf`, i.e., infinite loop (#95).
- Stop and throw an error when `timeout()` returns a missing value (#96 and #97).
- Fix memory management: resetting the environment was clearing but not
  deallocating memory (#98, fixed in #99).
- Fix object destruction: workaround for tidyverse/magrittr#146 (#98, fixed in effcb6b).

# simmer 3.6.1

## Minor changes and fixes:

- Recycle logical indexes when subsetting (2526e75).
- Implement replacement operators, `[<-` and `[[<-` (#88).
- Provide `rep()` S3 method for trajectories (7fa515e).
- Remove plotting functions (bb9656b), deprecated since v3.6.0. The new
  `simmer.plot` package (on CRAN) already covers these features among others.
- Don't evaluate vignette chunks if `Suggests` are not installed (e40e5b6).
- Rewrite DESCRIPTION (3f26516).
- Add an `every` parameter to the `from_to()` convenience function (9d68887).

# simmer 3.6.0

## New features:

- New subsetting operators, `[` and `[[`, for trajectories (1847898). Think
  about trajectories as lists of activities and these operators will do (almost)
  everything you expect. As a side effect, the generics `head()` and `tail()`
  automatically work with trajectories also as expected.
- New `length()` method to obtain the number of first-level activities in a
  trajectory (f86375a). Useful in combination with the subsetting operators.

## Minor changes and fixes:

- `create_trajectory()` has been deprecated in favor of `trajectory()` (76c1317).
- `plot_resource_usage()`, `plot_resource_utilization()`,
  `plot_evolution_arrival_times()` and `plot_attributes()` have been deprecated
  and will be removed in the next release in order to minimise dependencies
  (5b43f2b). We plan to release a new package on CRAN covering these features
  and new ones.
- All methods are now S3 methods, so that a nice intelligible error is displayed
  if you apply a method to the wrong object (e891045).
- All the activity management -related stuff has been removed, i.e, `get_head()`,
  `get_tail()`, `print_activity()`, `get_next_activity()`, `get_prev_activity()`
  (f86375a). These methods were only useful for development purposes and nobody
  should be using them. And it was never a good idea to directly expose external
  pointers.
- Clone all trajectories before passing them to the C++ core (f655cae).
- Update "Advanced Trajectory Usage" vignette.
- Update "Queueing Systems" vignette.

# simmer 3.5.1

## New features:

- New `renege_if()` activity triggers reneging upon reception of a signal
  broadcasted with `send()` (#84).
- Add support for uninterruptible handlers in `trap()` (bb2aa46).
- Add support for global attributes in `set_attribute()` (#82).

## Minor changes and fixes:

- Fix bug in `set_queue_size()` with `queue_size_strict=TRUE`: arrivals were
  not being dropped (#83).
- Fix bug in `set_capacity()` with a preemptive resource when the old value was
  `Inf`: arrivals were not being preempted (63beb2c).
- Fix bug in per-resource activity monitoring: activity was not being reset (55097c9).
- Fix `trap()` printed information.
- Update "Advanced Trajectory Usage" vignette.
- New "Other SimPy Examples" vignette.

# simmer 3.5.0

## New features:

- `set_capacity()` and `set_queue_size()` become activities (#77). Just like
  `seize()` and `release()`, they have the associated `set_capacity_selected()`
  and `set_queue_size_selected()` for a joint use together with `select()`.
- New `activate()` and `deactivate()` activities allow an arrival to start or
  stop a generator, respectively, from inside a trajectory (#80).
- New `set_trajectory()` and `set_distribution()` activities allow an arrival
  to install a new trajectory or distribution, respectively, in a generator
  from inside a trajectory (#80).
- Refactorised and improved arrival monitoring.
- New interarrival communication activities allowing asynchronous programming:
  `send()`, `trap()`, `untrap()` and `wait()` can be used to send signals, wait
  for signals, trap them and launch asynchronous handlers.
- New `log_()` activity simply prints messages for debugging purposes (eaa4554).

## Minor changes and fixes:

- Store inline trajectory objects inside the simulation environment to prevent
  them to be garbage-collected.
- Update "Advanced Trajectory Usage" vignette.

# simmer 3.4.4

## Minor changes and fixes:

- Fix non-defined behaviour caused by a race condition in object destruction
  under some platforms.

# simmer 3.4.3

## Minor changes and fixes:

- Remove warnings for unused arguments in `seize()` and `seize_selected()` (1c8c3bb).
- Fix crash on arrival reneging in non-triggered batches (8713d95).
- Fix crash on batches triggered before the timer expires (8713d95).
- Fix crash on non-released preemptive resources when capacity decreases (#75).
- Leaving without releasing a resource throws a warning (#76).

# simmer 3.4.2

## New features:

- Ongoing (unfinished) arrivals are reported with
  `get_mon_arrivals(ongoing = TRUE)` (#73).

## Minor changes and fixes:

- Simplify Rcpp glue: remove unnecessary `as<>()` calls (ec4e51a).
- Simplify trajectory's head/tail management (06432a8).
- Now, `run(until)` runs the simulation exactly until `until`, instead of until
  the first event scheduled at a time >= `until` (e7264f6).
- Fix batch cloning (c20bc1d).
- Coverage improved.

# simmer 3.4.1

## Minor changes and fixes:

- Fix memtest notes on CRAN (heap-use-after-free).
- Fix memory leaks.

# simmer 3.4.0

## New features:

- Prioritization (`priority`, `preemptible`, `restart`) has been moved from
  `seize()` to `add_generator()` (#69). This leads to a more natural
  interpretation of prioritization values as attributes of arrivals from the
  same generator, rather than attributes of a `seize()`. Still, prioritization
  values can be redefined dynamically from inside a trajectory with the new
  activity `set_prioritization()`.
- New optional `post.seize` and `reject` sub-trajectories in `seize()` and
  `seize_selected()` (#49). This feature allows us to fine-tune what happens to
  an arrival if it cannot seize a resource: instead of getting dropped, it may
  execute a given sub-trajectory.
- New `clone()` and `synchronize()` activities (#71). `clone()` implements the
  workflow pattern in which an entity is processed in multiple parallel threads.
  The user can define a different sub-trajectory for each clone. With
  `synchronize()`, multiple parallel clones converge and are synchronized: only
  one continues (the first or the last to arrive), and the others are removed.
- New `batch()` and `separate()` activities (#45). They can be used to implement
  a rollercoaster process: `batch()` collects a number of arrivals before they
  can continue processing as a block, and `separate()` splits a previousl
  established batch.
- New `renege_in()` and `renege_abort()` activities (#58). They can be used to
  set or unset a timer after which the arrival will abandon.

## Minor changes and fixes:

- If a `branch()`'s `option` returns `0`, the arrival skips the `branch()`
  and continues to the next activity instead of throwing an `index out of range`
  error (#70).
- Throw errors on incorrect releases (#72).
- Remove deprecated convenience function `every()` (#65) and `branch()`'s
  deprecated argument `merge` (#57).
- New "The Bank Tutorial: Part I" vignette, by Duncan Garmonsway @nacnudus (#68).
- Update "Advanced Trajectory Usage" vignette.

# simmer 3.3.0

## New features:

- New `join()` activity to concatenate trajectories (#50).
- Batched generation: the generation function can return more than one
  interarrival value at a time (#65).
- Add the option `queue_size_strict` to `add_resource()` to guarantee the queue
  size limit with preemption (#59).
- New `select()`, `seize_selected()` and `release_selected()` activities (#52).
- Modify resources (capacity, queue size) from inside a trajectory (#66).
- New `leave()` activity (#63).

## Major fixes:

- Fix per-resource activity time monitoring (#67). The problem emerged when an
  arrival revisited a resource and it was enqueued. An uninitialised variable
  could lead to an activity time greater than `end_time - start_time`. All
  versions 3.2.x are affected by this bug.

## Minor changes and fixes:

- Fix the description of `preemptible` in the documentation of `seize()` and
  force `preemptible` to be equal or greater than `priority` (#53).
- Reset finite generators (#51).
- Fix the handling of a capacity change when the new value is infinite (#60).
- Various doc fixes (#61).
- Change `branch()`'s `merge` parameter name to `continue`. The old name is
  deprecated (#57).
- Use `match.arg()` in multiple-choice arguments (#55).
- Fix `branch()` backwards linking and count (#56).
- Split `release()` in two steps to deal properly with capacity changes at
  the same point in time (#64).
- The convenience function `every()` is deprecated due to #65.
- Update and extend previous vignettes.

# simmer 3.2.1

## New features:

- Add time-specific resource availability support (#21). Both resources'
  `capacity` and `queue_size` can change over time following a user-defined
  scheduling, which can be generated with the new function `schedule()`.
- Advanced peek: inspect any number of future events in the event queue
  (8147820). For more details, see `?peek`.

## Minor changes and fixes:

- Fix steps grouping in `plot_resource_usage()` (8da9b97).
- Fix incorrect trajectory behaviour when a rejection occurs inside a branch
  with `merge=TRUE` (#46).
- Fix a couple of segmentation faults in preemptive resources (f64f6b2).
- Improve verbose output (9013db0).
- New multiset-based event queue with unscheduling capabilities (a615fea and d6a9d67).
- A simulation may run forever (until the user interrupts it), that is,
  `until=Inf` is allowed now (f47baa9).
- New vignette on queueing systems.
- New vignette on Continuous-Time Markov Chains.
- Update and extend previous vignettes.

# simmer 3.2.0

## Major fix:

- In previous versions, resources were monitored **before** performing the
  corresponding seize/release activity, before changing the status of the
  system. Thus, `t=3, queue=2` meant that, until `t=3`, the queue had
  2 customers, and at `t=3` the system changed (because of a new arrival or
  a new departure). The idea was to keep the values and time vectors aligned
  (see #28). But from this moment on, the resources are monitored **after**
  changing the status of the system. This is more consistent with what a user
  would expect, and more consistent with the behaviour of other related R
  functions (e.g., see `stepfun()`, from the `stats` package). Wrapping up and
  from now on, `t=3, queue=2` means that some event happened at `t=3` whose
  immediate and subsequent result was a queue with 2 customers.

## New features:

- Add preemption functionality (#34). Preemption comes into play when a resource
  is specified as `preemptive=TRUE`. Arrivals in the server can be preempted on
  a `preempt_order="fifo"` or `preempt_order="lifo"` basis. Each `seize()` has
  three basic properties:
    - `priority`: already present in previous versions.
    - `preemptible`: another `seize()` with a `priority` value greater than this
      may preempt the present `seize()`.
    - `restart`: whether the current task (a `timeout()` activity, for instance)
      should be restarted if the arrival is preempted.

## Minor changes and fixes:

- Remove deprecated functions `show_activity()` and `show_trajectory()`.
- Add `every()`, `to()` and `from_to()` convenience functions (8e524cd).
- Fix colour scale in `plot_resource_usage()` (6b034a7).
- Fix compatibility with the upcoming version of `testthat` (#41).
- The `branch()` activity now provides attributes to its `option` function, as
  the other activities (#42).
- Implement error handling in `plot_*()` functions (#44).

# simmer 3.1.2

## New features:

- Monitor arrivals' start/activity/end times on a per-resource basis (#38). So
  far, the function `get_mon_arrivals()` returned the start/activity/end times
  per arrival for the whole trajectory. This behaviour remains, but additionally,
  `get_mon_arrivals(per_resource = TRUE)` returns these times per resource, so
  that it is possible to retrieve queueing/system times per resource.

## Minor changes and fixes:

- Fix testing ERRORs reported on platforms using clang and Sparc Solaris.
- `get_mon_*()` functions accept a single simulation environment as well as a
  list of environments representing several replications (5ee2725). A new column
  (`replication`) in the resulting data frame indicates the corresponding
  replication number.
- Monitoring subsystem refactored (as a consequence of #38).

# simmer 3.1.1

## New features:

- Add attributes to arrivals and new `set_attribute()` activity (#16).
- New `rollback()` activity (#17 and #22).
- Add priorities to resources' queue (#20).

## Minor changes and fixes:

- Performance improvements with Boost (1b654fd).
- Fix arrivals' timing issues (#24).
- Nicer object printing (#26).
- Return `self` visibly, instead of invisibly (#35).
- Add `at()` and `from()` convenience functions (29cccd2 and 7cfdd90).
- Some work on vignettes (#29).
- Fix ggplot2 2.0.0 compatibility issues (#37).

# simmer 3.0.1

## Minor changes and fixes:

- Complete test coverage (#12).
- Minor fix in the documentation (42363d2).
- Set finalizer in the simulator object (#14).
- Fix test errors under Windows r-oldrelease (#15).

# simmer 3.0.0

## New features:

- First major release submitted to CRAN. The philosophy and workflow of the
  pre-release remain with a more robust event-based C++ backend and a more
  flexible frontend.
- **Enhanced programmability**. The timeout activity is more than just a delay.
  It admits a user-defined function, which can be as complex as needed in order
  to interact with the simulation model. The old v2.0 was no more than a
  queueing network simulator. This feature makes simmer a flexible and generic
  DES framework. Moreover, we have finally got rid of the infamous
  `add_skip_event()` function to implement a more flexible and user-friendly
  branching method.
- **Robustness**. The event-based core design is rigorous and simple, which
  makes simmer faster and less error-prone, at the same level of other
  state-of-the-art DES frameworks.
- **Much better performance**. Instead of creating `n` arrivals beforehand,
  this release leverages the concept of *generator* of arrivals, which is faster
  and more flexible. At the same time, the concept of *trajectory* as a chain of
  activities is implemented entirely in C++ internally. Our tests show that
  simmer is even faster than SimPy when it comes to simulate queueing networks.
- **Replication**. In the pre-release, replication was implemented inside simmer.
  This no longer makes sense since, with the current design, it is more than
  straightforward to replicate and even parallelize the execution of replicas
  using standard R tools.
