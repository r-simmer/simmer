library(simmer)

t0 <- create_trajectory() %>%
  batch(2, name="shared") %>%
  seize("dummy", 1) %>%
  timeout(10) %>%
  release("dummy", 1)

t1 <- create_trajectory() %>%
  renege_in(5) %>%
  join(t0)

env <- simmer(verbose = TRUE) %>%
  add_resource("dummy", 1) %>%
  add_generator("arrival0", t0, at(0)) %>%
  add_generator("arrival1", t1, at(0)) %>%
  run()

get_mon_arrivals(env, per_resource = FALSE)
get_mon_arrivals(env, per_resource = TRUE)
get_mon_resources(env)
reset(env)

t <- create_trajectory(verbose=TRUE) %>%
  batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
  renege_in(1) %>%
  batch(2, timeout=0, permanent=FALSE, rule=NULL) %>%
  batch(1, timeout=0, permanent=FALSE, rule=NULL) %>%
  seize("dummy", 1) %>%
  timeout(2) %>%
  release("dummy", 1)

env <- simmer(verbose=TRUE) %>%
  add_resource("dummy", 1, 0) %>%
  add_generator("arrival", t, at(0, 0, 0, 0)) %>%
  run()

get_mon_arrivals(env, per_resource = FALSE)
get_mon_arrivals(env, per_resource = TRUE)
get_mon_resources(env)
