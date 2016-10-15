library(simmer)

t <- create_trajectory(verbose=T) %>%
  renege_in(1) %>%
  seize("dummy", 1) %>%
  renege_abort() %>%
  timeout(2) %>%
  release("dummy", 1)

env <- simmer(verbose=TRUE) %>%
  add_resource("dummy", 1) %>%
  add_generator("arrival", t, at(0, 0)) %>%
  run()

get_mon_arrivals(env, per_resource=TRUE)
get_mon_arrivals(env, per_resource=FALSE)
get_mon_resources(env)
