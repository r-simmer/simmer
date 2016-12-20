library(simmer)

t <- trajectory(verbose=T) %>%
  batch(3, timeout=1, permanent=F, rule=NULL) %>%
  seize("dummy", 1) %>%
  timeout(1) %>%
  release("dummy", 1) %>%
  separate() %>%
  timeout(1)

env <- simmer(verbose=TRUE) %>%
  add_resource("dummy", 1, 0) %>%
  add_generator("arrival", t, at(0, 1, 2)) %>%
  run()

get_mon_arrivals(env, per_resource=TRUE)
get_mon_arrivals(env, per_resource=FALSE)
