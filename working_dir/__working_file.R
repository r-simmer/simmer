library(simmer)

t <- create_trajectory() %>%
  send("signal", 3) %>%
  trap("signal") %>%
  seize("res", 1) %>%
  timeout(10) %>%
  timeout(1) %>%
  release("res", 1)

env <- simmer(verbose = TRUE) %>%
  add_resource("res", 1) %>%
  add_generator("dummy", t, at(0, 1, 2)) %>%
  run()
get_mon_arrivals(env)
get_mon_arrivals(env, per_resource = TRUE)

quit()

t <- create_trajectory() %>%
  send("signal", 3) %>%
  trap("signal") %>%
  batch(1) %>%
  seize("res", 1) %>%
  wait() %>%
  timeout(1) %>%
  release("res", 1)

env <- simmer(verbose = TRUE) %>%
  add_generator("dummy", t, at(0, 1, 2)) %>%
  add_resource("res", 1) %>%
  run()
env %>% get_mon_arrivals()
env %>% get_mon_arrivals(per_resource = TRUE)

t <- create_trajectory() %>%
  send("signal", 3) %>%
  trap("signal") %>%
  batch(1) %>%
  seize("res", 1) %>%
  timeout(10) %>%
  timeout(1) %>%
  release("res", 1)

env <- simmer(verbose = TRUE) %>%
  add_generator("dummy", t, at(0, 1, 2)) %>%
  add_resource("res", 1) %>%
  run()
env %>% get_mon_arrivals()
env %>% get_mon_arrivals(per_resource = TRUE)
