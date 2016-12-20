library(simmer)

t <- trajectory(verbose=T) %>%
  select(c("r1", "r2", "r3"), policy="round-robin") %>%
  seize_selected(1) %>%
  #timeout(10) %>%
  #release_selected(1) %>%
  rollback(4, times=10)

env <- simmer(verbose=T) %>%
  add_resource("r1", Inf) %>%
  add_resource("r2", Inf) %>%
  add_resource("r3", Inf) %>%
  add_generator("arrival", t, at(0)) %>%
  run()
