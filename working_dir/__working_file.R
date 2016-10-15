library(simmer)

t <- create_trajectory() %>%
  deactivate("dummy")

env <- simmer(verbose = TRUE) %>%
  add_generator("dummy", t, function() 1) %>%
  run()

t <- create_trajectory() %>%
  deactivate("dummy")

env <- simmer(verbose = TRUE) %>%
  add_generator("dummy", t, at(0, 0, 1)) %>%
  run()

t <- create_trajectory() %>%
  activate("dummy")

env <- simmer(verbose = TRUE) %>%
  add_generator("dummy", t, at(0, 0, 1)) %>%
  run()
