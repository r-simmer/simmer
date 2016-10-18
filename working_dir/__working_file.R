library(simmer)

t <- create_trajectory() %>%
  send("signal", 3) %>%
  trap("signal") %>%
  wait() %>%
  timeout(1)

simmer(verbose = TRUE) %>%
  add_generator("dummy", t, at(0)) %>%
  run() %>%
  get_mon_arrivals()

t <- create_trajectory() %>%
  send("signal", 3) %>%
  trap("signal") %>%
  timeout(10) %>%
  timeout(1)

simmer(verbose = TRUE) %>%
  add_generator("dummy", t, at(0)) %>%
  run() %>%
  get_mon_arrivals()
