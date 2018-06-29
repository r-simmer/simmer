library(simmer)
library(simmer.plot)

mm1 <- trajectory() %>%
  branch(function() 1, T,
         trajectory() %>%
           seize("server", 1) %>%
           timeout(function() rexp(1, 2)) %>%
           release("server", 1)
  ) %>%
  rollback(0, 1)

env <- simmer(verbose=T) %>%
  add_resource("server", 1) %>%
  add_generator("customer", mm1, function() rexp(1, 1), mon=F) %>%
  run(1000)

plot(env, "res", "usage", "server")

#################################################################

system.time({
  mm1 <- trajectory() %>%
    seize("server", 1) %>%
    timeout(function() rexp(1, 66)) %>%
    release("server", 1)

  env <- simmer(verbose=F) %>%
    add_resource("server", 1) %>%
    add_generator("customer", mm1, function() rexp(50, 60), mon=F) %>%
    run(10000, progress=progress::progress_bar$new()$update)
})
# (Simpy: 30 seconds)
# 16 seconds with R 3.3.x
# 4.8 seconds with R 3.5.0

system.time({
  input <- data.frame(
    time = rexp(10000*60, 60),
    service = rexp(10000*60, 66)
  )

  mm1 <- trajectory() %>%
    seize("server", 1) %>%
    timeout_from_attribute("service") %>%
    release("server", 1)

  env <- simmer(verbose=F) %>%
    add_resource("server", 1) %>%
    add_dataframe("customer", mm1, input, mon=F, batch=50) %>%
    run(10000, progress=progress::progress_bar$new()$update)
})
# 2.1 seconds with R 3.4.3
# 2.4 seconds with R 3.5.0

plot(env, "res", "usage", "server")
