library(simmer)
library(simmer.plot)

mm1 <- trajectory() %>%
  branch(function() 1, T,
         trajectory() %>%
           seize("server", 1) %>%
           timeout(function() rexp(1, 2)) %>%
           release("server", 1)
  ) %>%
  rollback(0)

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
    add_generator("customer", mm1, function() rexp(100, 60), mon=F) %>%
    run(10000, progress=progress::progress_bar$new()$update)
})
# 16 seconds (Simpy: 30 seconds)

plot(env, "res", "usage", "server")
