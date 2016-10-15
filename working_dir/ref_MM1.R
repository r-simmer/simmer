library(simmer)

mm1 <- create_trajectory() %>%
  branch(function() 1, T,
         create_trajectory() %>%
           seize("server", 1) %>%
           timeout(function() rexp(1, 2)) %>%
           release("server", 1)
  ) %>%
  rollback(0)

env <- simmer(verbose=T) %>%
  add_resource("server", 1) %>%
  add_generator("customer", mm1, function() rexp(1, 1), mon=F) %>%
  run(1000)

plot_resource_usage(env, "server")

#################################################################

system.time({
  mm1 <- create_trajectory() %>%
    seize("server", 1) %>%
    timeout(function() rexp(1, 66)) %>%
    release("server", 1)

  env <- simmer(verbose=F) %>%
    add_resource("server", 1) %>%
    add_generator("customer", mm1, function() rexp(100, 60), mon=F) %>%
    run(10000)
})
# 16 seconds (Simpy: 30 seconds)

plot_resource_usage(env, "server")
