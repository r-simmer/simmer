library(simmer)

t0 <- create_trajectory("my trajectory") %>%
  ## add an intake activity
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

envs <- lapply(1:100, function(i) {
  simmer("SuperDuperSim", verbose=F) %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
    run(80)
})

plot_resource_usage(envs, "doctor", items="server", steps=T)
plot_resource_utilization(envs, c("nurse", "doctor","administration"))
plot_evolution_arrival_times(envs, type="flow_time")

#################################################################

mm1 <- create_trajectory() %>%
  branch(function() 1, T,
    create_trajectory() %>%
      seize("server", 1) %>%
      timeout(function() rexp(1, 2)) %>%
      release("server", 1)
  )

env <- simmer(verbose=F) %>%
  add_resource("server", 1) %>%
  add_generator("customer", mm1, function() rexp(1, 1), mon=F) %>% 
  run(1000)

plot_resource_usage(simmer, "server")

#################################################################

system.time({
mm1 <- create_trajectory() %>%
  seize("server", 1) %>%
  timeout(function() rexp(1, 66)) %>%
  release("server", 1)

env <- simmer(verbose=F) %>%
  add_resource("server", 1) %>%
  add_generator("customer", mm1, function() rexp(1, 60), mon=F) %>%
  run(10000)
})
# 24 seconds! (Simpy: 30 seconds)

plot_resource_usage(simmer, "server")

#################################################################

t0 <- create_trajectory("my trajectory") %>%
  seize("server", 1) %>%
  timeout(function() rexp(1, 1)) %>%
  branch(function() sample(1:2, 1), merge=c(F, T), 
    create_trajectory("branch1") %>%
      seize("server", 2) %>%
      timeout(function() 1) %>%
      release("server", 2), 
    create_trajectory("branch2") %>%
      seize("server", 4) %>%
      timeout(function() rexp(1, 3)) %>%
      release("server", 4)
  ) %>%
  timeout(function() 1) %>%
  release("server", 1)

t0 %>% show_trajectory()

a <- t0 %>% get_head()
a %>% show_activity(); a <- a %>% get_next_activity()
