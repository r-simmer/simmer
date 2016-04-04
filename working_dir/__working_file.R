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
  add_generator("customer", mm1, function() rexp(1, 60), mon=F) %>%
  run(10000)
})
# 26 seconds (Simpy: 30 seconds)

plot_resource_usage(env, "server")

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
  rollback(1) %>%
  timeout(function() 1) %>%
  release("server", 1)

t0 %>% show_trajectory()

a <- t0 %>% get_head()
a %>% show_activity(); a <- a %>% get_next_activity()


#################################

t0 <- create_trajectory() %>%
  set_attribute("health", function() sample(40:80,1)) %>%
  set_attribute("nurses_to_seize", 
                function(attrs){
                  if(attrs[["health"]]<50) 2
                  else 1
                }) %>%
  seize("nurse", 
        function(attrs){attrs[["nurses_to_seize"]]}) %>%
  timeout(function(attrs){(100 - attrs[["health"]])}) %>%
  set_attribute("health", 
                function(attrs){
                  min(attrs[["health"]] + sample(attrs[["health"]]:100, 1), 100)}) %>%
   release("nurse", 
        function(attrs){attrs[["nurses_to_seize"]]}) %>%
  
  ## some other functionality
  ## simply print the attrs using a 0 timeout
  timeout(function(attrs){print(attrs); 0})

env<-simmer() %>%
  add_generator("test", t0, at(seq(0,1000,200)), mon=2) %>%
  add_resource("nurse", 2) %>%
  run()

attributes <- env %>% get_mon_attributes()

plot_resource_usage(env, "nurse", items="server", steps=T)
plot_attributes(env)
plot_attributes(env, "health")

#################################

# segfault!

t0 <- create_trajectory() %>%
  seize("dummy", 1, restart=FALSE) %>%
  timeout(10) %>%
  release("dummy", 1)

t1 <- create_trajectory() %>%
  seize("dummy", 1, priority=1, preemptible=1) %>%
  timeout(10) %>%
  release("dummy", 1)

env <- simmer(verbose=F) %>%
  add_generator("p0a", t0, at(0, 0)) %>%
  add_generator("p1a", t1, at(2, 3)) %>%
  add_resource("dummy", 1, 2, preemptive=TRUE) %>%
  run()

get_mon_arrivals(env)
get_mon_arrivals(env, per_resource = TRUE)

#################################

t0 <- create_trajectory() %>%
  seize("dummy", 1, restart=TRUE) %>%
  timeout(10) %>%
  release("dummy", 1)

t1 <- create_trajectory() %>%
  seize("dummy", 2, priority=1, preemptible=1) %>%
  timeout(10) %>%
  release("dummy", 2)

env <- simmer(verbose=TRUE) %>%
  add_generator("p0a", t0, at(0, 0)) %>%
  add_generator("p1a", t1, at(2, 15)) %>%
  add_resource("dummy", 2, preemptive=TRUE) %>%
  run()

get_mon_arrivals(env)
get_mon_arrivals(env, per_resource = TRUE)
