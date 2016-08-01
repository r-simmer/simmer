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
  add_generator("customer", mm1, function() rexp(100, 60), mon=F) %>%
  run(10000)
})
# 16 seconds (Simpy: 30 seconds)

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

t0

a <- t0 %>% get_head()
a %>% print_activity(); a <- a %>% get_next_activity()


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

#################################################################

# define a 24-hour period
#  - from 8 to 16 h -> 10 units
#  - from 16 to 24 h -> 5 units
#  - from 24 to 8 h -> 1 unit
my_schedule <- schedule(timetable = c(8, 16, 24), 
                        period = 24, 
                        values = c(10, 5, 1))
my_schedule$get_schedule()

t0 <- create_trajectory() %>%
  seize("doctor", 1) %>%
  timeout(function() rexp(1, 1)) %>%
  release("doctor", 1)

env <- simmer() %>%
  add_resource("doctor", capacity=my_schedule) %>%
  add_generator("patient", t0, function() rexp(1, 5)) %>%
  run(200)

plot_resource_usage(env, "doctor")

t0<-
  create_trajectory() %>%
  seize("t-rex") %>%
  timeout(5) %>%
  release("t-rex")

sim<-
  simmer() %>%
  add_resource("t-rex", capacity = schedule(timetable = c(5,10,15), values = c(1,0,1), period=Inf)) %>%
  add_generator("piggy", t0, at(0,0,0)) %>%
  run()

get_mon_arrivals(sim)
plot_resource_usage(sim, "t-rex")

###################################################

t1 <- create_trajectory("my trajectory", verbose=T) %>%
  ## add an intake activity
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1)

t2 <- create_trajectory(verbose=T) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1)

t3 <- create_trajectory(verbose=T) %>%
  ## add a planning activity
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

t0 <- join(t1, t2, t3)
t0 %>% get_head; t1 %>% get_head
t0 %>% get_tail; t3 %>% get_tail

t0 <- create_trajectory() %>%
  join(t1) %>%
  ## add a consultation activity
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  join(t3)

t1 <- create_trajectory() %>%
  branch(function() 1, T, 
         create_trajectory() %>% timeout(1))

####################################################################

t <- create_trajectory(verbose=T) %>%
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

####################################################################

t <- create_trajectory(verbose=T) %>%
  batch(3, timeout=0, rule=NULL) %>%
  timeout(1) %>%
  separate() %>%
  timeout(1)

simmer(verbose=TRUE) %>%
  add_generator("arrival", t, at(0, 1, 2, 3)) %>%
  run()
