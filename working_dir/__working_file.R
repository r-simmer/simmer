library(simmer)

t0 <- Trajectory$new("my trajectory") $
  ## add an intake activity
  seize("nurse", 1) $
  timeout(function() rnorm(1, 15)) $
  release("nurse", 1) $
  ## add a consultation activity
  seize("doctor", 1) $
  timeout(function() rnorm(1, 20)) $
  release("doctor", 1) $
  ## add a planning activity
  seize("administration", 1) $
  timeout(function() rnorm(1, 5)) $
  release("administration", 1)

reps <- lapply(1:100, function(i) {
  Simmer$new("SuperDuperSim", verbose=F) $
    add_resource("nurse", 1) $
    add_resource("doctor", 2) $
    add_resource("administration", 1) $
    add_generator("patient", t0, function() rnorm(1, 10, 2)) $
    run(80)
})

plot_resource_usage(reps, "doctor", items="server", steps=T)
plot_resource_utilization(reps, c("nurse", "doctor","administration"))
plot_evolution_arrival_times(reps, type="flow_time")

#################################################################

mm1 <- Trajectory$new() $
  seize("server", 1) $
  timeout(function() rexp(1, 2)) $
  release("server", 1)

simmer <- Simmer$new(verbose=F) $
  add_resource("server", 1, 2) $
  add_generator("customer", mm1, function() rexp(1, 1), mon=F)
simmer$run(1000)

plot_resource_usage(simmer, "server")

#################################################################

system.time({
mm1 <- Trajectory$new() $
  seize("server", 1) $
  timeout(function() rexp(1, 66)) $
  release("server", 1)

simmer <- Simmer$new(verbose=F) $
  add_resource("server", 1) $
  add_generator("customer", mm1, function() rexp(1, 60), mon=F)
simmer$run(10000)
})
# 86 seconds (Python: ~ 6 seconds, pure R6: ~ 15300)

plot_resource_usage(simmer, "server")

#################################################################

t0 <- Trajectory$new("my trajectory") $
  seize("server", 1) $
  timeout(function() rexp(1, 1)) $
  branch(prob=0.1, merge=F, Trajectory$new("branch1") $
    seize("server", 2) $
    timeout(function() 1) $
    release("server", 2)
  ) $
  branch(prob=0.9, merge=T, Trajectory$new("branch2") $
    seize("server", 4) $
    timeout(function() rexp(1, 3)) $
    release("server", 4)
  ) $
  timeout(function() 1) $
  release("server", 1)

a <- t0$get_head()
a$next_activity
a <- a$next_activity

t0$show()

