# ---- setup

library(simmer)

env <- simmer("SuperDuperSim")
env

# ---- part1

patient <- create_trajectory("patients' path") %>%
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

# ---- part2

env %>%
  add_resource("nurse", 1) %>%
  add_resource("doctor", 2) %>%
  add_resource("administration", 1) %>%
  add_generator("patient", patient, function() rnorm(1, 10, 2))

# ---- part3

env %>% run(until=80)
env %>% now()
env %>% peek(3)

# ---- part4

env %>% onestep()
env %>% onestep() %>% onestep() %>% onestep()
env %>% now()
env %>% peek(Inf, verbose=TRUE)

# ---- part5

env %>% 
  run(until=120) %>%
  now()

# ---- part6

env %>% 
  reset() %>% 
  run(until=80) %>%
  now()

# ---- part7

envs <- lapply(1:100, function(i) {
  simmer("SuperDuperSim") %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
    run(80)
})

# ---- part8

library(parallel)

envs <- mclapply(1:100, function(i) {
  simmer("SuperDuperSim") %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", patient, function() rnorm(1, 10, 2)) %>%
    run(80) %>%
    wrap()
})

# ---- part9

envs[[1]] %>% get_n_generated("patient")
envs[[1]] %>% get_capacity("doctor")
envs[[1]] %>% get_queue_size("doctor")
head(
  envs %>% get_mon_resources()
)
head(
  envs %>% get_mon_arrivals()
)

# ---- part10

plot_resource_utilization(envs, c("nurse", "doctor","administration"))

# ---- part11

plot_resource_usage(envs, "doctor", items="server", steps=T)

# ---- part12

plot_resource_usage(envs[[6]], "doctor", items="server", steps=T)

# ---- part13

plot_evolution_arrival_times(envs, type = "flow_time")
