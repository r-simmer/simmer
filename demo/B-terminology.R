## ---- create-trajectory

library(simmer)

# create a trajectory "my_trajectory"
# this trajectory consists of 3 activities, a seize, timeout and release activity.
t0<-
  create_trajectory("my_trajectory") %>%
  # seize one unit of the resource called "operator"
  seize("operator", 1) %>%
  # spend some (stochastic) time holding that resource
  timeout(function() rpois(1,50)) %>%
  # release the previously seized unit of "operator"
  release("operator", 1)

## ---- setup-simmer

env<-simmer() %>%
  # a resource called "operator"" is created with a capacity of 1 and an infinite queue size
  add_resource("operator", 1, Inf) %>%
  # this generator will create arrivals at each interval rpois(1,40) during
  # the simulation lifetime (while "dist" returns positive time values)
  # the arrivals will follow the trajectory specified in t0
  add_generator("my_generator", 
                trajectory = t0,
                dist = function() rpois(1, 40)) %>%
  run
