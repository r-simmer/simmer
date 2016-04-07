library(simmer)
library(ggplot2)

lambda <- 3/20
mu <- c(1/8, 1/3)
p <- 0.75

A <- matrix(c(1,   mu[1],            0,
              1, -lambda, (1-p)*lambda,
              1,   mu[2],       -mu[2]), byrow=T, ncol=3)
B <- c(1, 0, 0)
P <- solve(t(A), B)
N_average_theor <- sum(P * c(1, 0, 1)); N_average_theor

####################################################

option.1 <- function(n) {
  car <- create_trajectory() %>%
    seize("pump", amount=1) %>%
    timeout(function() rexp(1, mu[1])) %>%
    release("pump", amount=1)
  
  mcycle <- create_trajectory() %>%
    seize("pump", amount=1) %>%
    timeout(function() rexp(1, mu[2])) %>%
    release("pump", amount=1)
  
  simmer() %>%
    add_resource("pump", capacity=1, queue_size=0) %>%
    add_generator("car", car, function() rexp(1, p*lambda)) %>%
    add_generator("mcycle", mcycle, function() rexp(1, (1-p)*lambda)) %>%
    run(until=n/lambda)
}

####################################################

option.2a <- function(n) {
  vehicle <- create_trajectory() %>%
    seize("pump", amount=1) %>%
    branch(function() sample(c(1, 2), 1, prob=c(p, 1-p)), c(T, T), 
           create_trajectory("car") %>%
             timeout(function() rexp(1, mu[1])),
           create_trajectory("mcycle") %>%
             timeout(function() rexp(1, mu[2]))) %>%
    release("pump", amount=1)
  
  simmer() %>%
    add_resource("pump", capacity=1, queue_size=0) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda)) %>%
    run(until=n/lambda)
}

####################################################

option.2b <- function(n) {
  vehicle <- create_trajectory() %>%
    seize("pump", amount=1) %>%
    branch(function() (runif(1) > p) + 1, c(T, T), 
           create_trajectory("car") %>%
             timeout(function() rexp(1, mu[1])),
           create_trajectory("mcycle") %>%
             timeout(function() rexp(1, mu[2]))) %>%
    release("pump", amount=1)
  
  simmer() %>%
    add_resource("pump", capacity=1, queue_size=0) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda)) %>%
    run(until=n/lambda)
}

####################################################

option.3 <- function(n) {
  vehicle <- create_trajectory() %>%
    seize("pump", amount=1) %>%
    timeout(function() {
      if (runif(1) < p) rexp(1, mu[1])
      else rexp(1, mu[2])
    }) %>%
    release("pump", amount=1)
  
  simmer() %>%
    add_resource("pump", capacity=1, queue_size=0) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda)) %>%
    run(until=n/lambda)
}

####################################################

library(microbenchmark)

tm <- microbenchmark(option.1(1000), option.2a(1000), option.2b(1000), option.3(1000))

autoplot(tm)

####################################################

gas.station <- option.3(1000)
# Evolution of the average number of customers in the system
graph <- plot_resource_usage(gas.station, "pump", items="system")
graph
# Theoretical value
graph + geom_hline(yintercept=N_average_theor)

####################################################
####################################################

lambda <- 3/20
mu <- c(1/8, 1/3)
p <- 0.75

A <- matrix(c(1,                   0,       0,               mu[1],            0,
              1, -(1-p)*lambda-mu[1],   mu[1],                   0,            0,
              1,            p*lambda, -lambda,        (1-p)*lambda,            0,
              1,                   0,   mu[2], -(1-p)*lambda-mu[2], (1-p)*lambda,
              1,                   0,       0,               mu[2],       -mu[2]), byrow=T, ncol=5)
B <- c(1, 0, 0, 0, 0)
P <- solve(t(A), B)
N_average_theor <- sum(P * c(2, 1, 0, 1, 2)) ; N_average_theor

####################################################

option.1 <- function(n) {
  car <- create_trajectory() %>%
    seize("pump", amount=function() {
      if (env %>% get_server_count("pump")) 2  # rejection
      else 1                                   # serve
    }) %>%
    timeout(function() rexp(1, mu[1])) %>%
    release("pump", amount=1)
  
  mcycle <- create_trajectory() %>%
    seize("pump", amount=1) %>%
    timeout(function() rexp(1, mu[2])) %>%
    release("pump", amount=1)
  
  env <- simmer() %>%
    add_resource("pump", capacity=1, queue_size=1) %>%
    add_generator("car", car, function() rexp(1, p*lambda)) %>%
    add_generator("mcycle", mcycle, function() rexp(1, (1-p)*lambda))
  env %>% run(until=n/lambda)
}

####################################################

## bug! reject -> the branch remains open!
option.2a <- function(n) {
  vehicle <- create_trajectory() %>%
    branch(function() sample(c(1, 2), 1, prob=c(p, 1-p)), c(T, T), # <-----------------
           create_trajectory("car") %>%
             seize("pump", amount=function() {
               if (env %>% get_server_count("pump")) 2  # rejection
               else 1                                   # serve
             }) %>%
             timeout(function() rexp(1, mu[1])) %>%
             release("pump", amount=1),                 # always 1
           create_trajectory("mcycle") %>%
             seize("pump", amount=1) %>%
             timeout(function() rexp(1, mu[2])) %>%
             release("pump", amount=1))
  
  env <- simmer() %>%
    add_resource("pump", capacity=1, queue_size=1) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda))
  env %>% run(until=n/lambda)
}

option.2b <- function(n) {
  vehicle <- create_trajectory() %>%
    branch(function() sample(c(1, 2), 1, prob=c(p, 1-p)), c(F, F), 
           create_trajectory("car") %>%
             seize("pump", amount=function() {
               if (env %>% get_server_count("pump")) 2  # rejection
               else 1                                   # serve
             }) %>%
             timeout(function() rexp(1, mu[1])) %>%
             release("pump", amount=1),                 # always 1
           create_trajectory("mcycle") %>%
             seize("pump", amount=1) %>%
             timeout(function() rexp(1, mu[2])) %>%
             release("pump", amount=1))
  
  env <- simmer() %>%
    add_resource("pump", capacity=1, queue_size=1) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda))
  env %>% run(until=n/lambda)
}

####################################################

option.3 <- function(n) {
  vehicle <- create_trajectory("car") %>%
    set_attribute("vehicle", function() sample(c(1, 2), 1, prob=c(p, 1-p))) %>%
    seize("pump", amount=function(attrs) {
      if (attrs["vehicle"] == 1 &&
          env %>% get_server_count("pump")) 2    # car rejection
      else 1                                     # serve
    }) %>%
    timeout(function(attrs) rexp(1, mu[attrs["vehicle"]])) %>%
    release("pump", amount=1)                    # always 1
  
  env <- simmer() %>%
    add_resource("pump", capacity=1, queue_size=1) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda))
  env %>% run(until=n/lambda)
}

####################################################

option.4 <- function(n) {
  vehicle <- create_trajectory() %>%
    branch(function() sample(c(1, 2), 1, prob=c(p, 1-p)), c(F, F), 
           create_trajectory("car") %>%
             seize("pump", amount=3) %>%
             timeout(function() rexp(1, mu[1])) %>%
             release("pump", amount=3),
           create_trajectory("mcycle") %>%
             seize("pump", amount=2) %>%
             timeout(function() rexp(1, mu[2])) %>%
             release("pump", amount=2))
  
  simmer() %>%
    add_resource("pump", capacity=3, queue_size=2) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda)) %>%
    run(until=n/lambda)
}

####################################################

option.5 <- function(n) {
  car <- create_trajectory() %>%
    seize("pump", amount=3) %>%
    timeout(function() rexp(1, mu[1])) %>%
    release("pump", amount=3)
  
  mcycle <- create_trajectory() %>%
    seize("pump", amount=2) %>%
    timeout(function() rexp(1, mu[2])) %>%
    release("pump", amount=2)
  
  simmer() %>%
    add_resource("pump", capacity=3, queue_size=2) %>%
    add_generator("car", car, function() rexp(1, p*lambda)) %>%
    add_generator("mcycle", mcycle, function() rexp(1, (1-p)*lambda)) %>%
    run(until=n/lambda)
}

####################################################

library(microbenchmark)

tm <- microbenchmark(option.1(1000), option.2b(1000), option.3(1000), option.4(1000), option.5(1000))

autoplot(tm)

####################################################

gas.station.1 <- option.1(5000)
gas.station.2a <- option.2a(5000)
gas.station.2b <- option.2b(5000)
gas.station.3 <- option.3(5000)
gas.station.4 <- option.4(5000)
gas.station.5 <- option.5(5000)
graph <- plot_resource_usage(gas.station.1, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)
graph <- plot_resource_usage(gas.station.2a, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)
graph <- plot_resource_usage(gas.station.2b, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)
graph <- plot_resource_usage(gas.station.3, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)
graph <- plot_resource_usage(gas.station.4, "pump", items="system")
graph + geom_hline(yintercept=sum(P * c(5, 3, 0, 2, 4)))
graph <- plot_resource_usage(gas.station.5, "pump", items="system")
graph + geom_hline(yintercept=sum(P * c(5, 3, 0, 2, 4)))

# Evolution of the average number of customers in the system
graph <- plot_resource_usage(gas.station, "pump", items="system")
graph
# Theoretical value
graph + geom_hline(yintercept=N_average_theor)
