library(simmer)
library(ggplot2)

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

## bug! reject -> the branch remains open!
option.1 <- function(n) {
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

option.2 <- function(n) {
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

gas.station.1 <- option.1(5000)
gas.station.2 <- option.2(5000)
graph <- plot_resource_usage(gas.station.1, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)
graph <- plot_resource_usage(gas.station.2, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)
