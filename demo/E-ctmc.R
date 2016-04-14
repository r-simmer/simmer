# ---- setup

library(simmer)
library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(1234)

# ---- example1-part1

# Arrival rate
lambda <- 3/20
# Service rate (cars, motorcycles)
mu <- c(1/8, 1/3)
# Probability of car
p <- 0.75

# Theoretical resolution
A <- matrix(c(1,   mu[1],            0,
              1, -lambda, (1-p)*lambda,
              1,   mu[2],       -mu[2]), byrow=T, ncol=3)
B <- c(1, 0, 0)
P <- solve(t(A), B)
N_average_theor <- sum(P * c(1, 0, 1)) ; N_average_theor

# ---- example1-part2

option.1 <- function(t) {
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
    run(until=t)
}

# ---- example1-part3

option.2 <- function(t) {
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
    run(until=t)
}

# ---- example1-part4

option.3 <- function(t) {
  vehicle <- create_trajectory() %>%
    seize("pump", amount=1) %>%
    timeout(function() {
      if (runif(1) < p) rexp(1, mu[1])  # car
      else rexp(1, mu[2])               # mcycle
    }) %>%
    release("pump", amount=1)
  
  simmer() %>%
    add_resource("pump", capacity=1, queue_size=0) %>%
    add_generator("vehicle", vehicle, function() rexp(1, lambda)) %>%
    run(until=t)
}

# ---- example1-part5

gas.station <- option.3(5000)

# Evolution + theoretical value
graph <- plot_resource_usage(gas.station, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)

# ---- example1-part6

library(microbenchmark)

t <- 1000/lambda
tm <- microbenchmark(option.1(t), 
                     option.2(t), 
                     option.3(t))
graph <- autoplot(tm)
graph + scale_y_log10(breaks=function(limits) pretty(limits, 5)) +
  ylab("Time [milliseconds]")

# ---- example2-part1

# Theoretical resolution
A <- matrix(c(1,                   0,       0,               mu[1],            0,
              1, -(1-p)*lambda-mu[1],   mu[1],                   0,            0,
              1,            p*lambda, -lambda,        (1-p)*lambda,            0,
              1,                   0,   mu[2], -(1-p)*lambda-mu[2], (1-p)*lambda,
              1,                   0,       0,               mu[2],       -mu[2]), 
            byrow=T, ncol=5)
B <- c(1, 0, 0, 0, 0)
P <- solve(t(A), B)
N_average_theor <- sum(P * c(2, 1, 0, 1, 2)) ; N_average_theor

# ---- example2-part2

option.1 <- function(t) {
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
  env %>% run(until=t)
}

# ---- example2-part3

option.2 <- function(t) {
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
  env %>% run(until=t)
}

# ---- example2-part4

option.3 <- function(t) {
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
  env %>% run(until=t)
}

# ---- example2-part5

option.4 <- function(t) {
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
    run(until=t)
}

# ---- example2-part6

option.5 <- function(t) {
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
    run(until=t)
}

# ---- example2-part7


gas.station <- option.1(5000)

# Evolution + theoretical value
graph <- plot_resource_usage(gas.station, "pump", items="system")
graph + geom_hline(yintercept=N_average_theor)

# ---- example2-part8

gas.station <- option.5(5000)

limits <- data.frame(item = c("queue", "server", "system"), value = c(1, 1, 2))

graph <- gas.station %>% get_mon_resources() %>% 
  gather(item, value, server, queue, system) %>%
  mutate(value = round(value * 2/5),                  # scaling here <------
         item = factor(item)) %>%
  filter(item %in% "system") %>%
  group_by(resource, replication, item) %>%
  mutate(mean = c(0, cumsum(head(value, -1) * diff(time))) / time) %>% 
  ungroup() %>%
  ggplot() + aes(x=time, color=item) +
  geom_line(aes(y=mean, group=interaction(replication, item))) +
  ggtitle("Resource usage: pump") +
  ylab("in use") + xlab("time") + expand_limits(y=0) +
  geom_hline(aes(yintercept=value, color=item), limits, lty=2)
graph + geom_hline(yintercept=N_average_theor)

# ---- example2-part9

library(microbenchmark)

t <- 1000/lambda
tm <- microbenchmark(option.1(t), 
                     option.2(t), 
                     option.3(t),
                     option.4(t),
                     option.5(t))
graph <- autoplot(tm)
graph + scale_y_log10(breaks=function(limits) pretty(limits, 5)) +
  ylab("Time [milliseconds]")
