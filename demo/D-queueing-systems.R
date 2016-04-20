# ---- setup


library(simmer)
library(ggplot2)
library(dplyr)
library(parallel)
set.seed(1234)

# ---- part1

lambda <- 2
mu <- 4
rho <- lambda/mu # = 2/4

mm1.trajectory <- create_trajectory() %>%
  seize("resource", amount=1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("resource", amount=1)

mm1.env <- simmer() %>%
  add_resource("resource", capacity=1, queue_size=Inf) %>%
  add_generator("arrival", mm1.trajectory, function() rexp(1, lambda)) %>% 
  run(until=2000)

# ---- part2

# Evolution of the average number of customers in the system
graph <- plot_resource_usage(mm1.env, "resource", items="system")

# Theoretical value
mm1.N <- rho/(1-rho)
graph + geom_hline(yintercept=mm1.N)

# ---- part3

plot_resource_usage(mm1.env, "resource", items=c("queue", "server"), steps=TRUE) +
  xlim(0, 20) + ylim(0, 4)

# ---- part4

mm1.arrivals <- get_mon_arrivals(mm1.env)
mm1.t_system <- mm1.arrivals$end_time - mm1.arrivals$start_time

mm1.T <- mm1.N / lambda
mm1.T ; mean(mm1.t_system)

# ---- part5

envs <- mclapply(1:1000, function(i) {
  simmer() %>%
    add_resource("resource", capacity=1, queue_size=Inf) %>%
    add_generator("arrival", mm1.trajectory, function() rexp(1, lambda)) %>%
    run(1000/lambda) %>%
    wrap()
})

# ---- part6

t_system <- get_mon_arrivals(envs) %>%
  mutate(t_system = end_time - start_time) %>%
  group_by(replication) %>%
  summarise(mean = mean(t_system))

t.test(t_system$mean)

# ---- part7


lambda; 1/mean(diff(subset(mm1.arrivals, finished==TRUE)$start_time))

# ---- part8

qqplot(mm1.t_system, rexp(1000, 1/mm1.T))
abline(0, 1, lty=2, col="red")

# ---- part9

lambda <- 2
mu <- 4

mm23.trajectory <- create_trajectory() %>%
  seize("server", amount=1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("server", amount=1)

mm23.env <- simmer() %>%
  add_resource("server", capacity=2, queue_size=1) %>%
  add_generator("arrival", mm23.trajectory, function() rexp(1, lambda)) %>%
  run(until=2000)

# ---- part10

mm23.arrivals <- get_mon_arrivals(mm23.env)
mm23.arrivals %>%
  summarise(rejection_rate = sum(!finished)/length(finished))

# ---- part11

mm23.t_system <- mm23.arrivals$end_time - mm23.arrivals$start_time
# Comparison with M/M/1 times
qqplot(mm1.t_system, mm23.t_system)
abline(0, 1, lty=2, col="red")