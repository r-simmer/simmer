library(simmer)
library(dplyr)

# http://www.grotto-networking.com/DiscreteEventPython.html#QueDisciplines

mean_pkt_size <- 100        # bytes
lambda1 <- 2                # pkts/s
lambda3 <- 0.5              # pkts/s
lambda4 <- 0.6              # pkts/s
rate <- 2.2 * mean_pkt_size # bytes/s

set_msg_size <- function(.)
  set_attribute(., "size", function() rexp(1, 1/mean_pkt_size))

switch_port <- function(., id)
  seize(., paste0("res", id), 1) %>%
  timeout(function(attrs) attrs[["size"]] / rate) %>%
  release(paste0("res", id), 1)

switch_port_1 <- trajectory() %>%
  set_msg_size() %>%
  switch_port(1) %>%
  leave(0.25) %>%
  switch_port(2) %>%
  branch(function() (runif(1) > 0.65) + 1, c(F, F), 
         trajectory() %>%
           switch_port(3),
         trajectory() %>%
           switch_port(4))

switch_port_3 <- trajectory() %>%
  set_msg_size() %>%
  switch_port(3)

switch_port_4 <- trajectory() %>%
  set_msg_size() %>%
  switch_port(4)

env <- simmer()
lapply(1:4, function(i) env %>% add_resource(paste0("res", i))) %>% invisible
env %>%
  add_generator("arrival1_", switch_port_1, function() rexp(1, lambda1), mon=2) %>%
  add_generator("arrival3_", switch_port_3, function() rexp(1, lambda3), mon=2) %>%
  add_generator("arrival4_", switch_port_4, function() rexp(1, lambda4), mon=2) %>%
  run(4000)

res <- get_mon_arrivals(env, per_resource = TRUE) %>%
  select(name, resource) %>%
  filter(resource %in% c("res3", "res4"))
arr <- get_mon_arrivals(env) %>%
  mutate(waiting_time = end_time - (start_time + activity_time),
         generator = regmatches(name, regexpr("arrival[[:digit:]]", name))) %>%
  left_join(res) %>%
  group_by(generator, resource)

summarise(arr, average = sum(waiting_time) / n())
get_n_generated(env, "arrival1_") + get_n_generated(env, "arrival4_")
count(arr)
