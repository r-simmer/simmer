library(simmer)

t1 <- create_trajectory() %>%
  set_attribute("global", function() runif(1))

t2 <- create_trajectory() %>%
  log_(function(attr) paste0(attr["global"]))

env <- simmer() %>%
  add_generator("writer", t1, at(0), mon = 2) %>% # monitoring flag??
  add_generator("reader", t2, at(1)) %>%
  run()

env %>% get_mon_attributes()

#####################################################################

library(simmer)

TICKETS <- 50     # Number of tickets per movie
SIM_TIME <- 120   # Simulate until
movies <- c("R Unchained", "Kill Process", "Pulp Implementation")

set.seed(42)
env <- simmer()

moviegoer <- create_trajectory() %>%
  # select a movie and set reneging condition
  set_attribute("movie", function() sample(3, 1)) %>%
  select(function(attr) movies[attr["movie"]]) %>%
  renege_if(function(attr) paste0(movies[attr["movie"]], " sold out")) %>%
  # check if movie is sold out
  branch(function(attr) get_capacity(env, movies[attr["movie"]]) == 0,
         continue = FALSE,
         # sold out, leave
         create_trajectory() %>% timeout(0)
  ) %>%
  # wait for my turn
  seize("counter", 1) %>%
  # buy tickets
  seize_selected(function() sample(6, 1), continue = FALSE,
                 reject = create_trajectory() %>%
                   # not enough tickets, leave after some discussion
                   timeout(0.5) %>%
                   release("counter", 1)
  ) %>%
  renege_abort() %>%
  timeout(1) %>%
  branch(function(attr) get_server_count(env, movies[attr["movie"]]) > (TICKETS - 2),
         continue = TRUE,
         # trigger the "sold out" event for the movie
         create_trajectory() %>%
           set_capacity_selected(0) %>%
           send(function(attr) paste0(movies[attr["movie"]], " sold out"))
  ) %>%
  release("counter", 1) %>%
  # watch the movie
  wait()

# add movies as resources with capacity TICKETS and no queue
lapply(movies, function(i) add_resource(env, i, TICKETS, 0)) %>% invisible

# add ticket counter with capacity 1 and infinite queue
# add moviegoer generator
# start simulation
env %>%
  add_resource("counter", 1) %>%
  add_generator("moviegoer", moviegoer, function() rexp(1, 1 / 0.5)) %>%
  run(SIM_TIME)
