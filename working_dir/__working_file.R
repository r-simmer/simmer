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
