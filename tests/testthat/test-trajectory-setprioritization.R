context("set_prioritization")

test_that("priority queues are adhered to (2)", {
  t0 <- create_trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  t1 <- create_trajectory() %>%
    set_prioritization(c(1, -1, -1)) %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 0))) %>%
    add_generator("__prior", t1, at(1)) %>% # should be served second
    run()
  
  arrs <-
    env%>%get_mon_arrivals()
  
  expect_equal(arrs[arrs$name=="__prior0",]$end_time, 4) 
})

test_that("priority queues are adhered to (3)", {
  t0 <- create_trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  t1 <- create_trajectory() %>%
    set_prioritization(function() c(1, -1, -1)) %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 0))) %>%
    add_generator("__prior", t1, at(1)) %>% # should be served second
    run()
  
  arrs <-
    env%>%get_mon_arrivals()
  
  expect_equal(arrs[arrs$name=="__prior0",]$end_time, 4) 
})
