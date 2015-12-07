context("resource")

test_that("resources are correctly created", {
  env <- simmer() %>%
    add_resource("server", 5, Inf)
  
  expect_warning(env%>%add_resource("server"))
  expect_error(env%>%get_capacity("asdf"))
  expect_equal(env%>%get_capacity("server"), 5)
  expect_error(env%>%get_queue_size("asdf"))
  expect_equal(env%>%get_queue_size("server"), Inf)
  expect_error(env%>%get_server_count("asdf"))
  expect_equal(env%>%get_server_count("server"), 0)
  expect_error(env%>%get_queue_count("asdf"))
  expect_equal(env%>%get_queue_count("server"), 0)
})

test_that("a negative capacity or queue_size is converted to positive", {
  env <- simmer() %>%
    add_resource("server", -4, -1)
  
  expect_equal(env%>%get_capacity("server"), 4)
  expect_equal(env%>%get_queue_size("server"), 1)
})

test_that("a non-existent resource fails", {
  t0 <- create_trajectory("") %>%
    seize("server", 1) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_generator("customer", t0, function() 1)
  
  expect_error(env%>%run())
})

test_that("resource slots are correctly filled", {
  t0 <- create_trajectory("") %>%
    seize("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 2, 2) %>%
    add_generator("customer", t0, function() 1) %>%
    run(5.5)
  
  arrivals <- env%>%get_mon_arrivals()
  resources <- env%>%get_mon_resources()
  
  expect_equal(arrivals[3,]$finished, FALSE)
  expect_equal(resources[5,]$server, 2)
  expect_equal(resources[5,]$queue, 2)
})


test_that("resources are  correctly monitored", {
  t0 <- create_trajectory("") %>%
    seize("server", 1) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 2) %>%
    add_generator("customer", t0, at(0)) %>%
    run()
  
  resources <- env%>%get_mon_resources()
  
  expect_equal(resources[2,]$server, 1) # to be discussed: debatable whether or not it should equal 1 or 0
})

test_that("priority queues are adhered to", {
  ### first run
  t0 <- create_trajectory("nonprior") %>%
    seize("server", 1, priority=0) %>%
    timeout(2) %>%
    release("server", 1)
  
  t1 <- create_trajectory("prior") %>%
    seize("server", 1, priority=1) %>%
    timeout(2) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 1))) %>%
    add_generator("__prior", t1, at(1)) %>% # should be served second
    run()
  
  resources <-
    env%>%get_mon_arrivals()
    
  expect_equal(resources[resources$name=="__prior0",]$end_time, 4) 
  
    ### second run
  t0 <- create_trajectory("nonprior") %>%
    seize("server", 1, priority=0) %>%
    timeout(2) %>%
    release("server", 1)
  
  t1 <- create_trajectory("prior") %>%
    seize("server", 1, priority=1) %>%
    timeout(2) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 0))) %>%
    add_generator("__prior", t1, at(1)) %>% # should be served second
    run()
  
  resources <-
    env%>%get_mon_arrivals()
    
  expect_equal(resources[resources$name=="__prior0",]$end_time, 4) 
  
})
