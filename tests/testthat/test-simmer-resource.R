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
    seize("server", 1) %>%
    set_attribute("dummy", 1)
  
  env <- simmer() %>%
    add_resource("server", 2, 2) %>%
    add_generator("customer", t0, at(1:5), mon=2) %>%
    run()
  
  arrivals <- env%>%get_mon_arrivals()
  arrivals_res <- env%>%get_mon_arrivals(TRUE)
  resources <- env%>%get_mon_resources()
  attributes <- env%>%get_mon_attributes()
  
  expect_equal(arrivals[3,]$finished, FALSE)
  expect_equal(nrow(arrivals_res), 0)
  expect_equal(resources[4,]$server, 2)
  expect_equal(resources[4,]$queue, 2)
  expect_equal(sum(attributes$value), 2)
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
  
  expect_equal(resources[1,]$server, 1)
  expect_equal(resources[2,]$server, 0)
})

test_that("priority queues are adhered to (1)", {
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
  
  arrs <-
    env%>%get_mon_arrivals()
    
  expect_equal(arrs[arrs$name=="__prior0",]$end_time, 4)
})

test_that("priority queues are adhered to (2)", {
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
  
  arrs <-
    env%>%get_mon_arrivals()
    
  expect_equal(arrs[arrs$name=="__prior0",]$end_time, 4) 
})

test_that("priority queues are adhered to and same level priorities are processed FIFO", {
  t0 <- create_trajectory("_t0_prior") %>%
    seize("server", 1, priority=1) %>%
    timeout(2) %>%
    release("server", 1)
  
  t1 <- create_trajectory("_t1_prior") %>%
    seize("server", 1, priority=1) %>%
    timeout(2) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("_t0_prior", t0, at(c(0, 2, 4, 6))) %>%
    add_generator("_t1_prior", t1, at(c(1, 3, 5, 7))) %>%
    run()
  
  arrs <-
    env%>%get_mon_arrivals()
  
  arrs_ordered <-
    arrs[order(arrs$end_time),]
    
  expect_equal(as.character(arrs_ordered$name), 
                c("_t0_prior0", "_t1_prior0", "_t0_prior1", "_t1_prior1", 
                  "_t0_prior2", "_t1_prior2", "_t0_prior3", "_t1_prior3"))
})
