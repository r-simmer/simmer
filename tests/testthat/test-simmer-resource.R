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

test_that("a big departure triggers more than one small seize from the queue", {
  t0 <- create_trajectory("") %>%
    seize("server", 2) %>%
    timeout(10) %>%
    release("server", 2)
  t1 <- create_trajectory("") %>%
    seize("server", 1) %>%
    timeout(10) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 2) %>%
    add_generator("a", t0, at(0)) %>%
    add_generator("b", t1, at(1, 2)) %>%
    run()
  
  arrs <- env%>%get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name),]
  
  expect_equal(as.character(arrs_ordered$name), c("a0", "b0", "b1"))
  expect_equal(arrs_ordered$end_time, c(10, 20, 20))
})
