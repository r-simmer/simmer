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
  
  expect_equal(arrivals[5,]$finished, FALSE)
  expect_equal(resources[5,]$server, 2)
  expect_equal(resources[5,]$queue, 2)
})
