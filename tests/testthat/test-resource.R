context("resource")

test_that("resources are correctly created", {
  simmer <- Simmer$new() $
    add_resource("server", 5, Inf)
  
  expect_warning(simmer$add_resource("server"))
  expect_error(simmer$get_res_capacity())
  expect_error(simmer$get_res_capacity("asdf"))
  expect_equal(simmer$get_res_capacity("server"), 5)
  expect_error(simmer$get_res_queue_size())
  expect_error(simmer$get_res_queue_size("asdf"))
  expect_equal(simmer$get_res_queue_size("server"), Inf)
})

test_that("a negative capacity or queue_size is converted to positive", {
  simmer <- Simmer$new() $
    add_resource("server", -4, -1)
  
  expect_equal(simmer$get_res_capacity("server"), 4)
  expect_equal(simmer$get_res_queue_size("server"), 1)
})

test_that("a non-existent resource fails", {
  t0 <- Trajectory$new("") $
    seize("server", 1) $
    release("server", 1)
  
  simmer <- Simmer$new() $
    add_generator("customer", t0, function() 1)
  
  expect_error(simmer$run())
})

test_that("resource slots are correctly filled", {
  t0 <- Trajectory$new("") $
    seize("server", 1)
  
  simmer <- Simmer$new() $
    add_resource("server", 2, 2) $
    add_generator("customer", t0, function() 1) $
    run(5.5)
  
  arrivals <- simmer$get_mon_arrivals()
  resources <- simmer$get_mon_resources()
  
  expect_equal(arrivals[5,]$finished, FALSE)
  expect_equal(resources[5,]$server, 2)
  expect_equal(resources[5,]$queue, 2)
})
