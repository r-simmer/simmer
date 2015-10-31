context("Simmer tests")

test_that("An empty environment throws errors", {
  simmer <- Simmer$new()
  
  expect_is(simmer, "Simmer")
  expect_equal(simmer$peek(), Inf)
  expect_error(simmer$step())
  expect_error(simmer$run())
})

test_that("Resources are correctly created", {
  simmer <- Simmer$new() $
    add_resource("server", 5, 10)
  
  expect_warning(simmer$add_resource("server"))
  expect_error(simmer$get_res_capacity())
  expect_error(simmer$get_res_capacity("asdf"))
  expect_equal(simmer$get_res_capacity("server"), 5)
  expect_error(simmer$get_res_queue_size())
  expect_error(simmer$get_res_queue_size("asdf"))
  expect_equal(simmer$get_res_queue_size("server"), 10)
})

t0 <- Trajectory$new("") $
  seize("server", 1) $
  timeout(function() 2) $
  release("server", 1)

test_that("A simple deterministic simulation behaves as expected", {
  simmer <- Simmer$new() $
    add_resource("server", 1, queue_size=0) $
    add_generator("entity", t0, function() 1)
  
  expect_warning(simmer$add_generator("entity", t0, function() 2))
  expect_equal(simmer$now(), 0)
  expect_equal(simmer$peek(), 1)
  
  simmer$step()$step()$step()
  
  expect_equal(simmer$now(), 1)
  expect_equal(simmer$peek(), 2)
  
  n <- 1000
  simmer$run(n)$step()$step()
  arrivals <- simmer$get_mon_arrivals()
  resources <- simmer$get_mon_resources()
  
  expect_equal(simmer$now(), n+1)
  expect_equal(simmer$peek(), n+1)
  expect_equal(nrow(arrivals), n)
  expect_equal(nrow(subset(arrivals, finished)), n/2)
  expect_equal(nrow(subset(arrivals, !finished)), n/2)
  
  expect_equal(nrow(resources), n*1.5)
  expect_equal(sum(resources$server), n)
  expect_equal(mean(resources$server), 2/3)
  expect_equal(sum(resources$queue), 0)
})

test_that("The simulator is reset", {
  simmer <- Simmer$new() $
    add_resource("server", 1, queue_size=0) $
    add_generator("entity", t0, function() 1) $
    run(1000)$reset()
  arrivals <- simmer$get_mon_arrivals()
  resources <- simmer$get_mon_resources()
  
  expect_equal(simmer$now(), 0)
  expect_equal(simmer$peek(), 1)
  expect_equal(nrow(arrivals), 0)
  expect_equal(nrow(resources), 0)
})
