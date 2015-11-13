context("simulation 1")

test_that("a simple deterministic simulation with rejections behaves as expected", {
  t0 <- Trajectory$new("") $
    seize("server", 1) $
    timeout(function() 2) $
    release("server", 1)
  
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
  expect_equal(sum(subset(arrivals, finished)$activity_time), n)
  expect_equal(sum(subset(arrivals, !finished)$activity_time), 0)
  
  expect_equal(nrow(resources), n*1.5)
  expect_equal(sum(resources$server), n)
  expect_equal(mean(resources$server), 2/3)
  expect_equal(sum(resources$queue), 0)
})
