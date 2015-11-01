context("basic Simmer functionality")

test_that("an empty environment behaves as expected", {
  simmer <- Simmer$new()
  
  expect_is(simmer, "Simmer")
  expect_equal(simmer$now(), 0)
  expect_equal(simmer$peek(), Inf)
  
  simmer$step()$run()
  
  expect_equal(simmer$now(), 0)
  expect_equal(simmer$peek(), Inf)
})

t0 <- Trajectory$new("") $
  seize("server", 1) $
  release("server", 1)

test_that("the simulator is reset", {
  simmer <- Simmer$new() $
    add_resource("server", 1, queue_size=0) $
    add_generator("entity", t0, function() 1) $
    run(100) $
    reset()
  arrivals <- simmer$get_mon_arrivals()
  resources <- simmer$get_mon_resources()
  
  expect_equal(simmer$now(), 0)
  expect_equal(simmer$peek(), 1)
  expect_equal(nrow(arrivals), 0)
  expect_equal(nrow(resources), 0)
})

test_that("the simulator stops if there are no more events", {
  onlyone <- function(){
    generate <- 1
    function() {
      if (generate) {
        generate <<- 0
        return(1)
      } else return(-1)
    }
  }
  simmer <- Simmer$new() $
    add_resource("server", 1) $
    add_generator("entity", t0, onlyone()) $
    run(10)
  
  expect_equal(simmer$now(), 1)
})

test_that("a negative simulation time is converted to positive", {
  simmer <- Simmer$new() $
    add_resource("server", 1) $
    add_generator("entity", t0, function() 1) $
    run(-10)
  
  expect_equal(simmer$now(), 10)
})

test_that("a stopped simulation can be resumed", {
  simmer <- Simmer$new() $
    add_resource("server", 1) $
    add_generator("entity", t0, function() 1) $
    run(10)
  
  expect_equal(simmer$now(), 10)
  simmer$run(20)
  expect_equal(simmer$now(), 20)
  simmer$run(30)
  expect_equal(simmer$now(), 30)
})
