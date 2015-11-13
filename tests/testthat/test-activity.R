context("activity")

test_that("the activity chain grows as expected", {
  t0 <- Trajectory$new() $
    seize("nurse", 1) $
    timeout(function() rnorm(1, 15)) $
    branch(function() 1, T, Trajectory$new()$timeout(function() 1)) $
    release("nurse", 1)
  
  act <- activity.get_next(
    activity.get_next(
    activity.get_next(t0$get_head())))
  
  expect_equal(act, t0$get_tail())
  expect_equal(activity.get_next(act), NULL)
})

test_that("a negative amount is converted to positive", {
  t0 <- Trajectory$new()
  
  expect_silent(t0$seize("nurse", -2))
  expect_silent(t0$release("nurse", -10))
})

test_that("a non-function duration fails", {
  expect_error(Trajectory$new()$timeout(3))
})

test_that("a duration function that returns a non-numeric value fails", {
  t0 <- Trajectory$new() $
    timeout(function() {})
  
  simmer <- Simmer$new() $
    add_generator("entity", t0, function() 1)
  
  expect_error(simmer$run(100))
})
