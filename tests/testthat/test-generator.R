context("generator")

test_that("a generator without a trajectory fails", {
  expect_error(Simmer$new()$add_generator("customer", 4, 1))
})

test_that("a non-function dist fails", {
  t0 <- Trajectory$new()
  
  expect_error(Simmer$new()$add_generator("customer", t0, 1))
})

test_that("an empty trajectory fails", {
  t0 <- Trajectory$new()
  
  expect_error(Simmer$new()$add_generator("customer", t0, function() {}))
})

test_that("a dist that returns a non-numeric value fails", {
  t0 <- Trajectory$new()$timeout(function() 1)
  
  expect_error(Simmer$new()$add_generator("customer", t0, function() {}))
})
