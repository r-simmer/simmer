context("branch")

test_that("a non-function option fails", {
  expect_error(Trajectory$new()$branch(1, T, Trajectory$new()))
})

test_that("the wrong number of elements fails", {
  expect_error(Trajectory$new()$branch(function() 1, T, Trajectory$new(), Trajectory$new()))
  expect_error(Trajectory$new()$branch(function() 1, c(T, T), Trajectory$new()))
})

test_that("an empty Trajectory fails", {
  expect_error(Trajectory$new()$branch(function() 1, T, Trajectory$new()))
})

test_that("an index out of range fails", {
  t0 <- Trajectory$new() $ 
    branch(function() 0, T, 
      Trajectory$new()$timeout(function() 1)
    )
  t1 <- Trajectory$new() $ 
    branch(function() 1, T, 
      Trajectory$new()$timeout(function() 1)
    )
  t2 <- Trajectory$new() $ 
    branch(function() 2, T, 
      Trajectory$new()$timeout(function() 1)
    )
  
  simmer <- Simmer$new() $
    add_generator("entity", t0, function() 1)
  expect_error(simmer$run(10))
  
  simmer <- Simmer$new() $
    add_generator("entity", t2, function() 1)
  expect_error(simmer$run(10))
  
  simmer <- Simmer$new() $
    add_generator("entity", t1, function() 1) $
    run(10)
  expect_equal(simmer$now(), 10)
})
