context("method chaining")

test_that("Trajectory's method chaining works", {
  t0 <- Trajectory$new() $
    show() $
    seize("one", 1) $
    release("one", 1) $
    timeout(function() 1) $
    branch(0.1, T, Trajectory$new())
  
  expect_is(t0, "Trajectory")
})

test_that("Simmer's method chaining works", {
  t0 <- Trajectory$new() $
    timeout(function() 1)
  
  simmer <- Simmer$new() $
    add_resource("server") $
    add_generator("customer", t0, function() 1) $
    step() $
    run(10) $
    reset()
  
  expect_is(simmer, "Simmer")
})
