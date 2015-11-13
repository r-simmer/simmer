context("method chaining")

test_that("Trajectory's method chaining works", {
  t0 <- create_trajectory() %>%
    seize("one", 1) %>%
    release("one", 1) %>%
    timeout(function() 1) %>%
    branch(function() 1, T, create_trajectory()%>%timeout(function() 1)) %>%
    seize("one", 1)
  
  expect_is(t0, "Trajectory")
})

test_that("Simmer's method chaining works", {
  t0 <- create_trajectory() %>%
    timeout(function() 1)
  
  env <- simmer() %>%
    add_resource("server") %>%
    add_generator("customer", t0, function() 1) %>%
    onestep() %>%
    run(10) %>%
    reset()
  
  expect_is(env, "Simmer")
})
