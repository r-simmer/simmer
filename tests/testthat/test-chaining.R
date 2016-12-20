context("method chaining")

test_that("trajectory's method chaining works", {
  t0 <- trajectory() %>%
    seize("one", 1) %>%
    release("one", 1) %>%
    timeout(function() 1) %>%
    branch(function() 1, T, trajectory() %>% timeout(function() 1)) %>%
    rollback(1) %>%
    seize("one", 1)

  expect_is(t0, "trajectory")
})

test_that("simmer's method chaining works", {
  t0 <- trajectory() %>%
    timeout(function() 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server") %>%
    add_generator("customer", t0, function() 1) %>%
    onestep() %>%
    run(10) %>%
    reset()

  expect_is(env, "simmer")
})
