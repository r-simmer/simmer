context("branch")

test_that("a non-function option fails", {
  expect_error(trajectory() %>% branch(1, T, trajectory()))
})

test_that("the wrong number of elements fails", {
  expect_error(trajectory() %>% branch(function() 1, T, trajectory(), trajectory()))
  expect_error(trajectory() %>% branch(function() 1, c(T, T), trajectory()))
})

test_that("an empty trajectory fails", {
  expect_error(trajectory() %>% branch(function() 1, T, trajectory()))
})

test_that("an index equal to 0 skips the branch", {
  t0 <- trajectory() %>%
    branch(function() 0, T,
           trajectory() %>% timeout(1)
    ) %>%
    timeout(2)

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0)) %>%
    run()
  expect_equal(env %>% now(), 2)
})

test_that("an index out of range fails", {
  t1 <- trajectory() %>%
    branch(function() 1, T,
      trajectory() %>% timeout(1)
    )
  t2 <- trajectory() %>%
    branch(function() 2, T,
      trajectory() %>% timeout(1)
    )

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t2, at(0))
  expect_error(env %>% run())

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t1, at(0)) %>%
    run()
  expect_equal(env %>% now(), 1)
})

test_that("accepts a list of trajectories", {
  t1 <- trajectory() %>% timeout(1)

  t2 <- trajectory() %>%
    branch(function() 1, continue=rep(TRUE, 10), replicate(10, t1))

  expect_equal(length(t2), 1)
  expect_equal(get_n_activities(t2), 11)
})
