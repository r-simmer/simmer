context("branch")

test_that("a non-function option fails", {
  t <- trajectory() %>% timeout(1)
  expect_error(trajectory() %>% branch(1, TRUE, t))
})

test_that("the wrong number of elements fails, but continue is recycled", {
  t <- trajectory() %>% timeout(1)
  expect_error(trajectory() %>% branch(function() 1, c(TRUE, TRUE), t))
  expect_silent(trajectory() %>% branch(function() 1, TRUE, t, t))
})

test_that("an empty trajectory fails", {
  expect_error(trajectory() %>% branch(function() 1, TRUE, trajectory()))
})

test_that("an index equal to 0 skips the branch", {
  t0 <- trajectory() %>%
    branch(function() 0, TRUE,
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
    branch(function() 1, TRUE,
      trajectory() %>% timeout(1)
    )
  t2 <- trajectory() %>%
    branch(function() 2, TRUE,
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
    branch(function() 1, continue=TRUE, replicate(10, t1))

  expect_equal(length(t2), 1)
  expect_equal(get_n_activities(t2), 11)
})
