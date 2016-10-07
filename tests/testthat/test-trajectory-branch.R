context("branch")

test_that("a non-function option fails", {
  expect_error(create_trajectory() %>% branch(1, T, create_trajectory()))
})

test_that("the wrong number of elements fails", {
  expect_error(create_trajectory() %>% branch(function() 1, T, create_trajectory(), create_trajectory()))
  expect_error(create_trajectory() %>% branch(function() 1, c(T, T), create_trajectory()))
})

test_that("an empty trajectory fails", {
  expect_error(create_trajectory() %>% branch(function() 1, T, create_trajectory()))
})

test_that("an index equal to 0 skips the branch", {
  t0 <- create_trajectory() %>%
    branch(function() 0, T,
           create_trajectory() %>% timeout(1)
    ) %>%
    timeout(2)

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0)) %>%
    run()
  expect_equal(env %>% now(), 2)
})

test_that("an index out of range fails", {
  t1 <- create_trajectory() %>%
    branch(function() 1, T,
      create_trajectory() %>% timeout(1)
    )
  t2 <- create_trajectory() %>%
    branch(function() 2, T,
      create_trajectory() %>% timeout(1)
    )

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t2, at(0))
  expect_error(env %>% run())

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t1, at(0)) %>%
    run()
  expect_equal(env %>% now(), 1)
})
