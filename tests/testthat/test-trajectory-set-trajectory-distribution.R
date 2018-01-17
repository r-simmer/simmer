context("set_trajectory/set_distribution")

test_that("we can set a new trajectory", {
  t2 <- trajectory() %>%
    timeout(2)
  t1 <- trajectory() %>%
    set_trajectory("dummy", t2) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t1, function() 1) %>%
    run(10)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(1, 2, 3, 4, 5, 6, 7))
  expect_equal(arr$end_time, c(2, 4, 5, 6, 7, 8, 9))
})

test_that("we can set a new distribution", {
  t <- trajectory() %>%
    set_distribution("dummy", function() 2)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, function() 1) %>%
    run(10)
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(1, 3, 5, 7, 9))
})

test_that("other activities cannot modify the behaviour", {
  t1 <- trajectory() %>%
    timeout(1)

  t2 <- trajectory() %>%
    set_distribution("dummy", function() 1) %>%
    set_trajectory("dummy", t1) %>%
    timeout(2)

  t3 <- trajectory() %>%
    set_attribute("asdf", 1) %>%
    set_distribution("dummy", function() 1) %>%
    set_trajectory("dummy", t1) %>%
    timeout(2)

  arr2 <- simmer(verbose=TRUE) %>%
    add_generator("dummy", t2, function() 2) %>%
    run(10) %>%
    get_mon_arrivals()

  arr3 <- simmer(verbose=TRUE) %>%
    add_generator("dummy", t3, function() 2) %>%
    run(10) %>%
    get_mon_arrivals()

  expect_true(all(arr2 == arr3))
})
