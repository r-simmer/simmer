context("seize/release")

test_that("resources are seized/released as expected (1)", {
  t0 <- create_trajectory() %>%
    seize("dummy", -1) %>%
    timeout(1) %>%
    seize("dummy", function() 2) %>%
    timeout(1) %>%
    release("dummy", -1) %>%
    timeout(1) %>%
    release("dummy", function() 2) %>%
    timeout(1) %>%
    seize("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 3, 0) %>%
    add_generator("arrival", t0, at(0))

  env %>% run(1)
  expect_equal(env %>% get_server_count("dummy"), 1)
  env %>% run(2)
  expect_equal(env %>% get_server_count("dummy"), 3)
  env %>% run(3)
  expect_equal(env %>% get_server_count("dummy"), 2)
  env %>% run(4)
  expect_equal(env %>% get_server_count("dummy"), 0)
})

test_that("resources are seized/released as expected (2)", {
  t0 <- create_trajectory() %>%
    select("dummy0", id = 0) %>%
    select(function() "dummy1", id = 1) %>%
    seize_selected(-1, id = 0) %>%
    timeout(1) %>%
    seize_selected(function() 2, id = 1) %>%
    timeout(1) %>%
    release_selected(-1, id = 0) %>%
    timeout(1) %>%
    release_selected(function() 2, id = 1) %>%
    timeout(1)

  expect_output(print(t0))

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 3, 0) %>%
    add_resource("dummy1", 3, 0) %>%
    add_generator("arrival", t0, at(0))

  env %>% run(1)
  expect_equal(env %>% get_server_count("dummy0"), 1)
  env %>% run(2)
  expect_equal(env %>% get_server_count("dummy1"), 2)
  env %>% run(3)
  expect_equal(env %>% get_server_count("dummy0"), 0)
  env %>% run(4)
  expect_equal(env %>% get_server_count("dummy1"), 0)
})

test_that("a release without a previous seize fails", {
  t <- create_trajectory() %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("asdf", t, at(0))

  expect_error(env %>% run)
})

test_that("a release greater than seize fails", {
  t <- create_trajectory() %>%
    seize("dummy", 1) %>%
    release("dummy", 2)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1) %>%
    add_generator("asdf", t, at(0))

  expect_error(env %>% run)
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% seize(0, 0))
  expect_error(create_trajectory() %>% release(0, 0))
  expect_error(create_trajectory() %>% seize("dummy", "dummy"))
  expect_error(create_trajectory() %>% release("dummy", "dummy"))
})

test_that("arrivals perform a post.seize and then stop", {
  t <- create_trajectory() %>%
    seize("dummy", 1, continue = FALSE,
          post.seize = create_trajectory() %>%
            timeout(2) %>%
            release("dummy", 1)) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()

  expect_true(arrs$finished)
  expect_equal(arrs$activity_time, 2)
})

test_that("arrivals can retry a seize", {
  t <- create_trajectory() %>%
    seize("dummy", 1, continue = FALSE,
          reject = create_trajectory() %>%
            timeout(1) %>%
            rollback(2, Inf)) %>%
    timeout(2) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()

  expect_equal(arrs$start_time, c(0, 1))
  expect_equal(arrs$finished, c(TRUE, TRUE))
  expect_equal(arrs$activity_time, c(2, 3))
})

test_that("arrivals go through post.seize or reject and then continue", {
  t <- create_trajectory() %>%
    seize("dummy", 1, continue = c(TRUE, TRUE),
          post.seize = create_trajectory() %>%
            timeout(2) %>%
            release("dummy"),
          reject = create_trajectory() %>%
            timeout(3)) %>%
    timeout(3)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 1, 0) %>%
    add_generator("arrival", t, at(0, 1)) %>%
    run()
  arrs <- env %>% get_mon_arrivals()

  expect_equal(arrs$start_time, c(0, 1))
  expect_equal(arrs$finished, c(TRUE, TRUE))
  expect_equal(arrs$activity_time, c(5, 6))
})

test_that("leaving without releasing throws a warning (arrivals)", {
  t <- create_trajectory() %>%
    seize("dummy0", 2) %>%
    seize("dummy1", 1) %>%
    release("dummy0", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 2) %>%
    add_resource("dummy1", 1) %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))
})

test_that("leaving without releasing throws a warning (batches)", {
  t <- create_trajectory() %>%
    batch(1) %>%
    seize("dummy0", 2) %>%
    seize("dummy1", 1) %>%
    release("dummy0", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy0", 2) %>%
    add_resource("dummy1", 1) %>%
    add_generator("arrival", t, at(0))

  expect_warning(run(env))
})
