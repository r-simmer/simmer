context("resource")

test_that("resources are correctly created", {
  env <- simmer() %>%
    add_resource("dummy", 5, Inf)

  expect_warning(env %>% add_resource("dummy"))
  expect_error(env %>% get_capacity("asdf"))
  expect_equal(env %>% get_capacity("dummy"), 5)
  expect_error(env %>% get_queue_size("asdf"))
  expect_equal(env %>% get_queue_size("dummy"), Inf)
  expect_error(env %>% get_server_count("asdf"))
  expect_equal(env %>% get_server_count("dummy"), 0)
  expect_error(env %>% get_queue_count("asdf"))
  expect_equal(env %>% get_queue_count("dummy"), 0)
})

test_that("capacity and queue size change", {
  env <- simmer() %>%
    add_resource("dummy", 5, Inf)

  expect_equal(env %>% get_capacity("dummy"), 5)
  expect_equal(env %>% get_queue_size("dummy"), Inf)

  env %>%
    set_capacity("dummy", Inf) %>%
    set_queue_size("dummy", 5)

  expect_equal(env %>% get_capacity("dummy"), Inf)
  expect_equal(env %>% get_queue_size("dummy"), 5)
})

test_that("a negative capacity or queue_size is converted to positive", {
  env <- simmer() %>%
    add_resource("dummy", -4, -1)

  expect_equal(env %>% get_capacity("dummy"), 4)
  expect_equal(env %>% get_queue_size("dummy"), 1)
})

test_that("a non-existent resource fails", {
  t0 <- create_trajectory("") %>%
    seize("dummy", 1) %>%
    release("dummy", 1)

  env <- simmer() %>%
    add_generator("customer", t0, function() 1)

  expect_error(env %>% run())
})

test_that("resource slots are correctly filled", {
  t0 <- create_trajectory("") %>%
    seize("dummy", 1) %>%
    set_attribute("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("dummy", 2, 2) %>%
    add_generator("customer", t0, at(1:5), mon = 2) %>%
    run()

  arrivals <- env %>% get_mon_arrivals()
  arrivals_res <- env %>% get_mon_arrivals(TRUE)
  resources <- env %>% get_mon_resources()
  attributes <- env %>% get_mon_attributes()

  expect_equal(arrivals[3, ]$finished, FALSE)
  expect_equal(nrow(arrivals_res), 0)
  expect_equal(resources[4, ]$server, 2)
  expect_equal(resources[4, ]$queue, 2)
  expect_equal(sum(attributes$value), 2)
})

test_that("resources are correctly monitored", {
  t0 <- create_trajectory("") %>%
    seize("dummy", 1) %>%
    release("dummy", 1)

  env <- simmer() %>%
    add_resource("dummy", 2) %>%
    add_generator("customer", t0, at(0)) %>%
    run()

  resources <- env %>% get_mon_resources()

  expect_equal(resources[1, ]$server, 1)
  expect_equal(resources[2, ]$server, 0)
})

test_that("a big departure triggers more than one small seize from the queue", {
  t0 <- create_trajectory("") %>%
    seize("dummy", 2) %>%
    timeout(10) %>%
    release("dummy", 2)
  t1 <- create_trajectory("") %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer() %>%
    add_resource("dummy", 2) %>%
    add_generator("a", t0, at(0)) %>%
    add_generator("b", t1, at(1, 2)) %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(as.character(arrs_ordered$name), c("a0", "b0", "b1"))
  expect_equal(arrs_ordered$end_time, c(10, 20, 20))
})
