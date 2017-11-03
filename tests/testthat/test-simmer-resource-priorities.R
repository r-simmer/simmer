context("resource-priorities")

test_that("priority queues are adhered to", {
  t <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t, at(c(0, 0)), priority = 0) %>%
    add_generator("__prior", t, at(1), priority = 1) %>% # should be served second
    run()

  arrs <-
    env %>% get_mon_arrivals()

  expect_equal(arrs[arrs$name == "__prior0", ]$end_time, 4)
})

test_that("priority queues are adhered to and same level priorities are processed FIFO", {
  t <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("_t0_prior", t, at(c(0, 2, 4, 6)), priority = 1) %>%
    add_generator("_t1_prior", t, at(c(1, 3, 5, 7)), priority = 1) %>%
    run()

  arrs <-
    env %>% get_mon_arrivals()

  arrs_ordered <-
    arrs[order(arrs$end_time), ]

  expect_equal(as.character(arrs_ordered$name),
               c("_t0_prior0", "_t1_prior0", "_t0_prior1", "_t1_prior1",
                 "_t0_prior2", "_t1_prior2", "_t0_prior3", "_t1_prior3"))
})

test_that("a lower priority arrival gets rejected before accessing the server", {
  t <- trajectory() %>%
    seize("dummy", 1) %>%
    timeout(10) %>%
    release("dummy", 1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("p0a", t, at(0, 0)) %>%
    add_generator("p1a", t, at(2, 3), priority = 1) %>%
    add_resource("dummy", 1, 2) %>%
    run()

  arrs <- env %>% get_mon_arrivals()
  arrs_ordered <- arrs[order(arrs$name), ]

  expect_equal(as.character(arrs[!arrs$finished, ]$name), "p0a1")
  expect_equal(arrs_ordered$end_time, c(10, 3, 20, 30))
})

test_that("priority works in non-saturated finite-queue resources", {
  low_prio <- trajectory() %>%
    seize("res", 1) %>%
    timeout(10) %>%
    release("res", 1)

  high_prio <- trajectory() %>%
    seize("res", 7) %>%
    timeout(10) %>%
    release("res", 7)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 0, 10) %>%
    add_generator("low_prio", low_prio, at(rep(0, 5))) %>%
    add_generator("high_prio", high_prio, at(1), priority = 1) %>%
    run()

  arr <- get_mon_arrivals(env)

  expect_true(all(grepl("low", arr$name)))
  expect_equal(arr$start_time, c(0, 0))
  expect_equal(arr$end_time, c(1, 1))
  expect_equal(arr$activity_time, c(0, 0))
})
