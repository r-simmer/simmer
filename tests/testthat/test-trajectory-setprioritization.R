context("set_prioritization")

test_that("the wrong number of parameters fails", {
  expect_output(trajectory() %>% set_prioritization(c(0, 0, 0)) %>% print,
                ".*SetPrior.*values:.*0, 0, 0.*")

  t <- trajectory() %>%
    set_prioritization(function() c(0, 0))

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0))

  expect_error(env %>% run)
})

test_that("priority queues are adhered to (2)", {
  t0 <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  t1 <- trajectory() %>%
    set_prioritization(c(1, 0, FALSE)) %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 0))) %>%
    add_generator("__prior", t1, at(1)) %>% # should be served second
    run()

  arrs <-
    env %>% get_mon_arrivals()

  expect_equal(arrs[arrs$name == "__prior0", ]$end_time, 4)
})

test_that("priority queues are adhered to (3)", {
  t0 <- trajectory() %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)
  t1 <- trajectory() %>%
    set_prioritization(function() c(1, 0, FALSE)) %>%
    seize("server", 1) %>%
    timeout(2) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1) %>%
    add_generator("__nonprior", t0, at(c(0, 0))) %>%
    add_generator("__prior", t1, at(1)) %>% # should be served second
    run()

  arrs <-
    env %>% get_mon_arrivals()

  expect_equal(arrs[arrs$name == "__prior0", ]$end_time, 4)
})
