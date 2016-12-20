context("simulation 1")

test_that("a simple deterministic simulation with rejections behaves as expected", {
  n <- 100

  t0 <- trajectory("") %>%
    seize("server", 1) %>%
    timeout(function() 1.5) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1, queue_size = 0) %>%
    add_generator("entity", t0, at(1:n))

  expect_warning(env %>% add_generator("entity", t0, function() 2))
  expect_equal(env %>% now(), 0)
  expect_equal(env %>% peek(), 0)

  env %>% run(1.5)

  expect_equal(env %>% now(), 1.5)
  expect_equal(env %>% peek(), 2)

  env %>% run()
  arrivals <- env %>% get_mon_arrivals()
  arrivals_res <- env %>% get_mon_arrivals(TRUE)
  resources <- env %>% get_mon_resources()

  expect_equal(env %>% now(), n + 0.5)
  expect_equal(env %>% peek(), numeric(0))
  expect_equal(nrow(arrivals), n)
  expect_equal(nrow(arrivals_res), n / 2)
  expect_true(arrivals_res[1, ]$resource == "server")
  expect_equal(nrow(subset(arrivals, finished)), n / 2)
  expect_equal(nrow(subset(arrivals, !finished)), n / 2)
  expect_equal(sum(subset(arrivals, finished)$activity_time), 1.5 * n / 2)
  expect_equal(sum(arrivals_res$activity_time), 1.5 * n / 2)
  expect_equal(sum(subset(arrivals, !finished)$activity_time), 0)

  expect_equal(nrow(resources), n)
  expect_equal(sum(resources$server), n / 2)
  expect_equal(sum(resources$queue), 0)
})
