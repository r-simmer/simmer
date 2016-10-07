context("simulation 2")

test_that("a release is executed before a seize in the same instant", {
  t0 <- create_trajectory() %>%
    seize("server", 1) %>%
    timeout(1) %>%
    release("server", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("server", 1, 0) %>%
    add_generator("dummy1", t0, at(c(1, 3, 5, 7, 9))) %>%
    add_generator("dummy0", t0, at(c(0, 2, 4, 6, 8))) %>%
    run()

  arrivals <- env %>% get_mon_arrivals()

  expect_equal(sum(arrivals$finished), nrow(arrivals))
})
