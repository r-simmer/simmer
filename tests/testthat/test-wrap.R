context("wrap")

test_that("the wrapper behaves as expected", {
  t0 <- create_trajectory() %>%
    seize("server", 1) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 5, Inf) %>%
    add_generator("dummy", t0, function() 1) %>%
    run(10.5) %>%
    wrap()
  arrivals <- env%>%get_mon_arrivals()
  resources <- env%>%get_mon_resources()
  
  expect_equal(nrow(arrivals), 10)
  expect_equal(nrow(resources), 20)
  expect_error(env%>%get_capacity("asdf"))
  expect_equal(env%>%get_capacity("server"), 5)
  expect_error(env%>%get_queue_size("asdf"))
  expect_equal(env%>%get_queue_size("server"), Inf)
})
