context("seize/release")

test_that("resources are seized/released as expected", {
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
  
  env <- simmer() %>%
    add_resource("dummy", 3, 0) %>%
    add_generator("arrival", t0, at(0))
  
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 1)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 3)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 2)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy"), 0)
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% seize(0, 0))
  expect_error(create_trajectory() %>% release(0, 0))
  expect_error(create_trajectory() %>% seize("dummy", "dummy"))
  expect_error(create_trajectory() %>% release("dummy", "dummy"))
})
