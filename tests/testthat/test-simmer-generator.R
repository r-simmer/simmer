context("generator")

test_that("a generator without a trajectory fails", {
  expect_error(simmer()%>%add_generator("customer", 4, 1))
})

test_that("a non-function dist fails", {
  t0 <- create_trajectory()
  
  expect_error(simmer()%>%add_generator("customer", t0, 1))
})

test_that("an empty trajectory fails", {
  t0 <- create_trajectory()
  
  expect_error(simmer()%>%add_generator("customer", t0, function() {}))
})

test_that("a dist that returns a non-numeric value fails", {
  t0 <- create_trajectory()%>%timeout(1)
  
  expect_error(simmer()%>%add_generator("customer", t0, function() {}))
})

test_that("generates the expected amount", {
  t0 <- create_trajectory()%>%timeout(1)
  
  env <- simmer() %>%
    add_generator("customer", t0, at(c(0, 1, 2))) %>%
    run(10)
  
  expect_error(env %>% get_n_generated("asdf"))
  expect_equal(env %>% get_n_generated("customer"), 3)
})

test_that("generators are reset", {
  t <- create_trajectory() %>% timeout(1)
  
  expect_equal(3, simmer() %>% 
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run() %>% reset() %>% run() %>%
    get_mon_arrivals() %>% nrow()
  )
})
