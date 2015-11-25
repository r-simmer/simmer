context("timeout")

test_that("a non-function is correctly converted to a function by the value_to_func functions", {
  expect_error(not(create_trajectory()%>%timeout(3)))
})

test_that("a duration function that returns a non-numeric value fails", {
  t0 <- create_trajectory() %>%
    timeout(function() {})
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  
  expect_error(env%>%run(100))
})
