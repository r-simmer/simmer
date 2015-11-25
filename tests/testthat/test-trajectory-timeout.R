context("timeout")

test_that("a non-function task fails", {
  expect_error(create_trajectory()%>%timeout(3))
})

test_that("a task function that returns a non-numeric value fails", {
  t0 <- create_trajectory() %>%
    timeout(function() {})
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  
  expect_error(env%>%run(100))
})
