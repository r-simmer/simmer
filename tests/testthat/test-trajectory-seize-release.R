context("seize/release")

test_that("a negative amount is converted to positive", {
  t0 <- create_trajectory()
  
  expect_silent(t0%>%seize("nurse", -2))
  expect_silent(t0%>%release("nurse", -10))
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% seize(0, 0))
  expect_error(create_trajectory() %>% release(0, 0))
  expect_error(create_trajectory() %>% seize("dummy", "dummy"))
  expect_error(create_trajectory() %>% release("dummy", "dummy"))
})
