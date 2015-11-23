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
  t0 <- create_trajectory()%>%timeout(function() 1)
  
  expect_error(simmer()%>%add_generator("customer", t0, function() {}))
})


context("generator - convenience funcs")

test_that("at returns the correct values", {
  gen_func<-at(c(0,10,15))
  expect_equal(gen_func(), 0)
  expect_equal(gen_func(), 10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), -1)
})
