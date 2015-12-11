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

context("generator - convenience funcs")

test_that("at returns the correct values", {
  gen_func<-at(c(0,10,15)) # values passed as vector
  expect_equal(gen_func(), 0)
  expect_equal(gen_func(), 10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), -1)
  
  gen_func<-at(0,10,15) # values passed as parameters
  expect_equal(gen_func(), 0)
  expect_equal(gen_func(), 10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), -1)
  
})

test_that("from returns the correct values", {
  gen_func<-from(5, function() 1)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)
  
  gen_func2<-from(5, function() 1, arrive = FALSE)
  expect_equal(gen_func2(), 6)
  expect_equal(gen_func2(), 1)
  
})
