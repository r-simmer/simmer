context("convenience funcs")

test_that("at returns the correct values", {
  gen_func <- at(c(0, 10, 15)) # values passed as vector
  expect_equal(gen_func(), 0)
  expect_equal(gen_func(), 10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), -1)
  
  gen_func <- at(0, 10, 15) # values passed as parameters
  expect_equal(gen_func(), 0)
  expect_equal(gen_func(), 10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), -1)
})

test_that("every returns the correct values", {
  gen_func <- every(1, 2, 3)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 2)
  expect_equal(gen_func(), 3)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 2)
  expect_equal(gen_func(), 3)
})

test_that("from returns the correct values", {
  gen_func <- from(5, function() 1)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)
  
  gen_func2 <- from(5, function() 1, arrive = FALSE)
  expect_equal(gen_func2(), 6)
  expect_equal(gen_func2(), 1)
})

test_that("to returns the correct values", {
  gen_func <- to(3, function() 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)
})

test_that("from_to returns the correct values", {
  gen_func <- from_to(5, 8, function() 1)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)
  
  gen_func2 <- from_to(5, 8, function() 1, arrive = FALSE)
  expect_equal(gen_func2(), 6)
  expect_equal(gen_func2(), 1)
  expect_equal(gen_func2(), -1)
})
