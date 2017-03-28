context("convenience funcs")

test_that("at returns the correct values", {
  gen_func <- at(c(0, 10, 15)) # values passed as vector
  expect_equal(gen_func(), c(0, 10, 5, -1))

  gen_func <- at(0, 10, 15) # values passed as parameters
  expect_equal(gen_func(), c(0, 10, 5, -1))
})

test_that("from returns the correct values", {
  gen_func <- from(5, function() 1)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)

  gen_func <- from(5, function() 1, arrive = FALSE)
  expect_equal(gen_func(), 6)
  expect_equal(gen_func(), 1)
})

test_that("to returns the correct values", {
  gen_func <- to(3, function() 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)

  gen_func <- to(3, function() c(1, 1))
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), -1)

  gen_func <- to(3, function() c(1, 1, 1))
  expect_equal(gen_func(), c(1, 1, -1))
})

test_that("from_to returns the correct values", {
  expect_error(from_to(5, 8, function() 1, every=7))

  gen_func <- from_to(5, 8, function() 1)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1))
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1, 1))
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1, -1))

  gen_func <- from_to(5, 8, function() 1, every=10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 8)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 8)

  gen_func <- from_to(5, 8, function() c(1, 1), every=10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)

  gen_func <- from_to(5, 8, function() c(1, 1, 1), every=10)
  expect_equal(gen_func(), 5)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)
  expect_equal(gen_func(), c(1, 1))
  expect_equal(gen_func(), 8)

  gen_func <- from_to(5, 8, function() 1, arrive = FALSE)
  expect_equal(gen_func(), 6)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1), arrive = FALSE)
  expect_equal(gen_func(), c(6, 1))
  expect_equal(gen_func(), -1)

  gen_func <- from_to(5, 8, function() c(1, 1, 1), arrive = FALSE)
  expect_equal(gen_func(), c(6, 1, -1))

  gen_func <- from_to(5, 8, function() 1, arrive = FALSE, every=10)
  expect_equal(gen_func(), 6)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 9)
  expect_equal(gen_func(), 1)
  expect_equal(gen_func(), 9)

  gen_func <- from_to(5, 8, function() c(1, 1), arrive = FALSE, every=10)
  expect_equal(gen_func(), c(6, 1))
  expect_equal(gen_func(), c(9, 1))
  expect_equal(gen_func(), c(9, 1))

  gen_func <- from_to(5, 8, function() c(1, 1, 1), arrive = FALSE, every=10)
  expect_equal(gen_func(), c(6, 1))
  expect_equal(gen_func(), c(9, 1))
  expect_equal(gen_func(), c(9, 1))
})

test_that("schedule returns the correct values", {
  expect_error(schedule(1, 1))
  expect_error(schedule(c(1, 2), 1))
  expect_error(schedule(1, c(1, 2)))
  expect_error(schedule(c(2, 1), c(1, 2)))
  expect_error(schedule(c(2, 1), c(1, 2),  1))
  expect_error(schedule(c(0, 1), c(1, 2),  1))

  sch <- schedule(c(1, 3), c(1, 2), Inf)$get_schedule()
  expect_equal(sch$init, 0)
  expect_equal(sch$intervals, c(1, 2))
  expect_equal(sch$values, c(1, 2))
  expect_equal(sch$period, -1)

  sch <- schedule(c(1, 3), c(1, 2), 3)$get_schedule()
  expect_equal(sch$init, 2)
  expect_equal(sch$intervals, c(1, 2, 1))
  expect_equal(sch$values, c(1, 2, 1))
  expect_equal(sch$period, 3)

  sch <- schedule(c(0, 2), c(1, 2), 3)$get_schedule()
  expect_equal(sch$init, 1)
  expect_equal(sch$intervals, c(0, 2, 1))
  expect_equal(sch$values, c(1, 2, 1))
  expect_equal(sch$period, 3)
})
