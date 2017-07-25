context("utils")

func <- function() {
  A <- TRUE
  B <- c(TRUE, 0)
  C <- "asdf"
  D <- -2.1
  E <- function() 1
  F <- trajectory()
  G <- schedule(c(1, 2), c(1, 1), 3)
  H <- c("asdf", "asdf")
  I <- c(1, Inf)
  J <- NA
  K <- NULL
  check_args(
    A, B, C, D, E, F, G, C, E, C, J, H, E, D, E, I, E, D, E, D, G, E, K, F, K,
    types=c(rep("flag", 2),
            "string",
            "number",
            "function",
            "trajectory",
            "schedule",
            rep("string or function", 2),
            rep("string or NA", 2),
            rep("string vector or function", 2),
            rep("number or function", 2),
            rep("number vector or function", 2),
            rep("numeric or function", 2),
            rep("number or schedule", 2),
            rep("function or NULL", 2),
            rep("trajectory or NULL", 2))
  )
}

test_that("argument matching work as expected", {
  expect_true(is_string("asdf"))
  expect_false(is_string(c("asdf", "asdf")))

  expect_true(is_flag(TRUE))
  expect_true(is_flag(2))
  expect_true(is_flag(c(TRUE, FALSE)))
  expect_true(is_flag(c(2, 0)))

  n <- 1
  expect_true(is_number(n, environment(), "n"))
  expect_equal(n, 1)
  n <- -1
  expect_true(is_number(n, environment(), "n"))
  expect_equal(n, 1)
  n <- Inf
  expect_true(is_number(n, environment(), "n"))
  expect_equal(n, -1)
  n <- c(1, 1)
  expect_false(is_number(n, environment(), "n"))

  n <- c(1, 1)
  expect_true(is_number_vector(n, environment(), "n"))
  expect_equal(n, c(1, 1))
  n <- c(1, -1)
  expect_true(is_number_vector(n, environment(), "n"))
  expect_equal(n, c(1, 1))
  n <- c(1, Inf)
  expect_true(is_number_vector(n, environment(), "n"))
  expect_equal(n, c(1, Inf))
  n <- 1
  expect_false(is_number_vector(n, environment(), "n"))

  func <- function() 1
  expect_true(is_function(func, environment(), "func"))
  expect_false(is_function(1, environment(), "func"))

  expect_silent(func())
  expect_error(check_args("asdf", types="flag"))
})

test_that("envs_apply works", {
  env <- new.env()
  env$func <- function(x, y) data.frame(x=x, y=y)
  df <- envs_apply(list(env, env), "func", 1, 2)
  expect_equal(df, data.frame(x=c(1, 1), y=c(2, 2), replication=c(1, 2)))
})

test_that("a function can be reset", {
  env <- new.env()
  env$.i <- 0
  func <- function() {
    j <<- 3
    .i <<- .i + 1
    .i
  }
  environment(func) <- env
  resetable <- make_resetable(func)
  expect_equal(resetable(), 1)
  expect_equal(resetable(), 2)
  expect_equal(resetable(), 3)
  attr(resetable, "reset")()
  expect_equal(resetable(), 1)
  expect_equal(resetable(), 2)
  expect_equal(resetable(), 3)

  .i <- 0
  func <- function() {
    j <<- 3
    .i <<- .i + 1
    .i
  }
  resetable <- make_resetable(func)
  expect_equal(resetable(), 1)
  expect_equal(resetable(), 2)
  expect_equal(resetable(), 3)
  attr(resetable, "reset")()
  expect_equal(resetable(), 1)
  expect_equal(resetable(), 2)
  expect_equal(resetable(), 3)
})

test_that("binarise works", {
  expect_equal(binarise(FALSE), 1)
  expect_equal(binarise(TRUE), 2)
  expect_equal(binarise(FALSE, TRUE), 3)
  expect_equal(binarise(TRUE, TRUE), 4)
  expect_equal(binarise(FALSE, FALSE, TRUE), 5)
  expect_equal(binarise(TRUE, FALSE, TRUE), 6)
  expect_equal(binarise(FALSE, TRUE, TRUE), 7)
  expect_equal(binarise(TRUE, TRUE, TRUE), 8)
})
