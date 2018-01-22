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
    A = "flag",
    B = "flag",
    C = "string",
    D = "number",
    E = "function",
    F = "trajectory",
    G = "schedule",
    C = c("string", "function"),
    E = c("string", "function"),
    C = c("string", "NA"),
    J = c("string", "NA"),
    H = c("string vector", "function"),
    E = c("string vector", "function"),
    D = c("number", "function"),
    E = c("number", "function"),
    I = c("number vector", "function"),
    E = c("number vector", "function"),
    D = c("numeric", "function"),
    E = c("numeric", "function"),
    D = c("number", "schedule"),
    G = c("number", "schedule"),
    E = c("function", "NULL"),
    K = c("function", "NULL"),
    F = c("trajectory", "NULL"),
    K = c("trajectory", "NULL")
  )
}

test_that("argument matching work as expected", {
  var <- "asdf"
  expect_true(is_string("var", environment()))
  var <- c("asdf", "asdf")
  expect_false(is_string("var", environment()))

  var <- TRUE
  expect_true(is_flag("var", environment()))
  var <- 2
  expect_true(is_flag("var", environment()))
  var <- c(TRUE, FALSE)
  expect_true(is_flag("var", environment()))
  var <- c(2, 0)
  expect_true(is_flag("var", environment()))

  var <- 1
  expect_true(is_number("var", environment()))
  expect_equal(var, 1)
  var <- -1
  expect_true(is_number("var", environment()))
  expect_equal(var, 1)
  var <- Inf
  expect_true(is_number("var", environment()))
  expect_equal(var, -1)
  var <- c(1, 1)
  expect_false(is_number("var", environment()))

  var <- c(1, 1)
  expect_true(is_number_vector("var", environment()))
  expect_equal(var, c(1, 1))
  var <- c(1, -1)
  expect_true(is_number_vector("var", environment()))
  expect_equal(var, c(1, 1))
  var <- c(1, Inf)
  expect_true(is_number_vector("var", environment()))
  expect_equal(var, c(1, Inf))
  var <- 1
  expect_false(is_number_vector("var", environment()))

  expect_false(is_function("var", environment()))
  var <- function() 1
  expect_true(is_function("var", environment()))

  expect_silent(func())
  var <- "asdf"
  expect_error(check_args(var="flag"))
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
