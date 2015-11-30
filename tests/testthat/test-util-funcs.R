context("util funcs")

test_that("needs_attrs gives the correct response", {
  expect_true(needs_attrs(function(attrs) 0))
  expect_false(needs_attrs(function() 0))
  expect_false(needs_attrs(0))
})


