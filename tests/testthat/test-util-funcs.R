context("util funcs")

test_that("needs_attrs gives the correct response", {
  expect_equal(needs_attrs(function(attrs, glb) 0), 2)
  expect_equal(needs_attrs(function(attrs) 0), 1)
  expect_equal(needs_attrs(function() 0), 0)
  expect_equal(needs_attrs(0), 0)
})
