context("Trajectory tests")

test_that("The trajectory stores the right number of activities", {
  t0 <- Trajectory$new("my trajectory") $
    seize("nurse", 1) $
    timeout(function() rnorm(1, 15)) $
    release("nurse", 1)
  
  expect_match(t0$name, "my trajectory")
  expect_is(t0, "Trajectory")
  expect_equal(t0$get_n_activities(), 3)
  
  t0 <- t0 $
    seize("doctor", 1) $
    timeout(function() rnorm(1, 20)) $
    release("doctor", 1) $
    seize("administration", 1) $
    timeout(function() rnorm(1, 5)) $
    release("administration", 1)
  
  expect_is(t0, "Trajectory")
  expect_equal(t0$get_n_activities(), 9)
})

test_that("The head/tail pointers are correctly placed", {
  t0 <- Trajectory$new()
  
  expect_equal(t0$get_head(), NULL)
  expect_equal(t0$get_tail(), NULL)
  
  t0$seize("nurse", 1)
  
  expect_is(t0$get_head(), "Activity")
  expect_is(t0$get_head(), "SeizeActivity")
  expect_equal(t0$get_head(), t0$get_tail())
  
  t0$timeout(function() rnorm(1, 15)) $
    release("nurse", 1)
  
  expect_is(t0$get_head(), "Activity")
  expect_is(t0$get_head(), "SeizeActivity")
  expect_is(t0$get_tail(), "Activity")
  expect_is(t0$get_tail(), "ReleaseActivity")
})

test_that("The activity chain grows correctly", {
  t0 <- Trajectory$new("my trajectory") $
    seize("nurse", 1) $
    timeout(function() rnorm(1, 15)) $
    release("nurse", 1)
  act <- t0$get_head()$next_activity$next_activity
  
  expect_equal(act, t0$get_tail())
  expect_equal(act$next_activity, NULL)
})
