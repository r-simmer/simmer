context("printing")

test_that("print methods return the objects invisibly", {
  expect_output(env1 <- print(simmer()))
  expect_output(env2 <- print(wrap(simmer())))
  expect_output(traj <- print(trajectory()))
  expect_output(schd <- print(schedule(c(8, 16, 24), c(3, 2, 1), period=24)))

  #expect_is(env1, "simmer")
  #expect_is(env2, "wrap")
  #expect_is(traj, "trajectory")
  #expect_is(schd, "schedule")
})
