context("Trajectory test")



test_that("building a basic trajectory", {
  # set-up simple trajectory
  trajectory1 <-
    create_trajectory() %>%
    add_event("seize", resource_name="vpk", resource_amount=2) %>%
    add_event("timeout", time_value="runif(1,5,10)") %>%
    add_event("release", resource_name="vpk", resource_amount=2)
  
  expect_that(NROW(trajectory1@timeline), is_more_than(0))
  
})