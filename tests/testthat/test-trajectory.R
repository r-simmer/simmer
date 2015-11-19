context("basic Trajectory functionality")

test_that("the trajectory stores the right number of activities", {
  t0 <- create_trajectory("my trajectory") %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1)
  
  expect_is(t0, "Trajectory")
  expect_equal(t0%>%get_n_activities(), 3)
  
  t0 <- t0 %>%
    branch(function() 1, TRUE,
      create_trajectory() %>%
        seize("doctor", 1) %>%
        timeout(function() rnorm(1, 20)) %>%
        release("doctor", 1) %>%
        branch(function() 1, TRUE, 
          create_trajectory() %>%
            seize("administration", 1) %>%
            timeout(function() rnorm(1, 5)) %>%
            release("administration", 1)
        )
    )
  
  expect_is(t0, "Trajectory")
  expect_equal(t0%>%get_n_activities(), 9)
  
  expect_output(t0%>%show_trajectory(), 
"Trajectory: my trajectory, 9 activities
{ Activity: Seize(nurse) | amount: 1 }
{ Activity: Timeout(none) | duration: function() }
{ Activity: Release(nurse) | amount: 1 }
{ Activity: Branch(none) | merge: 1 }
  Trajectory: anonymous, 6 activities
  { Activity: Seize(doctor) | amount: 1 }
  { Activity: Timeout(none) | duration: function() }
  { Activity: Release(doctor) | amount: 1 }
  { Activity: Branch(none) | merge: 1 }
    Trajectory: anonymous, 3 activities
    { Activity: Seize(administration) | amount: 1 }
    { Activity: Timeout(none) | duration: function() }
    { Activity: Release(administration) | amount: 1 }", fixed = TRUE)
})

test_that("the head/tail pointers are correctly placed", {
  t0 <- create_trajectory()
  
  expect_equal(t0%>%get_head(), NULL)
  expect_equal(t0%>%get_tail(), NULL)
  
  t0%>%seize("nurse", 1)
  
  expect_output(show_activity(t0%>%get_head()), "Seize")
  expect_output(show_activity(t0%>%get_tail()), "Seize")
  
  t0%>%timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1)
  
  expect_output(show_activity(t0%>%get_head()), "Seize")
  expect_output(show_activity(t0%>%get_tail()), "Release")
})

test_that("we can force some errors (just to complete coverage)", {
  expect_error(create_trajectory() %>% seize(0, 0))
  expect_error(create_trajectory() %>% release(0, 0))
})
