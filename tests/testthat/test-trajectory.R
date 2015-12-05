context("basic Trajectory functionality")

test_that("the activity chain grows as expected", {
  t0 <- create_trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, T, create_trajectory()%>%timeout(function() 1)) %>%
    set_attribute("dummy", 1) %>%
    rollback(1) %>%
    release("nurse", 1)
  
  head <- t0%>%get_head()
  for (i in 1:5) head <- get_next_activity(head)
  tail <- t0%>%get_tail()
  for (i in 1:5) tail <- get_prev_activity(tail)
  
  expect_output(print_activity(head), "Release")
  expect_output(print_activity(t0%>%get_tail()), "Release")
  expect_equal(get_next_activity(head), NULL)
  expect_output(print_activity(tail), "Seize")
  expect_output(print_activity(t0%>%get_head()), "Seize")
  expect_equal(get_prev_activity(tail), NULL)
})

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
    ) %>%
    rollback(1) %>%
    set_attribute("dummy", 1)
  
  expect_is(t0, "Trajectory")
  expect_equal(t0%>%get_n_activities(), 11)
  
  expect_output(t0, 
"Trajectory: my trajectory, 11 activities
{ Activity: Seize(nurse) | amount: 1 }
{ Activity: Timeout(none) | task: function() }
{ Activity: Release(nurse) | amount: 1 }
{ Activity: Branch(none) | merge: 1 }
  Trajectory: anonymous, 6 activities
  { Activity: Seize(doctor) | amount: 1 }
  { Activity: Timeout(none) | task: function() }
  { Activity: Release(doctor) | amount: 1 }
  { Activity: Branch(none) | merge: 1 }
    Trajectory: anonymous, 3 activities
    { Activity: Seize(administration) | amount: 1 }
    { Activity: Timeout(none) | task: function() }
    { Activity: Release(administration) | amount: 1 }
{ Activity: Rollback(none) | amount: 1 (Branch), times: 1 }
{ Activity: SetAttribute(none) | key: dummy, value: 1 }", fixed = TRUE)
})

test_that("the head/tail pointers are correctly placed", {
  t0 <- create_trajectory()
  
  expect_equal(t0%>%get_head(), NULL)
  expect_equal(t0%>%get_tail(), NULL)
  
  t0%>%seize("nurse", 1)
  
  expect_output(print_activity(t0%>%get_head()), "Seize")
  expect_output(print_activity(t0%>%get_tail()), "Seize")
  
  t0%>%timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1)
  
  expect_output(print_activity(t0%>%get_head()), "Seize")
  expect_output(print_activity(t0%>%get_tail()), "Release")
})

test_that("we can force some errors (just to complete coverage)", {
  expect_error(create_trajectory() %>% get_next_activity())
  expect_error(create_trajectory() %>% get_prev_activity())
  
  t0 <- create_trajectory() %>% timeout(function() {})
  t0$.__enclos_env__$private$head <- NULL
  expect_error(t0 %>% print)
  expect_error(t0$.__enclos_env__$private$add_activity(NULL))
  
  t0 <- create_trajectory() %>% timeout(function() {})
  t0$.__enclos_env__$private$tail <- NULL
  expect_error(t0 %>% timeout(function() {}))
})
