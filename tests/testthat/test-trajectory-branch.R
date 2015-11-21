context("branch")

test_that("a non-function option fails", {
  expect_error(create_trajectory()%>%branch(1, T, create_trajectory()))
})

test_that("the wrong number of elements fails", {
  expect_error(create_trajectory()%>%branch(function() 1, T, create_trajectory(), create_trajectory()))
  expect_error(create_trajectory()%>%branch(function() 1, c(T, T), create_trajectory()))
})

test_that("an empty Trajectory fails", {
  expect_error(create_trajectory()%>%branch(function() 1, T, create_trajectory()))
})

test_that("an index out of range fails", {
  t0 <- create_trajectory() %>% 
    branch(function() 0, T, 
      create_trajectory()%>%timeout(function() 1)
    )
  t1 <- create_trajectory() %>% 
    branch(function() 1, T, 
      create_trajectory()%>%timeout(function() 1)
    )
  t2 <- create_trajectory() %>% 
    branch(function() 2, T, 
      create_trajectory()%>%timeout(function() 1)
    )
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  expect_error(env%>%run(10))
  
  env <- simmer() %>%
    add_generator("entity", t2, function() 1)
  expect_error(env%>%run(10))
  
  env <- simmer() %>%
    add_generator("entity", t1, function() 1) %>%
    run(10)
  expect_equal(env%>%now(), 10)
})
