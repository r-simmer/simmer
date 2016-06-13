context("rollback")

test_that("a rollback points to the correct activity", {
  t0 <- create_trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, T, create_trajectory()%>%timeout(function() 1)) %>%
    rollback(3)
  expect_output(t0%>%get_tail()%>%print_activity(), "Seize")
  
  t0 <- create_trajectory() %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    branch(function() 1, T, create_trajectory()%>%timeout(function() 1)) %>%
    rollback(30)
  expect_output(t0%>%get_tail()%>%print_activity(), "Seize")
  
  t0 <- create_trajectory() %>%
    seize("dummy", 1) %>%
    branch(function() 1, T, 
           create_trajectory() %>% 
             rollback(2))
  
  env <- simmer() %>%
    add_resource("dummy", 2, 0) %>%
    add_generator("one", t0, at(0)) %>%
    run()
  
  expect_equal(env%>%get_server_count("dummy"), 2)
})

test_that("a rollback loops the correct number of times", {
  three_times <- function() {
    count <- 0
    function() {
      if (count < 3) {
        count <<- count + 1
        TRUE
      } else FALSE
    }
  }
  
  t0 <- create_trajectory() %>% rollback(0, 3)
  t1 <- create_trajectory() %>% rollback(0, check=three_times())
  
  env0 <- evaluate_promise(simmer(verbose=TRUE) %>% add_generator("dummy", t0, at(0)))$result
  env1 <- evaluate_promise(simmer(verbose=TRUE) %>% add_generator("dummy", t1, at(0)))$result
  
  output <- paste0(".*(",
    ".*0.*dummy0.*Rollback",
    ".*0.*dummy0.*Rollback",
    ".*0.*dummy0.*Rollback",
    ".*0.*dummy0.*Rollback",
  ").*")
  
  expect_output(env0%>%run(), output)
  expect_output(env1%>%run(), output)
  
  t0 <- create_trajectory() %>% rollback(0, Inf)
  expect_output(t0%>%get_tail()%>%print_activity(), "Inf")
})

test_that("a negative amount is converted to positive", {
  t0 <- create_trajectory() %>% seize("dummy", 1)

  expect_silent(t0%>%rollback(-1, -1))
  expect_output(t0%>%get_tail()%>%print_activity(), "amount: 1 (Seize), times: 1", fixed=TRUE)
})

test_that("a check function that returns a non-boolean value fails", {
  t0 <- create_trajectory() %>%
    rollback(1, check=function() "dummy")
  
  env <- simmer() %>%
    add_generator("entity", t0, function() 1)
  
  expect_error(env%>%run(100))
})

test_that("incorrect types fail", {
  expect_error(create_trajectory() %>% rollback("dummy", 0))
  expect_error(create_trajectory() %>% rollback(0, "dummy"))
  expect_error(create_trajectory() %>% rollback(0, check=0))
})
