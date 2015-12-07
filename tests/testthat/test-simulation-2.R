context("simulation 2")

test_that("a release is executed before a seize in the same instant", {
  t0 <- create_trajectory() %>%
    seize("server", 1) %>%
    timeout(1) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 1, 0) %>%
    add_generator("dummy", t0, function() 1) %>%
    run(11) 
  
  arrivals <- env%>%get_mon_arrivals()
  
  expect_equal(sum(arrivals$finished), nrow(arrivals))
})