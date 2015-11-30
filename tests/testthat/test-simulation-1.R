context("simulation 1")

test_that("a simple deterministic simulation with rejections behaves as expected", {
  t0 <- create_trajectory("") %>%
    seize("server", 1) %>%
    timeout(function() 1.5) %>%
    release("server", 1)
  
  env <- simmer() %>%
    add_resource("server", 1, queue_size=0) %>%
    add_generator("entity", t0, function() 1)
  
  expect_warning(env%>%add_generator("entity", t0, function() 2))
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), 1)
  
  env%>%onestep()%>%onestep()%>%onestep()
  
  expect_equal(env%>%now(), 1)
  expect_equal(env%>%peek(), 2)
  
  n <- 1000
  env%>%run(n+1)
  arrivals <- env%>%get_mon_arrivals()
  resources <- env%>%get_mon_resources()
  
  expect_equal(env%>%now(), n+1)
  expect_equal(env%>%peek(), n+1)
  expect_equal(nrow(arrivals), n)
  expect_equal(nrow(subset(arrivals, finished)), n/2)
  expect_equal(nrow(subset(arrivals, !finished)), n/2)
  expect_equal(sum(subset(arrivals, finished)$activity_time), 1.5*n/2)
  expect_equal(sum(subset(arrivals, !finished)$activity_time), 0)
  
  expect_equal(nrow(resources), n*1.5)
  expect_equal(sum(resources$server), n)
  expect_equal(mean(resources$server), 2/3)
  expect_equal(sum(resources$queue), 0)
})
