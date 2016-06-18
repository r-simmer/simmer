context("basic simmer functionality")

test_that("an empty environment behaves as expected", {
  env <- simmer()
  
  expect_output(print(env))
  
  expect_is(env, "simmer")
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), numeric(0))
  
  env%>%onestep()%>%run()
  
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), numeric(0))
})

t0 <- create_trajectory("") %>%
  seize("server", 1) %>%
  set_attribute("dummy", 1) %>%
  timeout(1) %>%
  release("server", 1)

test_that("the simulator is reset", {
  t1 <- create_trajectory() %>%
    seize("server", 1, preemptible=10, priority=10) %>%
    set_attribute("dummy", 1) %>%
    timeout(1) %>%
    release("server", 1)
  
  inf_sch <- schedule(c(0.5, 1), c(1, 1), Inf)
  
  env <- simmer() %>%
    add_resource("server", inf_sch, queue_size=1, preemptive=TRUE) %>%
    add_generator("entity0", t0, function() 0.5) %>%
    add_generator("entity1", t1, function() 0.5, mon=2) %>%
    run(4) %>%
    reset()
  
  arrivals <- env%>%get_mon_arrivals()
  arrivals_res <- env%>%get_mon_arrivals(TRUE)
  resources <- env%>%get_mon_resources()
  attributes <- env%>%get_mon_attributes()
  
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), 0.5)
  expect_equal(nrow(arrivals), 0)
  expect_equal(nrow(arrivals_res), 0)
  expect_equal(nrow(resources), 0)
  expect_equal(nrow(attributes), 0)
})

test_that("the simulator stops if there are no more events", {
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, at(0)) %>%
    run(10)
  
  expect_equal(env%>%now(), 1)
})

test_that("a negative simulation time is converted to positive", {
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, at(10)) %>%
    run(-10)
  
  expect_equal(env%>%now(), 10)
})

test_that("a stopped simulation can be resumed", {
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, function() 1) %>%
    run(10)
  
  expect_equal(env%>%now(), 10)
  env%>%run(20)
  expect_equal(env%>%now(), 20)
  env%>%run(30)
  expect_equal(env%>%now(), 30)
})

test_that("there is verbose output", {
  output <- paste0(".*(",
    ".*1.*arrival0.*Seize.*server",
    ".*1.*arrival0.*Release.*server",
  ").*")
  
  expect_output(
    env <- simmer(verbose=TRUE) %>%
      add_resource("server", 1) %>%
      add_generator("arrival", t0, at(1)) %>%
      run(),
    output
  )
})

test_that("we can force some errors (just to complete coverage)", {
  expect_error(simmer(0))
  expect_error(simmer() %>% add_resource(0))
  
  env <- simmer() %>% 
    add_resource("dummy") %>% 
    add_generator("dummy", create_trajectory()%>%timeout(0), function() 1, mon=1000)
  env$.__enclos_env__$private$sim_obj <- NULL
  
  expect_error(env %>% reset())
  expect_error(env %>% now())
  expect_error(env %>% peek())
  expect_error(env %>% onestep())
  expect_error(env %>% get_mon_arrivals(FALSE))
  expect_error(env %>% get_mon_arrivals(TRUE))
  expect_error(env %>% get_mon_attributes())
  expect_error(env %>% get_mon_resources("counts"))
  expect_error(env %>% get_mon_resources("limits"))
  expect_error(env %>% get_mon_resources(c("counts", "limits")))
  
  sch <- schedule(c(1,2), c(1,2), Inf)
  sch$.__enclos_env__$private$schedule$period <- "asdf"
  expect_error(simmer() %>% add_resource("dummy", sch))
  
  env <- simmer()
  expect_equal(env %>% get_mon_resources() %>% nrow(), 0)
  expect_equal(env %>% get_mon_resources(c("counts", "limits")) %>% nrow(), 0)
})
