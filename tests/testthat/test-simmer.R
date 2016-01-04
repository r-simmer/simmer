context("basic Simmer functionality")

test_that("an empty environment behaves as expected", {
  env <- simmer()
  
  expect_is(env, "Simmer")
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), Inf)
  
  env%>%onestep()%>%run()
  
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), Inf)
})

t0 <- create_trajectory("") %>%
  seize("server", 1) %>%
  set_attribute("dummy", 1) %>%
  release("server", 1)

test_that("the simulator is reset", {
  env <- simmer() %>%
    add_resource("server", 1, queue_size=0) %>%
    add_generator("entity", t0, function() 1) %>%
    run(100) %>%
    reset()
  arrivals <- env%>%get_mon_arrivals()
  arrivals_res <- env%>%get_mon_arrivals(TRUE)
  resources <- env%>%get_mon_resources()
  attributes <- env%>%get_mon_attributes()
  
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), 1)
  expect_equal(nrow(arrivals), 0)
  expect_equal(nrow(arrivals_res), 0)
  expect_equal(nrow(resources), 0)
  expect_equal(nrow(attributes), 0)
})

test_that("the simulator stops if there are no more events", {
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, at(1)) %>%
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
    ".*sim.*dummy.*time: 1.*arrival0.*Seize.*server",
    ".*sim.*dummy.*time: 1.*arrival0.*Release.*server",
  ").*")
  
  expect_output(
    env <- simmer("dummy", verbose=TRUE) %>%
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
  expect_error(env %>% get_mon_arrivals())
  expect_error(env %>% get_mon_arrivals(TRUE))
  expect_error(env %>% get_mon_attributes())
  expect_error(env %>% get_mon_resources())
})
