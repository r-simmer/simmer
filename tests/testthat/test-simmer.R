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
  release("server", 1)

test_that("the simulator is reset", {
  env <- simmer() %>%
    add_resource("server", 1, queue_size=0) %>%
    add_generator("entity", t0, function() 1) %>%
    run(100) %>%
    reset()
  arrivals <- env%>%get_mon_arrivals()
  resources <- env%>%get_mon_resources()
  
  expect_equal(env%>%now(), 0)
  expect_equal(env%>%peek(), 1)
  expect_equal(nrow(arrivals), 0)
  expect_equal(nrow(resources), 0)
})

test_that("the simulator stops if there are no more events", {
  onlyone <- function(){
    generate <- 1
    function() {
      if (generate) {
        generate <<- 0
        return(1)
      } else return(-1)
    }
  }
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, onlyone()) %>%
    run(10)
  
  expect_equal(env%>%now(), 1)
})

test_that("a negative simulation time is converted to positive", {
  env <- simmer() %>%
    add_resource("server", 1) %>%
    add_generator("entity", t0, function() 1) %>%
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
