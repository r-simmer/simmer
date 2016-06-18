context("select")

test_that("resources are seized/released as expected", {
  t0 <- create_trajectory() %>%
    select("dummy0", id=0) %>%
    select("dummy1", id=1) %>%
    seize_selected(-1, id=0) %>%
    timeout(1) %>%
    seize_selected(function() 2, id=1) %>%
    timeout(1) %>%
    release_selected(-1, id=0) %>%
    timeout(1) %>%
    release_selected(function() 2, id=1) %>%
    timeout(1)
  
  env <- simmer(verbose=TRUE) %>%
    add_resource("dummy0", 3, 0) %>%
    add_resource("dummy1", 3, 0) %>%
    add_generator("arrival", t0, at(0))
  
  env %>% onestep() %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy0"), 1)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy1"), 2)
  env %>% onestep() %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy0"), 0)
  env %>% onestep() %>% onestep()
  expect_equal(env %>% get_server_count("dummy1"), 0)
})

test_that("core selection algorithms work: shortest-queue", {
  t0 <- create_trajectory() %>% seize("r1", 1)
  t1 <- create_trajectory() %>% seize("r2", 1)
  
  t2 <- create_trajectory() %>%
    select(c("r1", "r2", "r3"), policy="shortest-queue") %>%
    seize_selected(1)
  
  env <- simmer() %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6))) %>%
    run()
  
  res <- get_mon_resources(env)
  res_ordered <- res[order(res$time),]
  res_ordered <- res_ordered[4:9,]
  
  expect_equal(res_ordered$server, c(1, 2, 1, 2, 3, 1))
  expect_equal(res_ordered$queue, c(0, 0, 1, 1, 0, 2))
  expect_equal(res_ordered$resource, c("r3", "r1", "r3", "r1", "r2", "r3"))
})

test_that("core selection algorithms work: round-robin", {
  t0 <- create_trajectory() %>% seize("r1", 1)
  t1 <- create_trajectory() %>% seize("r2", 1)
  
  t2 <- create_trajectory() %>%
    select(c("r1", "r2", "r3"), policy="round-robin") %>%
    seize_selected(1)
  
  env <- simmer() %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6))) %>%
    run()
  
  res <- get_mon_resources(env)
  res_ordered <- res[order(res$time),]
  res_ordered <- res_ordered[4:9,]
  
  expect_equal(res_ordered$server, c(2, 3, 1, 2, 3, 1))
  expect_equal(res_ordered$queue, c(0, 0, 0, 1, 1, 1))
  expect_equal(res_ordered$resource, c("r1", "r2", "r3", "r1", "r2", "r3"))
})

test_that("core selection algorithms work: first-available", {
  t0 <- create_trajectory() %>% seize("r1", 1)
  t1 <- create_trajectory() %>% seize("r2", 1)
  
  t2 <- create_trajectory() %>%
    select(c("r1", "r2", "r3"), policy="first-available") %>%
    seize_selected(1)
  
  env <- simmer() %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6))) %>%
    run()
  
  res <- get_mon_resources(env)
  res_ordered <- res[order(res$time),]
  res_ordered <- res_ordered[4:9,]
  
  expect_equal(res_ordered$server, c(2, 3, 1, 2, 2, 2))
  expect_equal(res_ordered$queue, c(0, 0, 0, 1, 2, 3))
  expect_equal(res_ordered$resource, c("r1", "r2", "r3", "r1", "r1", "r1"))
})

test_that("core selection algorithms work: random", {
  t0 <- create_trajectory() %>% seize("r1", 1)
  t1 <- create_trajectory() %>% seize("r2", 1)
  
  t2 <- create_trajectory() %>%
    select(c("r1", "r2", "r3"), policy="random") %>%
    seize_selected(1)
  
  env <- simmer() %>%
    add_resource("r1", 2) %>%
    add_resource("r2", 3) %>%
    add_resource("r3", 1) %>%
    add_generator("dummy0", t0, at(0)) %>%
    add_generator("dummy1", t1, at(0, 0)) %>%
    add_generator("dummy2", t2, at(seq(1, 6)))
  
  res1 <- get_mon_resources(env %>% reset %>% run)
  res2 <- get_mon_resources(env %>% reset %>% run)
  res3 <- get_mon_resources(env %>% reset %>% run)
  expect_true(!all(res1 == res2) || !all(res1 == res3))
})
