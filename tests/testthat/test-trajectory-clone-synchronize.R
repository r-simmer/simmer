context("clone/synchronize")

test_that("each clone follows a trajectory 1", {
  t <- create_trajectory() %>%
    clone(3,
          create_trajectory("original") %>%
            timeout(1),
          create_trajectory("clone 1") %>%
            timeout(2),
          create_trajectory("clone 2") %>%
            timeout(3)) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, c(1.5, 2.5, 3.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("each clone follows a trajectory 2", {
  t <- create_trajectory() %>%
    clone(3,
          create_trajectory("original") %>%
            timeout(1),
          create_trajectory("clone 1") %>%
            timeout(2)) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, c(0.5, 1.5, 2.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("each clone follows a trajectory 3", {
  t <- create_trajectory() %>%
    clone(3,
          create_trajectory("original") %>%
            timeout(1)) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, c(0.5, 0.5, 1.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("each clone follows a trajectory 4", {
  t <- create_trajectory() %>%
    clone(3) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, c(0.5, 0.5, 0.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("clones synchonize with the last 1", {
  t <- create_trajectory() %>%
    clone(3,
          create_trajectory("original") %>%
            timeout(1),
          create_trajectory("clone 1") %>%
            timeout(2),
          create_trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait=TRUE, mon_all=FALSE) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, 3.5)
  expect_equal(arrivals$finished, TRUE)
})

test_that("clones synchonize with the last 2", {
  t <- create_trajectory() %>%
    clone(3,
          create_trajectory("original") %>%
            timeout(1),
          create_trajectory("clone 1") %>%
            timeout(2),
          create_trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait=TRUE, mon_all=TRUE) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, c(1, 2, 3.5))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})

test_that("clones synchonize with the first 1", {
  t <- create_trajectory() %>%
    clone(3,
          create_trajectory("original") %>%
            timeout(1),
          create_trajectory("clone 1") %>%
            timeout(2),
          create_trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait=FALSE, mon_all=FALSE) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, 1.5)
  expect_equal(arrivals$finished, TRUE)
})

test_that("clones synchonize with the first 2", {
  t <- create_trajectory() %>%
    clone(3,
          create_trajectory("original") %>%
            timeout(1),
          create_trajectory("clone 1") %>%
            timeout(2),
          create_trajectory("clone 2") %>%
            timeout(3)) %>%
    synchronize(wait=FALSE, mon_all=TRUE) %>%
    timeout(0.5)
  
  arrivals <- simmer(verbose=TRUE) %>%
    add_generator("arrival", t, at(0)) %>%
    run() %>%
    get_mon_arrivals()
  
  expect_equal(arrivals$activity_time, c(1.5, 2, 3))
  expect_equal(arrivals$finished, rep(TRUE, 3))
})
