context("basic trajectory functionality")

t0 <- create_trajectory(verbose = TRUE) %>%
  seize("nurse", 1) %>%
  select(c("a", "b")) %>%
  seize_selected(1) %>%
  timeout(function() rnorm(1, 15)) %>%
  leave(0) %>%
  branch(function() 1, T, create_trajectory(verbose = TRUE) %>% timeout(function() 1)) %>%
  set_attribute("dummy", 1) %>%
  set_prioritization(function() c(0, 0, FALSE)) %>%
  set_capacity("nurse", function() 1) %>%
  set_capacity_selected(function() 1) %>%
  set_queue_size("nurse", function() 1) %>%
  set_queue_size_selected(function() 1) %>%
  rollback(1) %>%
  clone(function() 2, create_trajectory(verbose = TRUE) %>% timeout(1)) %>%
  synchronize() %>%
  batch(1) %>%
  separate() %>%
  renege_in(function() 1, create_trajectory(verbose = TRUE) %>% timeout(1)) %>%
  renege_abort() %>%
  release_selected(1) %>%
  release("nurse", 1)

trajs <- c(create_trajectory(verbose = TRUE) %>% seize("nurse", 1),
           create_trajectory(verbose = TRUE) %>% select(c("a", "b")),
           create_trajectory(verbose = TRUE) %>% seize_selected(1),
           create_trajectory(verbose = TRUE) %>% timeout(function() rnorm(1, 15)),
           create_trajectory(verbose = TRUE) %>% leave(0),
           create_trajectory(verbose = TRUE) %>% branch(function() 1, T,
                                                        create_trajectory(verbose = TRUE) %>%
                                                          timeout(function() 1)),
           create_trajectory(verbose = TRUE) %>% set_attribute("dummy", 1),
           create_trajectory(verbose = TRUE) %>% set_prioritization(function() c(0, 0, FALSE)),
           create_trajectory(verbose = TRUE) %>% set_capacity("nurse", function() 1),
           create_trajectory(verbose = TRUE) %>% set_capacity_selected(function() 1),
           create_trajectory(verbose = TRUE) %>% set_queue_size("nurse", function() 1),
           create_trajectory(verbose = TRUE) %>% set_queue_size_selected(function() 1),
           create_trajectory(verbose = TRUE) %>% rollback(1),
           create_trajectory(verbose = TRUE) %>% clone(function() 2,
                                                       create_trajectory(verbose = TRUE) %>% timeout(1)),
           create_trajectory(verbose = TRUE) %>% synchronize(),
           create_trajectory(verbose = TRUE) %>% batch(1),
           create_trajectory(verbose = TRUE) %>% separate(),
           create_trajectory(verbose = TRUE) %>% renege_in(function() 1,
                                                           create_trajectory(verbose = TRUE) %>% timeout(1)),
           create_trajectory(verbose = TRUE) %>% renege_abort(),
           create_trajectory(verbose = TRUE) %>% release_selected(1),
           create_trajectory(verbose = TRUE) %>% release("nurse", 1))

N <- 20

test_that("the activity chain grows as expected", {
  head <- t0 %>% get_head()
  for (i in 1:N) head <- get_next_activity(head)
  tail <- t0 %>% get_tail()
  for (i in 1:N) tail <- get_prev_activity(tail)

  expect_output(print_activity(head), "Release")
  expect_output(print_activity(t0 %>% get_tail()), "Release")
  expect_equal(get_next_activity(head), NULL)
  expect_output(print_activity(tail), "Seize")
  expect_output(print_activity(t0 %>% get_head()), "Seize")
  expect_equal(get_prev_activity(tail), NULL)
})

test_that("the activity chain grows as expected using join", {
  t <- join(trajs)

  head <- t %>% get_head()
  for (i in 1:N) head <- get_next_activity(head)
  tail <- t %>% get_tail()
  for (i in 1:N) tail <- get_prev_activity(tail)

  expect_output(print_activity(head), "Release")
  expect_output(print_activity(t %>% get_tail()), "Release")
  expect_equal(get_next_activity(head), NULL)
  expect_output(print_activity(tail), "Seize")
  expect_output(print_activity(t %>% get_head()), "Seize")
  expect_equal(get_prev_activity(tail), NULL)

  expect_true(length(capture.output(t)) == length(capture.output(t0)))

  # check that pointers differ
  ptrs <- lapply(trajs, function(i) {
    line <- capture.output(i)
    regmatches(line, regexpr("<- 0x[[:alnum:]]{7} ->", line))
  }) %>% unlist
  ptrs_t <- lapply(capture.output(t0), function(i) {
    regmatches(i, regexpr("<- 0x[[:alnum:]]{7} ->", i))
  }) %>% unlist

  expect_false(any(ptrs == ptrs_t))
})

test_that("the trajectory stores the right number of activities", {
  t0 <- create_trajectory("my trajectory") %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1)

  expect_is(t0, "simmer.trajectory")
  expect_equal(t0 %>% get_n_activities(), 3)

  t0 <- t0 %>%
    branch(function() 1, TRUE,
           create_trajectory() %>%
             seize("doctor", function() 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", function() 1) %>%
             branch(function() 1, TRUE,
                    create_trajectory() %>%
                      seize("administration", 1) %>%
                      timeout(1) %>%
                      release("administration", 1)
             )
    ) %>%
    rollback(1) %>%
    rollback(1, check = function() FALSE) %>%
    set_attribute("dummy", 1) %>%
    set_attribute("dummy", function() 1)

  expect_is(t0, "simmer.trajectory")
  expect_equal(t0 %>% get_n_activities(), 15)

  output <- paste0(".*(",
    "15 activities",
    ".*Seize.*nurse.*1",
    ".*Timeout.*0x",
    ".*Release.*nurse.*1",
    ".*Branch.*1",
      ".*7 activities",
      ".*Seize.*doctor.*0x",
      ".*Timeout.*0x",
      ".*Release.*doctor.*0x",
      ".*Branch.*1",
        ".*3 activities",
        ".*Seize.*administration.*1",
        ".*Timeout.*1",
        ".*Release.*administration.*1",
    ".*Rollback.*1.*Branch.*1",
    ".*Rollback.*1.*Rollback.*0x",
    ".*SetAttribute.*1",
    ".*SetAttribute.*0x",
  ").*")

  expect_output(print(t0), output)
})

test_that("the head/tail pointers are correctly placed", {
  t0 <- create_trajectory()

  expect_equal(t0 %>% get_head(), NULL)
  expect_equal(t0 %>% get_tail(), NULL)

  t0 %>% seize("nurse", 1)

  expect_output(print_activity(t0 %>% get_head()), "Seize")
  expect_output(print_activity(t0 %>% get_tail()), "Seize")

  t0 %>% timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1)

  expect_output(print_activity(t0 %>% get_head()), "Seize")
  expect_output(print_activity(t0 %>% get_tail()), "Release")
})

test_that("we can force some errors (just to complete coverage)", {
  expect_error(create_trajectory() %>% get_next_activity())
  expect_error(create_trajectory() %>% get_prev_activity())
})
