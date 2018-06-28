# Copyright (C) 2015-2016 Iñaki Ucar and Bart Smeets
# Copyright (C) 2016-2018 Iñaki Ucar
#
# This file is part of simmer.
#
# simmer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# simmer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with simmer. If not, see <http://www.gnu.org/licenses/>.

context("basic trajectory functionality")

t0 <- trajectory(verbose = TRUE) %>%
  seize("nurse", 1) %>%
  select(c("a", "b")) %>%
  seize_selected(1) %>%
  timeout(function() rnorm(1, 15)) %>%
  timeout_from_attribute("asdf") %>%
  leave(0) %>%
  branch(function() 1, T, trajectory(verbose = TRUE) %>% timeout(function() 1)) %>%
  set_attribute("dummy", 1) %>%
  set_prioritization(function() c(0, 0, FALSE)) %>%
  set_capacity("nurse", function() 1) %>%
  set_capacity_selected(function() 1) %>%
  set_queue_size("nurse", function() 1) %>%
  set_queue_size_selected(function() 1) %>%
  activate(function() "dummy") %>%
  deactivate(function() "dummy") %>%
  set_trajectory(function() "dummy", trajectory(verbose = TRUE) %>% timeout(1)) %>%
  set_source(function() "dummy", at(0)) %>%
  rollback(1) %>%
  clone(function() 2, trajectory(verbose = TRUE) %>% timeout(1)) %>%
  synchronize() %>%
  batch(1, rule = function() 0) %>%
  separate() %>%
  renege_in(function() 1, trajectory(verbose = TRUE) %>% timeout(1)) %>%
  renege_if(function() "1", trajectory(verbose = TRUE) %>% timeout(1)) %>%
  renege_abort() %>%
  send(function() "asdf", function() 0) %>%
  trap(function() "asdf", trajectory(verbose = TRUE) %>% timeout(1)) %>%
  untrap(function() "asdf") %>%
  wait() %>%
  log_(function() "asdf") %>%
  release_selected(1) %>%
  release("nurse", 1)

trajs <- c(
  trajectory(verbose = TRUE) %>% seize("nurse", 1),
  trajectory(verbose = TRUE) %>% select(c("a", "b")),
  trajectory(verbose = TRUE) %>% seize_selected(1),
  trajectory(verbose = TRUE) %>% timeout(function() rnorm(1, 15)),
  trajectory(verbose = TRUE) %>% timeout_from_attribute("asdf"),
  trajectory(verbose = TRUE) %>% leave(0),
  trajectory(verbose = TRUE) %>% branch(function() 1, T,
                                        trajectory(verbose = TRUE) %>%
                                          timeout(function() 1)),
  trajectory(verbose = TRUE) %>% set_attribute("dummy", 1),
  trajectory(verbose = TRUE) %>% set_prioritization(function() c(0, 0, FALSE)),
  trajectory(verbose = TRUE) %>% set_capacity("nurse", function() 1),
  trajectory(verbose = TRUE) %>% set_capacity_selected(function() 1),
  trajectory(verbose = TRUE) %>% set_queue_size("nurse", function() 1),
  trajectory(verbose = TRUE) %>% set_queue_size_selected(function() 1),
  trajectory(verbose = TRUE) %>% activate(function() "dummy"),
  trajectory(verbose = TRUE) %>% deactivate(function() "dummy"),
  trajectory(verbose = TRUE) %>% set_trajectory(function() "dummy",
                                                trajectory(verbose = TRUE) %>%
                                                  timeout(1)),
  trajectory(verbose = TRUE) %>% set_source(function() "dummy", at(0)),
  trajectory(verbose = TRUE) %>% rollback(1),
  trajectory(verbose = TRUE) %>% clone(function() 2,
                                       trajectory(verbose = TRUE) %>% timeout(1)),
  trajectory(verbose = TRUE) %>% synchronize(),
  trajectory(verbose = TRUE) %>% batch(1, rule = function() 0),
  trajectory(verbose = TRUE) %>% separate(),
  trajectory(verbose = TRUE) %>% renege_in(function() 1,
                                           trajectory(verbose = TRUE) %>% timeout(1)),
  trajectory(verbose = TRUE) %>% renege_if(function() "1",
                                           trajectory(verbose = TRUE) %>% timeout(1)),
  trajectory(verbose = TRUE) %>% renege_abort(),
  trajectory(verbose = TRUE) %>% send(function() "asdf", function() 0),
  trajectory(verbose = TRUE) %>% trap(function() "asdf",
                                      trajectory(verbose = TRUE) %>% timeout(1)),
  trajectory(verbose = TRUE) %>% untrap(function() "asdf"),
  trajectory(verbose = TRUE) %>% wait(),
  trajectory(verbose = TRUE) %>% log_(function() "asdf"),
  trajectory(verbose = TRUE) %>% release_selected(1),
  trajectory(verbose = TRUE) %>% release("nurse", 1)
)

N <- length(t0) - 1

test_that("the activity chain grows as expected", {
  ptr_head <- t0$head()
  for (i in 1:N) ptr_head <- activity_get_next_(ptr_head)
  ptr_tail <- t0$tail()
  for (i in 1:N) ptr_tail <- activity_get_prev_(ptr_tail)

  expect_output(activity_print_(ptr_head, 0, 0), "Release")
  expect_output(activity_print_(t0$tail(), 0, 0), "Release")
  expect_equal(activity_get_next_(ptr_head), NULL)
  expect_output(activity_print_(ptr_tail, 0, 0), "Seize")
  expect_output(activity_print_(t0$head(), 0, 0), "Seize")
  expect_equal(activity_get_prev_(ptr_tail), NULL)
})

test_that("the activity chain grows as expected using join", {
  t <- join(trajs)

  ptr_head <- t$head()
  for (i in 1:N) ptr_head <- activity_get_next_(ptr_head)
  ptr_tail <- t$tail()
  for (i in 1:N) ptr_tail <- activity_get_prev_(ptr_tail)

  expect_output(activity_print_(ptr_head, 0, 0), "Release")
  expect_output(activity_print_(t$tail(), 0, 0), "Release")
  expect_equal(activity_get_next_(ptr_head), NULL)
  expect_output(activity_print_(ptr_tail, 0, 0), "Seize")
  expect_output(activity_print_(t$head(), 0, 0), "Seize")
  expect_equal(activity_get_prev_(ptr_tail), NULL)

  expect_true(length(capture.output(t)) == length(capture.output(t0)))

  # check that pointers differ
  ptrs <- lapply(trajs, function(i) {
    line <- capture.output(i)
    regmatches(line, regexpr("<- 0x[[:alnum:]]{7} ->", line))
  }) %>% unlist
  ptrs_t <- lapply(capture.output(t), function(i) {
    regmatches(i, regexpr("<- 0x[[:alnum:]]{7} ->", i))
  }) %>% unlist

  expect_false(any(ptrs == ptrs_t))
})

test_that("the trajectory stores the right number of activities", {
  t0 <- trajectory("my trajectory") %>%
    seize("nurse", 1) %>%
    timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1)

  expect_is(t0, "trajectory")
  expect_equal(length(t0), 3)
  expect_equal(get_n_activities(t0), 3)

  t0 <- t0 %>%
    branch(function() 1, TRUE,
           trajectory() %>%
             seize("doctor", function() 1) %>%
             timeout(function() rnorm(1, 20)) %>%
             release("doctor", function() 1) %>%
             branch(function() 1, TRUE,
                    trajectory() %>%
                      seize("administration", 1) %>%
                      timeout(1) %>%
                      release("administration", 1)
             )
    ) %>%
    rollback(1) %>%
    rollback(1, check = function() FALSE) %>%
    set_attribute("dummy", 1) %>%
    set_attribute("dummy", function() 1)

  expect_is(t0, "trajectory")
  expect_equal(length(t0), 8)
  expect_equal(get_n_activities(t0), 15)

  output <- paste0(".*(",
    "15 activities",
    ".*Seize.*nurse.*1",
    ".*Timeout.*function",
    ".*Release.*nurse.*1",
    ".*Branch.*1",
      ".*7 activities",
      ".*Seize.*doctor.*function",
      ".*Timeout.*function",
      ".*Release.*doctor.*function",
      ".*Branch.*1",
        ".*3 activities",
        ".*Seize.*administration.*1",
        ".*Timeout.*1",
        ".*Release.*administration.*1",
    ".*Rollback.*1.*Branch.*1",
    ".*Rollback.*1.*Rollback.*function",
    ".*SetAttribute.*1",
    ".*SetAttribute.*function",
  ").*")

  expect_output(print(t0), output)
})

test_that("the head/tail pointers are correctly placed", {
  t0 <- trajectory()

  expect_equal(t0$head(), NULL)
  expect_equal(t0$tail(), NULL)

  t0 %>% seize("nurse", 1)

  expect_output(activity_print_(t0$head(), 0, 0), "Seize")
  expect_output(activity_print_(t0$tail(), 0, 0), "Seize")

  t0 %>% timeout(function() rnorm(1, 15)) %>%
    release("nurse", 1)

  expect_output(activity_print_(t0$head(), 0, 0), "Seize")
  expect_output(activity_print_(t0$tail(), 0, 0), "Release")
})

t0 <- trajectory(verbose = TRUE) %>%
  timeout(1)
t1 <- trajectory(verbose = TRUE) %>%
  branch(function() 1, c(TRUE), t0) %>%
  join(t0) %>%
  branch(function() 1, c(TRUE, TRUE, TRUE), t0, t0, t0) %>%
  join(t0) %>%
  branch(function() 1, c(TRUE, TRUE, TRUE, TRUE, TRUE), t0, t0, t0, t0, t0)

test_that("special cases subsetting with [ works as expected", {
  test <- t1[]
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 14)
  test <- t1[NULL]
  expect_equal(length(test), 0)
  expect_equal(get_n_activities(test), 0)
  test <- t1[NA]
  expect_equal(length(test), 0)
  expect_equal(get_n_activities(test), 0)
  expect_error(t1[t1])
})

test_that("special cases replacing with [ works as expected", {
  test <- t1[]
  test[] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 5)
  test <- t1[]
  test[NULL] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 14)
  test <- t1[]
  test[NA] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 14)
  test <- trajectory()
  test[] <- t0
  expect_equal(length(test), 0)
  expect_equal(get_n_activities(test), 0)
})

test_that("logical subsetting with [ works as expected", {
  test <- t1[TRUE]
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 14)
  test <- t1[c(rep(FALSE, 4), TRUE)]
  expect_equal(length(test), 1)
  expect_equal(get_n_activities(test), 6)
  test <- t1[c(TRUE, FALSE, TRUE, FALSE, TRUE)]
  expect_equal(length(test), 3)
  expect_equal(get_n_activities(test), 12)
  test <- t1[c(TRUE, FALSE)]
  expect_equal(length(test), 3)
  expect_equal(get_n_activities(test), 12)
  test <- t1[c(FALSE, TRUE)]
  expect_equal(length(test), 2)
  expect_equal(get_n_activities(test), 2)
  expect_error(t1[rep(TRUE, 20)])
})

test_that("logical replacing with [ works as expected", {
  test <- t1[]
  test[TRUE] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 5)
  test <- t1[]
  test[c(rep(FALSE, 4), TRUE)] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 9)
  test <- t1[]
  test[c(TRUE, FALSE, TRUE, FALSE, TRUE)] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 5)
  test <- t1[]
  test[c(TRUE, FALSE)] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 5)
  test <- t1[]
  test[c(FALSE, TRUE)] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 14)
})

test_that("integer subsetting with [ works as expected", {
  test <- t1[1]
  expect_equal(length(test), 1)
  expect_equal(get_n_activities(test), 2)
  test <- t1[length(t1)]
  expect_equal(length(test), 1)
  expect_equal(get_n_activities(test), 6)
  test <- t1[c(1, 3, 5)]
  expect_equal(length(test), 3)
  expect_equal(get_n_activities(test), 12)
  test <- t1[-c(2, 4)]
  expect_equal(length(test), 3)
  expect_equal(get_n_activities(test), 12)
  expect_error(t1[c(1, -1)])
})

test_that("integer replacing with [ works as expected", {
  test <- t1[]
  test[1] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 13)
  test <- t1[]
  test[length(t1)] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 9)
  test <- t1[]
  test[c(1, 3, 5)] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 5)
  test <- t1[]
  test[-c(2, 4)] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 5)
})

test_that("character subsetting with [ works as expected", {
  test <- t1["branch"]
  expect_equal(length(test), 3)
  expect_equal(get_n_activities(test), 12)
  test <- t1["asdf"]
  expect_equal(length(test), 0)
  expect_equal(get_n_activities(test), 0)
})

test_that("character replacing with [ works as expected", {
  test <- t1[]
  test["branch"] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 5)
  test <- t1[]
  test["asdf"] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 14)
})

test_that("integer subsetting with [[ works as expected", {
  test <- t1[[1]]
  expect_equal(length(test), 1)
  expect_equal(get_n_activities(test), 2)
  test <- t1[[length(t1)]]
  expect_equal(length(test), 1)
  expect_equal(get_n_activities(test), 6)
  expect_error(t1[[c(1, 3, 5)]])
  expect_error(t1[[-1]])
})

test_that("integer replacing with [[ works as expected", {
  test <- t1[]
  test[[1]] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 13)
  test <- t1[]
  test[[length(t1)]] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 9)
})

test_that("character subsetting with [ works as expected", {
  test <- t1[["branch"]]
  expect_equal(length(test), 1)
  expect_equal(get_n_activities(test), 2)
  expect_error(t1[["asdf"]])
})

test_that("character replacing with [ works as expected", {
  test <- t1[]
  test[["branch"]] <- t0
  expect_equal(length(test), 5)
  expect_equal(get_n_activities(test), 13)
})

test_that("rep works for trajectories", {
  test <- rep(t1, times=2, length.out=10, each=2)
  expect_equal(length(test), 10)
  expect_equal(get_n_activities(test), 28)
})
