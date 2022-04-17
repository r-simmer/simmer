# Copyright (C) 2016-2022 Iñaki Ucar
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

test_that("an arrival waits", {
  t <- trajectory() %>%
    wait()

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0)) %>%
    run(1000)

  expect_equal(env %>% peek, Inf)
  expect_equal(nrow(get_mon_arrivals(env)), 0)
})

test_that("a signal is immediately triggered", {
  t <- trajectory() %>%
    send("signal")

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0))

  expect_output(env %>% run, ".*0.*Broadcast")
})

test_that("a signal is triggered with some delay", {
  t <- trajectory() %>%
    send("signal", 3)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0))

  expect_output(env %>% run, ".*3.*Broadcast")
})

test_that("a signal is untrapped", {
  t <- trajectory() %>%
    untrap("asdf") %>%
    send("signal", 3) %>%
    trap("signal") %>%
    timeout(1) %>%
    untrap("signal") %>%
    timeout(8) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(10, 11, 12))
  expect_equal(arr$activity_time, c(10, 10, 10))
})

test_that("a signal is received while blocked", {
  t <- trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    wait() %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(4, 4, 4))
  expect_equal(arr$activity_time, c(1, 1, 1))
})

test_that("an empty handler is equivalent to NULL", {
  t <- trajectory() %>%
    send("signal", 3) %>%
    trap("signal", handler=trajectory()) %>%
    wait() %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(4, 4, 4))
  expect_equal(arr$activity_time, c(1, 1, 1))
})

test_that("a signal is received while in a timeout", {
  t <- trajectory() %>%
    send(function() "signal", 3) %>%
    trap(function() "signal") %>%
    timeout(10) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(4, 4, 4))
  expect_equal(arr$activity_time, c(4, 3, 2))
})

test_that("a signal is received while blocked inside a resource", {
  t <- trajectory() %>%
    send("signal", function() 3) %>%
    trap("signal") %>%
    seize("res", 1) %>%
    wait() %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2))
  expect_equal(arr_glb$end_time, c(4, 5, 6))
  expect_equal(arr_glb$activity_time, c(1, 1, 1))
  expect_equal(arr_res$start_time, c(0, 1, 2))
  expect_equal(arr_res$end_time, c(4, 5, 6))
  expect_equal(arr_res$activity_time, c(1, 1, 1))
})

test_that("a signal is received while in a timeout inside a resource", {
  t <- trajectory() %>%
    send(function() "signal", function() 3) %>%
    trap("signal") %>%
    seize("res", 1) %>%
    timeout(10) %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2))
  expect_equal(arr_glb$end_time, c(4, 5, 6))
  expect_equal(arr_glb$activity_time, c(4, 1, 1))
  expect_equal(arr_res$start_time, c(0, 1, 2))
  expect_equal(arr_res$end_time, c(4, 5, 6))
  expect_equal(arr_res$activity_time, c(4, 1, 1))
})

test_that("a signal is ignored inside a batch (1)", {
  t <- trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    batch(1) %>%
    seize("res", 1) %>%
    wait() %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run(1000)
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(nrow(arr_glb), 0)
  expect_equal(nrow(arr_res), 0)
})

test_that("a signal is ignored inside a batch (2)", {
  t <- trajectory() %>%
    send("signal", 3) %>%
    trap("signal") %>%
    batch(1) %>%
    seize("res", 1) %>%
    timeout(10) %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2))
  expect_equal(arr_glb$end_time, c(11, 22, 33))
  expect_equal(arr_glb$activity_time, c(11, 11, 11))
  expect_equal(arr_res$start_time, c(0, 1, 2))
  expect_equal(arr_res$end_time, c(11, 22, 33))
  expect_equal(arr_res$activity_time, c(11, 11, 11))
})

test_that("launch handler while blocked", {
  t <- trajectory() %>%
    send("signal", 3) %>%
    trap("signal", trajectory() %>% timeout(3)) %>%
    wait() %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(9, 9, 9))
  expect_equal(arr$activity_time, c(6, 6, 6))
})

test_that("launch handler while blocked (not interruptible)", {
  t <- trajectory() %>%
    send("signal", 3) %>%
    trap("signal",
         trajectory() %>% timeout(3),
         interruptible = FALSE) %>%
    wait() %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(7, 7, 7))
  expect_equal(arr$activity_time, c(4, 4, 4))
})

test_that("launch handler while in a timeout", {
  t <- trajectory() %>%
    send(function() "signal", 3) %>%
    trap(function() "signal", trajectory() %>% timeout(3)) %>%
    timeout(10) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(9, 9, 9))
  expect_equal(arr$activity_time, c(9, 8, 7))
})

test_that("launch handler while in a timeout (not interruptible)", {
  t <- trajectory() %>%
    send(function() "signal", 3) %>%
    trap(function() "signal",
         trajectory() %>% timeout(3),
         interruptible = FALSE) %>%
    timeout(10) %>%
    timeout(1)

  env <- simmer(verbose = TRUE) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$start_time, c(0, 1, 2))
  expect_equal(arr$end_time, c(7, 7, 7))
  expect_equal(arr$activity_time, c(7, 6, 5))
})

test_that("launch handler while blocked inside a resource", {
  t <- trajectory() %>%
    send("signal", function() 3) %>%
    trap("signal", trajectory() %>% timeout(1)) %>%
    seize("res", 1) %>%
    wait() %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run(1000)
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1))
  expect_equal(arr_glb$end_time, c(5, 7))
  expect_equal(arr_glb$activity_time, c(2, 2))
  expect_equal(arr_res$start_time, c(0, 1))
  expect_equal(arr_res$end_time, c(5, 7))
  expect_equal(arr_res$activity_time, c(2, 2))
})

test_that("launch handler while in a timeout inside a resource", {
  t <- trajectory() %>%
    send(function() "signal", function() 3) %>%
    trap("signal", trajectory() %>% timeout(1)) %>%
    seize("res", 1) %>%
    timeout(10) %>%
    timeout(1) %>%
    release("res", 1)

  env <- simmer(verbose = TRUE) %>%
    add_resource("res", 1) %>%
    add_generator("dummy", t, at(0, 1, 2)) %>%
    run()
  arr_glb <- get_mon_arrivals(env)
  arr_res <- get_mon_arrivals(env, per_resource = TRUE)

  expect_equal(arr_glb$start_time, c(0, 1, 2))
  expect_equal(arr_glb$end_time, c(5, 7, 18))
  expect_equal(arr_glb$activity_time, c(5, 2, 11))
  expect_equal(arr_res$start_time, c(0, 1, 2))
  expect_equal(arr_res$end_time, c(5, 7, 18))
  expect_equal(arr_res$activity_time, c(5, 2, 11))
})

test_that("an arrival cannot trap its own previously broadcasted signal", {
  t <- trajectory() %>%
    send("signal") %>%
    trap("signal") %>%
    timeout(1)

  env <- simmer(verbose=TRUE) %>%
    add_generator("dummy", t, at(0, 0, 0)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$end_time, c(1, 1, 1))

  t <- trajectory() %>%
    trap("signal") %>%
    send("signal")

  env <- simmer(verbose=TRUE) %>%
    add_generator("dummy", t, at(0, 0, 0))

  expect_error(run(env), NA)

  t <- trajectory() %>%
    trap("signal") %>%
    send("signal") %>%
    timeout(1)

  env <- simmer(verbose=TRUE) %>%
    add_generator("dummy", t, at(0, 0, 0)) %>%
    run()
  arr <- get_mon_arrivals(env)

  expect_equal(arr$end_time, c(1, 1, 1))
})

test_that("two consecutive signals execute a single handler", {
  new_timeout <- trajectory() %>%
    timeout(1)

  customer <- trajectory() %>%
    trap("signal", new_timeout) %>%
    timeout(5)

  blocker <- trajectory() %>%
    send("signal") %>%
    send("signal")

  arr <- simmer(verbose=TRUE) %>%
    add_generator("customer", customer, at(0)) %>%
    add_generator("blocker", blocker, at(2)) %>%
    run(10) %>%
    get_mon_arrivals()

  expect_equal(arr$start_time, c(2, 0))
  expect_equal(arr$end_time, c(2, 3))
  expect_equal(arr$activity_time, c(0, 3))
})

test_that("handler linking is done per arrival", {
  new_timeout <- trajectory() %>%
    timeout(10)

  customer <- trajectory() %>%
    trap("signal", new_timeout) %>%
    timeout(5) %>%
    timeout(5)

  signal <- trajectory() %>%
    send("signal")

  arr <- simmer(verbose=TRUE) %>%
    add_generator("customer", customer, at(0, 5)) %>%
    add_generator("signal", signal, at(6)) %>%
    run() %>%
    get_mon_arrivals()

  expect_equal(arr$start_time, c(6, 0, 5))
  expect_equal(arr$end_time, c(6, 16, 21))
  expect_equal(arr$activity_time, c(0, 16, 16))
})

test_that("activity time is correctly retrieved, even with preemption", {
  t <- trajectory() %>%
    timeout(1) %>%
    trap(
      "interrupt",
      handler=trajectory() %>%
        set_attribute("lifetime", function() get_activity_time(env)) %>%
        set_attribute("restime1", function() get_activity_time(env, "resource1")) %>%
        set_attribute("restime2", function() get_activity_time_selected(env)) %>%
        timeout(function() get_attribute(env, "restime1") - 5)
    ) %>%
    seize("resource1") %>%
    timeout(1) %>%
    select("resource2") %>%
    seize_selected() %>%
    timeout(4) %>%
    release_all()

  signal <- trajectory() %>%
    send("interrupt") %>%
    timeout(1) %>%
    send("interrupt")

  env <- simmer(verbose=TRUE) %>%
    add_resource(paste0("resource", 1:2), preemptive=TRUE) %>%
    add_generator("dummy", t, at(0), mon=2) %>%
    add_generator("prio", t, at(2), priority=1) %>%
    add_generator("signal", signal, at(8))
  env <- run(env)

  arr_trj <- subset(get_mon_arrivals(env), name == "dummy0")
  arr_res <- subset(get_mon_arrivals(env, TRUE), name == "dummy0")
  res <- get_mon_resources(env)
  attr <- get_mon_attributes(env)

  expect_equal(arr_trj$start_time, 0)
  expect_equal(arr_trj$end_time, 11)
  expect_equal(arr_trj$activity_time, 6)
  expect_equal(arr_res$start_time, c(1, 2))
  expect_equal(arr_res$end_time, c(11, 11))
  expect_equal(arr_res$activity_time, c(5, 4))
  expect_equal(arr_res$resource, paste0("resource", 1:2))

  expect_equal(res$resource, rep(paste0("resource", 1:2), 4))
  expect_equal(res$time, c(1:4, 8, 8, 11, 11))
  expect_equal(res$server, c(rep(1, 6), 0, 0))
  expect_equal(res$queue, c(0, 0, 1, 1, rep(0, 4)))

  expect_equal(attr$time, rep(c(8, 9), each=3))
  expect_equal(attr$key, rep(c("lifetime", paste0("restime", 1:2)), 2))
  expect_equal(attr$value, c(3:1, 4:2))
})
