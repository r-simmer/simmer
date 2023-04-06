# Copyright (C) 2018-2023 Iñaki Ucar
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

test_that("delim monitors create new files with right headers", {
  expect_error(monitor_csv(path="asdf"))
  mon <- monitor_csv()

  expect_equal(names(mon$handlers), c("arrivals", "releases", "attributes", "resources"))
  expect_output(print(mon), "^simmer monitor:.*disk.*delimited")

  expect_true(all(file.exists(unlist(mon$handlers))))
  expect_equal(unique(dirname(unlist(mon$handlers))), gsub("\\\\", "/", tempdir()))
  expect_match(basename(unlist(mon$handlers)), "\\.csv$")

  arr <- mon$get_arrivals(FALSE)
  rel <- mon$get_arrivals(TRUE)
  atr <- mon$get_attributes()
  res <- mon$get_resources()

  expect_is(arr, "data.frame")
  expect_is(rel, "data.frame")
  expect_is(atr, "data.frame")
  expect_is(res, "data.frame")
  expect_equal(nrow(arr), 0)
  expect_equal(nrow(rel), 0)
  expect_equal(nrow(atr), 0)
  expect_equal(nrow(res), 0)
  expect_equal(colnames(arr), c("name", "start_time", "end_time", "activity_time", "finished"))
  expect_equal(colnames(rel), c("name", "start_time", "end_time", "activity_time", "resource"))
  expect_equal(colnames(atr), c("time", "name", "key", "value"))
  expect_equal(colnames(res), c("resource", "time", "server", "queue", "capacity", "queue_size"))
})

test_that("delim monitors honor 'keep' argument upon destruction", {
  mon_keep <- monitor_csv(keep=TRUE)
  mon_del <- monitor_csv(keep=FALSE)

  files_keep <- unlist(mon_keep$handlers)
  files_del <- unlist(mon_del$handlers)

  expect_true(all(file.exists(files_keep)))
  expect_true(all(file.exists(files_del)))
  rm(mon_keep, mon_del); invisible(gc())
  expect_true(all(file.exists(files_keep)))
  expect_true(all(!file.exists(files_del)))
  unlink(files_keep)
})

test_that("delim monitors collect the same values as in-memory monitors", {
  set.seed(1234)

  t <- trajectory() %>%
    timeout(rexp(1)) %>%
    set_attribute("attr", rexp(1)) %>%
    seize("res") %>%
    timeout(rexp(1)) %>%
    release("res")

  arrivals <- cumsum(rexp(10))

  env_mem <- simmer(verbose = env_verbose, mon=monitor_mem()) %>%
    add_resource("res", 1, 10) %>%
    add_generator("dummy", t, at(arrivals), mon=2) %>%
    run()

  mon <- monitor_csv()
  env_csv <- simmer(verbose = env_verbose, mon=mon) %>%
    add_resource("res", 1, 10) %>%
    add_generator("dummy", t, at(arrivals), mon=2) %>%
    run()

  # do not compare the 'finished' column
  expect_equal(get_mon_arrivals(env_csv)[,-5], get_mon_arrivals(env_mem)[,-5], tolerance=1e-6)
  expect_equal(get_mon_arrivals(env_csv, TRUE), get_mon_arrivals(env_mem, TRUE), tolerance=1e-6)
  expect_equal(get_mon_attributes(env_csv), get_mon_attributes(env_mem), tolerance=1e-6)
  expect_equal(get_mon_resources(env_csv), get_mon_resources(env_mem), tolerance=1e-6)

  arr <- read.csv(mon$handlers[["arrivals"]], stringsAsFactors=FALSE)
  rel <- read.csv(mon$handlers[["releases"]], stringsAsFactors=FALSE)
  atr <- read.csv(mon$handlers[["attributes"]], stringsAsFactors=FALSE)
  res <- read.csv(mon$handlers[["resources"]], stringsAsFactors=FALSE)

  expect_equal(get_mon_arrivals(env_csv)[,seq_len(ncol(arr))], arr)
  expect_equal(get_mon_arrivals(env_csv, TRUE)[,seq_len(ncol(rel))], rel)
  expect_equal(get_mon_attributes(env_csv)[,seq_len(ncol(atr))], atr)
  expect_equal(get_mon_resources(env_csv)[,seq_len(ncol(res))], res)

  expect_output(print(reset(env_csv)), "^.*Monitor:.*disk.*delimited")

  expect_equal(nrow(get_mon_arrivals(env_csv)), 0)
  expect_equal(nrow(get_mon_arrivals(env_csv, TRUE)), 0)
  expect_equal(nrow(get_mon_attributes(env_csv)), 0)
  expect_equal(nrow(get_mon_resources(env_csv)), 0)

  run(env_csv)

  expect_equal(get_mon_arrivals(env_csv)[,seq_len(ncol(arr))], arr)
  expect_equal(get_mon_arrivals(env_csv, TRUE)[,seq_len(ncol(rel))], rel)
  expect_equal(get_mon_attributes(env_csv)[,seq_len(ncol(atr))], atr)
  expect_equal(get_mon_resources(env_csv)[,seq_len(ncol(res))], res)
})
