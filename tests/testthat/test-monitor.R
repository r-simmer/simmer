context("monitor")

test_that("delim monitors create new files with right headers", {
  expect_error(monitor_csv(path="asdf"))
  mon <- monitor_csv()

  expect_equal(names(mon$handlers), c("arrivals", "releases", "attributes", "resources"))
  expect_output(print(mon), "^simmer monitor:.*disk.*delimited")

  for (name in names(mon$handlers)) {
    expect_true(file.exists(mon$handlers[[name]]))
    expect_equal(dirname(mon$handlers[[name]]), tempdir())
    expect_match(basename(mon$handlers[[name]]), paste0(name, ".csv"))
  }

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

test_that("delim monitors collect the same values as in-memory monitors", {
  set.seed(1234)

  t <- trajectory() %>%
    timeout(rexp(1)) %>%
    set_attribute("attr", rexp(1)) %>%
    seize("res") %>%
    timeout(rexp(1)) %>%
    release("res")

  arrivals <- cumsum(rexp(10))

  env_mem <- simmer(verbose=TRUE, mon=monitor_mem()) %>%
    add_resource("res", 1, 10) %>%
    add_generator("dummy", t, at(arrivals), mon=2) %>%
    run()

  mon <- monitor_csv()
  env_csv <- simmer(verbose=TRUE, mon=mon) %>%
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

  reset(env_csv)

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
