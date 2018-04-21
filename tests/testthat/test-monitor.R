context("monitor")

test_that("a CSV monitor creates new files with right headers", {
  mon <- monitor_csv()

  expect_equal(names(mon$handlers), c("arrivals", "releases", "attributes", "resources"))
  expect_output(print(mon), "^simmer monitor:.*disk.*CSV")

  for (name in names(mon$handlers)) {
    expect_true(file.exists(mon$handlers[[name]]))
    expect_equal(dirname(mon$handlers[[name]]), tempdir())
    expect_match(basename(mon$handlers[[name]]), paste0(name, "_.*.csv"))
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
