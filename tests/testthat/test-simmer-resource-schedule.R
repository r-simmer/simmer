context("resource-schedule")

test_that("capacity & queue size change", {
  inf_sch <- schedule(c(8, 16, 24), c(1, 2, 3), Inf)
  fin_sch <- schedule(c(8, 16, 24), c(1, 2, 3), 24)
  
  limits <- simmer() %>%
    add_resource("dummy", inf_sch) %>%
    run(16) %>% reset() %>% run(48) %>%
    get_mon_resources("limits")
  
  expect_equal(limits$time, c(8, 16, 24))
  expect_equal(limits$server, c(1, 2, 3))
  
  limits <- simmer() %>%
    add_resource("dummy", fin_sch) %>%
    run(16) %>% reset() %>% run(48) %>%
    get_mon_resources("limits")
  
  expect_equal(limits$time, c(8, 16, 24, 32, 40, 48))
  expect_equal(limits$server, c(1, 2, 3, 1, 2, 3))
})

test_that("queue size changes", {
  inf_sch <- schedule(c(8, 16, 24), c(1, 2, 3), Inf)
  fin_sch <- schedule(c(8, 16, 24), c(1, 2, 3), 24)
  
  limits <- simmer() %>%
    add_resource("dummy", 1, inf_sch) %>%
    run(16) %>% reset() %>% run(48) %>%
    get_mon_resources("limits")
  
  expect_equal(limits$time, c(8, 16, 24))
  expect_equal(limits$queue, c(1, 2, 3))
  
  limits <- simmer() %>%
    add_resource("dummy", 1, fin_sch) %>%
    run(16) %>% reset() %>% run(48) %>%
    get_mon_resources("limits")
  
  expect_equal(limits$time, c(8, 16, 24, 32, 40, 48))
  expect_equal(limits$queue, c(1, 2, 3, 1, 2, 3))
})
