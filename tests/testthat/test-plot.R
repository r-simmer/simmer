context("plot")

t0 <- create_trajectory("my trajectory") %>%
  seize("nurse", 1) %>%
  timeout(function() rnorm(1, 15)) %>%
  release("nurse", 1) %>%
  seize("doctor", 1) %>%
  timeout(function() rnorm(1, 20)) %>%
  release("doctor", 1) %>%
  seize("administration", 1) %>%
  timeout(function() rnorm(1, 5)) %>%
  release("administration", 1)

test_that("single replication plots", {
  reps <- simmer() %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
    run(80)

  expect_is(plot_resource_usage(reps, "doctor"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items = "server"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items = "server", steps = T), "ggplot")
  expect_is(plot_resource_utilization(reps, "nurse"), "ggplot")
  expect_is(plot_resource_utilization(reps, c("nurse", "doctor", "administration")), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type = "flow_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type = "activity_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type = "waiting_time"), "ggplot")
})

test_that("multiple replication plots", {
  reps <- lapply(1:100, function(i) {
    simmer() %>%
      add_resource("nurse", 1) %>%
      add_resource("doctor", 2) %>%
      add_resource("administration", 1) %>%
      add_generator("patient", t0, function() rnorm(1, 10, 2)) %>%
      run(80)
  })

  expect_is(plot_resource_usage(reps, "doctor"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items = "server"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items = "server", steps = T), "ggplot")
  expect_is(plot_resource_utilization(reps, "nurse"), "ggplot")
  expect_is(plot_resource_utilization(reps, c("nurse", "doctor", "administration")), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type = "flow_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type = "activity_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type = "waiting_time"), "ggplot")
})


test_that("attributes are plottable", {
  t0 <-
    create_trajectory() %>%
    set_attribute("my_attr1", function() runif(1)) %>%
    set_attribute("my_attr2", function() runif(1))

  reps <- lapply(1:100, function(i) {
    simmer() %>%
      add_generator("frog", t0, function() rnorm(1, 10, 2), mon = 2) %>%
      run(80)
  })

  expect_is(plot_attributes(reps, "my_attr1"), "ggplot")
  expect_is(plot_attributes(reps), "ggplot")
})
