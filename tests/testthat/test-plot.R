context("Plot tests")

t0 <- Trajectory$new("my trajectory") $
  seize("nurse", 1) $
  timeout(function() rnorm(1, 15)) $
  release("nurse", 1) $
  seize("doctor", 1) $
  timeout(function() rnorm(1, 20)) $
  release("doctor", 1) $
  seize("administration", 1) $
  timeout(function() rnorm(1, 5)) $
  release("administration", 1)

test_that("Single replication plot", {
  reps <- Simmer$new() $
    add_resource("nurse", 1) $
    add_resource("doctor", 2) $
    add_resource("administration", 1) $
    add_generator("patient", t0, function() rnorm(1, 10, 2)) $
    run(80)
  
  expect_error(plot_resource_usage(reps, "asdf"))
  expect_is(plot_resource_usage(reps, "doctor"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items="server"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items="server", steps=T), "ggplot")
  expect_is(plot_resource_utilization(reps, "nurse"), "ggplot")
  expect_is(plot_resource_utilization(reps, c("nurse", "doctor","administration")), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type="flow_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type="activity_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type="waiting_time"), "ggplot")
})

test_that("Multiple replication plot", {
  reps <- lapply(1:100, function(i) {
    Simmer$new() $
      add_resource("nurse", 1) $
      add_resource("doctor", 2) $
      add_resource("administration", 1) $
      add_generator("patient", t0, function() rnorm(1, 10, 2)) $
      run(80)
  })
  
  expect_error(plot_resource_usage(reps, "asdf"))
  expect_is(plot_resource_usage(reps, "doctor"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items="server"), "ggplot")
  expect_is(plot_resource_usage(reps, "doctor", items="server", steps=T), "ggplot")
  expect_is(plot_resource_utilization(reps, "nurse"), "ggplot")
  expect_is(plot_resource_utilization(reps, c("nurse", "doctor","administration")), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type="flow_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type="activity_time"), "ggplot")
  expect_is(plot_evolution_arrival_times(reps, type="waiting_time"), "ggplot")
})