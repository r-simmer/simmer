context("Basic functionality test")

t1<-
  read.table(header=T, text=
               "event_id description resource amount duration successor
                1 intake nurse 1 rnorm(1,10) 2
                2 consult doctor 1 rnorm(1,10) 3
                3 administration admin 1 rnorm(1,10) NA"
  )



test_that("basic scenario can be simulated with one replication", {
  # set-up simulation
  sim <-
    create_simulator() %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 1) %>%
    add_resource("admin", 1) %>%
    add_trajectory("basic_track", t1) %>%
    add_entities_with_interval(10, "test", "basic_track", 5)
  
  expect_that(sim, is_a("Simulator"))
  
  # run the simulation
  sim<-
    sim %>%
    simmer()
  
  expect_that(sim, is_a("Simulator"))
  expect_that(sim$now(), is_more_than(0))
  expect_equal(all(unlist(sim$entities_current_event)), NA)
  
})

test_that("basic scenario can be simulated with one replication & until < regular runtime", {
  # set-up simulation
  sim <-
    create_simulator() %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 1) %>%
    add_resource("admin", 1) %>%
    add_trajectory("basic_track", t1) %>%
    add_entities_with_interval(10, "test", "basic_track", 5)
  
  expect_that(sim, is_a("Simulator"))
  
  # run the simulation
  sim<-
    sim %>%
    simmer(20)
  
  expect_that(sim, is_a("Simulator"))
  expect_that(sim$now(), is_more_than(0))
  
})


test_that("basic scenario can be simulated with >1 replication", {
  # set-up simulation
  sim <-
    create_simulator() %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 1) %>%
    add_resource("admin", 1) %>%
    add_trajectory("basic_track", t1) %>%
    add_entities_with_interval(10, "test", "basic_track", 5) %>%
    replicator(2)
  
  expect_that(sim, is_a("ReplicationContainer"))
  
  # run the simulation
  sim<-
    sim %>%
    simmer()
  
  expect_that(sim, is_a("ReplicationContainer"))
  expect_that(min(sapply(sim$simulators, function(obj) obj$now())), is_more_than(0))
  expect_equal(all(unlist(sapply(sim$simulators, function(obj) all(unlist(obj$entities_current_event))))), NA)
  
})