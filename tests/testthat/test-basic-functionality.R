context("Basic functionality test")

t1<-
  create_trajectory("my trajectory") %>%
  ## add an intake event 
  add_seize_event("nurse",1.0) %>%
  add_timeout_event("rnorm(1,15)") %>%
  add_release_event("nurse",1.0) %>%
  ## add a consultation event
  add_seize_event("doctor",1.0) %>%
  add_timeout_event("rnorm(1,20)") %>%
  add_release_event("doctor",1.0) %>%
  ## add a planning event
  add_seize_event("administration",1.0) %>%
  add_timeout_event("rnorm(1,5)") %>%
  add_release_event("administration",1.0)



test_that("basic scenario can be simulated with 100 replications", {
  # set-up simulation
  sim<-
    create_simulator("SuperDuperSim", n = 100) %>%
    add_resource("nurse", 1) %>%
    add_resource("doctor", 2) %>%
    add_resource("administration", 1) %>%
    add_entities_with_interval(trajectory = t1, n = 10, name_prefix = "patient", interval =  "rnorm(1, 10, 2)")
  
  expect_that(sim, is_a("Simulator"))
  
  # run the simulation
  sim<-
    sim %>%
    simmer()
  
  expect_that(sim, is_a("Simulator"))
  expect_that(now_(sim@simulators[[1]]), is_more_than(0))
  
})
