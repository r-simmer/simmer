context("set attributes")

test_that("only valid types can be passed to functions", {
  expect_error(trajectory() %>% set_attribute("test", "string_value"))
  expect_error(trajectory() %>% set_attribute("test", NA))
  expect_error(trajectory() %>% set_attribute(NA, 1))

  t0 <- trajectory() %>%
    set_attribute(c("a", "b", "c"), c(1, 2))

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0))

  expect_error(run(env))
})

test_that("an arrival attribute is correctly set and returned to a function", {
  t0 <- trajectory() %>%
    set_attribute(
      c("att1", "att2"),
      c(1, 2)
    ) %>%
    timeout(1) %>%
    set_global(
      function() c("glb1", "glb2"),
      c(1, 2)
    ) %>%
    timeout(1) %>%
    set_attribute(
      c("att3", "att4"),
      function() get_attribute(env, c("att1", "att2")) + 2
    ) %>%
    timeout(1) %>%
    set_global(
      function() c("glb3", "glb4"),
      function() get_global(env, c("glb1", "glb2")) + 2
    ) %>%
    log_(function()
      paste0(get_attribute(env, "att1"),
             get_attribute(env, "asdf"),
             get_global(env, "glb1"),
             get_global(env, "asdf")))

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0), mon=2)

  expect_output(run(env), ".*1NA1NA")

  attributes <- env %>% get_mon_attributes()

  expect_equal(nrow(attributes), 8)
  expect_equal(attributes$time, rep(0:3, each=2))
  expect_equal(attributes$name, rep(c("entity0", ""), each=2, times=2))
  values <- c(1, 2, 1, 2, 3, 4, 3, 4)
  expect_equal(attributes$key, paste0(rep(c("att", "glb"), each=2, times=2), values))
  expect_equal(attributes$value, values)
})

test_that("the attribute dataframe is returned with the expected columns", {
  t0 <- trajectory() %>%
    set_attribute("test", 123) %>%
    set_global("test", 456)

  env <-
    simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0), mon = 1) %>%
    run()

  attributes <- env %>% get_mon_attributes()

  expect_true(all(sapply(colnames(attributes), function(x) x %in% colnames(attributes))))
})


test_that("arrival attributes are returned empty when mon level is < 2", {
  t0 <- trajectory() %>%
    set_attribute("test", 123) %>%
    set_global("test", 456)

  env <-
    simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0), mon = 1) %>%
    run()

  attributes <- env %>% get_mon_attributes()

  expect_equal(nrow(attributes), 1)
  expect_equal(attributes[1, ]$name, "")
  expect_equal(attributes[1, ]$key, "test")
  expect_equal(attributes[1, ]$value, 456)
})
