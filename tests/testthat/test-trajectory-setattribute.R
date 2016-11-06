context("set attributes")

test_that("only valid types can be passed to functions", {
  expect_error(create_trajectory() %>% set_attribute("test", "string_value"))
  expect_error(create_trajectory() %>% set_attribute("test", 3), NA)
  expect_error(create_trajectory() %>% set_attribute("test", function() 3), NA)

})

test_that("an arrival attribute is correctly set and returned to a function", {
  t0 <- create_trajectory() %>%
    set_attribute("test", 123) %>%
    set_attribute("test", 456, global = TRUE) %>%
    log_(function(attrs) paste0(attrs["test"])) %>%
    log_(function(attrs, glb) paste0(attrs["test"], glb["test"]))

  expect_output({
    simmer(verbose = TRUE) %>%
      add_generator("entity", t0, at(0)) %>%
      run()
    },
    ".*123.*123456"
  )
})

test_that("attributes can be correctly retrieved using get_mon_attributes()", {
  t0 <- create_trajectory() %>%
    set_attribute("test", function() 123) %>%
    set_attribute("test", function() 456, global = TRUE)

  env <-
    simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0), mon = 2) %>%
    run()

  attributes <- env %>% get_mon_attributes()

  expect_equal(nrow(attributes), 2)
  expect_equal(attributes[1, ]$name, "entity0")
  expect_equal(attributes[1, ]$key, "test")
  expect_equal(attributes[1, ]$value, 123)
  expect_equal(attributes[2, ]$name, "")
  expect_equal(attributes[2, ]$key, "test")
  expect_equal(attributes[2, ]$value, 456)
})

test_that("the attribute dataframe is returned with the expected columns", {
  t0 <- create_trajectory() %>%
    set_attribute("test", 123) %>%
    set_attribute("test", 456, global = TRUE)

  env <-
    simmer(verbose = TRUE) %>%
    add_generator("entity", t0, at(0), mon = 1) %>%
    run()

  attributes <- env %>% get_mon_attributes()

  expect_true(all(sapply(colnames(attributes), function(x) x %in% colnames(attributes))))
})


test_that("arrival attributes are returned empty when mon level is <2", {
  t0 <- create_trajectory() %>%
    set_attribute("test", 123) %>%
    set_attribute("test", 456, global = TRUE)

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
