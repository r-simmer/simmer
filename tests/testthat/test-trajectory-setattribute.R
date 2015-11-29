context("set attributes")

test_that("only valid types can be passed to functions", {
  expect_error(create_trajectory()%>%set_attribute("test", "string_value"))
  expect_error(not(create_trajectory()%>%set_attribute("test", 3)))
  expect_error(not(create_trajectory()%>%set_attribute("test", function() 3)))
  
})

test_that("an attribute is correctly set and returned to a function that needs it", {
  
  t0 <- create_trajectory() %>%
    set_attribute("test", 123) %>%
    timeout(function(attrs) print(attrs[["test"]]))
  
  
  expect_output({
    simmer() %>%
      add_generator("entity", t0, at(0)) %>%
      run()
  }, "\\[1\\] 123")
  
})

test_that("attributes can be correctly retrieved using get_mon_attributes()", {
  t0 <- create_trajectory() %>%
    set_attribute("test", 123)
  
  env<-
    simmer() %>%
    add_generator("entity", t0, at(0), mon=2) %>%
    run()
  
  attributes <- env %>% get_mon_attributes()
  
  expect_equal(nrow(attributes), 1)
  expect_equal(attributes[1,]$key, factor("test"))
  expect_equal(attributes[1,]$value, 123)
})
