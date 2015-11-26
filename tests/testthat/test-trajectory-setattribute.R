context("set attributes")

test_that("only valid types can be passed to functions", {
  expect_error(create_trajectory()%>%set_attribute("test", "string_value"))
  expect_error(not(create_trajectory()%>%set_attribute("test", 3)))
  expect_error(not(create_trajectory()%>%set_attribute("test", function() 3)))
  
})

test_that("an attribute is correctly set and returned to a function that needs it", {
  ## a bit hackish > to refactor in the future 
  temp_file<-file()
  
  sink(temp_file)
  
  t0 <- create_trajectory() %>%
    set_attribute("test", 123) %>%
    timeout(function(attrs) print(attrs[["test"]]))
  
  env <- simmer() %>%
    add_generator("entity", t0, at(0)) %>%
    run()
  
  sink()
  
  expect_equal(readLines(temp_file), "[1] 123")
  close(temp_file)
})
