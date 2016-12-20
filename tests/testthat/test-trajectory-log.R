context("log")

test_that("a message is correctly printed (1)", {
  t <- trajectory() %>%
    log_("hello world!")

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t, at(5))

  expect_output(run(env), "5: entity0: hello world!")
})

test_that("a message is correctly printed (2)", {
  t <- trajectory() %>%
    log_(function() "hello world!")

  env <- simmer(verbose = TRUE) %>%
    add_generator("entity", t, at(5))

  expect_output(run(env), "5: entity0: hello world!")
})
