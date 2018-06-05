context("log")

test_that("messages are correctly printed depending on the log_level", {
  t <- trajectory() %>%
    log_(function() "Message 0") %>%
    log_("Message 1", level=1) %>%
    log_("Message 2", level=2) %>%
    log_("Message 3", level=Inf)

  expect_output(
    simmer(verbose=TRUE) %>%
      add_generator("dummy", t, at(5)) %>%
      run(),
    "5: dummy0: Message 0"
  )

  expect_output(
    simmer(verbose=TRUE, log_level=1) %>%
      add_generator("dummy", t, at(5)) %>%
      run(),
    "5: dummy0: Message 0.*5: dummy0: Message 1"
  )

  expect_output(
    simmer(verbose=TRUE, log_level=5) %>%
      add_generator("dummy", t, at(5)) %>%
      run(),
    "5: dummy0: Message 0.*5: dummy0: Message 1.*5: dummy0: Message 2"
  )

  expect_output(
    simmer(verbose=TRUE, log_level=Inf) %>%
      add_generator("dummy", t, at(5)) %>%
      run(),
    "5: dummy0: Message 0.*5: dummy0: Message 1.*5: dummy0: Message 2.*5: dummy0: Message 3"
  )
})
