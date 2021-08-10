# Copyright (C) 2016,2018 Iñaki Ucar
#
# This file is part of simmer.
#
# simmer is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# simmer is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with simmer. If not, see <http://www.gnu.org/licenses/>.

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
