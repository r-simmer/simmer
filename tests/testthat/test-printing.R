# Copyright (C) 2017-2018 Iñaki Ucar
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

test_that("print methods return the objects invisibly", {
  expect_output(env1 <- print(simmer()))
  expect_output(env2 <- print(wrap(simmer())))
  expect_output(traj <- print(trajectory()))
  expect_output(schd <- print(schedule(c(8, 16, 24), c(3, 2, 1), period=24)))

  expect_is(env1, "simmer")
  expect_is(env2, "wrap")
  expect_is(traj, "trajectory")
  expect_is(schd, "schedule")
})
