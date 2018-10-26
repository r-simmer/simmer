# Copyright (C) 2018 Iñaki Ucar
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

context("global")

test_that("globals are correctly initialised and managed", {
  inf_sch <- schedule(c(8, 16, 24), c(1, 2, 3), Inf)
  fin_sch <- schedule(c(8, 16, 24), c(1, 2, 3), 24)

  env <- simmer(verbose=TRUE) %>%
    add_global("a_fixed", 5.5) %>%
    add_global("b_inf_sch", inf_sch) %>%
    add_global("c_fin_sch", fin_sch)

  expect_equal(get_global(env, "a_fixed"), 5.5)
  expect_equal(get_global(env, "b_inf_sch"), 0)
  expect_equal(get_global(env, "c_fin_sch"), 3)

  run(env, 10)
  expect_equal(get_global(env, "a_fixed"), 5.5)
  expect_equal(get_global(env, "b_inf_sch"), 1)
  expect_equal(get_global(env, "c_fin_sch"), 1)

  attr <- get_mon_attributes(env)
  attr <- attr[order(attr$time, attr$key),]
  expect_equal(attr$time, c(0, 0, 0, 8, 8))
  expect_equal(attr$key, c("a_fixed", rep(c("b_inf_sch", "c_fin_sch"), 2)))
  expect_equal(attr$value, c(5.5, 0, 3, 1, 1))

  reset(env)
  attr2 <- get_mon_attributes(env)
  attr2 <- attr2[order(attr2$time, attr2$key),]
  expect_equal(get_global(env, "a_fixed"), 5.5)
  expect_equal(get_global(env, "b_inf_sch"), 0)
  expect_equal(get_global(env, "c_fin_sch"), 3)
  expect_equal(attr2, attr[1:3,])
})
