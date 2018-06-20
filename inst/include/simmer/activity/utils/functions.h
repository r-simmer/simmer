// Copyright (C) 2014-2018 Iñaki Ucar and Bart Smeets
//
// This file is part of simmer.
//
// simmer is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// simmer is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with simmer. If not, see <http://www.gnu.org/licenses/>.

#ifndef simmer__activity_utils_functions_h
#define simmer__activity_utils_functions_h

#include <simmer/common.h>

namespace simmer { namespace internal {

  template <typename T>
  Fn<T(T, T)> get_op(char mod) {
    switch(mod) {
    case '+':
      return BIND(std::plus<double>(), _1, _2);
    case '*':
      return BIND(std::multiplies<double>(), _1, _2);
    }
    return NULL;
  }

}} // namespace internal simmer

#endif
