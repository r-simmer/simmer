// Copyright (C) 2020 Iñaki Ucar
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

#ifndef simmer__activity_storage_h
#define simmer__activity_storage_h

#include <simmer/activity.h>
#include <simmer/process/arrival.h>

namespace simmer {

  namespace internal {

    template <typename U>
    U storage_key(Arrival* arrival);

    template <>
    inline Arrival* storage_key<Arrival*>(Arrival* arrival) {
      return arrival;
    }

    template <>
    inline std::string storage_key<std::string>(Arrival* arrival) {
      return arrival->name;
    }

  } // namespace internal

  // abstract class for activities with state
  template <typename U, typename V>
  class Storage : public virtual Activity {
  public:
    Storage() {}

    void remove(Arrival* arrival) {
      typename UMAP<U, V>::iterator search =
        storage.find(internal::storage_key<U>(arrival));
      if (search == storage.end())
        Rcpp::stop("illegal removal of arrival '%s'", arrival->name); // # nocov
      storage.erase(search);
      arrival->unregister_entity(this, boost::is_same<U, std::string>::value);
    }

  protected:
    bool storage_find(Arrival* arrival) {
      if (storage.find(internal::storage_key<U>(arrival)) == storage.end())
        return false;
      return true;
    }

    V& storage_get(Arrival* arrival) {
      if (!storage_find(arrival))
        arrival->register_entity(this, boost::is_same<U, std::string>::value);
      return storage[internal::storage_key<U>(arrival)];
    }

  private:
    UMAP<U, V> storage;
  };

} // namespace simmer

#endif
