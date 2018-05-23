#ifndef simmer__activity_utils_resgetter_h
#define simmer__activity_utils_resgetter_h

#include <simmer/simulator.h>

namespace simmer { namespace internal {

  // abstract class for resource retrieval
  class ResGetter {
  public:
    BASE_CLONEABLE(ResGetter)

    ResGetter(const std::string& activity, const std::string& resource, int id = -1)
      : resource(resource), id(id), activity(activity) {}

  protected:
    std::string resource;
    int id;

    Resource* get_resource(Arrival* arrival) const {
      Resource* selected = NULL;
      if (id < 0)
        selected = arrival->sim->get_resource(resource);
      else selected = arrival->get_resource_selected(id);
      if (!selected)
        Rcpp::stop("%s: %s(%s, %i): no resource selected", arrival->name, activity, resource, id);
      return selected;
    }

  private:
    std::string activity;
  };

}} // namespace internal simmer

#endif
