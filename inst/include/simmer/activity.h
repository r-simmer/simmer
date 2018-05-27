#ifndef simmer__activity_h
#define simmer__activity_h

#include <simmer/common.h>

#define MAX_PRINT_ARGS 4
#define ARG(arg) (#arg": "), arg

namespace simmer {

  /**
   *  Base class.
   */
  class Activity {
  public:
    BASE_CLONEABLE(Activity)

    std::string name;
    int count;
    int priority;

    /**
     * Constructor.
     * @param name          the name of the activity
     * @param resource      the resource associated
     * @param priority      simulation priority
     */
    Activity(const std::string& name, int priority = 0)
      : name(name), count(1), priority(priority), next(NULL), prev(NULL) {}

    Activity(const Activity& o)
      : name(o.name), count(o.count), priority(o.priority), next(NULL), prev(NULL) {}

    virtual ~Activity() {}

    /**
     * Print the activity info.
     * @param indent number of spaces at the beginning of each line
     */
    virtual void print(unsigned int indent = 0, bool verbose = false, bool brief = false) {
      if (brief) return;
      std::ios::fmtflags fmt(Rcpp::Rcout.flags());
      Rcpp::Rcout << IND(indent) <<
        "{ Activity: " << FMT(12, left) << name << " | ";
      if (verbose) Rcpp::Rcout <<
        FMT(9, right) << prev << " <- " <<
        FMT(9, right) << this << " -> " <<
        FMT(9, left)  << next << " | ";
      Rcpp::Rcout.flags(fmt);
    }

    /**
     * Run the activity.
     * @param arrival a pointer to the calling arrival
     */
    virtual double run(Arrival* arrival) = 0;

    /**
     * Getter/setter for the next activity in the chain.
     */
    virtual Activity* get_next() { return next; }
    virtual void set_next(Activity* activity) { next = activity; }

    /**
     * Getter/setter for the previous activity in the chain.
     */
    virtual Activity* get_prev() { return prev; }
    virtual void set_prev(Activity* activity) { prev = activity; }

  protected:
    Activity* next;
    Activity* prev;

    template <typename T>
    T get(const T& var, Arrival* arrival) const { return var; }

    template <typename T>
    T get(const RFn& call, Arrival* arrival) const { return Rcpp::as<T>(call()); }

    template <typename T>
    T get(const Fn<T(Arrival*)>& call, Arrival* arrival) const { return call(arrival); }
  };

  namespace internal {

    inline class Activity* head(const REnv& trajectory) {
      RFn method = trajectory["head"];
      return Rcpp::as<Rcpp::XPtr<Activity> >(method());
    }

    inline class Activity* tail(const REnv& trajectory) {
      RFn method = trajectory["tail"];
      return Rcpp::as<Rcpp::XPtr<Activity> >(method());
    }

    inline int get_n_activities(const REnv& trajectory) {
      RFn method = trajectory["get_n_activities"];
      return Rcpp::as<int>(method());
    }

    inline REnv clone(const REnv& trajectory) {
      RFn method = trajectory["clone"];
      return method();
    }

    inline void print(const REnv& trajectory, unsigned int indent, bool verbose) {
      RFn method = trajectory["print"];
      method(indent, verbose);
    }

    inline void print(bool brief, bool endl) {
      if (!brief) Rcpp::Rcout << " }" << std::endl;
      else if (endl) Rcpp::Rcout << std::endl;
    }

    #define PRINT_ARGS(Z, N, D) BOOST_PP_COMMA_IF(BOOST_PP_ADD(N, D))         \
      BOOST_PP_IF(D,, const char*) BOOST_PP_CAT(n, BOOST_PP_ADD(N, D))        \
      BOOST_PP_COMMA() BOOST_PP_IF(D,, BOOST_PP_CAT(T, BOOST_PP_ADD(N, D))&)  \
      BOOST_PP_CAT(v, BOOST_PP_ADD(N, D))

    #define NO_LAST_ARG(N) BOOST_PP_IF(BOOST_PP_SUB(N, 1), true, false)

    #define PRINT_FUNC(Z, N, D)                                               \
    template<BOOST_PP_ENUM_PARAMS(N, typename T)>                             \
    void print(bool brief, bool endl, BOOST_PP_REPEAT(N, PRINT_ARGS, 0)) {    \
      using simmer::operator<<; // gcc-4.8.4 fails miserably without this     \
      if (!brief) Rcpp::Rcout << n0;                                          \
      Rcpp::Rcout << v0 << (NO_LAST_ARG(N) || (brief && !endl) ? ", " : "");  \
      print(brief, endl BOOST_PP_REPEAT(BOOST_PP_SUB(N, 1), PRINT_ARGS, 1));  \
    }
    BOOST_PP_REPEAT_FROM_TO(1, BOOST_PP_ADD(MAX_PRINT_ARGS, 1), PRINT_FUNC, ~)

  } // namespace internal

} // namespace simmer

#endif
