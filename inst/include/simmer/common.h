#ifndef simmer__common_h
#define simmer__common_h

#include <Rcpp.h>

#define RFn   Rcpp::Function
#define REnv  Rcpp::Environment
#define RData Rcpp::DataFrame
#define RNum  Rcpp::NumericVector
#define RStr  Rcpp::CharacterVector
#define RBool Rcpp::LogicalVector

#include <boost/container/set.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>
#include <map>
#include <vector>

#define MSET  boost::container::multiset
#define USET  boost::unordered_set
#define UMAP  boost::unordered_map
#define MAP   std::map
#define VEC   std::vector

#include <boost/optional.hpp>
#include <boost/typeof/typeof.hpp>
#include <boost/any.hpp>

#define OPT   boost::optional
#define NONE  boost::none
#define ANY   boost::any

#include <boost/foreach.hpp>

#define foreach_    BOOST_FOREACH
#define foreach_r_  BOOST_REVERSE_FOREACH

#include <boost/function.hpp>
#include <boost/bind.hpp>

#define Fn    boost::function
#define BIND  boost::bind

#define FMT(n, justify) std::setw(n) << std::justify
#define IND(n) std::string(n, ' ')

#define PRIORITY_MAX            std::numeric_limits<int>::min()
#define PRIORITY_RELEASE        -6
#define PRIORITY_MANAGER        -5
#define PRIORITY_RELEASE_POST   -4
#define PRIORITY_SEND           -3
#define PRIORITY_SIGNAL         -2
#define PRIORITY_TRAP           -1
#define PRIORITY_MIN            std::numeric_limits<int>::max()

#define SUCCESS    0
#define ENQUEUE   -1
#define REJECT    -2
#define BLOCK     std::numeric_limits<double>::infinity()

#define COMMA ,

#define BASE_CLONEABLE(Type) virtual Type* clone() const = 0;
#define CLONEABLE(Type) virtual Type* clone() const { return new Type(*this); }

namespace simmer {

  typedef UMAP<std::string, double> Attr;

  template <typename T>
  struct vec_of : public VEC<T> {
    vec_of(const T& t) { (*this)(t); }
    vec_of& operator()(const T& t) {
      this->push_back(t);
      return *this;
    }
  };

  template <typename T>
  std::ostream& operator<<(std::ostream& out, const VEC<T>& v) {
    out << "[";
    if (!v.empty())
      std::copy(v.begin(), v.end(), std::ostream_iterator<T>(out, ", "));
    out << "\b\b]";
    return out;
  }

  inline std::ostream& operator<<(std::ostream& out, const RData& df) {
    out << "data.frame";
    return out;
  }

  inline std::ostream& operator<<(std::ostream& out, const RFn& fn) {
    out << "function()";
    return out;
  }

  inline std::ostream& operator<<(std::ostream& out, const REnv& env) {
    out << "function()";
    return out;
  }

  template <typename T, typename U, typename V>
  class FnWrap {
  public:
    FnWrap() {}
    FnWrap(const Fn<T(U)>& call, const V& arg) : call(call), arg(arg) {}

    T operator()(U param) { return call(param); }

    friend std::ostream& operator<<(std::ostream& out, const FnWrap<T, U, V>& fn) {
      out << fn.arg;
      return out;
    }

  private:
    Fn<T(U)> call;
    V arg;
  };

  class MakeString {
    std::ostringstream stream;
  public:
    operator std::string() const { return stream.str(); }
    template<class T>
    MakeString& operator<<(const T& v) { stream << v; return *this; }
  };

} // namespace simmer

#endif
