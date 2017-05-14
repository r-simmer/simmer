library(simmer)

traj <- trajectory() %>%
  timeout(1)

env <- simmer()
add_generator(env, "asdfasdfasdf", traj, function() rexp(100, 1)) %>%
  run(1e7)

rm(env)
gc() # memory deallocated

env <- simmer() %>%
  add_generator("asdfasdfasdf", traj, function() rexp(100, 1)) %>%
  run(1e7)

rm(env)
gc() # memory NOT deallocated

################################################################################

Rcpp::sourceCpp(code='
#include <Rcpp.h>
using namespace Rcpp;

class Bar {
public:
  Bar(Function fun) : fun(fun) {}
private:
  Function fun;
};

class Foo {
public:
  ~Foo() {
    warning("foo destructor called");
    delete map["bar"];
  }
  void add(Function fun) { map["bar"] = new Bar(fun); }
private:
  std::map<std::string, Bar*> map;
};

//[[Rcpp::export]]
SEXP Foo__new() { return XPtr<Foo>(new Foo()); }

//[[Rcpp::export]]
SEXP Foo__add(SEXP foo_, Function fun) {
  XPtr<Foo> foo(foo_);
  foo->add(fun);
  return foo_;
}')

foo <- Foo__new()
foo <- Foo__add(foo, function() 1)
rm(foo)
gc() # destructor called

library(magrittr)

foo <- Foo__new() %>%
  Foo__add(function() 1)
rm(foo)
gc() # ... nothing
