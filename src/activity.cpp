#include "activity.h"

int Batch::count = 0;
BatchedMap Batch::pool = BatchedMap();

template <>
void SetPrior<VEC<int> >::print(int indent, bool brief) {
  if (values.size() != 3) Rcpp::stop("%s: 3 values needed", name);
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "values: " << 
    values[0] << " " << values[1] << " " << values[2] << " }" << std::endl;
  else Rcpp::Rcout << values[0] << " " << values[1] << " " << values[2] << std::endl;
}

template <>
void SetPrior<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "values: " << values << " }" << std::endl;
  else Rcpp::Rcout << values << std::endl;
}

template <>
void Select<VEC<std::string> >::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << 
    "resources: " << resources[0] << (resources.size()>1 ? ", ..." : "") << " | " << 
      "policy: " << policy << " }" << std::endl;
  else {
    if (resources.size() > 1)
      Rcpp::Rcout << resources.size() << " options" << std::endl;
    else Rcpp::Rcout << resources[0] << std::endl;
  }
}

template <>
void Select<Rcpp::Function>::print(int indent, bool brief) {
  Activity::print(indent, brief);
  if (!brief) Rcpp::Rcout << "resources: " << resources << " }" << std::endl;
  else Rcpp::Rcout << resources << std::endl;
}
